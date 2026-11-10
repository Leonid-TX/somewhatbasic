// liboffice.cpp - Native C++ Office Module for sbasic
// Supports Sparse Unlimited Storage & Styled ZIP .ods / .xlsx / .odt / .docx / .csv / .txt

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <algorithm>
#include <cctype>
#include <iomanip>
#include <stdexcept>
#include <cstdint>
#include <limits>
#include <cstdio>

// ============================================================================
// ABI-COMPATIBLE VARIANT & ERROR TYPES (Matches sbasic.cpp & linalg.cpp)
// ============================================================================
enum class VarType { EMPTY, INTEGER, DOUBLE, STRING, ARRAY, ERRVAL };

struct DimBound {
    int low = 0;
    int high = 0;
    int size() const { return high - low + 1; }
};

struct Variant {
    VarType type = VarType::EMPTY;
    VarType declaredType = VarType::EMPTY;
    int64_t iVal = 0;
    double dVal = 0.0;
    std::string sVal;
    std::vector<DimBound> dims;
    std::vector<Variant> arrayData;

    Variant() = default;
    static Variant fromInt(int64_t v) { Variant x; x.type = VarType::INTEGER; x.iVal = v; x.dVal = static_cast<double>(v); return x; }
    static Variant fromDouble(double v) { Variant x; x.type = VarType::DOUBLE; x.dVal = v; x.iVal = static_cast<int64_t>(std::llround(v)); return x; }
    static Variant fromString(const std::string &v) { Variant x; x.type = VarType::STRING; x.sVal = v; return x; }
    static Variant fromError(int64_t code, const std::string &msg = "") {
        Variant x; x.type = VarType::ERRVAL; x.iVal = code; x.dVal = static_cast<double>(code);
        x.sVal = msg.empty() ? ("Error " + std::to_string(code)) : msg;
        return x;
    }

    static Variant makeArray(const std::vector<DimBound> &bounds, VarType elemType = VarType::EMPTY) {
        Variant x;
        x.type = VarType::ARRAY;
        x.declaredType = elemType;
        x.dims = bounds;
        size_t total = 1;
        for (const auto &b : bounds) {
            if (b.size() < 0) throw std::runtime_error("Invalid array dimension bounds.");
            total *= static_cast<size_t>(b.size());
        }
        Variant initElem;
        initElem.declaredType = elemType;
        if (elemType == VarType::INTEGER) initElem.type = VarType::INTEGER;
        else if (elemType == VarType::DOUBLE) initElem.type = VarType::DOUBLE;
        else if (elemType == VarType::STRING) initElem.type = VarType::STRING;
        x.arrayData.assign(total, initElem);
        return x;
    }

    double toDouble() const {
        if (type == VarType::DOUBLE) return dVal;
        if (type == VarType::INTEGER || type == VarType::ERRVAL) return static_cast<double>(iVal);
        if (type == VarType::STRING) {
            try { return std::stod(sVal); } catch (...) { return 0.0; }
        }
        return 0.0;
    }

    int64_t toInt() const {
        if (type == VarType::INTEGER || type == VarType::ERRVAL) return iVal;
        if (type == VarType::DOUBLE) return static_cast<int64_t>(std::llround(dVal));
        if (type == VarType::STRING) {
            try { return std::stoll(sVal); } catch (...) { return 0; }
        }
        return 0;
    }

    std::string toString() const {
        if (type == VarType::STRING || type == VarType::ERRVAL) return sVal;
        if (type == VarType::INTEGER) return std::to_string(iVal);
        if (type == VarType::DOUBLE) {
            std::ostringstream oss;
            oss << std::setprecision(12) << dVal;
            return oss.str();
        }
        if (type == VarType::ARRAY) return "[Array]";
        return "";
    }

    bool toBool() const {
        if (type == VarType::STRING) return !sVal.empty();
        return toDouble() != 0.0;
    }

    void assignCoerced(const Variant &rhs) {
        if (rhs.type == VarType::ARRAY || rhs.type == VarType::ERRVAL) { *this = rhs; return; }
        if (declaredType == VarType::INTEGER) {
            type = VarType::INTEGER;
            iVal = rhs.toInt();
            dVal = static_cast<double>(iVal);
        } else if (declaredType == VarType::DOUBLE) {
            type = VarType::DOUBLE;
            dVal = rhs.toDouble();
            iVal = static_cast<int64_t>(std::llround(dVal));
        } else if (declaredType == VarType::STRING) {
            type = VarType::STRING;
            sVal = rhs.toString();
        } else {
            type = rhs.type;
            iVal = rhs.iVal;
            dVal = rhs.dVal;
            sVal = rhs.sVal;
        }
    }

    Variant& at(const std::vector<int> &indices) {
        if (type != VarType::ARRAY) throw std::runtime_error("Variable is not an array.");
        if (indices.size() != dims.size()) throw std::runtime_error("Array dimension mismatch.");
        size_t flat = 0, stride = 1;
        for (int k = static_cast<int>(dims.size()) - 1; k >= 0; --k) {
            int idx = indices[k];
            if (idx < dims[k].low || idx > dims[k].high)
                throw std::runtime_error("Array index out of bounds: " + std::to_string(idx));
            flat += static_cast<size_t>(idx - dims[k].low) * stride;
            stride *= static_cast<size_t>(dims[k].size());
        }
        return arrayData[flat];
    }

    const Variant& at(const std::vector<int> &indices) const {
        return const_cast<Variant*>(this)->at(indices);
    }
};

struct BasicRuntimeError : public std::runtime_error {
    int code;
    int line;
    std::string sourceCodeLine;
    std::string badToken;
    std::string suggestion;

    BasicRuntimeError(int c, const std::string &msg, int ln = 0,
                      const std::string &src = "", const std::string &tok = "",
                      const std::string &sug = "")
        : std::runtime_error(msg), code(c), line(ln),
          sourceCodeLine(src), badToken(tok), suggestion(sug) {}
};

// ============================================================================
// NATIVE OFFICE IMPLEMENTATION (.ODS / .XLSX / .CSV + .ODT / .DOCX / .TXT)
// ============================================================================
namespace LibOffice {

    static constexpr int MAX_ROWS = std::numeric_limits<int>::max() - 1;
    static constexpr int MAX_COLS = std::numeric_limits<int>::max() - 1;

    struct CellSlot {
        int type = 0; // 0 = empty, 1 = numeric (double), 2 = string
        Variant val;
    };

    struct SheetSlot {
        std::string name;
        int maxRow = 0;
        int maxCol = 0;
        std::map<int, std::map<int, CellSlot>> rows;

        void clear() {
            rows.clear();
            maxRow = 0;
            maxCol = 0;
        }

        const CellSlot* getCell(int r, int c) const {
            auto rIt = rows.find(r);
            if (rIt == rows.end()) return nullptr;
            auto cIt = rIt->second.find(c);
            if (cIt == rIt->second.end() || cIt->second.type == 0) return nullptr;
            return &cIt->second;
        }

        void eraseCell(int r, int c) {
            auto rIt = rows.find(r);
            if (rIt != rows.end()) {
                rIt->second.erase(c);
                if (rIt->second.empty()) rows.erase(rIt);
            }
        }
    };

    struct DocParagraph {
        std::string text;
        int headingLevel = 0;   // 0 = body/custom style, 1..6 = Heading 1..6
        std::string styleName;  // Canonical style name (e.g. "Text Body", "Heading 1", "Title", "Subtitle", "Quotations", "Preformatted Text")
    };

    // Spreadsheet state
    static int gODS_Initialized = 0;
    static int gODS_SheetCount  = 0;
    static int gODS_ActiveSheet = 0;
    static std::string gODS_FilePath;

    static std::vector<SheetSlot> gODS_Sheets(2);

    static int gODS_RangeLeft   = 0;
    static int gODS_RangeTop    = 0;
    static int gODS_RangeRight  = 0;
    static int gODS_RangeBottom = 0;

    static int gODS_SearchCol = 0;
    static int gODS_SearchRow = -1;

    static int CellAddress_Sheet  = 0;
    static int CellAddress_Column = 0;
    static int CellAddress_Row    = 0;
    static std::string CellAddress_AbsoluteName;

    // Document (.odt / .docx / .txt) state
    static int gODT_Initialized = 0;
    static std::string gODT_FilePath;
    static std::vector<DocParagraph> gODT_Paragraphs;
    static int gODT_SearchIndex = -1;

    static std::string toUpperStr(std::string s) {
        for (char &c : s) c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        return s;
    }

    static std::string trimStr(const std::string &s) {
        size_t a = s.find_first_not_of(" \t\r\n");
        if (a == std::string::npos) return "";
        size_t b = s.find_last_not_of(" \t\r\n");
        return s.substr(a, b - a + 1);
    }

    static std::string rpadStr(const std::string &s, size_t width, char padChar = ' ') {
        if (s.size() >= width) return s;
        return s + std::string(width - s.size(), padChar);
    }

    static bool endsWithUpper(const std::string &path, const std::string &suffixUpper) {
        if (path.size() < suffixUpper.size()) return false;
        return toUpperStr(path.substr(path.size() - suffixUpper.size())) == suffixUpper;
    }

    static void raiseError(int code, const std::string &msg) {
        throw BasicRuntimeError(code, msg);
    }

    // ========================================================================
    // STYLE NAME MAPPING HELPERS FOR .ODT AND .DOCX
    // ========================================================================
    static void normalizeParagraphStyle(const std::string &rawStyle, int &inoutLevel, std::string &outCanonical) {
        std::string u = toUpperStr(trimStr(rawStyle));
        if (u.empty()) {
            if (inoutLevel >= 1 && inoutLevel <= 6) {
                outCanonical = "Heading " + std::to_string(inoutLevel);
            } else {
                inoutLevel = 0;
                outCanonical = "Text Body";
            }
            return;
        }
        if (u == "H1" || u == "HEADING 1" || u == "HEADING1" || u == "HEADING_20_1") {
            inoutLevel = 1; outCanonical = "Heading 1"; return;
        }
        if (u == "H2" || u == "HEADING 2" || u == "HEADING2" || u == "HEADING_20_2") {
            inoutLevel = 2; outCanonical = "Heading 2"; return;
        }
        if (u == "H3" || u == "HEADING 3" || u == "HEADING3" || u == "HEADING_20_3") {
            inoutLevel = 3; outCanonical = "Heading 3"; return;
        }
        if (u == "H4" || u == "HEADING 4" || u == "HEADING4" || u == "HEADING_20_4") {
            inoutLevel = 4; outCanonical = "Heading 4"; return;
        }
        if (u == "H5" || u == "HEADING 5" || u == "HEADING5" || u == "HEADING_20_5") {
            inoutLevel = 5; outCanonical = "Heading 5"; return;
        }
        if (u == "H6" || u == "HEADING 6" || u == "HEADING6" || u == "HEADING_20_6") {
            inoutLevel = 6; outCanonical = "Heading 6"; return;
        }
        if (u == "TITLE") {
            inoutLevel = 0; outCanonical = "Title"; return;
        }
        if (u == "SUBTITLE") {
            inoutLevel = 0; outCanonical = "Subtitle"; return;
        }
        if (u == "QUOTE" || u == "QUOTATION" || u == "QUOTATIONS" || u == "BLOCKQUOTE") {
            inoutLevel = 0; outCanonical = "Quotations"; return;
        }
        if (u == "CODE" || u == "PRE" || u == "PREFORMATTED" || u == "PREFORMATTED TEXT" || u == "PREFORMATTED_20_TEXT") {
            inoutLevel = 0; outCanonical = "Preformatted Text"; return;
        }
        if (u == "STANDARD" || u == "DEFAULT" || u == "NORMAL") {
            inoutLevel = 0; outCanonical = "Standard"; return;
        }
        if (u == "TEXT BODY" || u == "TEXT_20_BODY" || u == "BODY" || u == "BODYTEXT") {
            inoutLevel = 0; outCanonical = "Text Body"; return;
        }
        inoutLevel = 0;
        outCanonical = trimStr(rawStyle);
    }

    static std::string toOdtStyleId(const DocParagraph &p) {
        if (p.headingLevel >= 1 && p.headingLevel <= 6) {
            return "Heading_20_" + std::to_string(p.headingLevel);
        }
        std::string u = toUpperStr(trimStr(p.styleName));
        if (u == "TITLE") return "Title";
        if (u == "SUBTITLE") return "Subtitle";
        if (u == "QUOTATIONS" || u == "QUOTE") return "Quotations";
        if (u == "PREFORMATTED TEXT" || u == "CODE") return "Preformatted_20_Text";
        if (u == "STANDARD" || u == "NORMAL") return "Standard";
        return "Text_20_body";
    }

    static std::string toDocxStyleId(const DocParagraph &p) {
        if (p.headingLevel >= 1 && p.headingLevel <= 6) {
            return "Heading" + std::to_string(p.headingLevel);
        }
        std::string u = toUpperStr(trimStr(p.styleName));
        if (u == "TITLE") return "Title";
        if (u == "SUBTITLE") return "Subtitle";
        if (u == "QUOTATIONS" || u == "QUOTE") return "Quote";
        if (u == "PREFORMATTED TEXT" || u == "CODE") return "Code";
        if (u == "STANDARD" || u == "NORMAL") return "Normal";
        return "BodyText";
    }

    // ========================================================================
    // SELF-CONTAINED ZIP ARCHIVE WRITER & READER (Zero External Dependencies)
    // ========================================================================
    struct ZipEntryData {
        std::string filename;
        std::string content;
    };

    static uint32_t calcCRC32(const std::string &data) {
        uint32_t crc = 0xFFFFFFFFu;
        for (unsigned char b : data) {
            crc ^= b;
            for (int i = 0; i < 8; ++i) {
                crc = (crc >> 1) ^ (0xEDB88320u & (-(crc & 1u)));
            }
        }
        return ~crc;
    }

    static void writeLE16(std::ostream &os, uint16_t v) {
        char b[2] = { static_cast<char>(v & 0xFF), static_cast<char>((v >> 8) & 0xFF) };
        os.write(b, 2);
    }

    static void writeLE32(std::ostream &os, uint32_t v) {
        char b[4] = {
            static_cast<char>(v & 0xFF),
            static_cast<char>((v >> 8) & 0xFF),
            static_cast<char>((v >> 16) & 0xFF),
            static_cast<char>((v >> 24) & 0xFF)
        };
        os.write(b, 4);
    }

    static uint16_t readLE16(const std::string &buf, size_t pos) {
        return static_cast<uint16_t>(static_cast<unsigned char>(buf[pos])) |
               (static_cast<uint16_t>(static_cast<unsigned char>(buf[pos + 1])) << 8);
    }

    static uint32_t readLE32(const std::string &buf, size_t pos) {
        return static_cast<uint32_t>(static_cast<unsigned char>(buf[pos])) |
               (static_cast<uint32_t>(static_cast<unsigned char>(buf[pos + 1])) << 8) |
               (static_cast<uint32_t>(static_cast<unsigned char>(buf[pos + 2])) << 16) |
               (static_cast<uint32_t>(static_cast<unsigned char>(buf[pos + 3])) << 24);
    }

    static bool writeZipArchive(const std::string &zipPath, const std::vector<ZipEntryData> &entries) {
        std::ofstream ofs(zipPath, std::ios::binary | std::ios::trunc);
        if (!ofs.is_open()) return false;

        struct CDRec {
            std::string filename;
            uint32_t crc;
            uint32_t size;
            uint32_t offset;
        };
        std::vector<CDRec> cd;

        for (const auto &e : entries) {
            uint32_t offset = static_cast<uint32_t>(ofs.tellp());
            uint32_t crc    = calcCRC32(e.content);
            uint32_t sz     = static_cast<uint32_t>(e.content.size());

            writeLE32(ofs, 0x04034b50u); // Local file header signature
            writeLE16(ofs, 20);          // Version needed to extract (2.0)
            writeLE16(ofs, 0);           // General purpose bit flag
            writeLE16(ofs, 0);           // Compression method: 0 (STORE)
            writeLE16(ofs, 0);           // Last mod file time
            writeLE16(ofs, 0x0021);      // Last mod file date
            writeLE32(ofs, crc);
            writeLE32(ofs, sz);
            writeLE32(ofs, sz);
            writeLE16(ofs, static_cast<uint16_t>(e.filename.size()));
            writeLE16(ofs, 0);           // Extra field length
            ofs.write(e.filename.data(), static_cast<std::streamsize>(e.filename.size()));
            if (sz > 0) ofs.write(e.content.data(), static_cast<std::streamsize>(sz));

            cd.push_back({e.filename, crc, sz, offset});
        }

        uint32_t cdStart = static_cast<uint32_t>(ofs.tellp());
        for (const auto &r : cd) {
            writeLE32(ofs, 0x02014b50u); // Central directory file header signature
            writeLE16(ofs, 20);          // Version made by
            writeLE16(ofs, 20);          // Version needed to extract
            writeLE16(ofs, 0);           // General purpose bit flag
            writeLE16(ofs, 0);           // Compression method: 0 (STORE)
            writeLE16(ofs, 0);           // Last mod time
            writeLE16(ofs, 0x0021);      // Last mod date
            writeLE32(ofs, r.crc);
            writeLE32(ofs, r.size);
            writeLE32(ofs, r.size);
            writeLE16(ofs, static_cast<uint16_t>(r.filename.size()));
            writeLE16(ofs, 0);           // Extra field length
            writeLE16(ofs, 0);           // File comment length
            writeLE16(ofs, 0);           // Disk number start
            writeLE16(ofs, 0);           // Internal file attributes
            writeLE32(ofs, 0);           // External file attributes
            writeLE32(ofs, r.offset);
            ofs.write(r.filename.data(), static_cast<std::streamsize>(r.filename.size()));
        }
        uint32_t cdEnd  = static_cast<uint32_t>(ofs.tellp());
        uint32_t cdSize = cdEnd - cdStart;

        writeLE32(ofs, 0x06054b50u);     // End of central directory signature
        writeLE16(ofs, 0);               // Number of this disk
        writeLE16(ofs, 0);               // Disk where central directory starts
        writeLE16(ofs, static_cast<uint16_t>(cd.size()));
        writeLE16(ofs, static_cast<uint16_t>(cd.size()));
        writeLE32(ofs, cdSize);
        writeLE32(ofs, cdStart);
        writeLE16(ofs, 0);               // Comment length
        return ofs.good();
    }

    static std::string extractZipEntry(const std::string &zipPath, const std::string &fileBytes, const std::string &entryName) {
        // 1. Native in-memory reader for STORED (Method 0) ZIP entries via Central Directory
        if (fileBytes.size() >= 22) {
            size_t eocdPos = std::string::npos;
            size_t minSearch = (fileBytes.size() > 65557) ? (fileBytes.size() - 65557) : 0;
            for (size_t p = fileBytes.size() - 22; p >= minSearch; --p) {
                if (readLE32(fileBytes, p) == 0x06054b50u) {
                    eocdPos = p;
                    break;
                }
                if (p == 0) break;
            }
            if (eocdPos != std::string::npos) {
                uint16_t totalEntries = readLE16(fileBytes, eocdPos + 10);
                uint32_t cdOffset     = readLE32(fileBytes, eocdPos + 16);
                size_t pos = cdOffset;
                for (uint16_t i = 0; i < totalEntries && pos + 46 <= fileBytes.size(); ++i) {
                    if (readLE32(fileBytes, pos) != 0x02014b50u) break;
                    uint16_t method    = readLE16(fileBytes, pos + 10);
                    uint32_t compSz    = readLE32(fileBytes, pos + 20);
                    uint16_t nameLen   = readLE16(fileBytes, pos + 28);
                    uint16_t extraLen  = readLE16(fileBytes, pos + 30);
                    uint16_t commLen   = readLE16(fileBytes, pos + 32);
                    uint32_t lfhOffset = readLE32(fileBytes, pos + 42);
                    if (pos + 46 + nameLen > fileBytes.size()) break;

                    std::string fn = fileBytes.substr(pos + 46, nameLen);
                    if (fn == entryName && method == 0 && lfhOffset + 30 <= fileBytes.size()) {
                        uint16_t lfhNameLen  = readLE16(fileBytes, lfhOffset + 26);
                        uint16_t lfhExtraLen = readLE16(fileBytes, lfhOffset + 28);
                        size_t dataStart = lfhOffset + 30 + lfhNameLen + lfhExtraLen;
                        if (dataStart + compSz <= fileBytes.size()) {
                            return fileBytes.substr(dataStart, compSz);
                        }
                    }
                    pos += 46 + nameLen + extraLen + commLen;
                }
            }
        }

        // 2. Fallback for DEFLATE-compressed entries (Method 8) saved by external office suites
        std::string cmd = "unzip -p \"" + zipPath + "\" \"" + entryName + "\" 2>/dev/null";
        FILE *pipe = popen(cmd.c_str(), "r");
        std::string xml;
        if (pipe) {
            char buf[4096];
            while (size_t n = std::fread(buf, 1, sizeof(buf), pipe)) {
                xml.append(buf, n);
            }
            pclose(pipe);
        }
        if (!xml.empty()) return xml;

        std::string pyCmd = "python3 -c \"import zipfile,sys; z=zipfile.ZipFile(sys.argv[1]); sys.stdout.write(z.read(sys.argv[2]).decode('utf-8','ignore'))\" \"" +
                            zipPath + "\" \"" + entryName + "\" 2>/dev/null";
        pipe = popen(pyCmd.c_str(), "r");
        if (pipe) {
            char buf[4096];
            while (size_t n = std::fread(buf, 1, sizeof(buf), pipe)) {
                xml.append(buf, n);
            }
            pclose(pipe);
        }
        return xml;
    }

    // Forward declarations
    void ODS_New(std::unordered_map<std::string, Variant> *globals);
    void ODT_New(std::unordered_map<std::string, Variant> *globals);
    void ODT_Open(const std::string &filepath, std::unordered_map<std::string, Variant> *globals);
    void ODT_SaveAs(const std::string &filepath, std::unordered_map<std::string, Variant> *globals);

    void ODS_EnsureInit(std::unordered_map<std::string, Variant> *globals) {
        if (gODS_Initialized == 0) {
            ODS_New(globals);
        }
    }

    void ODT_EnsureInit(std::unordered_map<std::string, Variant> *globals) {
        if (gODT_Initialized == 0) {
            ODT_New(globals);
        }
    }

    std::string ColIndexToName(int colIdx) {
        int64_t n = static_cast<int64_t>(colIdx) + 1;
        std::string res;
        while (n > 0) {
            int remVal = static_cast<int>((n - 1) % 26);
            res = static_cast<char>(65 + remVal) + res;
            n = (n - 1) / 26;
        }
        return res;
    }

    int ColNameToIndex(const std::string &colName) {
        std::string s = toUpperStr(trimStr(colName));
        int64_t acc = 0;
        for (char c : s) {
            int ch = static_cast<unsigned char>(c);
            if (ch >= 65 && ch <= 90) {
                acc = acc * 26 + (ch - 64);
                if (acc > MAX_COLS) raiseError(9, "Column index overflow");
            }
        }
        return static_cast<int>(acc - 1);
    }

    void syncGlobals(std::unordered_map<std::string, Variant> *globals) {
        if (!globals) return;
        (*globals)["GODS_INITIALIZED"]         = Variant::fromInt(gODS_Initialized);
        (*globals)["GODS_SHEETCOUNT"]          = Variant::fromInt(gODS_SheetCount);
        (*globals)["GODS_ACTIVESHEET"]         = Variant::fromInt(gODS_ActiveSheet);
        (*globals)["GODS_FILEPATH"]            = Variant::fromString(gODS_FilePath);
        (*globals)["GODS_RANGELEFT"]           = Variant::fromInt(gODS_RangeLeft);
        (*globals)["GODS_RANGETOP"]            = Variant::fromInt(gODS_RangeTop);
        (*globals)["GODS_RANGERIGHT"]          = Variant::fromInt(gODS_RangeRight);
        (*globals)["GODS_RANGEBOTTOM"]         = Variant::fromInt(gODS_RangeBottom);
        (*globals)["GODS_SEARCHCOL"]           = Variant::fromInt(gODS_SearchCol);
        (*globals)["GODS_SEARCHROW"]           = Variant::fromInt(gODS_SearchRow);
        (*globals)["CELLADDRESS.SHEET"]        = Variant::fromInt(CellAddress_Sheet);
        (*globals)["CELLADDRESS.COLUMN"]       = Variant::fromInt(CellAddress_Column);
        (*globals)["CELLADDRESS.ROW"]          = Variant::fromInt(CellAddress_Row);
        (*globals)["CELLADDRESS.ABSOLUTENAME"] = Variant::fromString(CellAddress_AbsoluteName);

        // Writer / Document globals
        (*globals)["GODT_INITIALIZED"]         = Variant::fromInt(gODT_Initialized);
        (*globals)["GODT_FILEPATH"]            = Variant::fromString(gODT_FilePath);
        (*globals)["GODT_PARAGRAPHCOUNT"]      = Variant::fromInt(static_cast<int64_t>(gODT_Paragraphs.size()));
        (*globals)["GODT_SEARCHINDEX"]         = Variant::fromInt(gODT_SearchIndex);
    }

    void ODS_UpdateAddressGlobals(std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        CellAddress_Sheet  = gODS_ActiveSheet - 1;
        CellAddress_Column = gODS_RangeLeft;
        CellAddress_Row    = gODS_RangeTop;
        CellAddress_AbsoluteName = "$" + gODS_Sheets[gODS_ActiveSheet].name + ".$" +
                                   ColIndexToName(gODS_RangeLeft) + "$" +
                                   std::to_string(gODS_RangeTop + 1);
        syncGlobals(globals);
    }

    void ODS_ClearSheetSlot(int sIdx) {
        if (sIdx < 1 || sIdx >= static_cast<int>(gODS_Sheets.size())) return;
        gODS_Sheets[sIdx].clear();
    }

    int ODS_IsNumericText(const std::string &raw) {
        std::string s = trimStr(raw);
        size_t n = s.size();
        if (n == 0) return 0;

        int seenDigit = 0;
        int seenDot   = 0;
        int seenExp   = 0;

        for (size_t i = 0; i < n; ++i) {
            char ch = s[i];
            if (ch >= '0' && ch <= '9') {
                seenDigit = -1;
            } else if (ch == '.') {
                if (seenDot != 0 || seenExp != 0) return 0;
                seenDot = -1;
            } else if (ch == '+' || ch == '-') {
                if (i > 0) {
                    char prevCh = static_cast<char>(std::toupper(static_cast<unsigned char>(s[i - 1])));
                    if (prevCh != 'E') return 0;
                }
            } else if (std::toupper(static_cast<unsigned char>(ch)) == 'E') {
                if (seenExp != 0 || seenDigit == 0 || i + 1 == n) return 0;
                seenExp = -1;
            } else {
                return 0;
            }
        }
        return seenDigit;
    }

    void ODS_StoreCell(int sIdx, int cIdx, int rIdx, const Variant &val) {
        if (sIdx < 1 || sIdx > gODS_SheetCount) return;
        if (rIdx < 0 || rIdx > MAX_ROWS || cIdx < 0 || cIdx > MAX_COLS) {
            raiseError(9, "Subscript out of range");
        }
        SheetSlot &sheet = gODS_Sheets[sIdx];
        std::string sVal = val.toString();
        if (sVal.empty()) {
            sheet.eraseCell(rIdx, cIdx);
            return;
        }
        CellSlot &cell = sheet.rows[rIdx][cIdx];
        if (ODS_IsNumericText(sVal) != 0) {
            cell.type = 1;
            cell.val  = Variant::fromDouble(val.toDouble());
        } else {
            cell.type = 2;
            cell.val  = Variant::fromString(sVal);
        }
        if (rIdx > sheet.maxRow) sheet.maxRow = rIdx;
        if (cIdx > sheet.maxCol) sheet.maxCol = cIdx;
    }

    void ODS_ParseA1Ref(const std::string &refStr, int &outCol, int &outRow) {
        std::string s = trimStr(refStr);
        std::string colPart, rowPart;
        for (char ch : s) {
            if (ch == '$') {
                continue;
            } else if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) {
                colPart += ch;
            } else if (ch >= '0' && ch <= '9') {
                rowPart += ch;
            }
        }
        outCol = ColNameToIndex(colPart);
        int64_t parsedRow = 0;
        if (!rowPart.empty()) {
            try { parsedRow = std::stoll(rowPart); } catch (...) { parsedRow = 0; }
        }
        outRow = static_cast<int>(parsedRow - 1);
        if (outCol < 0) outCol = 0;
        if (outRow < 0) outRow = 0;
    }

    void ODS_New(std::unordered_map<std::string, Variant> *globals) {
        gODS_Sheets.clear();
        gODS_Sheets.resize(2);
        gODS_Sheets[1].name = "Sheet1";

        gODS_Initialized = -1;
        gODS_SheetCount  = 1;
        gODS_ActiveSheet = 1;
        gODS_FilePath.clear();

        gODS_RangeLeft   = 0;
        gODS_RangeTop    = 0;
        gODS_RangeRight  = 0;
        gODS_RangeBottom = 0;
        gODS_SearchCol   = 0;
        gODS_SearchRow   = -1;
        ODS_UpdateAddressGlobals(globals);
    }

    void ODT_New(std::unordered_map<std::string, Variant> *globals) {
        gODT_Paragraphs.clear();
        gODT_Initialized = -1;
        gODT_FilePath.clear();
        gODT_SearchIndex = -1;
        if (gODS_Initialized == 0) {
            ODS_New(globals);
        } else {
            syncGlobals(globals);
        }
    }

    std::string ODS_EscapeXML(const std::string &s) {
        std::string outStr;
        outStr.reserve(s.size());
        for (char ch : s) {
            if (ch == '&') outStr += "&amp;";
            else if (ch == '<') outStr += "&lt;";
            else if (ch == '>') outStr += "&gt;";
            else if (ch == '"') outStr += "&quot;";
            else outStr += ch;
        }
        return outStr;
    }

    std::string ODS_UnescapeXML(const std::string &s) {
        std::string res = s;
        auto replaceAll = [&](const std::string &from, const std::string &to) {
            size_t pos = 0;
            while ((pos = res.find(from, pos)) != std::string::npos) {
                res.replace(pos, from.size(), to);
                pos += to.size();
            }
        };
        replaceAll("&quot;", "\"");
        replaceAll("&apos;", "'");
        replaceAll("&lt;", "<");
        replaceAll("&gt;", ">");
        replaceAll("&amp;", "&");
        return res;
    }

    std::string ODS_ExtractAttr(const std::string &lineStr, const std::string &attrName) {
        std::string pat = attrName + "=\"";
        size_t p1 = lineStr.find(pat);
        if (p1 == std::string::npos) return "";
        p1 += pat.size();
        size_t p2 = lineStr.find('"', p1);
        if (p2 == std::string::npos) return "";
        return ODS_UnescapeXML(lineStr.substr(p1, p2 - p1));
    }

    std::string ODS_ExtractTagText(const std::string &lineStr, const std::string &openTag, const std::string &closeTag) {
        size_t p1 = lineStr.find(openTag);
        if (p1 == std::string::npos) return "";
        p1 += openTag.size();
        size_t p2 = lineStr.find(closeTag, p1);
        if (p2 == std::string::npos) return "";
        return ODS_UnescapeXML(lineStr.substr(p1, p2 - p1));
    }

    static std::string stripXmlTags(const std::string &xmlFragment) {
        std::string out;
        bool inTag = false;
        std::string curTag;
        for (char c : xmlFragment) {
            if (c == '<') {
                inTag = true;
                curTag.clear();
            } else if (c == '>') {
                inTag = false;
                if (curTag.rfind("text:tab", 0) == 0 || curTag.rfind("w:tab", 0) == 0) {
                    out += '\t';
                } else if (curTag.rfind("text:line-break", 0) == 0 || curTag.rfind("w:br", 0) == 0) {
                    out += '\n';
                }
            } else if (inTag) {
                curTag += c;
            } else {
                out += c;
            }
        }
        return ODS_UnescapeXML(out);
    }

    // ========================================================================
    // .ODT & .DOCX PARSERS AND SERIALIZERS (WITH PARAGRAPH & TABLE STYLES)
    // ========================================================================
    static void parseOdsOrXlsxXmlContent(const std::string &xmlContent);

    static void parseOdtXmlContent(const std::string &xmlContent) {
        gODT_Paragraphs.clear();
        if (xmlContent.find("<table:table ") != std::string::npos) {
            parseOdsOrXlsxXmlContent(xmlContent);
        }

        int tableDepth = 0;
        size_t pos = 0;
        size_t n = xmlContent.size();
        while (pos < n) {
            size_t lt = xmlContent.find('<', pos);
            if (lt == std::string::npos) break;
            size_t gt = xmlContent.find('>', lt + 1);
            if (gt == std::string::npos) break;

            std::string tag = xmlContent.substr(lt, gt - lt + 1);
            pos = gt + 1;

            bool selfClosing = (tag.size() >= 2 && tag[tag.size() - 2] == '/');

            if (tag.rfind("<table:table", 0) == 0 &&
                (tag.size() == 13 || tag[12] == ' ' || tag[12] == '>' || tag[12] == '/')) {
                if (!selfClosing) ++tableDepth;
                continue;
            }
            if (tag.rfind("</table:table>", 0) == 0) {
                if (tableDepth > 0) --tableDepth;
                continue;
            }
            if (tableDepth > 0) continue;

            if (tag.rfind("<text:h", 0) == 0 &&
                (tag.size() == 8 || tag[7] == ' ' || tag[7] == '>' || tag[7] == '/')) {
                int lvl = 1;
                std::string lvlStr = ODS_ExtractAttr(tag, "text:outline-level");
                if (!lvlStr.empty()) {
                    try { lvl = std::clamp(std::stoi(lvlStr), 1, 6); } catch (...) { lvl = 1; }
                }
                std::string rawStyle = ODS_ExtractAttr(tag, "text:style-name");
                std::string canonStyle;
                normalizeParagraphStyle(rawStyle, lvl, canonStyle);
                if (lvl < 1 || lvl > 6) {
                    lvl = 1;
                    canonStyle = "Heading 1";
                }
                if (selfClosing) {
                    gODT_Paragraphs.push_back({"", lvl, canonStyle});
                    continue;
                }
                size_t closeH = xmlContent.find("</text:h>", pos);
                if (closeH != std::string::npos) {
                    std::string inner = xmlContent.substr(pos, closeH - pos);
                    gODT_Paragraphs.push_back({stripXmlTags(inner), lvl, canonStyle});
                    pos = closeH + 9;
                }
            } else if (tag.rfind("<text:p", 0) == 0 &&
                       (tag.size() == 8 || tag[7] == ' ' || tag[7] == '>' || tag[7] == '/')) {
                std::string rawStyle = ODS_ExtractAttr(tag, "text:style-name");
                int lvl = 0;
                std::string canonStyle;
                normalizeParagraphStyle(rawStyle, lvl, canonStyle);
                if (selfClosing) {
                    gODT_Paragraphs.push_back({"", lvl, canonStyle});
                    continue;
                }
                size_t closeP = xmlContent.find("</text:p>", pos);
                if (closeP != std::string::npos) {
                    std::string inner = xmlContent.substr(pos, closeP - pos);
                    gODT_Paragraphs.push_back({stripXmlTags(inner), lvl, canonStyle});
                    pos = closeP + 9;
                }
            }
        }
    }

    static std::string extractDocxParagraphText(const std::string &pBody, int &outHeadingLevel, std::string &outStyleName) {
        outHeadingLevel = 0;
        outStyleName = "Text Body";
        size_t stylePos = pBody.find("<w:pStyle");
        if (stylePos != std::string::npos) {
            size_t styleGt = pBody.find('>', stylePos);
            if (styleGt != std::string::npos) {
                std::string styleTag = pBody.substr(stylePos, styleGt - stylePos + 1);
                std::string val = ODS_ExtractAttr(styleTag, "w:val");
                normalizeParagraphStyle(val, outHeadingLevel, outStyleName);
            }
        }

        std::string textOut;
        size_t pos = 0;
        while (pos < pBody.size()) {
            size_t lt = pBody.find('<', pos);
            if (lt == std::string::npos) break;
            size_t gt = pBody.find('>', lt + 1);
            if (gt == std::string::npos) break;
            std::string tag = pBody.substr(lt, gt - lt + 1);
            pos = gt + 1;

            if (tag.rfind("<w:t", 0) == 0 &&
                (tag.size() == 5 || tag[4] == ' ' || tag[4] == '>' || tag[4] == '/')) {
                bool selfClosing = (tag.size() >= 2 && tag[tag.size() - 2] == '/');
                if (!selfClosing) {
                    size_t closeT = pBody.find("</w:t>", pos);
                    if (closeT != std::string::npos) {
                        textOut += ODS_UnescapeXML(pBody.substr(pos, closeT - pos));
                        pos = closeT + 6;
                    }
                }
            } else if (tag.rfind("<w:tab", 0) == 0) {
                textOut += '\t';
            } else if (tag.rfind("<w:br", 0) == 0) {
                textOut += '\n';
            }
        }
        return textOut;
    }

    static void parseDocxTableIntoSheet(int sheetIdx, const std::string &tblBody) {
        if (sheetIdx > gODS_SheetCount) {
            gODS_SheetCount = sheetIdx;
            if (static_cast<size_t>(sheetIdx + 1) > gODS_Sheets.size()) {
                gODS_Sheets.resize(sheetIdx + 1);
            }
        }
        gODS_Sheets[sheetIdx].clear();
        gODS_Sheets[sheetIdx].name = "Table" + std::to_string(sheetIdx);

        int rIdx = 0;
        size_t trPos = 0;
        while (trPos < tblBody.size()) {
            size_t trStart = tblBody.find("<w:tr", trPos);
            if (trStart == std::string::npos) break;
            if (trStart + 5 < tblBody.size() &&
                tblBody[trStart + 5] != '>' && tblBody[trStart + 5] != ' ' && tblBody[trStart + 5] != '/') {
                trPos = trStart + 5;
                continue;
            }
            size_t trGt = tblBody.find('>', trStart);
            size_t trEnd = tblBody.find("</w:tr>", trGt);
            if (trGt == std::string::npos || trEnd == std::string::npos) break;

            std::string rowXml = tblBody.substr(trGt + 1, trEnd - (trGt + 1));
            int cIdx = 0;
            size_t tcPos = 0;
            while (tcPos < rowXml.size()) {
                size_t tcStart = rowXml.find("<w:tc", tcPos);
                if (tcStart == std::string::npos) break;
                if (tcStart + 5 < rowXml.size() &&
                    rowXml[tcStart + 5] != '>' && rowXml[tcStart + 5] != ' ' && rowXml[tcStart + 5] != '/') {
                    tcPos = tcStart + 5;
                    continue;
                }
                size_t tcGt = rowXml.find('>', tcStart);
                size_t tcEnd = rowXml.find("</w:tc>", tcGt);
                if (tcGt == std::string::npos || tcEnd == std::string::npos) break;

                std::string cellXml = rowXml.substr(tcGt + 1, tcEnd - (tcGt + 1));
                int dummyLvl = 0;
                std::string dummyStyle;
                std::string cellText = extractDocxParagraphText(cellXml, dummyLvl, dummyStyle);
                ODS_StoreCell(sheetIdx, cIdx, rIdx, Variant::fromString(cellText));
                ++cIdx;
                tcPos = tcEnd + 7;
            }
            ++rIdx;
            trPos = trEnd + 7;
        }
    }

    static void parseDocxDocumentXml(const std::string &docXml) {
        gODT_Paragraphs.clear();
        int tableCount = 0;
        int tblDepth = 0;

        size_t pos = 0;
        size_t n = docXml.size();
        while (pos < n) {
            size_t lt = docXml.find('<', pos);
            if (lt == std::string::npos) break;
            size_t gt = docXml.find('>', lt + 1);
            if (gt == std::string::npos) break;

            std::string tag = docXml.substr(lt, gt - lt + 1);
            pos = gt + 1;
            bool selfClosing = (tag.size() >= 2 && tag[tag.size() - 2] == '/');

            if (tag.rfind("<w:tbl", 0) == 0 &&
                (tag.size() == 7 || tag[6] == '>' || tag[6] == ' ' || tag[6] == '/')) {
                if (!selfClosing) {
                    size_t closeTbl = docXml.find("</w:tbl>", pos);
                    if (closeTbl != std::string::npos) {
                        ++tableCount;
                        parseDocxTableIntoSheet(tableCount, docXml.substr(pos, closeTbl - pos));
                        pos = closeTbl + 8;
                        continue;
                    }
                    ++tblDepth;
                }
                continue;
            }
            if (tag.rfind("</w:tbl>", 0) == 0) {
                if (tblDepth > 0) --tblDepth;
                continue;
            }
            if (tblDepth > 0) continue;

            if (tag.rfind("<w:p", 0) == 0 &&
                (tag.size() == 5 || tag[4] == '>' || tag[4] == ' ' || tag[4] == '/')) {
                if (selfClosing) {
                    gODT_Paragraphs.push_back({"", 0, "Text Body"});
                    continue;
                }
                size_t closeP = docXml.find("</w:p>", pos);
                if (closeP != std::string::npos) {
                    std::string pBody = docXml.substr(pos, closeP - pos);
                    int hLvl = 0;
                    std::string pStyle;
                    std::string pTxt = extractDocxParagraphText(pBody, hLvl, pStyle);
                    gODT_Paragraphs.push_back({pTxt, hLvl, pStyle});
                    pos = closeP + 6;
                }
            }
        }
    }

    static std::string buildOdtCommonStylesXml() {
        return
            "    <style:default-style style:family=\"paragraph\">\n"
            "      <style:paragraph-properties fo:hyphenation-ladder-count=\"no-limit\" style:text-autospace=\"ideograph-alpha\" style:line-break=\"strict\" style:writing-mode=\"lr-tb\"/>\n"
            "      <style:text-properties style:font-name=\"Liberation Serif\" fo:font-family=\"'Liberation Serif'\" style:font-family-generic=\"roman\" style:font-pitch=\"variable\" fo:font-size=\"12pt\" fo:language=\"en\" fo:country=\"US\"/>\n"
            "    </style:default-style>\n"
            "    <style:style style:name=\"Standard\" style:family=\"paragraph\" style:class=\"text\"/>\n"
            "    <style:style style:name=\"Text_20_body\" style:display-name=\"Text Body\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0in\" fo:margin-bottom=\"0.0972in\" fo:line-height=\"115%\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Heading\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:next-style-name=\"Text_20_body\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.1665in\" fo:margin-bottom=\"0.0835in\" fo:keep-with-next=\"always\"/>\n"
            "      <style:text-properties style:font-name=\"Liberation Sans\" fo:font-family=\"'Liberation Sans'\" style:font-family-generic=\"swiss\" style:font-pitch=\"variable\" fo:font-size=\"14pt\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Title\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:class=\"chapter\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.1in\" fo:margin-bottom=\"0.15in\" fo:text-align=\"center\" style:justify-single-word=\"false\"/>\n"
            "      <style:text-properties fo:font-size=\"24pt\" fo:font-weight=\"bold\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Subtitle\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:class=\"chapter\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.04in\" fo:margin-bottom=\"0.15in\" fo:text-align=\"center\" style:justify-single-word=\"false\"/>\n"
            "      <style:text-properties fo:font-size=\"15pt\" fo:font-style=\"italic\" fo:color=\"#444444\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Heading_20_1\" style:display-name=\"Heading 1\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"1\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.1665in\" fo:margin-bottom=\"0.0835in\" fo:keep-with-next=\"always\"/>\n"
            "      <style:text-properties fo:font-size=\"18pt\" fo:font-weight=\"bold\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Heading_20_2\" style:display-name=\"Heading 2\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"2\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.139in\" fo:margin-bottom=\"0.069in\" fo:keep-with-next=\"always\"/>\n"
            "      <style:text-properties fo:font-size=\"14pt\" fo:font-weight=\"bold\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Heading_20_3\" style:display-name=\"Heading 3\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"3\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.111in\" fo:margin-bottom=\"0.055in\" fo:keep-with-next=\"always\"/>\n"
            "      <style:text-properties fo:font-size=\"12pt\" fo:font-weight=\"bold\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Heading_20_4\" style:display-name=\"Heading 4\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"4\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.0835in\" fo:margin-bottom=\"0.0417in\" fo:keep-with-next=\"always\"/>\n"
            "      <style:text-properties fo:font-size=\"11pt\" fo:font-weight=\"bold\" fo:font-style=\"italic\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Heading_20_5\" style:display-name=\"Heading 5\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"5\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.0835in\" fo:margin-bottom=\"0.0417in\" fo:keep-with-next=\"always\"/>\n"
            "      <style:text-properties fo:font-size=\"10.5pt\" fo:font-weight=\"bold\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Heading_20_6\" style:display-name=\"Heading 6\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"6\" style:class=\"text\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.06in\" fo:margin-bottom=\"0.04in\" fo:keep-with-next=\"always\"/>\n"
            "      <style:text-properties fo:font-size=\"10pt\" fo:font-style=\"italic\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Quotations\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"html\">\n"
            "      <style:paragraph-properties fo:margin-left=\"0.3937in\" fo:margin-right=\"0.3937in\" fo:margin-top=\"0.05in\" fo:margin-bottom=\"0.1in\"/>\n"
            "      <style:text-properties fo:font-style=\"italic\" fo:color=\"#333333\"/>\n"
            "    </style:style>\n"
            "    <style:style style:name=\"Preformatted_20_Text\" style:display-name=\"Preformatted Text\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"html\">\n"
            "      <style:paragraph-properties fo:margin-top=\"0.04in\" fo:margin-bottom=\"0.04in\" fo:background-color=\"#f4f4f4\" fo:padding=\"0.04in\"/>\n"
            "      <style:text-properties style:font-name=\"Liberation Mono\" fo:font-family=\"'Liberation Mono'\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\" fo:font-size=\"10pt\"/>\n"
            "    </style:style>\n";
    }

    static void saveOdtZipPackage(const std::string &filepath) {
        std::vector<ZipEntryData> entries;
        // 1. mimetype MUST be first entry in ODF ZIP
        entries.push_back({"mimetype", "application/vnd.oasis.opendocument.text"});

        // 2. META-INF/manifest.xml
        std::string manifestXml =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            "<manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.2\">\n"
            "  <manifest:file-entry manifest:full-path=\"/\" manifest:version=\"1.2\" manifest:media-type=\"application/vnd.oasis.opendocument.text\"/>\n"
            "  <manifest:file-entry manifest:full-path=\"content.xml\" manifest:media-type=\"text/xml\"/>\n"
            "  <manifest:file-entry manifest:full-path=\"styles.xml\" manifest:media-type=\"text/xml\"/>\n"
            "</manifest:manifest>\n";
        entries.push_back({"META-INF/manifest.xml", manifestXml});

        std::string fontDecls =
            "  <office:font-face-decls>\n"
            "    <style:font-face style:name=\"Liberation Serif\" svg:font-family=\"'Liberation Serif'\" style:font-family-generic=\"roman\" style:font-pitch=\"variable\"/>\n"
            "    <style:font-face style:name=\"Liberation Sans\" svg:font-family=\"'Liberation Sans'\" style:font-family-generic=\"swiss\" style:font-pitch=\"variable\"/>\n"
            "    <style:font-face style:name=\"Liberation Mono\" svg:font-family=\"'Liberation Mono'\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\"/>\n"
            "  </office:font-face-decls>\n";

        // 3. styles.xml with full Paragraph, Heading, Outline, and Page Layout styles
        std::ostringstream styles;
        styles << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
               << "<office:document-styles "
                  "xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" "
                  "xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" "
                  "xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" "
                  "xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" "
                  "xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" "
                  "xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" "
                  "office:version=\"1.2\">\n"
               << fontDecls
               << "  <office:styles>\n"
               << buildOdtCommonStylesXml()
               << "    <text:outline-style style:name=\"Outline\">\n"
               << "      <text:outline-level-style text:level=\"1\" style:num-format=\"\"/>\n"
               << "      <text:outline-level-style text:level=\"2\" style:num-format=\"\"/>\n"
               << "      <text:outline-level-style text:level=\"3\" style:num-format=\"\"/>\n"
               << "      <text:outline-level-style text:level=\"4\" style:num-format=\"\"/>\n"
               << "      <text:outline-level-style text:level=\"5\" style:num-format=\"\"/>\n"
               << "      <text:outline-level-style text:level=\"6\" style:num-format=\"\"/>\n"
               << "    </text:outline-style>\n"
               << "  </office:styles>\n"
               << "  <office:automatic-styles>\n"
               << "    <style:page-layout style:name=\"pm1\">\n"
               << "      <style:page-layout-properties fo:page-width=\"8.5in\" fo:page-height=\"11in\" fo:margin-top=\"0.7874in\" fo:margin-bottom=\"0.7874in\" fo:margin-left=\"0.7874in\" fo:margin-right=\"0.7874in\"/>\n"
               << "    </style:page-layout>\n"
               << "  </office:automatic-styles>\n"
               << "  <office:master-styles>\n"
               << "    <style:master-page style:name=\"Standard\" style:page-layout-name=\"pm1\"/>\n"
               << "  </office:master-styles>\n"
               << "</office:document-styles>\n";
        entries.push_back({"styles.xml", styles.str()});

        // 4. content.xml with automatic styles (tables + cells) AND explicit text:style-name on all paragraphs/headings
        std::ostringstream content;
        content << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                << "<office:document-content "
                   "xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" "
                   "xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" "
                   "xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" "
                   "xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" "
                   "xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" "
                   "xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" "
                   "office:version=\"1.2\">\n"
                << fontDecls
                << "  <office:automatic-styles>\n"
                << buildOdtCommonStylesXml()
                << "    <style:style style:name=\"Tbl1\" style:family=\"table\">\n"
                << "      <style:table-properties style:width=\"6.5in\" fo:margin-top=\"0.08in\" fo:margin-bottom=\"0.12in\" table:align=\"margins\" table:border-model=\"collapsing\"/>\n"
                << "    </style:style>\n"
                << "    <style:style style:name=\"TblCol\" style:family=\"table-column\">\n"
                << "      <style:table-column-properties style:column-width=\"1.5in\"/>\n"
                << "    </style:style>\n"
                << "    <style:style style:name=\"TblHdrCell\" style:family=\"table-cell\">\n"
                << "      <style:table-cell-properties fo:padding=\"0.05in\" fo:border=\"0.5pt solid #000000\" fo:background-color=\"#e8eef5\"/>\n"
                << "    </style:style>\n"
                << "    <style:style style:name=\"TblDataCell\" style:family=\"table-cell\">\n"
                << "      <style:table-cell-properties fo:padding=\"0.05in\" fo:border=\"0.5pt solid #000000\"/>\n"
                << "    </style:style>\n"
                << "    <style:style style:name=\"TblHdrPara\" style:family=\"paragraph\" style:parent-style-name=\"Standard\">\n"
                << "      <style:text-properties style:font-name=\"Liberation Sans\" fo:font-family=\"'Liberation Sans'\" fo:font-size=\"10.5pt\" fo:font-weight=\"bold\"/>\n"
                << "    </style:style>\n"
                << "    <style:style style:name=\"TblDataPara\" style:family=\"paragraph\" style:parent-style-name=\"Standard\">\n"
                << "      <style:text-properties fo:font-size=\"10.5pt\"/>\n"
                << "    </style:style>\n"
                << "  </office:automatic-styles>\n"
                << "  <office:body>\n"
                << "    <office:text>\n";

        for (const auto &p : gODT_Paragraphs) {
            std::string styleId = toOdtStyleId(p);
            if (p.headingLevel >= 1 && p.headingLevel <= 6) {
                content << "      <text:h text:style-name=\"" << styleId
                        << "\" text:outline-level=\"" << p.headingLevel << "\">"
                        << ODS_EscapeXML(p.text) << "</text:h>\n";
            } else {
                content << "      <text:p text:style-name=\"" << styleId << "\">"
                        << ODS_EscapeXML(p.text) << "</text:p>\n";
            }
        }

        // Write any non-empty tables from gODS_Sheets with column definitions and cell borders
        for (int s = 1; s <= gODS_SheetCount; ++s) {
            const SheetSlot &sh = gODS_Sheets[s];
            if (sh.rows.empty()) continue;
            int nCols = std::max(1, sh.maxCol + 1);
            content << "      <table:table table:name=\"" << ODS_EscapeXML(sh.name) << "\" table:style-name=\"Tbl1\">\n"
                    << "        <table:table-column table:style-name=\"TblCol\" table:number-columns-repeated=\"" << nCols << "\"/>\n";
            for (int r = 0; r <= sh.maxRow; ++r) {
                std::string cellStyle = (r == 0) ? "TblHdrCell" : "TblDataCell";
                std::string paraStyle = (r == 0) ? "TblHdrPara" : "TblDataPara";
                content << "        <table:table-row>\n";
                for (int c = 0; c <= sh.maxCol; ++c) {
                    const CellSlot *cell = sh.getCell(r, c);
                    if (cell && cell->type == 1) {
                        std::string numStr = cell->val.toString();
                        content << "          <table:table-cell table:style-name=\"" << cellStyle
                                << "\" office:value-type=\"float\" office:value=\"" << numStr
                                << "\"><text:p text:style-name=\"" << paraStyle << "\">"
                                << numStr << "</text:p></table:table-cell>\n";
                    } else if (cell && cell->type == 2) {
                        std::string txtStr = ODS_EscapeXML(cell->val.toString());
                        content << "          <table:table-cell table:style-name=\"" << cellStyle
                                << "\" office:value-type=\"string\"><text:p text:style-name=\"" << paraStyle << "\">"
                                << txtStr << "</text:p></table:table-cell>\n";
                    } else {
                        content << "          <table:table-cell table:style-name=\"" << cellStyle
                                << "\"><text:p text:style-name=\"" << paraStyle << "\"/></table:table-cell>\n";
                    }
                }
                content << "        </table:table-row>\n";
            }
            content << "      </table:table>\n";
        }

        content << "    </office:text>\n"
                << "  </office:body>\n"
                << "</office:document-content>\n";
        entries.push_back({"content.xml", content.str()});

        if (!writeZipArchive(filepath, entries)) {
            raiseError(53, "File not found or cannot open: " + filepath);
        }
    }

    static void saveDocxZipPackage(const std::string &filepath) {
        std::vector<ZipEntryData> entries;

        std::string contentTypes =
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
            "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">\n"
            "  <Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>\n"
            "  <Default Extension=\"xml\" ContentType=\"application/xml\"/>\n"
            "  <Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/>\n"
            "  <Override PartName=\"/word/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml\"/>\n"
            "</Types>\n";
        entries.push_back({"[Content_Types].xml", contentTypes});

        std::string rootRels =
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
            "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">\n"
            "  <Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/>\n"
            "</Relationships>\n";
        entries.push_back({"_rels/.rels", rootRels});

        std::string docRels =
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
            "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">\n"
            "  <Relationship Id=\"rIdStyles\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>\n"
            "</Relationships>\n";
        entries.push_back({"word/_rels/document.xml.rels", docRels});

        std::string stylesXml =
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
            "<w:styles xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">\n"
            "  <w:style w:type=\"paragraph\" w:default=\"1\" w:styleId=\"Normal\">\n"
            "    <w:name w:val=\"Normal\"/>\n"
            "    <w:rPr><w:sz w:val=\"24\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"BodyText\">\n"
            "    <w:name w:val=\"Body Text\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:spacing w:after=\"140\" w:line=\"276\" w:lineRule=\"auto\"/></w:pPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Title\">\n"
            "    <w:name w:val=\"Title\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:jc w:val=\"center\"/><w:spacing w:before=\"120\" w:after=\"200\"/></w:pPr>\n"
            "    <w:rPr><w:b/><w:sz w:val=\"48\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Subtitle\">\n"
            "    <w:name w:val=\"Subtitle\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:jc w:val=\"center\"/><w:spacing w:after=\"200\"/></w:pPr>\n"
            "    <w:rPr><w:i/><w:color w:val=\"555555\"/><w:sz w:val=\"30\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Heading1\">\n"
            "    <w:name w:val=\"heading 1\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:keepNext/><w:spacing w:before=\"240\" w:after=\"120\"/><w:outlineLvl w:val=\"0\"/></w:pPr>\n"
            "    <w:rPr><w:b/><w:sz w:val=\"36\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Heading2\">\n"
            "    <w:name w:val=\"heading 2\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:keepNext/><w:spacing w:before=\"200\" w:after=\"100\"/><w:outlineLvl w:val=\"1\"/></w:pPr>\n"
            "    <w:rPr><w:b/><w:sz w:val=\"28\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Heading3\">\n"
            "    <w:name w:val=\"heading 3\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:keepNext/><w:spacing w:before=\"160\" w:after=\"80\"/><w:outlineLvl w:val=\"2\"/></w:pPr>\n"
            "    <w:rPr><w:b/><w:sz w:val=\"24\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Heading4\">\n"
            "    <w:name w:val=\"heading 4\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:keepNext/><w:spacing w:before=\"120\" w:after=\"60\"/><w:outlineLvl w:val=\"3\"/></w:pPr>\n"
            "    <w:rPr><w:b/><w:i/><w:sz w:val=\"22\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Heading5\">\n"
            "    <w:name w:val=\"heading 5\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:keepNext/><w:spacing w:before=\"100\" w:after=\"40\"/><w:outlineLvl w:val=\"4\"/></w:pPr>\n"
            "    <w:rPr><w:b/><w:sz w:val=\"21\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Heading6\">\n"
            "    <w:name w:val=\"heading 6\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:keepNext/><w:spacing w:before=\"80\" w:after=\"40\"/><w:outlineLvl w:val=\"5\"/></w:pPr>\n"
            "    <w:rPr><w:i/><w:sz w:val=\"20\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Quote\">\n"
            "    <w:name w:val=\"Quote\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:ind w:left=\"560\" w:right=\"560\"/><w:spacing w:before=\"80\" w:after=\"120\"/></w:pPr>\n"
            "    <w:rPr><w:i/><w:color w:val=\"333333\"/></w:rPr>\n"
            "  </w:style>\n"
            "  <w:style w:type=\"paragraph\" w:styleId=\"Code\">\n"
            "    <w:name w:val=\"HTML Preformatted\"/>\n"
            "    <w:basedOn w:val=\"Normal\"/>\n"
            "    <w:pPr><w:spacing w:before=\"60\" w:after=\"60\"/></w:pPr>\n"
            "    <w:rPr><w:rFonts w:ascii=\"Courier New\" w:hAnsi=\"Courier New\"/><w:sz w:val=\"20\"/></w:rPr>\n"
            "  </w:style>\n"
            "</w:styles>\n";
        entries.push_back({"word/styles.xml", stylesXml});

        std::ostringstream doc;
        doc << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
            << "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" "
               "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">\n"
            << "  <w:body>\n";

        for (const auto &p : gODT_Paragraphs) {
            std::string docxStyle = toDocxStyleId(p);
            doc << "    <w:p><w:pPr><w:pStyle w:val=\"" << docxStyle << "\"/></w:pPr>"
                << "<w:r><w:t xml:space=\"preserve\">" << ODS_EscapeXML(p.text) << "</w:t></w:r></w:p>\n";
        }

        // Write any non-empty tables from gODS_Sheets as <w:tbl> with visible borders
        for (int s = 1; s <= gODS_SheetCount; ++s) {
            const SheetSlot &sh = gODS_Sheets[s];
            if (sh.rows.empty()) continue;
            doc << "    <w:tbl>\n"
                << "      <w:tblPr>\n"
                << "        <w:tblW w:w=\"0\" w:type=\"auto\"/>\n"
                << "        <w:tblBorders>\n"
                << "          <w:top w:val=\"single\" w:sz=\"4\" w:space=\"0\" w:color=\"000000\"/>\n"
                << "          <w:left w:val=\"single\" w:sz=\"4\" w:space=\"0\" w:color=\"000000\"/>\n"
                << "          <w:bottom w:val=\"single\" w:sz=\"4\" w:space=\"0\" w:color=\"000000\"/>\n"
                << "          <w:right w:val=\"single\" w:sz=\"4\" w:space=\"0\" w:color=\"000000\"/>\n"
                << "          <w:insideH w:val=\"single\" w:sz=\"4\" w:space=\"0\" w:color=\"000000\"/>\n"
                << "          <w:insideV w:val=\"single\" w:sz=\"4\" w:space=\"0\" w:color=\"000000\"/>\n"
                << "        </w:tblBorders>\n"
                << "      </w:tblPr>\n";
            for (int r = 0; r <= sh.maxRow; ++r) {
                doc << "      <w:tr>\n";
                for (int c = 0; c <= sh.maxCol; ++c) {
                    const CellSlot *cell = sh.getCell(r, c);
                    std::string cellTxt = cell ? cell->val.toString() : "";
                    doc << "        <w:tc><w:p><w:r>";
                    if (r == 0) doc << "<w:rPr><w:b/></w:rPr>";
                    doc << "<w:t xml:space=\"preserve\">"
                        << ODS_EscapeXML(cellTxt) << "</w:t></w:r></w:p></w:tc>\n";
                }
                doc << "      </w:tr>\n";
            }
            doc << "    </w:tbl>\n";
        }

        doc << "  </w:body>\n"
            << "</w:document>\n";
        entries.push_back({"word/document.xml", doc.str()});

        if (!writeZipArchive(filepath, entries)) {
            raiseError(53, "File not found or cannot open: " + filepath);
        }
    }

    // ========================================================================
    // SPREADSHEET (.ODS / .XLSX / .CSV) SAVE & OPEN
    // ========================================================================
    void ODS_SaveAs(const std::string &filepath, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        std::string trimmedPath = trimStr(filepath);
        if (endsWithUpper(trimmedPath, ".ODT") || endsWithUpper(trimmedPath, ".DOCX") || endsWithUpper(trimmedPath, ".TXT")) {
            ODT_SaveAs(trimmedPath, globals);
            return;
        }

        gODS_FilePath = trimmedPath;
        syncGlobals(globals);

        std::string ext = (gODS_FilePath.size() >= 4)
                              ? toUpperStr(gODS_FilePath.substr(gODS_FilePath.size() - 4))
                              : toUpperStr(gODS_FilePath);

        if (ext == ".CSV") {
            std::ofstream fn(gODS_FilePath, std::ios::out | std::ios::trunc);
            if (!fn.is_open()) {
                raiseError(53, "File not found or cannot open: " + gODS_FilePath);
            }
            const SheetSlot &sh = gODS_Sheets[gODS_ActiveSheet];
            for (int r = 0; r <= sh.maxRow; ++r) {
                std::string rowLine;
                for (int c = 0; c <= sh.maxCol; ++c) {
                    if (c > 0) rowLine += ",";
                    const CellSlot *cell = sh.getCell(r, c);
                    if (cell && (cell->type == 1 || cell->type == 2)) {
                        rowLine += cell->val.toString();
                    }
                }
                fn << rowLine << "\n";
            }
            return;
        }

        if (ext == "XLSX") {
            std::vector<ZipEntryData> entries;

            std::ostringstream ct;
            ct << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
               << "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">\n"
               << "  <Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>\n"
               << "  <Default Extension=\"xml\" ContentType=\"application/xml\"/>\n"
               << "  <Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>\n"
               << "  <Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>\n";
            for (int s = 1; s <= gODS_SheetCount; ++s) {
                ct << "  <Override PartName=\"/xl/worksheets/sheet" << s
                   << ".xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>\n";
            }
            ct << "</Types>\n";
            entries.push_back({"[Content_Types].xml", ct.str()});

            std::string rootRels =
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">\n"
                "  <Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>\n"
                "</Relationships>\n";
            entries.push_back({"_rels/.rels", rootRels});

            std::ostringstream wb;
            wb << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
               << "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" "
                  "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">\n"
               << "  <sheets>\n";
            for (int s = 1; s <= gODS_SheetCount; ++s) {
                wb << "    <sheet name=\"" << ODS_EscapeXML(gODS_Sheets[s].name)
                   << "\" sheetId=\"" << s << "\" r:id=\"rId" << s << "\"/>\n";
            }
            wb << "  </sheets>\n"
               << "</workbook>\n";
            entries.push_back({"xl/workbook.xml", wb.str()});

            std::ostringstream wbRels;
            wbRels << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                   << "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">\n";
            for (int s = 1; s <= gODS_SheetCount; ++s) {
                wbRels << "  <Relationship Id=\"rId" << s
                       << "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" "
                          "Target=\"worksheets/sheet" << s << ".xml\"/>\n";
            }
            wbRels << "  <Relationship Id=\"rIdStyles\" "
                      "Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" "
                      "Target=\"styles.xml\"/>\n"
                   << "</Relationships>\n";
            entries.push_back({"xl/_rels/workbook.xml.rels", wbRels.str()});

            std::string stylesXml =
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">\n"
                "  <fonts count=\"1\"><font><sz val=\"10\"/><name val=\"Arial\"/></font></fonts>\n"
                "  <fills count=\"2\"><fill><patternFill patternType=\"none\"/></fill><fill><patternFill patternType=\"gray125\"/></fill></fills>\n"
                "  <borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>\n"
                "  <cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>\n"
                "  <cellXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/></cellXfs>\n"
                "  <cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>\n"
                "</styleSheet>\n";
            entries.push_back({"xl/styles.xml", stylesXml});

            for (int s = 1; s <= gODS_SheetCount; ++s) {
                const SheetSlot &sh = gODS_Sheets[s];
                std::ostringstream ws;
                ws << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                   << "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">\n"
                   << "  <sheetData>\n";
                for (const auto &rPair : sh.rows) {
                    int r = rPair.first;
                    if (rPair.second.empty()) continue;
                    ws << "    <row r=\"" << (r + 1) << "\">\n";
                    for (const auto &cPair : rPair.second) {
                        int c = cPair.first;
                        const CellSlot &cell = cPair.second;
                        std::string cellRef = ColIndexToName(c) + std::to_string(r + 1);
                        if (cell.type == 1) {
                            ws << "      <c r=\"" << cellRef << "\" t=\"n\"><v>"
                               << cell.val.toString() << "</v></c>\n";
                        } else if (cell.type == 2) {
                            ws << "      <c r=\"" << cellRef << "\" t=\"inlineStr\"><is><t>"
                               << ODS_EscapeXML(cell.val.toString()) << "</t></is></c>\n";
                        }
                    }
                    ws << "    </row>\n";
                }
                ws << "  </sheetData>\n"
                   << "</worksheet>\n";
                entries.push_back({"xl/worksheets/sheet" + std::to_string(s) + ".xml", ws.str()});
            }

            if (!writeZipArchive(gODS_FilePath, entries)) {
                raiseError(53, "File not found or cannot open: " + gODS_FilePath);
            }
            return;
        }

        // Default: OpenDocument Spreadsheet (.ods)
        std::vector<ZipEntryData> odsEntries;
        odsEntries.push_back({"mimetype", "application/vnd.oasis.opendocument.spreadsheet"});

        std::string manifestXml =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            "<manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.2\">\n"
            "  <manifest:file-entry manifest:full-path=\"/\" manifest:version=\"1.2\" manifest:media-type=\"application/vnd.oasis.opendocument.spreadsheet\"/>\n"
            "  <manifest:file-entry manifest:full-path=\"content.xml\" manifest:media-type=\"text/xml\"/>\n"
            "</manifest:manifest>\n";
        odsEntries.push_back({"META-INF/manifest.xml", manifestXml});

        std::ostringstream content;
        content << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                << "<office:document-content "
                   "xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" "
                   "xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" "
                   "xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" "
                   "office:version=\"1.2\">\n"
                << "  <office:body>\n"
                << "    <office:spreadsheet>\n";
        for (int s = 1; s <= gODS_SheetCount; ++s) {
            const SheetSlot &sh = gODS_Sheets[s];
            content << "      <table:table table:name=\"" << ODS_EscapeXML(sh.name) << "\">\n";
            for (int r = 0; r <= sh.maxRow; ++r) {
                content << "        <table:table-row>\n";
                for (int c = 0; c <= sh.maxCol; ++c) {
                    const CellSlot *cell = sh.getCell(r, c);
                    if (cell && cell->type == 1) {
                        std::string numStr = cell->val.toString();
                        content << "          <table:table-cell office:value-type=\"float\" office:value=\""
                                << numStr << "\"><text:p>" << numStr << "</text:p></table:table-cell>\n";
                    } else if (cell && cell->type == 2) {
                        std::string txtStr = ODS_EscapeXML(cell->val.toString());
                        content << "          <table:table-cell office:value-type=\"string\"><text:p>"
                                << txtStr << "</text:p></table:table-cell>\n";
                    } else {
                        content << "          <table:table-cell/>\n";
                    }
                }
                content << "        </table:table-row>\n";
            }
            content << "      </table:table>\n";
        }
        content << "    </office:spreadsheet>\n"
                << "  </office:body>\n"
                << "</office:document-content>\n";
        odsEntries.push_back({"content.xml", content.str()});

        if (!writeZipArchive(gODS_FilePath, odsEntries)) {
            raiseError(53, "File not found or cannot open: " + gODS_FilePath);
        }
    }

    static void parseXlsxWorksheetCells(int sheetIdx,
                                        const std::string &wsXml,
                                        const std::vector<std::string> &sharedStrings) {
        if (sheetIdx < 1 || sheetIdx > gODS_SheetCount) return;
        SheetSlot &sh = gODS_Sheets[sheetIdx];

        size_t pos = 0;
        size_t n = wsXml.size();
        while (pos < n) {
            size_t lt = wsXml.find("<c", pos);
            if (lt == std::string::npos) break;
            if (lt + 2 < n && wsXml[lt + 2] != ' ' && wsXml[lt + 2] != '>' && wsXml[lt + 2] != '/') {
                pos = lt + 2;
                continue;
            }
            size_t gt = wsXml.find('>', lt + 2);
            if (gt == std::string::npos) break;

            std::string openTag = wsXml.substr(lt, gt - lt + 1);
            pos = gt + 1;

            bool selfClosing = (openTag.size() >= 2 && openTag[openTag.size() - 2] == '/');
            std::string cellChunk = openTag;
            if (!selfClosing) {
                size_t closeC = wsXml.find("</c>", pos);
                if (closeC != std::string::npos) {
                    cellChunk = wsXml.substr(lt, (closeC + 4) - lt);
                    pos = closeC + 4;
                }
            }

            std::string cRef  = ODS_ExtractAttr(openTag, "r");
            std::string cType = ODS_ExtractAttr(openTag, "t");
            if (cRef.empty()) continue;

            int xCol = 0, xRow = 0;
            ODS_ParseA1Ref(cRef, xCol, xRow);
            if (xRow < 0 || xCol < 0) continue;

            if (cType == "inlineStr") {
                std::string xTxt = ODS_ExtractTagText(cellChunk, "<t>", "</text>");
                if (xTxt.empty()) xTxt = ODS_ExtractTagText(cellChunk, "<t>", "</t>");
                if (xTxt.empty() && cellChunk.find("<is>") != std::string::npos) {
                    xTxt = stripXmlTags(ODS_ExtractTagText(cellChunk, "<is>", "</is>"));
                }
                sh.rows[xRow][xCol] = {2, Variant::fromString(xTxt)};
                if (xRow > sh.maxRow) sh.maxRow = xRow;
                if (xCol > sh.maxCol) sh.maxCol = xCol;
            } else if (cType == "s") {
                std::string vStr = ODS_ExtractTagText(cellChunk, "<v>", "</v>");
                std::string xTxt;
                if (!vStr.empty()) {
                    try {
                        int ssIdx = std::stoi(vStr);
                        if (ssIdx >= 0 && static_cast<size_t>(ssIdx) < sharedStrings.size()) {
                            xTxt = sharedStrings[ssIdx];
                        }
                    } catch (...) {
                        xTxt = ODS_ExtractTagText(cellChunk, "<t>", "</t>");
                    }
                } else {
                    xTxt = ODS_ExtractTagText(cellChunk, "<t>", "</t>");
                }
                sh.rows[xRow][xCol] = {2, Variant::fromString(xTxt)};
                if (xRow > sh.maxRow) sh.maxRow = xRow;
                if (xCol > sh.maxCol) sh.maxCol = xCol;
            } else if (cType == "str") {
                std::string xTxt = ODS_ExtractTagText(cellChunk, "<v>", "</v>");
                sh.rows[xRow][xCol] = {2, Variant::fromString(xTxt)};
                if (xRow > sh.maxRow) sh.maxRow = xRow;
                if (xCol > sh.maxCol) sh.maxCol = xCol;
            } else if (cType == "n" || (cType.empty() && cellChunk.find("<v>") != std::string::npos)) {
                std::string xVal = ODS_ExtractTagText(cellChunk, "<v>", "</v>");
                if (!xVal.empty()) {
                    double d = 0.0;
                    try { d = std::stod(xVal); } catch (...) { d = 0.0; }
                    sh.rows[xRow][xCol] = {1, Variant::fromDouble(d)};
                    if (xRow > sh.maxRow) sh.maxRow = xRow;
                    if (xCol > sh.maxCol) sh.maxCol = xCol;
                }
            }
        }
    }

    static void parseXlsxZipPackage(const std::string &zipPath, const std::string &fileBytes, const std::string &workbookXml) {
        std::vector<std::string> sharedStrings;
        std::string ssXml = extractZipEntry(zipPath, fileBytes, "xl/sharedStrings.xml");
        if (!ssXml.empty()) {
            size_t pos = 0;
            while (true) {
                size_t siStart = ssXml.find("<si", pos);
                if (siStart == std::string::npos) break;
                size_t siGt = ssXml.find('>', siStart);
                size_t siEnd = ssXml.find("</si>", siGt);
                if (siGt == std::string::npos || siEnd == std::string::npos) break;
                std::string siBody = ssXml.substr(siGt + 1, siEnd - (siGt + 1));
                sharedStrings.push_back(stripXmlTags(siBody));
                pos = siEnd + 5;
            }
        }

        std::unordered_map<std::string, std::string> relsMap;
        std::string relsXml = extractZipEntry(zipPath, fileBytes, "xl/_rels/workbook.xml.rels");
        if (!relsXml.empty()) {
            size_t pos = 0;
            while (true) {
                size_t lt = relsXml.find("<Relationship ", pos);
                if (lt == std::string::npos) break;
                size_t gt = relsXml.find('>', lt);
                if (gt == std::string::npos) break;
                std::string tag = relsXml.substr(lt, gt - lt + 1);
                std::string rId = ODS_ExtractAttr(tag, "Id");
                std::string target = ODS_ExtractAttr(tag, "Target");
                if (!rId.empty() && !target.empty()) {
                    if (!target.empty() && target[0] == '/') target.erase(0, 1);
                    else if (target.rfind("xl/", 0) != 0) target = "xl/" + target;
                    relsMap[rId] = target;
                }
                pos = gt + 1;
            }
        }

        int sheetIdx = 0;
        size_t pos = 0;
        while (true) {
            size_t lt = workbookXml.find("<sheet ", pos);
            if (lt == std::string::npos) break;
            size_t gt = workbookXml.find('>', lt);
            if (gt == std::string::npos) break;
            std::string tag = workbookXml.substr(lt, gt - lt + 1);
            pos = gt + 1;

            std::string sName = ODS_ExtractAttr(tag, "name");
            std::string rId   = ODS_ExtractAttr(tag, "r:id");
            ++sheetIdx;
            if (sheetIdx > gODS_SheetCount) {
                gODS_SheetCount = sheetIdx;
                if (static_cast<size_t>(sheetIdx + 1) > gODS_Sheets.size()) {
                    gODS_Sheets.resize(sheetIdx + 1);
                }
            }
            gODS_Sheets[sheetIdx].clear();
            gODS_Sheets[sheetIdx].name = sName.empty() ? ("Sheet" + std::to_string(sheetIdx)) : sName;

            std::string wsPath = "xl/worksheets/sheet" + std::to_string(sheetIdx) + ".xml";
            if (!rId.empty() && relsMap.count(rId)) {
                wsPath = relsMap[rId];
            }
            std::string wsXml = extractZipEntry(zipPath, fileBytes, wsPath);
            if (!wsXml.empty()) {
                parseXlsxWorksheetCells(sheetIdx, wsXml, sharedStrings);
            }
        }
    }

    static void parseOdsOrXlsxXmlContent(const std::string &xmlContent) {
        int curSheet = 0;
        int curRow   = -1;
        int curRowRepeat = 1;
        int curCol   = -1;

        size_t pos = 0;
        size_t n = xmlContent.size();
        while (pos < n) {
            size_t lt = xmlContent.find('<', pos);
            if (lt == std::string::npos) break;
            size_t gt = xmlContent.find('>', lt + 1);
            if (gt == std::string::npos) break;

            std::string tag = xmlContent.substr(lt, gt - lt + 1);
            pos = gt + 1;

            if (tag.rfind("<table:table ", 0) == 0) {
                std::string sName = ODS_ExtractAttr(tag, "table:name");
                ++curSheet;
                if (curSheet > gODS_SheetCount) {
                    gODS_SheetCount = curSheet;
                    if (static_cast<size_t>(curSheet + 1) > gODS_Sheets.size()) {
                        gODS_Sheets.resize(curSheet + 1);
                    }
                }
                gODS_Sheets[curSheet].clear();
                gODS_Sheets[curSheet].name = sName.empty() ? ("Sheet" + std::to_string(curSheet)) : sName;
                curRow = 0;
            } else if (tag.rfind("<table:table-row", 0) == 0) {
                std::string repStr = ODS_ExtractAttr(tag, "table:number-rows-repeated");
                curRowRepeat = 1;
                if (!repStr.empty()) {
                    try { curRowRepeat = std::max(1, std::stoi(repStr)); } catch (...) { curRowRepeat = 1; }
                }
                curCol = 0;
                if (tag.size() >= 2 && tag[tag.size() - 2] == '/') {
                    if (curRowRepeat < 10000) curRow += curRowRepeat;
                }
            } else if (tag.rfind("</table:table-row>", 0) == 0) {
                if (curRowRepeat < 10000) curRow += curRowRepeat;
                else ++curRow;
                curRowRepeat = 1;
            } else if (tag.rfind("<table:table-cell", 0) == 0) {
                std::string colRepStr = ODS_ExtractAttr(tag, "table:number-columns-repeated");
                int colRep = 1;
                if (!colRepStr.empty()) {
                    try { colRep = std::max(1, std::stoi(colRepStr)); } catch (...) { colRep = 1; }
                }

                bool selfClosing = (tag.size() >= 2 && tag[tag.size() - 2] == '/');
                std::string cellChunk = tag;
                if (!selfClosing) {
                    size_t closeCell = xmlContent.find("</table:table-cell>", pos);
                    if (closeCell != std::string::npos) {
                        cellChunk = xmlContent.substr(lt, (closeCell + 19) - lt);
                        pos = closeCell + 19;
                    }
                }

                std::string vType = ODS_ExtractAttr(tag, "office:value-type");
                if (curSheet >= 1 && curRow >= 0 && curCol >= 0) {
                    SheetSlot &sh = gODS_Sheets[curSheet];
                    int effRowRep = (curRowRepeat < 1000) ? curRowRepeat : 1;
                    int effColRep = (colRep < 1000) ? colRep : 1;
                    if (vType == "float" || vType == "percentage" || vType == "currency") {
                        std::string vStr = ODS_ExtractAttr(tag, "office:value");
                        double d = 0.0;
                        try { d = std::stod(vStr); } catch (...) { d = 0.0; }
                        for (int rr = 0; rr < effRowRep; ++rr) {
                            for (int cc = 0; cc < effColRep; ++cc) {
                                int rTarget = curRow + rr;
                                int cTarget = curCol + cc;
                                sh.rows[rTarget][cTarget] = {1, Variant::fromDouble(d)};
                                if (rTarget > sh.maxRow) sh.maxRow = rTarget;
                                if (cTarget > sh.maxCol) sh.maxCol = cTarget;
                            }
                        }
                    } else if (vType == "string" || (!selfClosing && cellChunk.find("<text:p") != std::string::npos)) {
                        std::string tStr = ODS_ExtractTagText(cellChunk, "<text:p>", "</text:p>");
                        if (tStr.empty() && cellChunk.find("<text:p ") != std::string::npos) {
                            size_t pStart = cellChunk.find("<text:p ");
                            size_t pGt = cellChunk.find('>', pStart);
                            size_t pEnd = cellChunk.find("</text:p>", pGt);
                            if (pGt != std::string::npos && pEnd != std::string::npos) {
                                tStr = stripXmlTags(cellChunk.substr(pGt + 1, pEnd - (pGt + 1)));
                            }
                        }
                        if (!tStr.empty() || vType == "string") {
                            for (int rr = 0; rr < effRowRep; ++rr) {
                                for (int cc = 0; cc < effColRep; ++cc) {
                                    int rTarget = curRow + rr;
                                    int cTarget = curCol + cc;
                                    sh.rows[rTarget][cTarget] = {2, Variant::fromString(tStr)};
                                    if (rTarget > sh.maxRow) sh.maxRow = rTarget;
                                    if (cTarget > sh.maxCol) sh.maxCol = cTarget;
                                }
                            }
                        }
                    }
                }
                if (colRep < 10000) curCol += colRep;
                else ++curCol;
            } else if (tag.rfind("<worksheet ", 0) == 0) {
                std::string xName = ODS_ExtractAttr(tag, "name");
                ++curSheet;
                if (curSheet > gODS_SheetCount) {
                    gODS_SheetCount = curSheet;
                    if (static_cast<size_t>(curSheet + 1) > gODS_Sheets.size()) {
                        gODS_Sheets.resize(curSheet + 1);
                    }
                }
                gODS_Sheets[curSheet].clear();
                gODS_Sheets[curSheet].name = xName;
            } else if (tag.rfind("<c ", 0) == 0) {
                bool selfClosing = (tag.size() >= 2 && tag[tag.size() - 2] == '/');
                std::string cellChunk = tag;
                if (!selfClosing) {
                    size_t closeC = xmlContent.find("</c>", pos);
                    if (closeC != std::string::npos) {
                        cellChunk = xmlContent.substr(lt, (closeC + 4) - lt);
                        pos = closeC + 4;
                    }
                }
                std::string cRef  = ODS_ExtractAttr(tag, "r");
                std::string cType = ODS_ExtractAttr(tag, "t");
                int xCol = 0, xRow = 0;
                ODS_ParseA1Ref(cRef, xCol, xRow);
                if (curSheet >= 1 && xRow >= 0 && xCol >= 0) {
                    SheetSlot &sh = gODS_Sheets[curSheet];
                    if (cType == "n" || (cType.empty() && cellChunk.find("<v>") != std::string::npos)) {
                        std::string xVal = ODS_ExtractTagText(cellChunk, "<v>", "</v>");
                        double d = 0.0;
                        try { d = std::stod(xVal); } catch (...) { d = 0.0; }
                        sh.rows[xRow][xCol] = {1, Variant::fromDouble(d)};
                        if (xRow > sh.maxRow) sh.maxRow = xRow;
                        if (xCol > sh.maxCol) sh.maxCol = xCol;
                    } else if (cType == "inlineStr" || cType == "s") {
                        std::string xTxt = ODS_ExtractTagText(cellChunk, "<t>", "</t>");
                        sh.rows[xRow][xCol] = {2, Variant::fromString(xTxt)};
                        if (xRow > sh.maxRow) sh.maxRow = xRow;
                        if (xCol > sh.maxCol) sh.maxCol = xCol;
                    }
                }
            }
        }
    }

    void ODS_Open(const std::string &filepath, std::unordered_map<std::string, Variant> *globals) {
        std::string trimmedPath = trimStr(filepath);
        if (endsWithUpper(trimmedPath, ".ODT") || endsWithUpper(trimmedPath, ".DOCX") || endsWithUpper(trimmedPath, ".TXT")) {
            ODT_Open(trimmedPath, globals);
            return;
        }

        ODS_New(globals);
        gODS_FilePath = trimmedPath;
        std::string ext = (gODS_FilePath.size() >= 4)
                              ? toUpperStr(gODS_FilePath.substr(gODS_FilePath.size() - 4))
                              : toUpperStr(gODS_FilePath);

        std::ifstream fn(gODS_FilePath, std::ios::binary);
        if (!fn.is_open()) {
            raiseError(53, "File not found or cannot open: " + gODS_FilePath);
        }

        if (ext == ".CSV") {
            int csvRow = 0;
            std::string rawCsv;
            while (std::getline(fn, rawCsv)) {
                if (!rawCsv.empty() && rawCsv.back() == '\r') rawCsv.pop_back();
                if (!trimStr(rawCsv).empty()) {
                    size_t pos = 0;
                    int colIdx = 0;
                    while (true) {
                        size_t comma = rawCsv.find(',', pos);
                        if (comma == std::string::npos) {
                            ODS_StoreCell(1, colIdx, csvRow, Variant::fromString(trimStr(rawCsv.substr(pos))));
                            break;
                        }
                        ODS_StoreCell(1, colIdx, csvRow, Variant::fromString(trimStr(rawCsv.substr(pos, comma - pos))));
                        pos = comma + 1;
                        ++colIdx;
                    }
                    ++csvRow;
                }
            }
            ODS_UpdateAddressGlobals(globals);
            return;
        }

        std::ostringstream rawBuf;
        rawBuf << fn.rdbuf();
        std::string fileBytes = rawBuf.str();
        fn.close();

        if (fileBytes.size() >= 4 && fileBytes[0] == 'P' && fileBytes[1] == 'K') {
            std::string odsContentXml = extractZipEntry(gODS_FilePath, fileBytes, "content.xml");
            if (!odsContentXml.empty()) {
                if (odsContentXml.find("<office:text") != std::string::npos) {
                    parseOdtXmlContent(odsContentXml);
                } else {
                    parseOdsOrXlsxXmlContent(odsContentXml);
                }
            } else {
                std::string xlsxWorkbookXml = extractZipEntry(gODS_FilePath, fileBytes, "xl/workbook.xml");
                if (!xlsxWorkbookXml.empty()) {
                    parseXlsxZipPackage(gODS_FilePath, fileBytes, xlsxWorkbookXml);
                } else {
                    std::string docxXml = extractZipEntry(gODS_FilePath, fileBytes, "word/document.xml");
                    if (!docxXml.empty()) {
                        parseDocxDocumentXml(docxXml);
                    }
                }
            }
        } else {
            parseOdsOrXlsxXmlContent(fileBytes);
        }

        gODS_ActiveSheet = 1;
        gODS_RangeLeft   = 0;
        gODS_RangeTop    = 0;
        gODS_RangeRight  = 0;
        gODS_RangeBottom = 0;
        ODS_UpdateAddressGlobals(globals);
    }

    // ========================================================================
    // DOCUMENT (.ODT / .DOCX / .TXT) OPERATIONS
    // ========================================================================
    void ODT_Open(const std::string &filepath, std::unordered_map<std::string, Variant> *globals) {
        ODS_New(globals);
        ODT_New(globals);
        gODT_FilePath = trimStr(filepath);
        gODS_FilePath = gODT_FilePath;

        std::ifstream fn(gODT_FilePath, std::ios::binary);
        if (!fn.is_open()) {
            raiseError(53, "File not found or cannot open: " + gODT_FilePath);
        }

        if (endsWithUpper(gODT_FilePath, ".TXT")) {
            std::string line;
            while (std::getline(fn, line)) {
                if (!line.empty() && line.back() == '\r') line.pop_back();
                gODT_Paragraphs.push_back({line, 0, "Text Body"});
            }
            ODS_UpdateAddressGlobals(globals);
            return;
        }

        std::ostringstream rawBuf;
        rawBuf << fn.rdbuf();
        std::string fileBytes = rawBuf.str();
        fn.close();

        if (fileBytes.size() >= 4 && fileBytes[0] == 'P' && fileBytes[1] == 'K') {
            std::string odtXml = extractZipEntry(gODT_FilePath, fileBytes, "content.xml");
            if (!odtXml.empty()) {
                parseOdtXmlContent(odtXml);
            } else {
                std::string docxXml = extractZipEntry(gODT_FilePath, fileBytes, "word/document.xml");
                if (!docxXml.empty()) {
                    parseDocxDocumentXml(docxXml);
                }
            }
        } else if (fileBytes.find("<w:document") != std::string::npos) {
            parseDocxDocumentXml(fileBytes);
        } else if (fileBytes.find("<office:document") != std::string::npos) {
            parseOdtXmlContent(fileBytes);
        } else {
            std::istringstream iss(fileBytes);
            std::string line;
            while (std::getline(iss, line)) {
                if (!line.empty() && line.back() == '\r') line.pop_back();
                gODT_Paragraphs.push_back({line, 0, "Text Body"});
            }
        }

        ODS_UpdateAddressGlobals(globals);
    }

    void ODT_SaveAs(const std::string &filepath, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        ODS_EnsureInit(globals);
        gODT_FilePath = trimStr(filepath);
        gODS_FilePath = gODT_FilePath;

        if (endsWithUpper(gODT_FilePath, ".TXT")) {
            std::ofstream ofs(gODT_FilePath, std::ios::out | std::ios::trunc);
            if (!ofs.is_open()) {
                raiseError(53, "File not found or cannot open: " + gODT_FilePath);
            }
            for (const auto &p : gODT_Paragraphs) {
                ofs << p.text << "\n";
            }
            syncGlobals(globals);
            return;
        }

        if (endsWithUpper(gODT_FilePath, ".DOCX")) {
            saveDocxZipPackage(gODT_FilePath);
            syncGlobals(globals);
            return;
        }

        saveOdtZipPackage(gODT_FilePath);
        syncGlobals(globals);
    }

    int ODT_AppendParagraph(const std::string &text, int headingLevel, const std::string &rawStyle, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        int lvl = std::clamp(headingLevel, 0, 6);
        std::string canonStyle;
        normalizeParagraphStyle(rawStyle, lvl, canonStyle);
        gODT_Paragraphs.push_back({text, lvl, canonStyle});
        syncGlobals(globals);
        return static_cast<int>(gODT_Paragraphs.size()) - 1;
    }

    void ODT_InsertParagraph(int idx, const std::string &text, int headingLevel, const std::string &rawStyle, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx > static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        int lvl = std::clamp(headingLevel, 0, 6);
        std::string canonStyle;
        normalizeParagraphStyle(rawStyle, lvl, canonStyle);
        gODT_Paragraphs.insert(gODT_Paragraphs.begin() + idx, {text, lvl, canonStyle});
        syncGlobals(globals);
    }

    void ODT_RemoveParagraph(int idx, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx >= static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        gODT_Paragraphs.erase(gODT_Paragraphs.begin() + idx);
        syncGlobals(globals);
    }

    std::string ODT_GetParagraph(int idx, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx >= static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        return gODT_Paragraphs[idx].text;
    }

    void ODT_SetParagraph(int idx, const std::string &text, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx >= static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        gODT_Paragraphs[idx].text = text;
    }

    int ODT_GetHeadingLevel(int idx, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx >= static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        return gODT_Paragraphs[idx].headingLevel;
    }

    void ODT_SetHeadingLevel(int idx, int level, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx >= static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        int lvl = std::clamp(level, 0, 6);
        gODT_Paragraphs[idx].headingLevel = lvl;
        if (lvl >= 1 && lvl <= 6) {
            gODT_Paragraphs[idx].styleName = "Heading " + std::to_string(lvl);
        } else {
            gODT_Paragraphs[idx].styleName = "Text Body";
        }
    }

    std::string ODT_GetParagraphStyle(int idx, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx >= static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        return gODT_Paragraphs[idx].styleName;
    }

    void ODT_SetParagraphStyle(int idx, const std::string &styleName, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (idx < 0 || idx >= static_cast<int>(gODT_Paragraphs.size())) {
            raiseError(9, "Paragraph index out of range");
        }
        int lvl = gODT_Paragraphs[idx].headingLevel;
        std::string canon;
        normalizeParagraphStyle(styleName, lvl, canon);
        gODT_Paragraphs[idx].headingLevel = lvl;
        gODT_Paragraphs[idx].styleName = canon;
    }

    std::string ODT_GetText(std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        std::ostringstream oss;
        for (size_t i = 0; i < gODT_Paragraphs.size(); ++i) {
            if (i > 0) oss << "\n";
            oss << gODT_Paragraphs[i].text;
        }
        return oss.str();
    }

    void ODT_SetText(const std::string &fullText, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        gODT_Paragraphs.clear();
        std::istringstream iss(fullText);
        std::string line;
        while (std::getline(iss, line)) {
            if (!line.empty() && line.back() == '\r') line.pop_back();
            gODT_Paragraphs.push_back({line, 0, "Text Body"});
        }
        syncGlobals(globals);
    }

    int ODT_SearchFrom(const std::string &needleRaw, int startIdx, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        std::string needle = toUpperStr(trimStr(needleRaw));
        if (needle.empty()) return -1;
        for (int i = std::max(0, startIdx); i < static_cast<int>(gODT_Paragraphs.size()); ++i) {
            if (toUpperStr(gODT_Paragraphs[i].text).find(needle) != std::string::npos) {
                gODT_SearchIndex = i;
                syncGlobals(globals);
                return i;
            }
        }
        return -1;
    }

    int ODT_ReplaceAll(const std::string &findStr, const std::string &replaceStr, std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        if (findStr.empty()) return 0;
        int count = 0;
        for (auto &p : gODT_Paragraphs) {
            size_t pos = 0;
            while ((pos = p.text.find(findStr, pos)) != std::string::npos) {
                p.text.replace(pos, findStr.size(), replaceStr);
                pos += replaceStr.size();
                ++count;
            }
        }
        return count;
    }

    void Sheet_PrintTable(std::unordered_map<std::string, Variant> *globals);

    void ODT_PrintDocument(std::unordered_map<std::string, Variant> *globals) {
        ODT_EnsureInit(globals);
        std::cout << "=== Document: " << (gODT_FilePath.empty() ? "(Untitled)" : gODT_FilePath)
                  << " (" << gODT_Paragraphs.size() << " paragraphs) ===\n";
        for (size_t i = 0; i < gODT_Paragraphs.size(); ++i) {
            const auto &p = gODT_Paragraphs[i];
            std::cout << "[" << i << " | " << p.styleName << "] ";
            if (p.headingLevel > 0) {
                std::cout << std::string(static_cast<size_t>(p.headingLevel), '#') << " ";
            }
            std::cout << p.text << "\n";
        }
        for (int s = 1; s <= gODS_SheetCount; ++s) {
            if (!gODS_Sheets[s].rows.empty()) {
                int prevActive = gODS_ActiveSheet;
                gODS_ActiveSheet = s;
                std::cout << "\n";
                Sheet_PrintTable(globals);
                gODS_ActiveSheet = prevActive;
            }
        }
        std::cout << "\n";
    }

    // ========================================================================
    // SPREADSHEET / TABLE SHEET OPERATIONS
    // ========================================================================
    int Sheets_hasByName(const std::string &sheetName, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        std::string target = toUpperStr(trimStr(sheetName));
        for (int i = 1; i <= gODS_SheetCount; ++i) {
            if (toUpperStr(gODS_Sheets[i].name) == target) return -1;
        }
        return 0;
    }

    int Sheets_getByName(const std::string &sheetName, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        std::string target = toUpperStr(trimStr(sheetName));
        for (int i = 1; i <= gODS_SheetCount; ++i) {
            if (toUpperStr(gODS_Sheets[i].name) == target) {
                gODS_ActiveSheet = i;
                gODS_RangeLeft   = 0;
                gODS_RangeTop    = 0;
                gODS_RangeRight  = 0;
                gODS_RangeBottom = 0;
                gODS_SearchCol   = 0;
                gODS_SearchRow   = -1;
                ODS_UpdateAddressGlobals(globals);
                return i - 1;
            }
        }
        raiseError(9, "Subscript out of range");
        return 0;
    }

    std::string Sheets_getByIndex(int idx, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        int targetSlot = idx + 1;
        if (targetSlot < 1 || targetSlot > gODS_SheetCount) {
            raiseError(9, "Subscript out of range");
        }
        gODS_ActiveSheet = targetSlot;
        gODS_RangeLeft   = 0;
        gODS_RangeTop    = 0;
        gODS_RangeRight  = 0;
        gODS_RangeBottom = 0;
        gODS_SearchCol   = 0;
        gODS_SearchRow   = -1;
        ODS_UpdateAddressGlobals(globals);
        return gODS_Sheets[gODS_ActiveSheet].name;
    }

    void Sheets_insertNewByName(const std::string &sheetName, int posIndex, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        if (Sheets_hasByName(sheetName, globals) != 0) raiseError(10, "Duplicate definition");

        int insSlot = posIndex + 1;
        if (insSlot < 1) insSlot = 1;
        if (insSlot > gODS_SheetCount + 1) insSlot = gODS_SheetCount + 1;

        SheetSlot newSheet;
        newSheet.name = trimStr(sheetName);

        gODS_Sheets.insert(gODS_Sheets.begin() + insSlot, std::move(newSheet));
        ++gODS_SheetCount;
        gODS_ActiveSheet = insSlot;
        ODS_UpdateAddressGlobals(globals);
    }

    void Sheets_removeByName(const std::string &sheetName, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        std::string target = toUpperStr(trimStr(sheetName));
        int foundSlot = 0;
        for (int i = 1; i <= gODS_SheetCount; ++i) {
            if (toUpperStr(gODS_Sheets[i].name) == target) {
                foundSlot = i;
                break;
            }
        }
        if (foundSlot == 0) raiseError(9, "Subscript out of range");
        if (gODS_SheetCount <= 1) raiseError(5, "Invalid procedure call or argument");

        gODS_Sheets.erase(gODS_Sheets.begin() + foundSlot);
        --gODS_SheetCount;
        if (gODS_ActiveSheet > gODS_SheetCount) gODS_ActiveSheet = gODS_SheetCount;
        ODS_UpdateAddressGlobals(globals);
    }

    void Sheets_copyByName(const std::string &srcName, const std::string &newName, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        std::string target = toUpperStr(trimStr(srcName));
        int srcSlot = 0;
        for (int i = 1; i <= gODS_SheetCount; ++i) {
            if (toUpperStr(gODS_Sheets[i].name) == target) {
                srcSlot = i;
                break;
            }
        }
        if (srcSlot == 0) raiseError(9, "Subscript out of range");

        SheetSlot copySheet = gODS_Sheets[srcSlot];
        Sheets_insertNewByName(newName, gODS_SheetCount, globals);
        int dstSlot = gODS_SheetCount;
        gODS_Sheets[dstSlot].maxRow = copySheet.maxRow;
        gODS_Sheets[dstSlot].maxCol = copySheet.maxCol;
        gODS_Sheets[dstSlot].rows   = std::move(copySheet.rows);
    }

    std::string Sheet_getCellRangeByPosition(int nLeft, int nTop, int nRight, int nBottom, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        if (nLeft < 0 || nTop < 0 || nRight < nLeft || nBottom < nTop) {
            raiseError(9, "Subscript out of range");
        }
        gODS_RangeLeft   = nLeft;
        gODS_RangeTop    = nTop;
        gODS_RangeRight  = nRight;
        gODS_RangeBottom = nBottom;
        ODS_UpdateAddressGlobals(globals);
        return ColIndexToName(nLeft) + std::to_string(nTop + 1) + ":" +
               ColIndexToName(nRight) + std::to_string(nBottom + 1);
    }

    std::string Sheet_getCellRangeByName(const std::string &rangeRef, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        std::string s = trimStr(rangeRef);
        size_t colonPos = s.find(':');
        int c1 = 0, r1 = 0, c2 = 0, r2 = 0;
        if (colonPos != std::string::npos) {
            ODS_ParseA1Ref(s.substr(0, colonPos), c1, r1);
            ODS_ParseA1Ref(s.substr(colonPos + 1), c2, r2);
        } else {
            ODS_ParseA1Ref(s, c1, r1);
            c2 = c1;
            r2 = r1;
        }
        return Sheet_getCellRangeByPosition(c1, r1, c2, r2, globals);
    }

    void Sheet_clearContents(std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        SheetSlot &sh = gODS_Sheets[gODS_ActiveSheet];
        auto rIt = sh.rows.lower_bound(gODS_RangeTop);
        while (rIt != sh.rows.end() && rIt->first <= gODS_RangeBottom) {
            auto cIt = rIt->second.lower_bound(gODS_RangeLeft);
            while (cIt != rIt->second.end() && cIt->first <= gODS_RangeRight) {
                cIt = rIt->second.erase(cIt);
            }
            if (rIt->second.empty()) {
                rIt = sh.rows.erase(rIt);
            } else {
                ++rIt;
            }
        }
    }

    Variant getValue(std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        const CellSlot *cell = gODS_Sheets[gODS_ActiveSheet].getCell(gODS_RangeTop, gODS_RangeLeft);
        if (!cell) return Variant::fromDouble(0.0);
        if (cell->type == 1) return Variant::fromDouble(cell->val.toDouble());
        if (cell->type == 2) return Variant::fromString(cell->val.toString());
        return Variant::fromDouble(0.0);
    }

    void setValue(const Variant &val, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        ODS_StoreCell(gODS_ActiveSheet, gODS_RangeLeft, gODS_RangeTop, val);
    }

    std::string getString(std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        const CellSlot *cell = gODS_Sheets[gODS_ActiveSheet].getCell(gODS_RangeTop, gODS_RangeLeft);
        if (!cell) return "";
        return cell->val.toString();
    }

    void setString(const std::string &txt, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        SheetSlot &sh = gODS_Sheets[gODS_ActiveSheet];
        sh.rows[gODS_RangeTop][gODS_RangeLeft] = {2, Variant::fromString(txt)};
        if (gODS_RangeTop > sh.maxRow) sh.maxRow = gODS_RangeTop;
        if (gODS_RangeLeft > sh.maxCol) sh.maxCol = gODS_RangeLeft;
    }

    void setDataArray(const Variant &dataArr, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        if (dataArr.type != VarType::ARRAY || dataArr.dims.empty()) {
            raiseError(9, "Subscript out of range");
        }

        int rLow  = dataArr.dims[0].low;
        int rHigh = dataArr.dims[0].high;

        if (dataArr.dims.size() >= 2) {
            int cLow  = dataArr.dims[1].low;
            int cHigh = dataArr.dims[1].high;
            for (int r = rLow; r <= rHigh; ++r) {
                int targetRow = gODS_RangeTop + (r - rLow);
                if (targetRow <= gODS_RangeBottom || gODS_RangeTop == gODS_RangeBottom) {
                    for (int c = cLow; c <= cHigh; ++c) {
                        int targetCol = gODS_RangeLeft + (c - cLow);
                        if (targetCol <= gODS_RangeRight || gODS_RangeLeft == gODS_RangeRight) {
                            ODS_StoreCell(gODS_ActiveSheet, targetCol, targetRow, dataArr.at({r, c}));
                        }
                    }
                }
            }
            if (gODS_RangeTop + (rHigh - rLow) > gODS_RangeBottom) {
                gODS_RangeBottom = gODS_RangeTop + (rHigh - rLow);
            }
            if (gODS_RangeLeft + (cHigh - cLow) > gODS_RangeRight) {
                gODS_RangeRight = gODS_RangeLeft + (cHigh - cLow);
            }
            syncGlobals(globals);
            return;
        }

        for (int idx = rLow; idx <= rHigh; ++idx) {
            ODS_StoreCell(gODS_ActiveSheet, gODS_RangeLeft + (idx - rLow), gODS_RangeTop, dataArr.at({idx}));
        }
    }

    Variant getData(std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        int nRows = std::max(1, gODS_RangeBottom - gODS_RangeTop + 1);
        int nCols = std::max(1, gODS_RangeRight - gODS_RangeLeft + 1);

        Variant outArr = Variant::makeArray({{0, nRows - 1}, {0, nCols - 1}}, VarType::EMPTY);
        const SheetSlot &sh = gODS_Sheets[gODS_ActiveSheet];
        for (int r = 0; r < nRows; ++r) {
            int sheetR = gODS_RangeTop + r;
            for (int c = 0; c < nCols; ++c) {
                int sheetC = gODS_RangeLeft + c;
                const CellSlot *cell = sh.getCell(sheetR, sheetC);
                if (cell && cell->type == 1) {
                    outArr.at({r, c}) = Variant::fromDouble(cell->val.toDouble());
                } else if (cell && cell->type == 2) {
                    outArr.at({r, c}) = Variant::fromString(cell->val.toString());
                } else {
                    outArr.at({r, c}) = Variant::fromString("");
                }
            }
        }
        return outArr;
    }

    int ODS_SearchFrom(const std::string &searchText, int startRow, int startCol, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        std::string needle = toUpperStr(trimStr(searchText));
        if (needle.empty()) return 0;

        const SheetSlot &sh = gODS_Sheets[gODS_ActiveSheet];
        for (auto rIt = sh.rows.lower_bound(startRow); rIt != sh.rows.end(); ++rIt) {
            int r = rIt->first;
            int cBegin = (r == startRow) ? startCol : 0;
            for (auto cIt = rIt->second.lower_bound(cBegin); cIt != rIt->second.end(); ++cIt) {
                int c = cIt->first;
                if (cIt->second.type != 0) {
                    std::string cellStr = toUpperStr(cIt->second.val.toString());
                    if (cellStr.find(needle) != std::string::npos) {
                        gODS_RangeLeft   = c;
                        gODS_RangeTop    = r;
                        gODS_RangeRight  = c;
                        gODS_RangeBottom = r;
                        gODS_SearchCol   = c;
                        gODS_SearchRow   = r;
                        ODS_UpdateAddressGlobals(globals);
                        return -1;
                    }
                }
            }
        }
        return 0;
    }

    int SearchNext(const std::string &searchText, std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        int nextR = 0, nextC = 0;
        if (gODS_SearchRow < 0) {
            nextR = 0;
            nextC = 0;
        } else {
            nextR = gODS_SearchRow;
            nextC = gODS_SearchCol + 1;
            if (nextC > gODS_Sheets[gODS_ActiveSheet].maxCol) {
                nextC = 0;
                ++nextR;
            }
        }
        return ODS_SearchFrom(searchText, nextR, nextC, globals);
    }

    Variant getCellAddress(std::unordered_map<std::string, Variant> *globals) {
        ODS_UpdateAddressGlobals(globals);
        Variant addr = Variant::makeArray({{0, 2}}, VarType::INTEGER);
        addr.at({0}).assignCoerced(Variant::fromInt(CellAddress_Sheet));
        addr.at({1}).assignCoerced(Variant::fromInt(CellAddress_Column));
        addr.at({2}).assignCoerced(Variant::fromInt(CellAddress_Row));
        return addr;
    }

    void Sheet_PrintTable(std::unordered_map<std::string, Variant> *globals) {
        ODS_EnsureInit(globals);
        int s = gODS_ActiveSheet;
        const SheetSlot &sh = gODS_Sheets[s];
        std::cout << "=== Sheet [" << (s - 1) << "]: " << sh.name << " ===\n";
        std::string hdr = rpadStr("Row", 6, ' ');
        for (int c = 0; c <= sh.maxCol; ++c) {
            hdr += rpadStr(ColIndexToName(c), 14, ' ');
        }
        std::cout << hdr << "\n";
        std::cout << std::string(static_cast<size_t>(6 + (sh.maxCol + 1) * 14), '-') << "\n";
        for (int r = 0; r <= sh.maxRow; ++r) {
            std::string lineStr = rpadStr(std::to_string(r + 1), 6, ' ');
            for (int c = 0; c <= sh.maxCol; ++c) {
                const CellSlot *cell = sh.getCell(r, c);
                std::string cellDisp = (!cell) ? "." : cell->val.toString();
                lineStr += rpadStr(cellDisp, 14, ' ');
            }
            std::cout << lineStr << "\n";
        }
        std::cout << "\n";
    }

    bool isOfficeRoutine(const std::string &u) {
        static const std::unordered_set<std::string> officeNames = {
            // Spreadsheet (.ods / .xlsx / .csv) routines
            "ODS_ENSUREINIT", "COLINDEXTONAME", "COLNAMETOINDEX",
            "ODS_UPDATEADDRESSGLOBALS", "ODS_CLEARSHEETSLOT", "ODS_ISNUMERICTEXT",
            "ODS_STORECELL", "ODS_PARSEA1REF",
            "ODS.NEW", "WORKBOOK.NEW", "ODS.CLOSE", "WORKBOOK.CLOSE",
            "ODS_ESCAPEXML", "ODS_UNESCAPEXML", "ODS_EXTRACTATTR", "ODS_EXTRACTTAGTEXT",
            "ODS.SAVEAS", "ODS.SAVE", "WORKBOOK.SAVEAS", "WORKBOOK.SAVE",
            "ODS.OPEN", "WORKBOOK.OPEN",
            "SHEETS.GETCOUNT", "SHEETS.HASBYNAME", "SHEETS.GETBYNAME", "SHEETS.GETBYINDEX",
            "SHEETS.GETNAME", "SHEET.GETNAME",
            "SHEETS.INSERTNEWBYNAME", "SHEETS.INSERTBYNAME", "SHEETS.REMOVEBYNAME", "SHEETS.COPYBYNAME",
            "SHEET.GETCELLRANGEBYPOSITION", "GETCELLRANGEBYPOSITION",
            "SHEET.GETCELLBYPOSITION", "GETCELLBYPOSITION",
            "SHEET.GETCELLRANGEBYNAME", "GETCELLRANGEBYNAME",
            "SHEET.GETCELLBYNAME", "GETCELLBYNAME",
            "SHEET.CLEARCONTENTS",
            "GETVALUE", "CELL.GETVALUE", "SETVALUE", "CELL.SETVALUE",
            "GETSTRING", "CELL.GETSTRING", "SETSTRING", "CELL.SETSTRING",
            "GETCELLVALUE", "SETCELLVALUE",
            "SETDATAARRAY", "RANGE.SETDATAARRAY", "SETDATA",
            "GETDATA", "GETDATAARRAY", "RANGE.GETDATA", "RANGE.GETDATAARRAY",
            "ODS_SEARCHFROM", "SEARCHSTRING", "SHEET.SEARCHSTRING", "SEARCHNEXT",
            "GETCELLADDRESS", "CELL.GETCELLADDRESS",
            "GETCELLADDRESSSHEET", "GETCELLADDRESSCOL", "GETCELLADDRESSROW", "GETCELLADDRESSSTRING",
            "SHEET.PRINTTABLE",

            // Document (.odt / .docx / .txt) routines
            "ODT_ENSUREINIT",
            "ODT.NEW", "DOC.NEW", "DOCX.NEW", "DOCUMENT.NEW", "WRITER.NEW",
            "ODT.CLOSE", "DOC.CLOSE", "DOCX.CLOSE", "DOCUMENT.CLOSE", "WRITER.CLOSE",
            "ODT.OPEN", "DOC.OPEN", "DOCX.OPEN", "DOCUMENT.OPEN", "WRITER.OPEN",
            "ODT.SAVEAS", "DOC.SAVEAS", "DOCX.SAVEAS", "DOCUMENT.SAVEAS", "WRITER.SAVEAS",
            "ODT.SAVE", "DOC.SAVE", "DOCX.SAVE", "DOCUMENT.SAVE", "WRITER.SAVE",
            "ODT.CLEAR", "DOC.CLEAR", "DOCX.CLEAR", "DOCUMENT.CLEAR", "WRITER.CLEAR",
            "ODT.APPENDPARAGRAPH", "DOC.APPENDPARAGRAPH", "DOCX.APPENDPARAGRAPH", "DOCUMENT.APPENDPARAGRAPH", "WRITER.APPENDPARAGRAPH", "APPENDPARAGRAPH",
            "ODT.APPENDHEADING", "DOC.APPENDHEADING", "DOCX.APPENDHEADING", "DOCUMENT.APPENDHEADING", "WRITER.APPENDHEADING", "APPENDHEADING",
            "ODT.INSERTPARAGRAPH", "DOC.INSERTPARAGRAPH", "DOCX.INSERTPARAGRAPH", "DOCUMENT.INSERTPARAGRAPH", "WRITER.INSERTPARAGRAPH", "INSERTPARAGRAPH",
            "ODT.REMOVEPARAGRAPH", "DOC.REMOVEPARAGRAPH", "DOCX.REMOVEPARAGRAPH", "DOCUMENT.REMOVEPARAGRAPH", "WRITER.REMOVEPARAGRAPH", "REMOVEPARAGRAPH",
            "ODT.GETPARAGRAPHCOUNT", "DOC.GETPARAGRAPHCOUNT", "DOCX.GETPARAGRAPHCOUNT", "DOCUMENT.GETPARAGRAPHCOUNT", "WRITER.GETPARAGRAPHCOUNT", "PARAGRAPHS.GETCOUNT", "GETPARAGRAPHCOUNT",
            "ODT.GETPARAGRAPH", "DOC.GETPARAGRAPH", "DOCX.GETPARAGRAPH", "DOCUMENT.GETPARAGRAPH", "WRITER.GETPARAGRAPH", "PARAGRAPHS.GETBYINDEX", "GETPARAGRAPH",
            "ODT.SETPARAGRAPH", "DOC.SETPARAGRAPH", "DOCX.SETPARAGRAPH", "DOCUMENT.SETPARAGRAPH", "WRITER.SETPARAGRAPH", "PARAGRAPHS.SETBYINDEX", "SETPARAGRAPH",
            "ODT.GETHEADINGLEVEL", "DOC.GETHEADINGLEVEL", "DOCX.GETHEADINGLEVEL", "DOCUMENT.GETHEADINGLEVEL", "WRITER.GETHEADINGLEVEL", "GETHEADINGLEVEL",
            "ODT.SETHEADINGLEVEL", "DOC.SETHEADINGLEVEL", "DOCX.SETHEADINGLEVEL", "DOCUMENT.SETHEADINGLEVEL", "WRITER.SETHEADINGLEVEL", "SETHEADINGLEVEL",
            "ODT.GETPARAGRAPHSTYLE", "DOC.GETPARAGRAPHSTYLE", "DOCX.GETPARAGRAPHSTYLE", "DOCUMENT.GETPARAGRAPHSTYLE", "WRITER.GETPARAGRAPHSTYLE", "GETPARAGRAPHSTYLE",
            "ODT.SETPARAGRAPHSTYLE", "DOC.SETPARAGRAPHSTYLE", "DOCX.SETPARAGRAPHSTYLE", "DOCUMENT.SETPARAGRAPHSTYLE", "WRITER.SETPARAGRAPHSTYLE", "SETPARAGRAPHSTYLE",
            "ODT.GETTEXT", "DOC.GETTEXT", "DOCX.GETTEXT", "DOCUMENT.GETTEXT", "WRITER.GETTEXT",
            "ODT.SETTEXT", "DOC.SETTEXT", "DOCX.SETTEXT", "DOCUMENT.SETTEXT", "WRITER.SETTEXT",
            "ODT.SEARCHTEXT", "DOC.SEARCHTEXT", "DOCX.SEARCHTEXT", "DOCUMENT.SEARCHTEXT", "WRITER.SEARCHTEXT", "SEARCHTEXT",
            "ODT.SEARCHNEXT", "DOC.SEARCHNEXT", "DOCX.SEARCHNEXT", "DOCUMENT.SEARCHNEXT", "WRITER.SEARCHNEXT",
            "ODT.REPLACEALL", "DOC.REPLACEALL", "DOCX.REPLACEALL", "DOCUMENT.REPLACEALL", "WRITER.REPLACEALL", "REPLACEALL",
            "ODT.PRINTDOCUMENT", "DOC.PRINTDOCUMENT", "DOCX.PRINTDOCUMENT", "DOCUMENT.PRINT", "DOCUMENT.PRINTDOCUMENT", "WRITER.PRINTDOCUMENT", "PRINTDOCUMENT"
        };
        return officeNames.count(u) > 0;
    }

    Variant dispatch(const std::string &u,
                     const std::vector<Variant> &args,
                     const std::vector<std::string> &refNames,
                     std::unordered_map<std::string, Variant> &globals,
                     std::unordered_map<std::string, Variant> *localFrame) {
        auto requireArgs = [&](size_t n) {
            if (args.size() < n) {
                raiseError(5, "Argument count mismatch calling '" + u + "': expected " +
                              std::to_string(n) + ", got " + std::to_string(args.size()) + ".");
            }
        };

        auto writeBackRef = [&](size_t idx, const Variant &val) {
            if (idx >= refNames.size() || refNames[idx].empty()) return;
            std::string vn = toUpperStr(refNames[idx]);
            if (!vn.empty() && (vn.back() == '$' || vn.back() == '%')) vn.pop_back();
            if (localFrame && localFrame->count(vn)) {
                (*localFrame)[vn].assignCoerced(val);
            } else {
                globals[vn].assignCoerced(val);
            }
        };

        // --------------------------------------------------------------------
        // Document (.odt / .docx / .txt) dispatch
        // --------------------------------------------------------------------
        if (u == "ODT_ENSUREINIT") { ODT_EnsureInit(&globals); return Variant(); }
        if (u == "ODT.NEW" || u == "DOC.NEW" || u == "DOCX.NEW" || u == "DOCUMENT.NEW" || u == "WRITER.NEW" ||
            u == "ODT.CLOSE" || u == "DOC.CLOSE" || u == "DOCX.CLOSE" || u == "DOCUMENT.CLOSE" || u == "WRITER.CLOSE" ||
            u == "ODT.CLEAR" || u == "DOC.CLEAR" || u == "DOCX.CLEAR" || u == "DOCUMENT.CLEAR" || u == "WRITER.CLEAR") {
            ODS_New(&globals);
            ODT_New(&globals);
            return Variant();
        }
        if (u == "ODT.OPEN" || u == "DOC.OPEN" || u == "DOCX.OPEN" || u == "DOCUMENT.OPEN" || u == "WRITER.OPEN") {
            requireArgs(1);
            ODT_Open(args[0].toString(), &globals);
            return Variant();
        }
        if (u == "ODT.SAVEAS" || u == "DOC.SAVEAS" || u == "DOCX.SAVEAS" || u == "DOCUMENT.SAVEAS" || u == "WRITER.SAVEAS") {
            requireArgs(1);
            ODT_SaveAs(args[0].toString(), &globals);
            return Variant();
        }
        if (u == "ODT.SAVE" || u == "DOC.SAVE" || u == "DOCX.SAVE" || u == "DOCUMENT.SAVE" || u == "WRITER.SAVE") {
            if (args.size() >= 1) {
                ODT_SaveAs(args[0].toString(), &globals);
            } else {
                if (gODT_FilePath.empty()) raiseError(52, "Invalid file name or file number");
                ODT_SaveAs(gODT_FilePath, &globals);
            }
            return Variant();
        }
        if (u == "ODT.APPENDPARAGRAPH" || u == "DOC.APPENDPARAGRAPH" || u == "DOCX.APPENDPARAGRAPH" ||
            u == "DOCUMENT.APPENDPARAGRAPH" || u == "WRITER.APPENDPARAGRAPH" || u == "APPENDPARAGRAPH") {
            requireArgs(1);
            int lvl = 0;
            std::string styleStr;
            if (args.size() >= 2) {
                if (args[1].type == VarType::STRING && ODS_IsNumericText(args[1].sVal) == 0) {
                    styleStr = args[1].sVal;
                } else {
                    lvl = static_cast<int>(args[1].toInt());
                }
            }
            return Variant::fromInt(ODT_AppendParagraph(args[0].toString(), lvl, styleStr, &globals));
        }
        if (u == "ODT.APPENDHEADING" || u == "DOC.APPENDHEADING" || u == "DOCX.APPENDHEADING" ||
            u == "DOCUMENT.APPENDHEADING" || u == "WRITER.APPENDHEADING" || u == "APPENDHEADING") {
            requireArgs(1);
            int lvl = (args.size() >= 2) ? std::clamp(static_cast<int>(args[1].toInt()), 1, 6) : 1;
            return Variant::fromInt(ODT_AppendParagraph(args[0].toString(), lvl, "", &globals));
        }
        if (u == "ODT.INSERTPARAGRAPH" || u == "DOC.INSERTPARAGRAPH" || u == "DOCX.INSERTPARAGRAPH" ||
            u == "DOCUMENT.INSERTPARAGRAPH" || u == "WRITER.INSERTPARAGRAPH" || u == "INSERTPARAGRAPH") {
            requireArgs(2);
            int lvl = 0;
            std::string styleStr;
            if (args.size() >= 3) {
                if (args[2].type == VarType::STRING && ODS_IsNumericText(args[2].sVal) == 0) {
                    styleStr = args[2].sVal;
                } else {
                    lvl = static_cast<int>(args[2].toInt());
                }
            }
            ODT_InsertParagraph(static_cast<int>(args[0].toInt()), args[1].toString(), lvl, styleStr, &globals);
            return Variant();
        }
        if (u == "ODT.REMOVEPARAGRAPH" || u == "DOC.REMOVEPARAGRAPH" || u == "DOCX.REMOVEPARAGRAPH" ||
            u == "DOCUMENT.REMOVEPARAGRAPH" || u == "WRITER.REMOVEPARAGRAPH" || u == "REMOVEPARAGRAPH") {
            requireArgs(1);
            ODT_RemoveParagraph(static_cast<int>(args[0].toInt()), &globals);
            return Variant();
        }
        if (u == "ODT.GETPARAGRAPHCOUNT" || u == "DOC.GETPARAGRAPHCOUNT" || u == "DOCX.GETPARAGRAPHCOUNT" ||
            u == "DOCUMENT.GETPARAGRAPHCOUNT" || u == "WRITER.GETPARAGRAPHCOUNT" ||
            u == "PARAGRAPHS.GETCOUNT" || u == "GETPARAGRAPHCOUNT") {
            ODT_EnsureInit(&globals);
            return Variant::fromInt(static_cast<int64_t>(gODT_Paragraphs.size()));
        }
        if (u == "ODT.GETPARAGRAPH" || u == "DOC.GETPARAGRAPH" || u == "DOCX.GETPARAGRAPH" ||
            u == "DOCUMENT.GETPARAGRAPH" || u == "WRITER.GETPARAGRAPH" ||
            u == "PARAGRAPHS.GETBYINDEX" || u == "GETPARAGRAPH") {
            requireArgs(1);
            return Variant::fromString(ODT_GetParagraph(static_cast<int>(args[0].toInt()), &globals));
        }
        if (u == "ODT.SETPARAGRAPH" || u == "DOC.SETPARAGRAPH" || u == "DOCX.SETPARAGRAPH" ||
            u == "DOCUMENT.SETPARAGRAPH" || u == "WRITER.SETPARAGRAPH" ||
            u == "PARAGRAPHS.SETBYINDEX" || u == "SETPARAGRAPH") {
            requireArgs(2);
            ODT_SetParagraph(static_cast<int>(args[0].toInt()), args[1].toString(), &globals);
            return Variant();
        }
        if (u == "ODT.GETHEADINGLEVEL" || u == "DOC.GETHEADINGLEVEL" || u == "DOCX.GETHEADINGLEVEL" ||
            u == "DOCUMENT.GETHEADINGLEVEL" || u == "WRITER.GETHEADINGLEVEL" || u == "GETHEADINGLEVEL") {
            requireArgs(1);
            return Variant::fromInt(ODT_GetHeadingLevel(static_cast<int>(args[0].toInt()), &globals));
        }
        if (u == "ODT.SETHEADINGLEVEL" || u == "DOC.SETHEADINGLEVEL" || u == "DOCX.SETHEADINGLEVEL" ||
            u == "DOCUMENT.SETHEADINGLEVEL" || u == "WRITER.SETHEADINGLEVEL" || u == "SETHEADINGLEVEL") {
            requireArgs(2);
            ODT_SetHeadingLevel(static_cast<int>(args[0].toInt()), static_cast<int>(args[1].toInt()), &globals);
            return Variant();
        }
        if (u == "ODT.GETPARAGRAPHSTYLE" || u == "DOC.GETPARAGRAPHSTYLE" || u == "DOCX.GETPARAGRAPHSTYLE" ||
            u == "DOCUMENT.GETPARAGRAPHSTYLE" || u == "WRITER.GETPARAGRAPHSTYLE" || u == "GETPARAGRAPHSTYLE") {
            requireArgs(1);
            return Variant::fromString(ODT_GetParagraphStyle(static_cast<int>(args[0].toInt()), &globals));
        }
        if (u == "ODT.SETPARAGRAPHSTYLE" || u == "DOC.SETPARAGRAPHSTYLE" || u == "DOCX.SETPARAGRAPHSTYLE" ||
            u == "DOCUMENT.SETPARAGRAPHSTYLE" || u == "WRITER.SETPARAGRAPHSTYLE" || u == "SETPARAGRAPHSTYLE") {
            requireArgs(2);
            ODT_SetParagraphStyle(static_cast<int>(args[0].toInt()), args[1].toString(), &globals);
            return Variant();
        }
        if (u == "ODT.GETTEXT" || u == "DOC.GETTEXT" || u == "DOCX.GETTEXT" ||
            u == "DOCUMENT.GETTEXT" || u == "WRITER.GETTEXT") {
            return Variant::fromString(ODT_GetText(&globals));
        }
        if (u == "ODT.SETTEXT" || u == "DOC.SETTEXT" || u == "DOCX.SETTEXT" ||
            u == "DOCUMENT.SETTEXT" || u == "WRITER.SETTEXT") {
            requireArgs(1);
            ODT_SetText(args[0].toString(), &globals);
            return Variant();
        }
        if (u == "ODT.SEARCHTEXT" || u == "DOC.SEARCHTEXT" || u == "DOCX.SEARCHTEXT" ||
            u == "DOCUMENT.SEARCHTEXT" || u == "WRITER.SEARCHTEXT" || u == "SEARCHTEXT") {
            requireArgs(1);
            int startIdx = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 0;
            return Variant::fromInt(ODT_SearchFrom(args[0].toString(), startIdx, &globals));
        }
        if (u == "ODT.SEARCHNEXT" || u == "DOC.SEARCHNEXT" || u == "DOCX.SEARCHNEXT" ||
            u == "DOCUMENT.SEARCHNEXT" || u == "WRITER.SEARCHNEXT") {
            requireArgs(1);
            int nextIdx = (gODT_SearchIndex < 0) ? 0 : (gODT_SearchIndex + 1);
            return Variant::fromInt(ODT_SearchFrom(args[0].toString(), nextIdx, &globals));
        }
        if (u == "ODT.REPLACEALL" || u == "DOC.REPLACEALL" || u == "DOCX.REPLACEALL" ||
            u == "DOCUMENT.REPLACEALL" || u == "WRITER.REPLACEALL" || u == "REPLACEALL") {
            requireArgs(2);
            return Variant::fromInt(ODT_ReplaceAll(args[0].toString(), args[1].toString(), &globals));
        }
        if (u == "ODT.PRINTDOCUMENT" || u == "DOC.PRINTDOCUMENT" || u == "DOCX.PRINTDOCUMENT" ||
            u == "DOCUMENT.PRINT" || u == "DOCUMENT.PRINTDOCUMENT" ||
            u == "WRITER.PRINTDOCUMENT" || u == "PRINTDOCUMENT") {
            ODT_PrintDocument(&globals);
            return Variant();
        }

        // --------------------------------------------------------------------
        // Spreadsheet (.ods / .xlsx / .csv) dispatch
        // --------------------------------------------------------------------
        if (u == "ODS_ENSUREINIT") { ODS_EnsureInit(&globals); return Variant(); }
        if (u == "COLINDEXTONAME") { requireArgs(1); return Variant::fromString(ColIndexToName(static_cast<int>(args[0].toInt()))); }
        if (u == "COLNAMETOINDEX") { requireArgs(1); return Variant::fromInt(ColNameToIndex(args[0].toString())); }
        if (u == "ODS_UPDATEADDRESSGLOBALS") { ODS_UpdateAddressGlobals(&globals); return Variant(); }
        if (u == "ODS_CLEARSHEETSLOT") { requireArgs(1); ODS_ClearSheetSlot(static_cast<int>(args[0].toInt())); return Variant(); }
        if (u == "ODS_ISNUMERICTEXT") { requireArgs(1); return Variant::fromInt(ODS_IsNumericText(args[0].toString())); }
        if (u == "ODS_STORECELL") {
            requireArgs(4);
            ODS_StoreCell(static_cast<int>(args[0].toInt()),
                          static_cast<int>(args[1].toInt()),
                          static_cast<int>(args[2].toInt()),
                          args[3]);
            return Variant();
        }
        if (u == "ODS_PARSEA1REF") {
            requireArgs(3);
            int outCol = 0, outRow = 0;
            ODS_ParseA1Ref(args[0].toString(), outCol, outRow);
            writeBackRef(1, Variant::fromInt(outCol));
            writeBackRef(2, Variant::fromInt(outRow));
            return Variant();
        }
        if (u == "ODS.NEW" || u == "WORKBOOK.NEW" || u == "ODS.CLOSE" || u == "WORKBOOK.CLOSE") {
            ODS_New(&globals);
            return Variant();
        }
        if (u == "ODS_ESCAPEXML") { requireArgs(1); return Variant::fromString(ODS_EscapeXML(args[0].toString())); }
        if (u == "ODS_UNESCAPEXML") { requireArgs(1); return Variant::fromString(ODS_UnescapeXML(args[0].toString())); }
        if (u == "ODS_EXTRACTATTR") { requireArgs(2); return Variant::fromString(ODS_ExtractAttr(args[0].toString(), args[1].toString())); }
        if (u == "ODS_EXTRACTTAGTEXT") {
            requireArgs(3);
            return Variant::fromString(ODS_ExtractTagText(args[0].toString(), args[1].toString(), args[2].toString()));
        }
        if (u == "ODS.SAVEAS" || u == "ODS.SAVE" || u == "WORKBOOK.SAVEAS") {
            requireArgs(1);
            ODS_SaveAs(args[0].toString(), &globals);
            return Variant();
        }
        if (u == "WORKBOOK.SAVE") {
            if (gODS_FilePath.empty()) raiseError(52, "Invalid file name or file number");
            ODS_SaveAs(gODS_FilePath, &globals);
            return Variant();
        }
        if (u == "ODS.OPEN" || u == "WORKBOOK.OPEN") {
            requireArgs(1);
            ODS_Open(args[0].toString(), &globals);
            return Variant();
        }
        if (u == "SHEETS.GETCOUNT") {
            ODS_EnsureInit(&globals);
            return Variant::fromInt(gODS_SheetCount);
        }
        if (u == "SHEETS.HASBYNAME") {
            requireArgs(1);
            return Variant::fromInt(Sheets_hasByName(args[0].toString(), &globals));
        }
        if (u == "SHEETS.GETBYNAME") {
            requireArgs(1);
            return Variant::fromInt(Sheets_getByName(args[0].toString(), &globals));
        }
        if (u == "SHEETS.GETBYINDEX") {
            requireArgs(1);
            return Variant::fromString(Sheets_getByIndex(static_cast<int>(args[0].toInt()), &globals));
        }
        if (u == "SHEETS.GETNAME" || u == "SHEET.GETNAME") {
            ODS_EnsureInit(&globals);
            return Variant::fromString(gODS_Sheets[gODS_ActiveSheet].name);
        }
        if (u == "SHEETS.INSERTNEWBYNAME") {
            requireArgs(2);
            Sheets_insertNewByName(args[0].toString(), static_cast<int>(args[1].toInt()), &globals);
            return Variant();
        }
        if (u == "SHEETS.INSERTBYNAME") {
            requireArgs(1);
            ODS_EnsureInit(&globals);
            Sheets_insertNewByName(args[0].toString(), gODS_SheetCount, &globals);
            return Variant();
        }
        if (u == "SHEETS.REMOVEBYNAME") {
            requireArgs(1);
            Sheets_removeByName(args[0].toString(), &globals);
            return Variant();
        }
        if (u == "SHEETS.COPYBYNAME") {
            requireArgs(2);
            Sheets_copyByName(args[0].toString(), args[1].toString(), &globals);
            return Variant();
        }
        if (u == "SHEET.GETCELLRANGEBYPOSITION" || u == "GETCELLRANGEBYPOSITION") {
            requireArgs(4);
            return Variant::fromString(Sheet_getCellRangeByPosition(
                static_cast<int>(args[0].toInt()),
                static_cast<int>(args[1].toInt()),
                static_cast<int>(args[2].toInt()),
                static_cast<int>(args[3].toInt()),
                &globals));
        }
        if (u == "SHEET.GETCELLBYPOSITION" || u == "GETCELLBYPOSITION") {
            requireArgs(2);
            int c = static_cast<int>(args[0].toInt());
            int r = static_cast<int>(args[1].toInt());
            return Variant::fromString(Sheet_getCellRangeByPosition(c, r, c, r, &globals));
        }
        if (u == "SHEET.GETCELLRANGEBYNAME" || u == "GETCELLRANGEBYNAME" ||
            u == "SHEET.GETCELLBYNAME" || u == "GETCELLBYNAME") {
            requireArgs(1);
            return Variant::fromString(Sheet_getCellRangeByName(args[0].toString(), &globals));
        }
        if (u == "SHEET.CLEARCONTENTS") {
            Sheet_clearContents(&globals);
            return Variant();
        }
        if (u == "GETVALUE" || u == "CELL.GETVALUE") {
            return getValue(&globals);
        }
        if (u == "SETVALUE" || u == "CELL.SETVALUE") {
            requireArgs(1);
            setValue(args[0], &globals);
            return Variant();
        }
        if (u == "GETSTRING" || u == "CELL.GETSTRING") {
            return Variant::fromString(getString(&globals));
        }
        if (u == "SETSTRING" || u == "CELL.SETSTRING") {
            requireArgs(1);
            setString(args[0].toString(), &globals);
            return Variant();
        }
        if (u == "GETCELLVALUE") {
            requireArgs(2);
            int c = static_cast<int>(args[0].toInt());
            int r = static_cast<int>(args[1].toInt());
            Sheet_getCellRangeByPosition(c, r, c, r, &globals);
            return getValue(&globals);
        }
        if (u == "SETCELLVALUE") {
            requireArgs(3);
            int c = static_cast<int>(args[0].toInt());
            int r = static_cast<int>(args[1].toInt());
            Sheet_getCellRangeByPosition(c, r, c, r, &globals);
            setValue(args[2], &globals);
            return Variant();
        }
        if (u == "SETDATAARRAY" || u == "RANGE.SETDATAARRAY" || u == "SETDATA") {
            requireArgs(1);
            setDataArray(args[0], &globals);
            return Variant();
        }
        if (u == "GETDATA" || u == "GETDATAARRAY" || u == "RANGE.GETDATA" || u == "RANGE.GETDATAARRAY") {
            return getData(&globals);
        }
        if (u == "ODS_SEARCHFROM") {
            requireArgs(3);
            return Variant::fromInt(ODS_SearchFrom(
                args[0].toString(),
                static_cast<int>(args[1].toInt()),
                static_cast<int>(args[2].toInt()),
                &globals));
        }
        if (u == "SEARCHSTRING" || u == "SHEET.SEARCHSTRING") {
            requireArgs(1);
            return Variant::fromInt(ODS_SearchFrom(args[0].toString(), 0, 0, &globals));
        }
        if (u == "SEARCHNEXT") {
            requireArgs(1);
            return Variant::fromInt(SearchNext(args[0].toString(), &globals));
        }
        if (u == "GETCELLADDRESS" || u == "CELL.GETCELLADDRESS") {
            return getCellAddress(&globals);
        }
        if (u == "GETCELLADDRESSSHEET") {
            ODS_UpdateAddressGlobals(&globals);
            return Variant::fromInt(CellAddress_Sheet);
        }
        if (u == "GETCELLADDRESSCOL") {
            ODS_UpdateAddressGlobals(&globals);
            return Variant::fromInt(CellAddress_Column);
        }
        if (u == "GETCELLADDRESSROW") {
            ODS_UpdateAddressGlobals(&globals);
            return Variant::fromInt(CellAddress_Row);
        }
        if (u == "GETCELLADDRESSSTRING") {
            ODS_UpdateAddressGlobals(&globals);
            return Variant::fromString(CellAddress_AbsoluteName);
        }
        if (u == "SHEET.PRINTTABLE") {
            Sheet_PrintTable(&globals);
            return Variant();
        }

        raiseError(35, "Sub-procedure or function procedure not defined: " + u);
        return Variant();
    }
}