//*     This program is free software; you can redistribute it and/or     */
//*     modify it under the terms of the GNU General Public License as    */
//*     published by the Free Software Foundation; version 2 of           */
//*     the License.                                                      */
//*                                                                       */
//*     This program is distributed in the hope that it will be useful,   */
//*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
//*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
//*     GNU General Public License for more details.                      */
//*                                                                       */
//*     You should have received a copy of the GNU General Public License */
//*     along with this program; if not, write to the Free Software       */
//*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */
//* --------------------------------------------------------------------  */
// sbasic.cpp - Standalone sBasic (StarBasic) Interpreter
// Compile: make (g++ -std=c++17 -O2 -Wall -Wextra sbasic.o linalg.o -o sbasic -static -static-libgcc -static-libstdc++)
// Run CLI: ./sbasic test   OR   ./sbasic test.bas   (use -v / --verbose to enable verbose trace)

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
#include <cstdlib>
#include <ctime>
#include <cstdint>
#include <chrono>
#include <thread>
#include <filesystem>
#include <functional>
#include <sys/stat.h>
#if defined(_WIN32)
#include <io.h>
#define SBASIC_ISATTY _isatty(_fileno(stdin))
#else
#include <unistd.h>
#define SBASIC_ISATTY isatty(STDIN_FILENO)
#endif

static bool gVerboseMode = false;

// ============================================================================
// STRING, FILE RESOLUTION & DIAGNOSTIC UTILITIES
// ============================================================================
static std::string toUpper(std::string s) {
    for (char &c : s) c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
    return s;
}

static std::string trim(const std::string &s) {
    size_t a = s.find_first_not_of(" \t\r\n");
    if (a == std::string::npos) return "";
    size_t b = s.find_last_not_of(" \t\r\n");
    return s.substr(a, b - a + 1);
}

static bool fileExists(const std::string &path) {
    std::ifstream f(path);
    return f.good();
}

static std::string resolveScriptPath(const std::string &rawPath) {
    std::string p = trim(rawPath);
    if (p.empty()) return p;
    if (fileExists(p)) return p;

    size_t commaPos = p.rfind(',');
    if (commaPos != std::string::npos) {
        std::string fixedDot = p.substr(0, commaPos) + "." + p.substr(commaPos + 1);
        if (fileExists(fixedDot)) return fixedDot;
    }

    if (fileExists(p + ".bas")) return p + ".bas";
    if (fileExists(p + ".BAS")) return p + ".BAS";
    return p;
}

static int editDistance(const std::string &a, const std::string &b) {
    std::string ua = toUpper(a), ub = toUpper(b);
    size_t m = ua.size(), n = ub.size();
    std::vector<std::vector<int>> dp(m + 1, std::vector<int>(n + 1, 0));
    for (size_t i = 0; i <= m; ++i) dp[i][0] = static_cast<int>(i);
    for (size_t j = 0; j <= n; ++j) dp[0][j] = static_cast<int>(j);
    for (size_t i = 1; i <= m; ++i) {
        for (size_t j = 1; j <= n; ++j) {
            int cost = (ua[i - 1] == ub[j - 1]) ? 0 : 1;
            dp[i][j] = std::min({dp[i - 1][j] + 1, dp[i][j - 1] + 1, dp[i - 1][j - 1] + cost});
            if (i > 1 && j > 1 && ua[i - 1] == ub[j - 2] && ua[i - 2] == ub[j - 1]) {
                dp[i][j] = std::min(dp[i][j], dp[i - 2][j - 2] + 1);
            }
        }
    }
    return dp[m][n];
}

static std::string suggestBestMatch(const std::string &badToken,
                                    const std::vector<std::string> &extraCandidates = {}) {
    static const std::vector<std::string> builtInVocabulary = {
        "Print", "Write", "Line Input", "Open", "Close", "Dim", "ReDim", "Preserve",
        "Global", "Public", "Private", "Option Base", "Sub", "End Sub", "Function", "End Function",
        "If", "Then", "Else", "ElseIf", "End If", "Select Case", "Case", "Case Else", "End Select",
        "For", "To", "Step", "Next", "Do", "Loop", "While", "Wend", "Until",
        "Exit For", "Exit Do", "Exit Sub", "Exit Function", "GoTo", "GoSub", "Return",
        "On Error GoTo", "On Error Resume Next", "On Error GoTo 0", "Resume", "Resume Next",
        "Error", "Err.Clear", "Err.Raise", "Wait", "Run", "Call", "LSet", "RSet", "End", "Rem",
        "Format", "Format$", "FormatNumber", "FormatPercent", "FormatCurrency", "FormatScientific",
        "Round", "Space$", "String$", "Tab", "Spc", "LPad", "RPad", "Hex$", "Oct$",
        "InStr", "Mid", "Mid$", "Left", "Left$", "Right", "Right$", "Len",
        "Trim", "Trim$", "LTrim", "RTrim", "Split", "Join", "UCase", "LCase", "Chr", "Asc",
        "CInt", "CLng", "CDbl", "CSng", "CStr", "Val", "Str", "CVErr", "IsError",
        "Err", "Erl", "Error$", "FreeFile", "EOF", "MsgBox", "InputBox",
        "Sin", "Cos", "Tan", "Atn", "Sqr", "Exp", "Log", "Abs", "Sgn", "Int", "Fix", "Rnd",
        "LBound", "UBound", "Array", "Erase", "Option Explicit",
        "Replace", "Replace$", "InStrRev", "StrReverse", "StrReverse$", "Filter", "StrComp", "String",
        "Date", "Date$", "Time", "Time$", "Now", "Timer",
        "Year", "Month", "Day", "Hour", "Minute", "Second", "Weekday",
        "DateSerial", "TimeSerial", "DateValue", "TimeValue",
        "DateAdd", "DateDiff", "DatePart", "FormatDateTime",
        "Dir", "Dir$", "Kill", "Name", "FileCopy", "MkDir", "RmDir", "ChDir", "ChDrive",
        "CurDir", "CurDir$", "FileDateTime", "FileLen", "GetAttr", "SetAttr",
        "Seek", "Loc", "LOF", "Input", "Input$", "Put", "Get", "Lock", "Unlock",
        "MatMul", "MatInv", "MatTranspose", "MatEig", "MatEigVec",
        "LinearSolver", "NonlinearSolver", "Solver",
        "DOC.New", "DOC.Open", "DOC.SaveAs", "DOC.Save", "DOC.AppendParagraph", "DOC.AppendHeading",
        "DOC.GetParagraphCount", "DOC.GetParagraph", "DOC.SetParagraph", "DOC.ReplaceAll", "DOC.PrintDocument",
        "calc.SINH", "calc.COSH", "calc.TANH", "calc.COTH", "calc.SECH", "calc.CSCH",
        "calc.ASINH", "calc.ACOSH", "calc.ATANH", "calc.ACOTH", "calc.ASECH", "calc.ACSCH",
        "calc.ASIN", "calc.ACOS", "calc.ATAN", "calc.ATAN2", "calc.COT", "calc.ACOT", "calc.RADIANS", "calc.DEGREES",
        "calc.LN", "calc.LOG", "calc.LOG10", "calc.POWER", "calc.SQRT", "calc.SQRTPI",
        "calc.FACT", "calc.FACTDOUBLE", "calc.COMBIN", "calc.COMBINA", "calc.PERMUT", "calc.GCD", "calc.LCM",
        "calc.ROUND", "calc.ROUNDUP", "calc.ROUNDDOWN", "calc.MROUND", "calc.CEILING", "calc.FLOOR", "calc.TRUNC",
        "calc.EVEN", "calc.ODD", "calc.SIGN", "calc.MOD", "calc.QUOTIENT", "calc.RAND", "calc.RANDBETWEEN",
        "calc.SUM", "calc.PRODUCT", "calc.SUMSQ", "calc.SUMPRODUCT", "calc.SUMX2MY2", "calc.SUMX2PY2", "calc.SUMXMY2",
        "calc.SUMIF", "calc.SUMIFS", "calc.AVERAGE", "calc.AVERAGEIF", "calc.AVERAGEIFS",
        "calc.COUNT", "calc.COUNTA", "calc.COUNTBLANK", "calc.COUNTIF", "calc.COUNTIFS",
        "calc.MAX", "calc.MIN", "calc.MAXIFS", "calc.MINIFS", "calc.MEDIAN", "calc.MODE",
        "calc.LARGE", "calc.SMALL", "calc.PERCENTILE", "calc.QUARTILE", "calc.RANK",
        "calc.VAR", "calc.VARP", "calc.STDEV", "calc.STDEVP", "calc.AVEDEV", "calc.DEVSQ", "calc.GEOMEAN", "calc.HARMEAN",
        "calc.CORREL", "calc.COVAR", "calc.SLOPE", "calc.INTERCEPT", "calc.RSQ", "calc.FORECAST",
        "calc.VLOOKUP", "calc.HLOOKUP", "calc.LOOKUP", "calc.XLOOKUP", "calc.MATCH", "calc.INDEX", "calc.CHOOSE",
        "calc.ROWS", "calc.COLUMNS",
        "calc.DGET", "calc.DSUM", "calc.DAVERAGE", "calc.DCOUNT", "calc.DCOUNTA",
        "calc.DMAX", "calc.DMIN", "calc.DPRODUCT", "calc.DSTDEV", "calc.DSTDEVP", "calc.DVAR", "calc.DVARP",
        "calc.CONCAT", "calc.CONCATENATE", "calc.TEXTJOIN", "calc.EXACT", "calc.FIND", "calc.SEARCH",
        "calc.SUBSTITUTE", "calc.REPLACE", "calc.PROPER", "calc.CLEAN", "calc.REPT", "calc.VALUE", "calc.TEXT",
        "calc.IF", "calc.IFS", "calc.SWITCH", "calc.AND", "calc.OR", "calc.XOR", "calc.NOT",
        "calc.IFERROR", "calc.IFNA", "calc.ISERROR", "calc.ISNA", "calc.ISNUMBER", "calc.ISTEXT", "calc.ISBLANK"
    };

    std::string clean = badToken;
    if (!clean.empty() && (clean.back() == '$' || clean.back() == '%')) clean.pop_back();
    if (clean.empty()) return "";

    std::string best;
    int bestDist = 999;

    auto consider = [&](const std::string &cand) {
        int d = editDistance(clean, cand);
        if (d < bestDist) {
            bestDist = d;
            best = cand;
        }
    };

    for (const auto &w : builtInVocabulary) consider(w);
    for (const auto &w : extraCandidates) consider(w);

    int threshold = (clean.size() <= 3) ? 1 : (clean.size() <= 6 ? 2 : 3);
    return (bestDist <= threshold) ? best : "";
}

static std::string readSourceFile(const std::string &rawFilepath) {
    std::string filepath = resolveScriptPath(rawFilepath);
    std::ifstream ifs(filepath);
    if (!ifs.is_open()) {
        throw std::runtime_error("Cannot open Basic source file: '" + rawFilepath +
                                 "' (also tried '" + rawFilepath + ".bas')");
    }
    if (gVerboseMode) {
        std::cout << "[VERBOSE] Loaded Basic file: '" << filepath << "'\n";
    }

    std::string dirPrefix;
    size_t slashPos = filepath.find_last_of("/\\");
    if (slashPos != std::string::npos) {
        dirPrefix = filepath.substr(0, slashPos + 1);
    }

    std::ostringstream out;
    std::string rawLine;
    std::string accumulated;
    bool firstLine = true;

    while (std::getline(ifs, rawLine)) {
        if (firstLine) {
            firstLine = false;
            if (rawLine.size() >= 3 &&
                static_cast<unsigned char>(rawLine[0]) == 0xEF &&
                static_cast<unsigned char>(rawLine[1]) == 0xBB &&
                static_cast<unsigned char>(rawLine[2]) == 0xBF) {
                rawLine.erase(0, 3);
            }
        }

        std::string t = trim(rawLine);
        if (!t.empty() && t.back() == '_') {
            accumulated += t.substr(0, t.size() - 1) + " ";
            continue;
        }
        std::string fullLine = accumulated + t;
        accumulated.clear();

        std::string u = toUpper(fullLine);
        if (u.rfind("#INCLUDE", 0) == 0) {
            size_t q1 = fullLine.find('"');
            size_t q2 = fullLine.find_last_of('"');
            if (q1 != std::string::npos && q2 > q1) {
                std::string incFile = trim(fullLine.substr(q1 + 1, q2 - q1 - 1));
                std::string baseInc = incFile;
                size_t sp = baseInc.find_last_of("/\\");
                if (sp != std::string::npos) baseInc = baseInc.substr(sp + 1);
                if (toUpper(baseInc) == "ODS.BAS") {
                    if (gVerboseMode) {
                        std::cout << "[VERBOSE] Skipping '#Include \"ods.bas\"' (using native C++ liboffice module)\n";
                    }
                    continue;
                }
                out << readSourceFile(dirPrefix + incFile) << "\n";
                continue;
            }
        }

        out << fullLine << "\n";
    }
    return out.str();
}

// ============================================================================
// VARIANT & MULTI-DIMENSIONAL ARRAY TYPE SYSTEM (ABI-compatible with linalg.cpp)
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
        if (rhs.type == VarType::ARRAY || rhs.type == VarType::ERRVAL) {
            VarType prevDecl = declaredType;
            *this = rhs;
            if (rhs.type == VarType::ARRAY && declaredType == VarType::EMPTY && prevDecl != VarType::EMPTY) {
                declaredType = prevDecl;
            }
            return;
        }
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
        if (dims.empty()) throw std::runtime_error("Array index out of bounds: dynamic array is not allocated.");
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

// ============================================================================
// LINEAR ALGEBRA ENGINE DECLARATIONS (Implemented in linalg.cpp)
// ============================================================================
namespace LinAlg {
    Variant matMul(const Variant &A, const Variant &B);
    Variant matInv(const Variant &A);
    Variant matTranspose(const Variant &A);
    void jacobiEigen(const Variant &A, Variant &eigValsOut, Variant &eigVecsOut);

    struct SolverCallbacks {
        std::function<double(const std::string &expr, const std::vector<std::string> &changingVars)> evalExpr;
        std::function<Variant(const std::string &name)> getVar;
        std::function<void(const std::string &name, double val)> setVar;
        std::function<bool(const std::string &name)> varExists;
        std::function<bool(const std::string &name)> funcExists;
        std::function<std::string(const std::string &name)> getSymbolFormula;
        std::function<void(const std::string &status, double objVal, int iters, double elapsedSec)> setSolverStats;
    };

    Variant linearSolver(const std::string &specString, const SolverCallbacks &cb);
    Variant nonlinearSolver(const std::string &specString, const SolverCallbacks &cb);
}

namespace LibOffice {
    bool isOfficeRoutine(const std::string &u);
    void syncGlobals(std::unordered_map<std::string, Variant> *globals);
    Variant dispatch(const std::string &u,
                     const std::vector<Variant> &args,
                     const std::vector<std::string> &refNames,
                     std::unordered_map<std::string, Variant> &globals,
                     std::unordered_map<std::string, Variant> *localFrame);
}

// ============================================================================
// SBASIC ERROR HANDLING & DIAGNOSTICS
// ============================================================================
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

static std::string getStandardErrorMessage(int code) {
    switch (code) {
        case 0:  return "";
        case 1:  return "An exception occurred";
        case 2:  return "Syntax error";
        case 3:  return "Return without Gosub";
        case 5:  return "Invalid procedure call or argument";
        case 6:  return "Overflow";
        case 7:  return "Not enough memory";
        case 9:  return "Subscript out of range";
        case 10: return "Duplicate definition";
        case 11: return "Division by zero";
        case 12: return "Variable not defined";
        case 13: return "Type mismatch";
        case 14: return "Invalid parameter";
        case 18: return "User interrupt occurred";
        case 20: return "Resume without error";
        case 28: return "Not enough stack memory";
        case 35: return "Sub-procedure or function procedure not defined";
        case 51: return "Internal error";
        case 52: return "Invalid file name or file number";
        case 53: return "File not found";
        case 54: return "Incorrect file mode";
        case 55: return "File already open";
        case 57: return "Device I/O error";
        case 58: return "File already exists";
        case 61: return "Disk or hard drive full";
        case 62: return "Reading exceeds EOF";
        case 67: return "Too many files";
        case 68: return "Device not available";
        case 70: return "Access denied";
        case 71: return "Disk not ready";
        case 75: return "Path/File access error";
        case 76: return "Path not found";
        case 91: return "Object variable not set";
        case 94: return "Invalid use of Null";
        default: return "Application-defined or object-defined error (" + std::to_string(code) + ")";
    }
}

static int classifyExceptionToErrorCode(const std::string &msg) {
    std::string u = toUpper(msg);
    if (u.find("DIVISION BY ZERO") != std::string::npos || u.find("SINGULAR") != std::string::npos) return 11;
    if (u.find("OUT OF BOUNDS") != std::string::npos || u.find("DIMENSION") != std::string::npos ||
        u.find("NOT AN ARRAY") != std::string::npos || u.find("REQUIRES A") != std::string::npos ||
        u.find("REQUIRES AN ARRAY") != std::string::npos) return 9;
    if (u.find("CANNOT OPEN") != std::string::npos || u.find("NOT FOUND") != std::string::npos) return 53;
    if (u.find("FILE CHANNEL") != std::string::npos) return 52;
    if (u.find("UNKNOWN") != std::string::npos || u.find("NOT DEFINED") != std::string::npos) return 35;
    if (u.find("ARGUMENT") != std::string::npos || u.find("DOMAIN") != std::string::npos) return 5;
    if (u.find("TYPE MISMATCH") != std::string::npos) return 13;
    return 1;
}

static void printDiagnosticReport(const BasicRuntimeError &bre) {
    std::cerr << "\n====================================================================\n";
    std::cerr << "[StarBasic Diagnostic] Error #" << bre.code
              << " (" << getStandardErrorMessage(bre.code) << ")";
    if (bre.line > 0) std::cerr << " at line " << bre.line;
    std::cerr << "\nReason : " << bre.what() << "\n";

    if (!bre.sourceCodeLine.empty()) {
        std::cerr << "Source : " << bre.sourceCodeLine << "\n";
        if (!bre.badToken.empty()) {
            size_t pos = toUpper(bre.sourceCodeLine).find(toUpper(bre.badToken));
            if (pos != std::string::npos) {
                std::cerr << "         " << std::string(pos, ' ') << "^"
                          << std::string(std::max<size_t>(1, bre.badToken.size()) - 1, '~') << "\n";
            }
        }
    }
    if (!bre.suggestion.empty()) {
        std::cerr << "Hint   : Did you mean '" << bre.suggestion << "'?\n";
    }
    std::cerr << "====================================================================\n";
}

// ============================================================================
// NUMBER FORMATTING UTILITIES FOR TEXT FILE & CONSOLE OUTPUT
// ============================================================================
namespace NumberFormatter {

    static std::string insertThousandsSeparators(const std::string &intDigits) {
        std::string out;
        int count = 0;
        for (int i = static_cast<int>(intDigits.size()) - 1; i >= 0; --i) {
            if (count > 0 && count % 3 == 0 && std::isdigit(static_cast<unsigned char>(intDigits[i]))) {
                out.push_back(',');
            }
            out.push_back(intDigits[i]);
            ++count;
        }
        std::reverse(out.begin(), out.end());
        return out;
    }

    static std::string formatNumberWithOptions(double val, int decimals, bool leadingDigit,
                                               bool parensForNeg, bool groupDigits,
                                               const std::string &currencyPrefix = "",
                                               const std::string &percentSuffix = "") {
        bool isNeg = (val < 0.0 && std::fabs(val) > 1e-15);
        double absVal = std::fabs(val);
        if (decimals < 0) decimals = 2;

        std::ostringstream oss;
        oss << std::fixed << std::setprecision(decimals) << absVal;
        std::string s = oss.str();

        std::string intPart = s;
        std::string fracPart;
        size_t dotPos = s.find('.');
        if (dotPos != std::string::npos) {
            intPart = s.substr(0, dotPos);
            fracPart = s.substr(dotPos + 1);
        }

        if (groupDigits) intPart = insertThousandsSeparators(intPart);
        if (!leadingDigit && intPart == "0" && decimals > 0) intPart.clear();

        std::string core = currencyPrefix + intPart + (decimals > 0 ? ("." + fracPart) : "") + percentSuffix;
        if (isNeg) return parensForNeg ? ("(" + core + ")") : ("-" + core);
        return core;
    }

    static std::string formatScientificValue(double val, int minIntDigits, int minFracDigits,
                                             int maxFracDigits, int expDigits, char expChar,
                                             bool alwaysSignExp, bool showMinus,
                                             const std::string &prefix, const std::string &suffix) {
        bool isNeg = (val < 0.0);
        double absVal = std::fabs(val);
        int expVal = 0;
        if (absVal > 0.0) {
            expVal = static_cast<int>(std::floor(std::log10(absVal)));
            int shift = std::max(1, minIntDigits) - 1;
            expVal -= shift;
        }
        double mantissa = (absVal > 0.0) ? (absVal / std::pow(10.0, expVal)) : 0.0;

        double roundFactor = std::pow(10.0, maxFracDigits);
        double roundedMantissa = std::round(mantissa * roundFactor) / roundFactor;
        double upperLim = std::pow(10.0, std::max(1, minIntDigits));
        if (roundedMantissa >= upperLim && absVal > 0.0) {
            roundedMantissa /= 10.0;
            expVal += 1;
        }

        std::ostringstream moss;
        moss << std::fixed << std::setprecision(maxFracDigits) << roundedMantissa;
        std::string mStr = moss.str();
        std::string intPart = mStr, fracPart;
        size_t dotPos = mStr.find('.');
        if (dotPos != std::string::npos) {
            intPart = mStr.substr(0, dotPos);
            fracPart = mStr.substr(dotPos + 1);
        }
        while (static_cast<int>(intPart.size()) < minIntDigits) intPart = "0" + intPart;
        while (static_cast<int>(fracPart.size()) > minFracDigits && !fracPart.empty() && fracPart.back() == '0') {
            fracPart.pop_back();
        }

        std::ostringstream eoss;
        eoss << expChar;
        if (expVal < 0) eoss << '-';
        else if (alwaysSignExp) eoss << '+';
        eoss << std::setw(std::max(1, expDigits)) << std::setfill('0') << std::abs(expVal);

        std::string signStr = (isNeg && showMinus) ? "-" : "";
        return prefix + signStr + intPart +
               (fracPart.empty() ? "" : "." + fracPart) + eoss.str() + suffix;
    }

    static std::string applySingleMask(double val, const std::string &mask, bool showMinus) {
        std::string prefix, suffix;
        bool hasPercent = false, hasDecimal = false, useThousands = false, isSci = false;
        char sciChar = 'E';
        bool sciAlwaysSign = true;

        int minInt = 0, sharpInt = 0, spaceInt = 0;
        int minFrac = 0, maxFrac = 0, expDigits = 0, trailingCommas = 0;
        std::vector<char> fracPlaceholders;

        enum class Phase { BEFORE_NUM, INT_PART, FRAC_PART, EXP_PART };
        Phase phase = Phase::BEFORE_NUM;

        for (size_t i = 0; i < mask.size(); ++i) {
            char c = mask[i];
            if (c == '"') {
                ++i;
                std::string lit;
                while (i < mask.size() && mask[i] != '"') lit += mask[i++];
                if (phase == Phase::BEFORE_NUM) prefix += lit;
                else suffix += lit;
                continue;
            }
            if (c == '\\' && i + 1 < mask.size()) {
                char lit = mask[++i];
                if (phase == Phase::BEFORE_NUM) prefix += lit;
                else suffix += lit;
                continue;
            }
            if ((c == 'E' || c == 'e') && i + 1 < mask.size() && (mask[i + 1] == '+' || mask[i + 1] == '-')) {
                isSci = true;
                sciChar = c;
                sciAlwaysSign = (mask[i + 1] == '+');
                ++i;
                phase = Phase::EXP_PART;
                continue;
            }
            if (c == '0' || c == '#' || c == '?') {
                if (phase == Phase::BEFORE_NUM) phase = Phase::INT_PART;
                if (phase == Phase::INT_PART) {
                    trailingCommas = 0;
                    if (c == '0') ++minInt;
                    else if (c == '#') ++sharpInt;
                    else if (c == '?') ++spaceInt;
                } else if (phase == Phase::FRAC_PART) {
                    ++maxFrac;
                    if (c == '0') minFrac = maxFrac;
                    fracPlaceholders.push_back(c);
                } else if (phase == Phase::EXP_PART) {
                    ++expDigits;
                }
                continue;
            }
            if (c == '.' && phase != Phase::FRAC_PART && phase != Phase::EXP_PART) {
                hasDecimal = true;
                phase = Phase::FRAC_PART;
                continue;
            }
            if (c == ',' && (phase == Phase::INT_PART || phase == Phase::BEFORE_NUM)) {
                bool followedByDigit = false;
                for (size_t k = i + 1; k < mask.size(); ++k) {
                    if (mask[k] == '0' || mask[k] == '#' || mask[k] == '?') { followedByDigit = true; break; }
                    if (mask[k] == '.') break;
                }
                if (followedByDigit) useThousands = true;
                else ++trailingCommas;
                continue;
            }
            if (c == '%') {
                hasPercent = true;
                if (phase == Phase::BEFORE_NUM) prefix += '%';
                else suffix += '%';
                continue;
            }
            if (phase == Phase::BEFORE_NUM) prefix += c;
            else suffix += c;
        }

        (void)sharpInt;
        if (phase == Phase::BEFORE_NUM && !hasDecimal) return mask;

        double scaledVal = val;
        if (hasPercent) scaledVal *= 100.0;
        for (int tc = 0; tc < trailingCommas; ++tc) scaledVal /= 1000.0;

        if (isSci) {
            return formatScientificValue(scaledVal, minInt, minFrac, maxFrac,
                                         std::max(1, expDigits), sciChar, sciAlwaysSign,
                                         showMinus, prefix, suffix);
        }

        bool isNeg = (scaledVal < 0.0 && std::fabs(scaledVal) > 1e-15);
        double absVal = std::fabs(scaledVal);

        std::ostringstream oss;
        oss << std::fixed << std::setprecision(maxFrac) << absVal;
        std::string s = oss.str();

        std::string intPart = s, fracPart;
        size_t dotPos = s.find('.');
        if (dotPos != std::string::npos) {
            intPart = s.substr(0, dotPos);
            fracPart = s.substr(dotPos + 1);
        }

        if (intPart == "0" && minInt == 0) intPart.clear();
        while (static_cast<int>(intPart.size()) < minInt) intPart = "0" + intPart;
        if (useThousands && !intPart.empty()) intPart = insertThousandsSeparators(intPart);

        for (int idx = static_cast<int>(fracPart.size()) - 1; idx >= minFrac; --idx) {
            if (fracPart[idx] == '0') {
                char ph = (idx < static_cast<int>(fracPlaceholders.size())) ? fracPlaceholders[idx] : '#';
                if (ph == '?') fracPart[idx] = ' ';
                else fracPart.erase(static_cast<size_t>(idx), 1);
            } else {
                break;
            }
        }

        std::string signStr = (isNeg && showMinus) ? "-" : "";
        std::string numCore = signStr + intPart;

        int targetIntWidth = minInt + spaceInt;
        if (spaceInt > 0 && static_cast<int>(numCore.size()) < targetIntWidth) {
            numCore = std::string(static_cast<size_t>(targetIntWidth - static_cast<int>(numCore.size())), ' ') + numCore;
        }

        std::string decStr = (!fracPart.empty()) ? ("." + fracPart) : "";
        return prefix + numCore + decStr + suffix;
    }

    static std::string formatCustomOrNamed(const Variant &v, const std::string &rawMask) {
        if (rawMask.empty()) return v.toString();
        std::string uMask = toUpper(trim(rawMask));
        double d = v.toDouble();

        if (uMask == "GENERAL NUMBER") return Variant::fromDouble(d).toString();
        if (uMask == "FIXED") return formatNumberWithOptions(d, 2, true, false, false);
        if (uMask == "STANDARD") return formatNumberWithOptions(d, 2, true, false, true);
        if (uMask == "PERCENT") return formatNumberWithOptions(d * 100.0, 2, true, false, false, "", "%");
        if (uMask == "SCIENTIFIC") return applySingleMask(d, "0.00E+00", true);
        if (uMask == "CURRENCY") return formatNumberWithOptions(d, 2, true, true, true, "$", "");
        if (uMask == "TRUE/FALSE") return v.toBool() ? "True" : "False";
        if (uMask == "YES/NO") return v.toBool() ? "Yes" : "No";
        if (uMask == "ON/OFF") return v.toBool() ? "On" : "Off";

        std::vector<std::string> sections;
        std::string cur;
        bool inQuotes = false;
        for (size_t i = 0; i < rawMask.size(); ++i) {
            char c = rawMask[i];
            if (c == '\\' && i + 1 < rawMask.size()) {
                cur += c; cur += rawMask[++i]; continue;
            }
            if (c == '"') inQuotes = !inQuotes;
            if (!inQuotes && c == ';') {
                sections.push_back(cur);
                cur.clear();
            } else {
                cur += c;
            }
        }
        sections.push_back(cur);

        if (sections.size() == 1) return applySingleMask(d, sections[0], true);
        if (sections.size() == 2) {
            if (d < 0.0 && std::fabs(d) > 1e-15) return applySingleMask(std::fabs(d), sections[1], false);
            return applySingleMask(d, sections[0], false);
        }
        if (std::fabs(d) <= 1e-15) return applySingleMask(0.0, sections[2], false);
        if (d < 0.0) return applySingleMask(std::fabs(d), sections[1], false);
        return applySingleMask(d, sections[0], false);
    }
}

// ============================================================================
// DATE & TIME ENGINE AND FILE SYSTEM UTILITIES
// ============================================================================
namespace DateTimeUtil {

    static int64_t gSystemTimeOffsetSec = 0;

    static int64_t daysFromCivil(int y, int m, int d) {
        y -= (m <= 2) ? 1 : 0;
        const int64_t era = (y >= 0 ? y : y - 399) / 400;
        const unsigned yoe = static_cast<unsigned>(y - era * 400);
        const unsigned doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + static_cast<unsigned>(d) - 1;
        const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
        return era * 146097 + static_cast<int64_t>(doe) - 719468;
    }

    static void civilFromDays(int64_t z, int &y, int &m, int &d) {
        z += 719468;
        const int64_t era = (z >= 0 ? z : z - 146096) / 146097;
        const unsigned doe = static_cast<unsigned>(z - era * 146097);
        const unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        y = static_cast<int>(yoe) + static_cast<int>(era) * 400;
        const unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        const unsigned mp = (5 * doy + 2) / 153;
        d = static_cast<int>(doy - (153 * mp + 2) / 5 + 1);
        m = static_cast<int>(mp + (mp < 10 ? 3 : -9));
        y += (m <= 2) ? 1 : 0;
    }

    static bool isLeap(int y) {
        return (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);
    }

    static int daysInMonth(int y, int m) {
        static const int dim[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
        if (m < 1 || m > 12) return 30;
        if (m == 2 && isLeap(y)) return 29;
        return dim[m - 1];
    }

    static int dayOfYear(int y, int m, int d) {
        return static_cast<int>(daysFromCivil(y, m, d) - daysFromCivil(y, 1, 1) + 1);
    }

    // OLE Automation / StarBasic epoch: 1899-12-30 = 0.0
    static constexpr int64_t kBasicEpochDays = -25569; // daysFromCivil(1899, 12, 30)

    struct DateParts {
        int year = 1899;
        int month = 12;
        int day = 30;
        int hour = 0;
        int minute = 0;
        int second = 0;
        bool hasDate = true;
        bool hasTime = false;

        double toSerial() const {
            int64_t days = hasDate ? (daysFromCivil(year, month, day) - kBasicEpochDays) : 0;
            double frac = (hour * 3600.0 + minute * 60.0 + second) / 86400.0;
            if (days >= 0) return static_cast<double>(days) + frac;
            return static_cast<double>(days) - frac;
        }

        static DateParts fromSerial(double serial) {
            DateParts dp;
            int64_t days = static_cast<int64_t>(std::floor(serial));
            double frac = std::fabs(serial - static_cast<double>(days));
            int64_t totalSec = static_cast<int64_t>(std::llround(frac * 86400.0));
            if (totalSec >= 86400) {
                days += (serial >= 0.0) ? 1 : -1;
                totalSec -= 86400;
            }
            civilFromDays(days + kBasicEpochDays, dp.year, dp.month, dp.day);
            dp.hour = static_cast<int>((totalSec / 3600) % 24);
            dp.minute = static_cast<int>((totalSec / 60) % 60);
            dp.second = static_cast<int>(totalSec % 60);
            dp.hasDate = (days != 0 || totalSec == 0);
            dp.hasTime = (totalSec != 0 || !dp.hasDate);
            return dp;
        }

        std::string formatDate() const {
            std::ostringstream oss;
            oss << std::setfill('0') << std::setw(4) << year << "-"
                << std::setw(2) << month << "-" << std::setw(2) << day;
            return oss.str();
        }

        std::string formatTime() const {
            std::ostringstream oss;
            oss << std::setfill('0') << std::setw(2) << hour << ":"
                << std::setw(2) << minute << ":" << std::setw(2) << second;
            return oss.str();
        }

        std::string formatGeneral() const {
            if (hasDate && hasTime) return formatDate() + " " + formatTime();
            if (hasTime && !hasDate) return formatTime();
            return formatDate();
        }

        Variant toVariant() const {
            Variant v = Variant::fromString(formatGeneral());
            v.dVal = toSerial();
            v.iVal = static_cast<int64_t>(std::llround(v.dVal));
            return v;
        }
    };

    static std::tm getLocalNowTm() {
        auto nowSys = std::chrono::system_clock::now() + std::chrono::seconds(gSystemTimeOffsetSec);
        std::time_t tt = std::chrono::system_clock::to_time_t(nowSys);
        std::tm localTm{};
#if defined(_WIN32)
        localtime_s(&localTm, &tt);
#else
        localtime_r(&tt, &localTm);
#endif
        return localTm;
    }

    static DateParts currentNow() {
        std::tm lt = getLocalNowTm();
        DateParts dp;
        dp.year = lt.tm_year + 1900;
        dp.month = lt.tm_mon + 1;
        dp.day = lt.tm_mday;
        dp.hour = lt.tm_hour;
        dp.minute = lt.tm_min;
        dp.second = lt.tm_sec;
        dp.hasDate = true;
        dp.hasTime = true;
        return dp;
    }

    static DateParts currentDate() {
        DateParts dp = currentNow();
        dp.hour = 0; dp.minute = 0; dp.second = 0;
        dp.hasDate = true; dp.hasTime = false;
        return dp;
    }

    static DateParts currentTime() {
        DateParts dp = currentNow();
        dp.year = 1899; dp.month = 12; dp.day = 30;
        dp.hasDate = false; dp.hasTime = true;
        return dp;
    }

    static double currentTimer() {
        auto nowSys = std::chrono::system_clock::now() + std::chrono::seconds(gSystemTimeOffsetSec);
        std::time_t tt = std::chrono::system_clock::to_time_t(nowSys);
        std::tm lt{};
#if defined(_WIN32)
        localtime_s(&lt, &tt);
#else
        localtime_r(&tt, &lt);
#endif
        auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(nowSys.time_since_epoch()).count() % 1000;
        if (ms < 0) ms += 1000;
        return lt.tm_hour * 3600.0 + lt.tm_min * 60.0 + lt.tm_sec + (ms / 1000.0);
    }

    static int monthNameToNum(const std::string &name) {
        std::string u = toUpper(name);
        if (u.size() >= 3) u = u.substr(0, 3);
        if (u == "JAN") return 1;
        if (u == "FEB") return 2;
        if (u == "MAR") return 3;
        if (u == "APR") return 4;
        if (u == "MAY") return 5;
        if (u == "JUN") return 6;
        if (u == "JUL") return 7;
        if (u == "AUG") return 8;
        if (u == "SEP") return 9;
        if (u == "OCT") return 10;
        if (u == "NOV") return 11;
        if (u == "DEC") return 12;
        return 0;
    }

    static bool parseTimePortion(const std::string &s, int &h, int &m, int &sec) {
        std::string t = trim(s);
        if (t.empty()) return false;
        bool isPM = false, isAM = false;
        std::string u = toUpper(t);
        if (u.size() >= 2 && u.substr(u.size() - 2) == "PM") {
            isPM = true;
            t = trim(t.substr(0, t.size() - 2));
        } else if (u.size() >= 2 && u.substr(u.size() - 2) == "AM") {
            isAM = true;
            t = trim(t.substr(0, t.size() - 2));
        }
        int p1 = 0, p2 = 0, p3 = 0;
        char c1 = 0, c2 = 0;
        std::istringstream iss(t);
        if (!(iss >> p1 >> c1 >> p2) || c1 != ':') return false;
        if (iss >> c2) {
            if (c2 != ':' || !(iss >> p3)) return false;
        }
        if (isPM && p1 < 12) p1 += 12;
        if (isAM && p1 == 12) p1 = 0;
        if (p1 < 0 || p1 > 23 || p2 < 0 || p2 > 59 || p3 < 0 || p3 > 59) return false;
        h = p1; m = p2; sec = p3;
        return true;
    }

    static bool parseDateString(const std::string &raw, DateParts &out) {
        std::string s = trim(raw);
        if (s.empty()) return false;

        int h = 0, min = 0, sec = 0;
        if (s.find('-') == std::string::npos && s.find('/') == std::string::npos &&
            s.find('.') == std::string::npos && s.find(':') != std::string::npos) {
            if (parseTimePortion(s, h, min, sec)) {
                out = DateParts();
                out.hour = h; out.minute = min; out.second = sec;
                out.hasDate = false; out.hasTime = true;
                return true;
            }
        }

        std::string datePart = s;
        std::string timePart;
        size_t colonPos = s.find(':');
        if (colonPos != std::string::npos) {
            size_t spBeforeColon = s.rfind(' ', colonPos);
            size_t tPos = s.rfind('T', colonPos);
            size_t splitAt = (spBeforeColon != std::string::npos) ? spBeforeColon : tPos;
            if (splitAt != std::string::npos) {
                datePart = trim(s.substr(0, splitAt));
                timePart = trim(s.substr(splitAt + 1));
            }
        }

        bool gotTime = false;
        if (!timePart.empty()) {
            if (!parseTimePortion(timePart, h, min, sec)) return false;
            gotTime = true;
        }

        int y = 0, m = 0, d = 0;
        bool gotDate = false;

        bool hasAlpha = false;
        for (char c : datePart) {
            if (std::isalpha(static_cast<unsigned char>(c))) { hasAlpha = true; break; }
        }

        if (hasAlpha) {
            std::string cleaned = datePart;
            for (char &c : cleaned) {
                if (c == ',' || c == '-' || c == '/') c = ' ';
            }
            std::istringstream iss(cleaned);
            std::string t1, t2, t3;
            if (iss >> t1 >> t2 >> t3) {
                if ( std::isalpha(static_cast<unsigned char>(t1[0])) ) {
                    m = monthNameToNum(t1);
                    try { d = std::stoi(t2); y = std::stoi(t3); } catch (...) { return false; }
                } else {
                    m = monthNameToNum(t2);
                    try { d = std::stoi(t1); y = std::stoi(t3); } catch (...) { return false; }
                }
                gotDate = (m >= 1 && m <= 12);
            }
        } else {
            char sep = 0;
            if (datePart.find('-') != std::string::npos) sep = '-';
            else if (datePart.find('/') != std::string::npos) sep = '/';
            else if (datePart.find('.') != std::string::npos) sep = '.';

            if (sep != 0) {
                int a = 0, b = 0, c = 0;
                char s1 = 0, s2 = 0;
                std::istringstream iss(datePart);
                if (iss >> a >> s1 >> b >> s2 >> c && s1 == sep && s2 == sep) {
                    if (a > 31) {
                        y = a; m = b; d = c;
                    } else if (sep == '.') {
                        d = a; m = b; y = c;
                    } else if (a > 12) {
                        d = a; m = b; y = c;
                    } else {
                        m = a; d = b; y = c;
                    }
                    gotDate = true;
                }
            }
        }

        if (!gotDate) return false;
        if (y >= 0 && y < 100) y += (y < 30) ? 2000 : 1900;
        if (m < 1 || m > 12 || d < 1 || d > daysInMonth(y, m)) return false;

        out.year = y; out.month = m; out.day = d;
        out.hour = h; out.minute = min; out.second = sec;
        out.hasDate = true;
        out.hasTime = gotTime;
        return true;
    }

    static DateParts parseDateVariant(const Variant &v) {
        if (v.type == VarType::STRING) {
            DateParts dp;
            if (parseDateString(v.sVal, dp)) return dp;
            try {
                double num = std::stod(v.sVal);
                return DateParts::fromSerial(num);
            } catch (...) {
                throw BasicRuntimeError(13, "Type mismatch: invalid date/time string '" + v.sVal + "'.");
            }
        }
        if (v.type == VarType::INTEGER || v.type == VarType::DOUBLE) {
            return DateParts::fromSerial(v.toDouble());
        }
        if (v.type == VarType::EMPTY) {
            return DateParts::fromSerial(0.0);
        }
        throw BasicRuntimeError(13, "Type mismatch in date/time expression.");
    }

    static DateParts makeDateSerial(int y, int m, int d) {
        if (y >= 0 && y < 100) y += (y < 30) ? 2000 : 1900;
        int m0 = m - 1;
        int yrOffset = (m0 >= 0) ? (m0 / 12) : ((m0 - 11) / 12);
        y += yrOffset;
        m = m0 - yrOffset * 12 + 1;
        int64_t totalDays = daysFromCivil(y, m, 1) + (d - 1);
        DateParts dp;
        civilFromDays(totalDays, dp.year, dp.month, dp.day);
        dp.hasDate = true;
        dp.hasTime = false;
        return dp;
    }

    static DateParts makeTimeSerial(int h, int m, int s) {
        int64_t totalSec = static_cast<int64_t>(h) * 3600 + static_cast<int64_t>(m) * 60 + s;
        DateParts dp;
        int64_t dayOffset = (totalSec >= 0) ? (totalSec / 86400) : ((totalSec - 86399) / 86400);
        int64_t remSec = totalSec - dayOffset * 86400;
        civilFromDays(kBasicEpochDays + dayOffset, dp.year, dp.month, dp.day);
        dp.hour = static_cast<int>(remSec / 3600);
        dp.minute = static_cast<int>((remSec / 60) % 60);
        dp.second = static_cast<int>(remSec % 60);
        dp.hasDate = (dayOffset != 0);
        dp.hasTime = true;
        return dp;
    }

    static int computeWeekday(const DateParts &dp, int firstDayOfWeek = 1) {
        if (firstDayOfWeek == 0) firstDayOfWeek = 1;
        if (firstDayOfWeek < 1 || firstDayOfWeek > 7)
            throw BasicRuntimeError(5, "Invalid procedure call: firstDayOfWeek in Weekday must be 0..7.");
        int64_t z = daysFromCivil(dp.year, dp.month, dp.day);
        int sunBased = static_cast<int>(((z + 4) % 7 + 7) % 7) + 1; // 1 = Sunday .. 7 = Saturday
        return ((sunBased - firstDayOfWeek + 7) % 7) + 1;
    }

    static int computeDatePart(const std::string &rawInterval, const DateParts &dp, int firstDay = 1) {
        std::string iv = toUpper(trim(rawInterval));
        if (iv == "YYYY") return dp.year;
        if (iv == "Q") return ((dp.month - 1) / 3) + 1;
        if (iv == "M") return dp.month;
        if (iv == "Y") return dayOfYear(dp.year, dp.month, dp.day);
        if (iv == "D") return dp.day;
        if (iv == "W") return computeWeekday(dp, firstDay);
        if (iv == "WW") {
            int doy = dayOfYear(dp.year, dp.month, dp.day);
            DateParts jan1 = makeDateSerial(dp.year, 1, 1);
            int jan1Dow = computeWeekday(jan1, firstDay);
            return ((doy + jan1Dow - 2) / 7) + 1;
        }
        if (iv == "H") return dp.hour;
        if (iv == "N") return dp.minute;
        if (iv == "S") return dp.second;
        throw BasicRuntimeError(5, "Invalid interval '" + rawInterval + "' in DatePart.");
    }

    static DateParts computeDateAdd(const std::string &rawInterval, double number, const DateParts &dp) {
        std::string iv = toUpper(trim(rawInterval));
        int64_t n = static_cast<int64_t>(std::llround(number));
        DateParts res = dp;

        if (iv == "YYYY" || iv == "Q" || iv == "M") {
            int64_t addMonths = (iv == "YYYY") ? (n * 12) : ((iv == "Q") ? (n * 3) : n);
            int64_t totalM = static_cast<int64_t>(res.year) * 12 + (res.month - 1) + addMonths;
            int64_t newY = (totalM >= 0) ? (totalM / 12) : ((totalM - 11) / 12);
            int newM = static_cast<int>(totalM - newY * 12 + 1);
            res.year = static_cast<int>(newY);
            res.month = newM;
            int maxD = daysInMonth(res.year, res.month);
            if (res.day > maxD) res.day = maxD;
            res.hasDate = true;
            return res;
        }
        if (iv == "Y" || iv == "D" || iv == "W" || iv == "WW") {
            int64_t addDays = (iv == "WW") ? (n * 7) : n;
            int64_t z = daysFromCivil(res.year, res.month, res.day) + addDays;
            civilFromDays(z, res.year, res.month, res.day);
            res.hasDate = true;
            return res;
        }
        if (iv == "H" || iv == "N" || iv == "S") {
            int64_t mult = (iv == "H") ? 3600 : ((iv == "N") ? 60 : 1);
            int64_t baseDays = res.hasDate ? daysFromCivil(res.year, res.month, res.day) : kBasicEpochDays;
            int64_t totalSec = res.hour * 3600LL + res.minute * 60LL + res.second + n * mult;
            int64_t dayDelta = (totalSec >= 0) ? (totalSec / 86400) : ((totalSec - 86399) / 86400);
            int64_t remSec = totalSec - dayDelta * 86400;
            civilFromDays(baseDays + dayDelta, res.year, res.month, res.day);
            res.hour = static_cast<int>(remSec / 3600);
            res.minute = static_cast<int>((remSec / 60) % 60);
            res.second = static_cast<int>(remSec % 60);
            res.hasDate = res.hasDate || (dayDelta != 0);
            res.hasTime = true;
            return res;
        }
        throw BasicRuntimeError(5, "Invalid interval '" + rawInterval + "' in DateAdd.");
    }

    static int64_t computeDateDiff(const std::string &rawInterval, const DateParts &d1,
                                   const DateParts &d2, int firstDay = 1) {
        std::string iv = toUpper(trim(rawInterval));
        if (iv == "YYYY") return static_cast<int64_t>(d2.year - d1.year);
        if (iv == "Q") {
            int q1 = (d1.month - 1) / 3;
            int q2 = (d2.month - 1) / 3;
            return static_cast<int64_t>(d2.year - d1.year) * 4 + (q2 - q1);
        }
        if (iv == "M") {
            return static_cast<int64_t>(d2.year - d1.year) * 12 + (d2.month - d1.month);
        }
        int64_t days1 = daysFromCivil(d1.year, d1.month, d1.day);
        int64_t days2 = daysFromCivil(d2.year, d2.month, d2.day);
        int64_t diffDays = days2 - days1;
        if (iv == "D" || iv == "Y") return diffDays;
        if (iv == "W") return diffDays / 7;
        if (iv == "WW") {
            int dow1 = computeWeekday(d1, firstDay);
            int dow2 = computeWeekday(d2, firstDay);
            int64_t startWeek1 = days1 - (dow1 - 1);
            int64_t startWeek2 = days2 - (dow2 - 1);
            return (startWeek2 - startWeek1) / 7;
        }
        int64_t sec1 = days1 * 86400LL + d1.hour * 3600LL + d1.minute * 60LL + d1.second;
        int64_t sec2 = days2 * 86400LL + d2.hour * 3600LL + d2.minute * 60LL + d2.second;
        int64_t diffSec = sec2 - sec1;
        if (iv == "H") return diffSec / 3600;
        if (iv == "N") return diffSec / 60;
        if (iv == "S") return diffSec;
        throw BasicRuntimeError(5, "Invalid interval '" + rawInterval + "' in DateDiff.");
    }

    static std::string formatDateTimeValue(const Variant &v, int namedFormat = 0) {
        static const char *monthNames[12] = {
            "January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"
        };
        static const char *dayNames[7] = {
            "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
        };
        DateParts dp = parseDateVariant(v);
        if (namedFormat == 0) { // vbGeneralDate
            return dp.formatGeneral();
        }
        if (namedFormat == 1) { // vbLongDate
            int wd = computeWeekday(dp, 1) - 1;
            int mn = std::max(1, std::min(12, dp.month)) - 1;
            return std::string(dayNames[wd]) + ", " + monthNames[mn] + " " +
                   std::to_string(dp.day) + ", " + std::to_string(dp.year);
        }
        if (namedFormat == 2) { // vbShortDate
            return dp.formatDate();
        }
        if (namedFormat == 3) { // vbLongTime
            return dp.formatTime();
        }
        if (namedFormat == 4) { // vbShortTime (HH:MM)
            std::ostringstream oss;
            oss << std::setfill('0') << std::setw(2) << dp.hour << ":" << std::setw(2) << dp.minute;
            return oss.str();
        }
        throw BasicRuntimeError(5, "Invalid NamedFormat in FormatDateTime.");
    }

    static void setSystemDate(const Variant &v) {
        DateParts target = parseDateVariant(v);
        DateParts cur = currentNow();
        int64_t diffDays = daysFromCivil(target.year, target.month, target.day) -
                           daysFromCivil(cur.year, cur.month, cur.day);
        gSystemTimeOffsetSec += diffDays * 86400LL;
    }

    static void setSystemTime(const Variant &v) {
        DateParts target = parseDateVariant(v);
        DateParts cur = currentNow();
        int64_t diffSec = (target.hour * 3600LL + target.minute * 60LL + target.second) -
                          (cur.hour * 3600LL + cur.minute * 60LL + cur.second);
        gSystemTimeOffsetSec += diffSec;
    }
}

namespace FileSystemUtil {
    namespace fs = std::filesystem;

    static bool wildcardMatch(const std::string &pat, const std::string &str) {
        std::string up = toUpper(pat), us = toUpper(str);
        size_t p = 0, s = 0, starIdx = std::string::npos, matchIdx = 0;
        while (s < us.size()) {
            if (p < up.size() && (up[p] == '?' || up[p] == us[s])) {
                ++p; ++s;
            } else if (p < up.size() && up[p] == '*') {
                starIdx = p++;
                matchIdx = s;
            } else if (starIdx != std::string::npos) {
                p = starIdx + 1;
                s = ++matchIdx;
            } else {
                return false;
            }
        }
        while (p < up.size() && up[p] == '*') ++p;
        return p == up.size();
    }

    struct DirState {
        std::vector<std::string> matches;
        size_t index = 0;
        bool initialized = false;

        std::string startSearch(const std::string &rawPattern, int attrMask) {
            matches.clear();
            index = 0;
            initialized = true;

            std::string pat = trim(rawPattern);
            if (pat.empty()) pat = "*";

            fs::path p(pat);
            std::error_code ec;
            if (fs::exists(p, ec) && pat.find('*') == std::string::npos && pat.find('?') == std::string::npos) {
                if (fs::is_directory(p, ec)) {
                    if (attrMask & 16) {
                        std::string name = p.filename().string();
                        if (name.empty()) name = p.string();
                        matches.push_back(name);
                    }
                } else {
                    matches.push_back(p.filename().string());
                }
                return nextMatch();
            }

            fs::path parentDir = p.parent_path();
            std::string fileMask = p.filename().string();
            if (parentDir.empty()) parentDir = fs::current_path(ec);
            if (fileMask.empty()) fileMask = "*";

            if (!fs::exists(parentDir, ec) || !fs::is_directory(parentDir, ec)) {
                return "";
            }

            for (const auto &entry : fs::directory_iterator(parentDir, ec)) {
                std::string fname = entry.path().filename().string();
                if (!fname.empty() && fname[0] == '.' && (attrMask & 2) == 0 && fileMask[0] != '.') {
                    continue;
                }
                bool isDir = entry.is_directory(ec);
                if (isDir && (attrMask & 16) == 0) continue;
                if (wildcardMatch(fileMask, fname)) {
                    matches.push_back(fname);
                }
            }
            std::sort(matches.begin(), matches.end());
            return nextMatch();
        }

        std::string nextMatch() {
            if (!initialized) throw BasicRuntimeError(5, "Invalid procedure call: Dir called without initial pattern.");
            if (index >= matches.size()) return "";
            return matches[index++];
        }
    };

    static int getFileAttributes(const std::string &rawPath) {
        std::error_code ec;
        fs::path p(rawPath);
        if (!fs::exists(p, ec)) throw BasicRuntimeError(53, "File not found: " + rawPath);
        int attr = 0;
        if (fs::is_directory(p, ec)) attr |= 16; // vbDirectory
        auto perms = fs::status(p, ec).permissions();
        if ((perms & fs::perms::owner_write) == fs::perms::none) attr |= 1; // vbReadOnly
        std::string fname = p.filename().string();
        if (!fname.empty() && fname[0] == '.') attr |= 2; // vbHidden
        return attr;
    }

    static void setFileAttributes(const std::string &rawPath, int attr) {
        std::error_code ec;
        fs::path p(rawPath);
        if (!fs::exists(p, ec)) throw BasicRuntimeError(53, "File not found: " + rawPath);
        auto st = fs::status(p, ec);
        auto perms = st.permissions();
        if (attr & 1) { // vbReadOnly
            perms &= ~(fs::perms::owner_write | fs::perms::group_write | fs::perms::others_write);
        } else {
            perms |= fs::perms::owner_write;
        }
        fs::permissions(p, perms, ec);
        if (ec) throw BasicRuntimeError(75, "Path/File access error setting attributes on: " + rawPath);
    }

    static std::string getFileDateTime(const std::string &rawPath) {
        struct stat st{};
        if (stat(rawPath.c_str(), &st) != 0) {
            throw BasicRuntimeError(53, "File not found: " + rawPath);
        }
        std::time_t modTime = st.st_mtime;
        std::tm lt{};
#if defined(_WIN32)
        localtime_s(&lt, &modTime);
#else
        localtime_r(&modTime, &lt);
#endif
        DateTimeUtil::DateParts dp;
        dp.year = lt.tm_year + 1900;
        dp.month = lt.tm_mon + 1;
        dp.day = lt.tm_mday;
        dp.hour = lt.tm_hour;
        dp.minute = lt.tm_min;
        dp.second = lt.tm_sec;
        dp.hasDate = true;
        dp.hasTime = true;
        return dp.formatGeneral();
    }

    static int64_t getFileLength(const std::string &rawPath) {
        std::error_code ec;
        fs::path p(rawPath);
        if (!fs::exists(p, ec)) throw BasicRuntimeError(53, "File not found: " + rawPath);
        if (fs::is_directory(p, ec)) return 0;
        auto sz = fs::file_size(p, ec);
        if (ec) throw BasicRuntimeError(57, "Device I/O error reading length of: " + rawPath);
        return static_cast<int64_t>(sz);
    }
}

// ============================================================================
// LIBREOFFICE CALC SPREADSHEET FUNCTIONS MODULE (Prefix: calc.* / calc_*)
// ============================================================================
namespace CalcFunctions {

    static const std::unordered_set<std::string>& knownCalcNames() {
        static const std::unordered_set<std::string> names = {
            // Hyperbolic & Inverse Hyperbolic
            "SINH", "COSH", "TANH", "COTH", "SECH", "CSCH",
            "ASINH", "ARSINH", "ACOSH", "ARCOSH", "ATANH", "ARTANH",
            "ACOTH", "ARCOTH", "ASECH", "ACSCH",
            // Trigonometric & Mathematical
            "SIN", "COS", "TAN", "COT", "SEC", "CSC",
            "ASIN", "ACOS", "ATAN", "ATAN2", "ACOT",
            "RADIANS", "DEGREES", "PI",
            "EXP", "LN", "LOG", "LOG10", "POWER", "SQRT", "SQRTPI",
            "ABS", "SIGN", "FACT", "FACTDOUBLE", "COMBIN", "COMBINA", "PERMUT",
            "GCD", "LCM", "MOD", "QUOTIENT",
            "ROUND", "ROUNDUP", "ROUNDDOWN", "MROUND", "CEILING", "FLOOR",
            "TRUNC", "INT", "EVEN", "ODD", "RAND", "RANDBETWEEN",
            "SUM", "PRODUCT", "SUMSQ", "SUMPRODUCT",
            "SUMX2MY2", "SUMX2PY2", "SUMXMY2", "SUMIF", "SUMIFS",
            // Lookup & Reference
            "VLOOKUP", "HLOOKUP", "LOOKUP", "XLOOKUP",
            "MATCH", "INDEX", "CHOOSE", "ROWS", "COLUMNS",
            // Database Functions (2D Array Tables)
            "DGET", "DSUM", "DAVERAGE", "DCOUNT", "DCOUNTA",
            "DMAX", "DMIN", "DPRODUCT", "DSTDEV", "DSTDEVP", "DVAR", "DVARP",
            // Statistical & Aggregation
            "AVERAGE", "AVERAGEA", "AVERAGEIF", "AVERAGEIFS",
            "COUNT", "COUNTA", "COUNTBLANK", "COUNTIF", "COUNTIFS",
            "MAX", "MAXA", "MIN", "MINA", "MAXIFS", "MINIFS",
            "MEDIAN", "MODE", "LARGE", "SMALL", "PERCENTILE", "QUARTILE", "RANK",
            "VAR", "VAR.S", "VARP", "VAR.P", "STDEV", "STDEV.S", "STDEVP", "STDEV.P",
            "AVEDEV", "DEVSQ", "GEOMEAN", "HARMEAN",
            "CORREL", "PEARSON", "COVAR", "SLOPE", "INTERCEPT", "RSQ", "FORECAST",
            // Text Functions
            "CONCAT", "CONCATENATE", "TEXTJOIN", "EXACT", "FIND", "SEARCH",
            "SUBSTITUTE", "REPLACE", "PROPER", "CLEAN", "REPT",
            "CHAR", "CODE", "VALUE", "TEXT",
            "LEFT", "RIGHT", "MID", "LEN", "TRIM", "UPPER", "LOWER",
            // Logical & Information
            "IF", "IFS", "SWITCH", "AND", "OR", "XOR", "NOT",
            "IFERROR", "IFNA", "ISERROR", "ISNA", "ISNUMBER", "ISTEXT",
            "ISBLANK", "ISEVEN", "ISODD"
        };
        return names;
    }

    static std::string extractCalcName(const std::string &u) {
        std::string core;
        if (u.rfind("CALC.", 0) == 0) {
            core = u.substr(5);
        } else if (u.rfind("CALC_", 0) == 0) {
            core = u.substr(5);
        } else if (u.rfind("CALC", 0) == 0 && u.size() > 4) {
            core = u.substr(4);
        } else {
            return "";
        }
        if (!core.empty() && (core.back() == '$' || core.back() == '%')) {
            core.pop_back();
        }
        if (knownCalcNames().count(core)) return core;
        return "";
    }

    static bool isCalcFunction(const std::string &u) {
        return !extractCalcName(u).empty();
    }

    static void flattenVariants(const std::vector<Variant> &args, std::vector<Variant> &out) {
        for (const auto &a : args) {
            if (a.type == VarType::ARRAY) {
                for (const auto &el : a.arrayData) out.push_back(el);
            } else {
                out.push_back(a);
            }
        }
    }

    static bool isNumericVariant(const Variant &v) {
        return (v.type == VarType::INTEGER || v.type == VarType::DOUBLE);
    }

    static std::vector<double> extractNumericList(const std::vector<Variant> &args, bool includeTextAndEmptyAsZero = false) {
        std::vector<Variant> flat;
        flattenVariants(args, flat);
        std::vector<double> nums;
        for (const auto &v : flat) {
            if (isNumericVariant(v)) {
                nums.push_back(v.toDouble());
            } else if (includeTextAndEmptyAsZero && v.type != VarType::EMPTY && v.type != VarType::ERRVAL) {
                nums.push_back(0.0);
            }
        }
        return nums;
    }

    static bool variantEquals(const Variant &a, const Variant &b) {
        if (a.type == VarType::STRING && b.type == VarType::STRING) {
            return toUpper(a.sVal) == toUpper(b.sVal);
        }
        if (isNumericVariant(a) && isNumericVariant(b)) {
            return std::fabs(a.toDouble() - b.toDouble()) <= 1e-12;
        }
        if (a.type == VarType::EMPTY && b.type == VarType::EMPTY) return true;
        return toUpper(a.toString()) == toUpper(b.toString());
    }

    static int variantCompare(const Variant &a, const Variant &b) {
        if (isNumericVariant(a) && isNumericVariant(b)) {
            double da = a.toDouble(), db = b.toDouble();
            if (std::fabs(da - db) <= 1e-12) return 0;
            return (da < db) ? -1 : 1;
        }
        std::string sa = toUpper(a.toString());
        std::string sb = toUpper(b.toString());
        if (sa == sb) return 0;
        return (sa < sb) ? -1 : 1;
    }

    // Evaluates a Calc criteria expression (e.g. ">10", "<=50", "<>Bolt", "App*", 42) against cell value
    static bool matchesCalcCriteria(const Variant &cell, const Variant &crit) {
        if (crit.type == VarType::EMPTY) return true;
        if (isNumericVariant(crit)) {
            if (!isNumericVariant(cell)) {
                try {
                    size_t idx = 0;
                    double cd = std::stod(trim(cell.toString()), &idx);
                    if (idx == trim(cell.toString()).size()) {
                        return std::fabs(cd - crit.toDouble()) <= 1e-12;
                    }
                } catch (...) {}
                return false;
            }
            return std::fabs(cell.toDouble() - crit.toDouble()) <= 1e-12;
        }

        std::string s = trim(crit.toString());
        if (s.empty()) return (cell.type == VarType::EMPTY || cell.toString().empty());

        std::string op = "=";
        std::string rhs = s;
        if (s.rfind("<=", 0) == 0 || s.rfind(">=", 0) == 0 || s.rfind("<>", 0) == 0) {
            op = s.substr(0, 2);
            rhs = trim(s.substr(2));
        } else if (s[0] == '<' || s[0] == '>' || s[0] == '=') {
            op = s.substr(0, 1);
            rhs = trim(s.substr(1));
        }

        bool rhsIsNum = false;
        double rhsNum = 0.0;
        if (!rhs.empty()) {
            try {
                size_t idx = 0;
                rhsNum = std::stod(rhs, &idx);
                if (idx == rhs.size()) rhsIsNum = true;
            } catch (...) {}
        }

        bool cellIsNum = isNumericVariant(cell);
        double cellNum = cell.toDouble();
        if (!cellIsNum && cell.type == VarType::STRING) {
            std::string cs = trim(cell.sVal);
            if (!cs.empty()) {
                try {
                    size_t idx = 0;
                    cellNum = std::stod(cs, &idx);
                    if (idx == cs.size()) cellIsNum = true;
                } catch (...) {}
            }
        }

        if (rhsIsNum && cellIsNum) {
            if (op == "=")  return std::fabs(cellNum - rhsNum) <= 1e-12;
            if (op == "<>") return std::fabs(cellNum - rhsNum) > 1e-12;
            if (op == "<")  return cellNum < rhsNum - 1e-12;
            if (op == "<=") return cellNum <= rhsNum + 1e-12;
            if (op == ">")  return cellNum > rhsNum + 1e-12;
            if (op == ">=") return cellNum >= rhsNum - 1e-12;
        }

        if (rhsIsNum && !cellIsNum) {
            return (op == "<>");
        }

        std::string cellStr = cell.toString();
        if (op == "=" && (rhs.find('*') != std::string::npos || rhs.find('?') != std::string::npos)) {
            return FileSystemUtil::wildcardMatch(rhs, cellStr);
        }
        if (op == "<>" && (rhs.find('*') != std::string::npos || rhs.find('?') != std::string::npos)) {
            return !FileSystemUtil::wildcardMatch(rhs, cellStr);
        }

        std::string uCell = toUpper(cellStr);
        std::string uRhs = toUpper(rhs);
        if (op == "=")  return uCell == uRhs;
        if (op == "<>") return uCell != uRhs;
        if (op == "<")  return uCell < uRhs;
        if (op == "<=") return uCell <= uRhs;
        if (op == ">")  return uCell > uRhs;
        if (op == ">=") return uCell >= uRhs;
        return false;
    }

    // Helper for 2D/1D array shape normalized to (rows, cols) with 0-based (r, c) access
    struct GridView {
        const Variant &arr;
        int rows = 0;
        int cols = 0;
        bool is2D = false;

        explicit GridView(const Variant &a) : arr(a) {
            if (a.type != VarType::ARRAY || a.dims.empty()) {
                throw BasicRuntimeError(5, "Calc function requires an allocated array.");
            }
            if (a.dims.size() == 1) {
                rows = a.dims[0].size();
                cols = 1;
                is2D = false;
            } else if (a.dims.size() == 2) {
                rows = a.dims[0].size();
                cols = a.dims[1].size();
                is2D = true;
            } else {
                throw BasicRuntimeError(5, "Calc table/range functions support 1D or 2D arrays.");
            }
        }

        const Variant& cell(int r, int c) const {
            if (r < 0 || r >= rows || c < 0 || c >= cols) {
                throw BasicRuntimeError(9, "Subscript out of range in Calc array operation.");
            }
            if (!is2D) {
                return arr.at({arr.dims[0].low + r});
            }
            return arr.at({arr.dims[0].low + r, arr.dims[1].low + c});
        }
    };

    // Helper for Database functions (DGET, DSUM, DAVERAGE, DCOUNT, DCOUNTA, DMAX, DMIN, DPRODUCT, DSTDEV, etc.)
    static int resolveDatabaseFieldCol(const GridView &db, const Variant &fieldArg) {
        if (fieldArg.type == VarType::EMPTY) return -1;
        if (isNumericVariant(fieldArg)) {
            int col1 = static_cast<int>(fieldArg.toInt());
            if (col1 < 1 || col1 > db.cols) {
                throw BasicRuntimeError(5, "Database field column index " + std::to_string(col1) + " is out of range.");
            }
            return col1 - 1;
        }
        std::string fname = trim(fieldArg.toString());
        if (fname.empty()) return -1;
        std::string uName = toUpper(fname);
        for (int c = 0; c < db.cols; ++c) {
            if (toUpper(trim(db.cell(0, c).toString())) == uName) {
                return c;
            }
        }
        // Also allow numeric string like "2"
        try {
            size_t idx = 0;
            int col1 = std::stoi(fname, &idx);
            if (idx == fname.size() && col1 >= 1 && col1 <= db.cols) return col1 - 1;
        } catch (...) {}
        throw BasicRuntimeError(5, "Database field '" + fname + "' not found in header row.");
    }

    static std::vector<int> matchDatabaseRows(const GridView &db, const Variant &critVar) {
        if (db.rows < 1) return {};
        std::vector<int> matchedRows;

        // Support both 2D Criteria array (row 0 = headers, rows 1..k = OR-ed condition rows)
        // and 1D 2-element array [fieldName, condition]
        if (critVar.type != VarType::ARRAY || critVar.dims.empty()) {
            throw BasicRuntimeError(5, "Database criteria must be an array (2D criteria range or 1D [field, condition]).");
        }

        if (critVar.dims.size() == 1) {
            int len = critVar.dims[0].size();
            if (len < 2) throw BasicRuntimeError(5, "1D Database criteria array requires [field, condition].");
            int colIdx = resolveDatabaseFieldCol(db, critVar.at({critVar.dims[0].low}));
            const Variant &cond = critVar.at({critVar.dims[0].low + 1});
            for (int r = 1; r < db.rows; ++r) {
                if (colIdx < 0 || matchesCalcCriteria(db.cell(r, colIdx), cond)) {
                    matchedRows.push_back(r);
                }
            }
            return matchedRows;
        }

        GridView crit(critVar);
        if (crit.rows < 2) {
            for (int r = 1; r < db.rows; ++r) matchedRows.push_back(r);
            return matchedRows;
        }

        std::vector<int> critColMap(crit.cols, -1);
        for (int c = 0; c < crit.cols; ++c) {
            std::string h = trim(crit.cell(0, c).toString());
            if (!h.empty()) {
                critColMap[c] = resolveDatabaseFieldCol(db, Variant::fromString(h));
            }
        }

        for (int r = 1; r < db.rows; ++r) {
            bool rowMatchedAnyOrGroup = false;
            for (int cr = 1; cr < crit.rows; ++cr) {
                bool allColsInCritRowMatch = true;
                bool critRowHasAnyCondition = false;
                for (int cc = 0; cc < crit.cols; ++cc) {
                    const Variant &condVal = crit.cell(cr, cc);
                    if (condVal.type == VarType::EMPTY || trim(condVal.toString()).empty()) {
                        continue;
                    }
                    critRowHasAnyCondition = true;
                    int dbCol = critColMap[cc];
                    if (dbCol < 0 || !matchesCalcCriteria(db.cell(r, dbCol), condVal)) {
                        allColsInCritRowMatch = false;
                        break;
                    }
                }
                if (!critRowHasAnyCondition || allColsInCritRowMatch) {
                    rowMatchedAnyOrGroup = true;
                    break;
                }
            }
            if (rowMatchedAnyOrGroup) {
                matchedRows.push_back(r);
            }
        }
        return matchedRows;
    }

    static int64_t calcGcdPair(int64_t a, int64_t b) {
        a = std::llabs(a);
        b = std::llabs(b);
        while (b != 0) {
            int64_t t = b;
            b = a % b;
            a = t;
        }
        return a;
    }

    static Variant dispatch(const std::string &rawName, const std::vector<Variant> &args) {
        std::string fn = extractCalcName(toUpper(rawName));
        if (fn.empty()) {
            throw BasicRuntimeError(35, "Unknown Calc function '" + rawName + "'.");
        }

        auto requireArgs = [&](size_t minA, size_t maxA = 999999) {
            if (args.size() < minA || args.size() > maxA) {
                throw BasicRuntimeError(5, "Calc." + fn + " requires " + std::to_string(minA) +
                                        (minA == maxA ? "" : (" to " + std::to_string(maxA))) + " argument(s).");
            }
        };

        // ====================================================================
        // 1. HYPERBOLIC & INVERSE HYPERBOLIC FUNCTIONS
        // ====================================================================
        if (fn == "SINH") {
            requireArgs(1, 1);
            return Variant::fromDouble(std::sinh(args[0].toDouble()));
        }
        if (fn == "COSH") {
            requireArgs(1, 1);
            return Variant::fromDouble(std::cosh(args[0].toDouble()));
        }
        if (fn == "TANH") {
            requireArgs(1, 1);
            return Variant::fromDouble(std::tanh(args[0].toDouble()));
        }
        if (fn == "COTH") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            double s = std::sinh(x);
            if (std::fabs(s) < 1e-18) throw BasicRuntimeError(11, "Calc.COTH division by zero (x = 0).");
            return Variant::fromDouble(std::cosh(x) / s);
        }
        if (fn == "SECH") {
            requireArgs(1, 1);
            return Variant::fromDouble(1.0 / std::cosh(args[0].toDouble()));
        }
        if (fn == "CSCH") {
            requireArgs(1, 1);
            double s = std::sinh(args[0].toDouble());
            if (std::fabs(s) < 1e-18) throw BasicRuntimeError(11, "Calc.CSCH division by zero (x = 0).");
            return Variant::fromDouble(1.0 / s);
        }
        if (fn == "ASINH" || fn == "ARSINH") {
            requireArgs(1, 1);
            return Variant::fromDouble(std::asinh(args[0].toDouble()));
        }
        if (fn == "ACOSH" || fn == "ARCOSH") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x < 1.0) throw BasicRuntimeError(5, "Calc.ACOSH domain error: argument must be >= 1.");
            return Variant::fromDouble(std::acosh(x));
        }
        if (fn == "ATANH" || fn == "ARTANH") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x <= -1.0 || x >= 1.0) throw BasicRuntimeError(5, "Calc.ATANH domain error: argument must be in (-1, 1).");
            return Variant::fromDouble(std::atanh(x));
        }
        if (fn == "ACOTH" || fn == "ARCOTH") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (std::fabs(x) <= 1.0) throw BasicRuntimeError(5, "Calc.ACOTH domain error: |x| must be > 1.");
            return Variant::fromDouble(0.5 * std::log((x + 1.0) / (x - 1.0)));
        }
        if (fn == "ASECH") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x <= 0.0 || x > 1.0) throw BasicRuntimeError(5, "Calc.ASECH domain error: x must be in (0, 1].");
            return Variant::fromDouble(std::acosh(1.0 / x));
        }
        if (fn == "ACSCH") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (std::fabs(x) < 1e-18) throw BasicRuntimeError(11, "Calc.ACSCH division by zero (x = 0).");
            return Variant::fromDouble(std::asinh(1.0 / x));
        }

        // ====================================================================
        // 2. MATHEMATICAL & TRIGONOMETRIC FUNCTIONS
        // ====================================================================
        if (fn == "SIN") { requireArgs(1, 1); return Variant::fromDouble(std::sin(args[0].toDouble())); }
        if (fn == "COS") { requireArgs(1, 1); return Variant::fromDouble(std::cos(args[0].toDouble())); }
        if (fn == "TAN") { requireArgs(1, 1); return Variant::fromDouble(std::tan(args[0].toDouble())); }
        if (fn == "COT") {
            requireArgs(1, 1);
            double t = std::tan(args[0].toDouble());
            if (std::fabs(t) < 1e-18) throw BasicRuntimeError(11, "Calc.COT division by zero.");
            return Variant::fromDouble(1.0 / t);
        }
        if (fn == "SEC") {
            requireArgs(1, 1);
            double c = std::cos(args[0].toDouble());
            if (std::fabs(c) < 1e-18) throw BasicRuntimeError(11, "Calc.SEC division by zero.");
            return Variant::fromDouble(1.0 / c);
        }
        if (fn == "CSC") {
            requireArgs(1, 1);
            double s = std::sin(args[0].toDouble());
            if (std::fabs(s) < 1e-18) throw BasicRuntimeError(11, "Calc.CSC division by zero.");
            return Variant::fromDouble(1.0 / s);
        }
        if (fn == "ASIN") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x < -1.0 || x > 1.0) throw BasicRuntimeError(5, "Calc.ASIN domain error: x must be in [-1, 1].");
            return Variant::fromDouble(std::asin(x));
        }
        if (fn == "ACOS") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x < -1.0 || x > 1.0) throw BasicRuntimeError(5, "Calc.ACOS domain error: x must be in [-1, 1].");
            return Variant::fromDouble(std::acos(x));
        }
        if (fn == "ATAN") { requireArgs(1, 1); return Variant::fromDouble(std::atan(args[0].toDouble())); }
        if (fn == "ATAN2") {
            requireArgs(2, 2);
            // LibreOffice Calc ATAN2(x, y) returns atan2(y, x)
            double x = args[0].toDouble(), y = args[1].toDouble();
            if (std::fabs(x) < 1e-18 && std::fabs(y) < 1e-18) throw BasicRuntimeError(11, "Calc.ATAN2(0, 0) is undefined.");
            return Variant::fromDouble(std::atan2(y, x));
        }
        if (fn == "ACOT") {
            requireArgs(1, 1);
            return Variant::fromDouble((std::acos(-1.0) / 2.0) - std::atan(args[0].toDouble()));
        }
        if (fn == "RADIANS") {
            requireArgs(1, 1);
            return Variant::fromDouble(args[0].toDouble() * (std::acos(-1.0) / 180.0));
        }
        if (fn == "DEGREES") {
            requireArgs(1, 1);
            return Variant::fromDouble(args[0].toDouble() * (180.0 / std::acos(-1.0)));
        }
        if (fn == "PI") {
            requireArgs(0, 0);
            return Variant::fromDouble(std::acos(-1.0));
        }
        if (fn == "EXP") { requireArgs(1, 1); return Variant::fromDouble(std::exp(args[0].toDouble())); }
        if (fn == "LN") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x <= 0.0) throw BasicRuntimeError(5, "Calc.LN domain error: x must be > 0.");
            return Variant::fromDouble(std::log(x));
        }
        if (fn == "LOG10") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x <= 0.0) throw BasicRuntimeError(5, "Calc.LOG10 domain error: x must be > 0.");
            return Variant::fromDouble(std::log10(x));
        }
        if (fn == "LOG") {
            requireArgs(1, 2);
            double x = args[0].toDouble();
            double b = (args.size() == 2) ? args[1].toDouble() : 10.0;
            if (x <= 0.0 || b <= 0.0 || std::fabs(b - 1.0) < 1e-15) {
                throw BasicRuntimeError(5, "Calc.LOG domain error: invalid number or base.");
            }
            return Variant::fromDouble(std::log(x) / std::log(b));
        }
        if (fn == "POWER") {
            requireArgs(2, 2);
            return Variant::fromDouble(std::pow(args[0].toDouble(), args[1].toDouble()));
        }
        if (fn == "SQRT") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x < 0.0) throw BasicRuntimeError(5, "Calc.SQRT domain error: negative number.");
            return Variant::fromDouble(std::sqrt(x));
        }
        if (fn == "SQRTPI") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (x < 0.0) throw BasicRuntimeError(5, "Calc.SQRTPI domain error: negative number.");
            return Variant::fromDouble(std::sqrt(x * std::acos(-1.0)));
        }
        if (fn == "ABS") { requireArgs(1, 1); return Variant::fromDouble(std::fabs(args[0].toDouble())); }
        if (fn == "SIGN") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            return Variant::fromInt((x > 0.0) ? 1 : ((x < 0.0) ? -1 : 0));
        }
        if (fn == "FACT") {
            requireArgs(1, 1);
            int64_t n = static_cast<int64_t>(std::floor(args[0].toDouble()));
            if (n < 0) throw BasicRuntimeError(5, "Calc.FACT domain error: negative argument.");
            double res = 1.0;
            for (int64_t i = 2; i <= n; ++i) res *= static_cast<double>(i);
            return Variant::fromDouble(res);
        }
        if (fn == "FACTDOUBLE") {
            requireArgs(1, 1);
            int64_t n = static_cast<int64_t>(std::floor(args[0].toDouble()));
            if (n < -1) throw BasicRuntimeError(5, "Calc.FACTDOUBLE domain error.");
            double res = 1.0;
            for (int64_t i = n; i >= 2; i -= 2) res *= static_cast<double>(i);
            return Variant::fromDouble(res);
        }
        if (fn == "COMBIN" || fn == "COMBINA") {
            requireArgs(2, 2);
            int64_t n = static_cast<int64_t>(std::floor(args[0].toDouble()));
            int64_t k = static_cast<int64_t>(std::floor(args[1].toDouble()));
            if (fn == "COMBINA") n = n + k - 1;
            if (k < 0 || n < 0 || (k > n && fn == "COMBIN")) throw BasicRuntimeError(5, "Calc." + fn + " domain error.");
            if (k == 0 || k == n) return Variant::fromInt(1);
            if (k > n - k) k = n - k;
            double res = 1.0;
            for (int64_t i = 1; i <= k; ++i) {
                res = res * static_cast<double>(n - k + i) / static_cast<double>(i);
            }
            return Variant::fromDouble(std::round(res));
        }
        if (fn == "PERMUT") {
            requireArgs(2, 2);
            int64_t n = static_cast<int64_t>(std::floor(args[0].toDouble()));
            int64_t k = static_cast<int64_t>(std::floor(args[1].toDouble()));
            if (n < 0 || k < 0 || k > n) throw BasicRuntimeError(5, "Calc.PERMUT domain error.");
            double res = 1.0;
            for (int64_t i = 0; i < k; ++i) res *= static_cast<double>(n - i);
            return Variant::fromDouble(res);
        }
        if (fn == "GCD") {
            requireArgs(1);
            std::vector<double> nums = extractNumericList(args);
            if (nums.empty()) return Variant::fromInt(0);
            int64_t g = static_cast<int64_t>(std::trunc(nums[0]));
            for (size_t i = 1; i < nums.size(); ++i) {
                g = calcGcdPair(g, static_cast<int64_t>(std::trunc(nums[i])));
            }
            return Variant::fromInt(g);
        }
        if (fn == "LCM") {
            requireArgs(1);
            std::vector<double> nums = extractNumericList(args);
            if (nums.empty()) return Variant::fromInt(0);
            int64_t l = std::llabs(static_cast<int64_t>(std::trunc(nums[0])));
            for (size_t i = 1; i < nums.size(); ++i) {
                int64_t v = std::llabs(static_cast<int64_t>(std::trunc(nums[i])));
                if (l == 0 || v == 0) { l = 0; break; }
                l = (l / calcGcdPair(l, v)) * v;
            }
            return Variant::fromInt(l);
        }
        if (fn == "MOD") {
            requireArgs(2, 2);
            double n = args[0].toDouble(), d = args[1].toDouble();
            if (std::fabs(d) < 1e-18) throw BasicRuntimeError(11, "Calc.MOD division by zero.");
            return Variant::fromDouble(n - d * std::floor(n / d));
        }
        if (fn == "QUOTIENT") {
            requireArgs(2, 2);
            double n = args[0].toDouble(), d = args[1].toDouble();
            if (std::fabs(d) < 1e-18) throw BasicRuntimeError(11, "Calc.QUOTIENT division by zero.");
            return Variant::fromInt(static_cast<int64_t>(std::trunc(n / d)));
        }
        if (fn == "ROUND" || fn == "ROUNDUP" || fn == "ROUNDDOWN" || fn == "TRUNC") {
            requireArgs(1, 2);
            double x = args[0].toDouble();
            int dec = (args.size() == 2) ? static_cast<int>(args[1].toInt()) : 0;
            double f = std::pow(10.0, dec);
            double scaled = x * f;
            double r = 0.0;
            if (fn == "ROUND") r = std::round(scaled);
            else if (fn == "ROUNDUP") r = (scaled >= 0.0) ? std::ceil(scaled - 1e-15) : std::floor(scaled + 1e-15);
            else r = std::trunc(scaled + (scaled >= 0.0 ? 1e-15 : -1e-15));
            return Variant::fromDouble(r / f);
        }
        if (fn == "MROUND") {
            requireArgs(2, 2);
            double x = args[0].toDouble(), m = args[1].toDouble();
            if (std::fabs(m) < 1e-18) return Variant::fromInt(0);
            if ((x > 0.0 && m < 0.0) || (x < 0.0 && m > 0.0)) {
                throw BasicRuntimeError(5, "Calc.MROUND: number and multiple must have same sign.");
            }
            return Variant::fromDouble(std::round(x / m) * m);
        }
        if (fn == "CEILING" || fn == "FLOOR") {
            requireArgs(1, 2);
            double x = args[0].toDouble();
            double sig = (args.size() == 2) ? args[1].toDouble() : (x >= 0.0 ? 1.0 : -1.0);
            if (std::fabs(sig) < 1e-18) return Variant::fromInt(0);
            double q = x / sig;
            double r = (fn == "CEILING") ? std::ceil(q - 1e-15) : std::floor(q + 1e-15);
            return Variant::fromDouble(r * sig);
        }
        if (fn == "INT") {
            requireArgs(1, 1);
            return Variant::fromInt(static_cast<int64_t>(std::floor(args[0].toDouble())));
        }
        if (fn == "EVEN") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            int64_t n = static_cast<int64_t>(x >= 0.0 ? std::ceil(x) : std::floor(x));
            if (n % 2 != 0) n += (x >= 0.0 ? 1 : -1);
            return Variant::fromInt(n);
        }
        if (fn == "ODD") {
            requireArgs(1, 1);
            double x = args[0].toDouble();
            if (std::fabs(x) < 1e-18) return Variant::fromInt(1);
            int64_t n = static_cast<int64_t>(x >= 0.0 ? std::ceil(x) : std::floor(x));
            if (n % 2 == 0) n += (x >= 0.0 ? 1 : -1);
            return Variant::fromInt(n);
        }
        if (fn == "RAND") {
            requireArgs(0, 0);
            return Variant::fromDouble(static_cast<double>(std::rand()) / RAND_MAX);
        }
        if (fn == "RANDBETWEEN") {
            requireArgs(2, 2);
            int64_t low = args[0].toInt(), high = args[1].toInt();
            if (low > high) throw BasicRuntimeError(5, "Calc.RANDBETWEEN: bottom > top.");
            int64_t span = high - low + 1;
            return Variant::fromInt(low + (std::rand() % span));
        }
        if (fn == "SUM") {
            std::vector<double> nums = extractNumericList(args);
            double s = 0.0;
            for (double v : nums) s += v;
            return Variant::fromDouble(s);
        }
        if (fn == "PRODUCT") {
            std::vector<double> nums = extractNumericList(args);
            if (nums.empty()) return Variant::fromInt(0);
            double p = 1.0;
            for (double v : nums) p *= v;
            return Variant::fromDouble(p);
        }
        if (fn == "SUMSQ") {
            std::vector<double> nums = extractNumericList(args);
            double s = 0.0;
            for (double v : nums) s += v * v;
            return Variant::fromDouble(s);
        }
        if (fn == "SUMPRODUCT") {
            requireArgs(1);
            std::vector<std::vector<Variant>> lists(args.size());
            for (size_t i = 0; i < args.size(); ++i) {
                flattenVariants({args[i]}, lists[i]);
                if (lists[i].size() != lists[0].size()) {
                    throw BasicRuntimeError(5, "Calc.SUMPRODUCT: array size mismatch.");
                }
            }
            double total = 0.0;
            for (size_t k = 0; k < lists[0].size(); ++k) {
                double prod = 1.0;
                for (size_t i = 0; i < lists.size(); ++i) {
                    prod *= isNumericVariant(lists[i][k]) ? lists[i][k].toDouble() : 0.0;
                }
                total += prod;
            }
            return Variant::fromDouble(total);
        }
        if (fn == "SUMX2MY2" || fn == "SUMX2PY2" || fn == "SUMXMY2") {
            requireArgs(2, 2);
            std::vector<Variant> xList, yList;
            flattenVariants({args[0]}, xList);
            flattenVariants({args[1]}, yList);
            if (xList.size() != yList.size()) throw BasicRuntimeError(5, "Calc." + fn + ": array size mismatch.");
            double total = 0.0;
            for (size_t i = 0; i < xList.size(); ++i) {
                if (isNumericVariant(xList[i]) && isNumericVariant(yList[i])) {
                    double x = xList[i].toDouble(), y = yList[i].toDouble();
                    if (fn == "SUMX2MY2") total += (x * x - y * y);
                    else if (fn == "SUMX2PY2") total += (x * x + y * y);
                    else total += (x - y) * (x - y);
                }
            }
            return Variant::fromDouble(total);
        }

        // ====================================================================
        // 3. CONDITIONAL AGGREGATIONS (SUMIF, SUMIFS, COUNTIF, COUNTIFS, AVERAGEIF, AVERAGEIFS, MAXIFS, MINIFS)
        // ====================================================================
        if (fn == "SUMIF" || fn == "AVERAGEIF") {
            requireArgs(2, 3);
            std::vector<Variant> rangeCells, valCells;
            flattenVariants({args[0]}, rangeCells);
            if (args.size() == 3) flattenVariants({args[2]}, valCells);
            else valCells = rangeCells;
            if (rangeCells.size() != valCells.size()) {
                throw BasicRuntimeError(5, "Calc." + fn + ": range and sum_range must have equal size.");
            }
            double sum = 0.0;
            int64_t cnt = 0;
            for (size_t i = 0; i < rangeCells.size(); ++i) {
                if (matchesCalcCriteria(rangeCells[i], args[1]) && isNumericVariant(valCells[i])) {
                    sum += valCells[i].toDouble();
                    ++cnt;
                }
            }
            if (fn == "AVERAGEIF") {
                if (cnt == 0) throw BasicRuntimeError(11, "Calc.AVERAGEIF division by zero (no matching numeric cells).");
                return Variant::fromDouble(sum / static_cast<double>(cnt));
            }
            return Variant::fromDouble(sum);
        }
        if (fn == "COUNTIF") {
            requireArgs(2, 2);
            std::vector<Variant> rangeCells;
            flattenVariants({args[0]}, rangeCells);
            int64_t cnt = 0;
            for (const auto &cell : rangeCells) {
                if (matchesCalcCriteria(cell, args[1])) ++cnt;
            }
            return Variant::fromInt(cnt);
        }
        if (fn == "COUNTIFS") {
            if (args.size() < 2 || args.size() % 2 != 0) {
                throw BasicRuntimeError(5, "Calc.COUNTIFS requires pairs of (criteria_range, criteria).");
            }
            size_t numPairs = args.size() / 2;
            std::vector<std::vector<Variant>> ranges(numPairs);
            for (size_t p = 0; p < numPairs; ++p) {
                flattenVariants({args[p * 2]}, ranges[p]);
                if (ranges[p].size() != ranges[0].size()) {
                    throw BasicRuntimeError(5, "Calc.COUNTIFS: all criteria ranges must have equal size.");
                }
            }
            int64_t cnt = 0;
            for (size_t i = 0; i < ranges[0].size(); ++i) {
                bool ok = true;
                for (size_t p = 0; p < numPairs; ++p) {
                    if (!matchesCalcCriteria(ranges[p][i], args[p * 2 + 1])) { ok = false; break; }
                }
                if (ok) ++cnt;
            }
            return Variant::fromInt(cnt);
        }
        if (fn == "SUMIFS" || fn == "AVERAGEIFS" || fn == "MAXIFS" || fn == "MINIFS") {
            if (args.size() < 3 || args.size() % 2 == 0) {
                throw BasicRuntimeError(5, "Calc." + fn + " requires (target_range, range1, criteria1, ...).");
            }
            std::vector<Variant> targetCells;
            flattenVariants({args[0]}, targetCells);
            size_t numPairs = (args.size() - 1) / 2;
            std::vector<std::vector<Variant>> ranges(numPairs);
            for (size_t p = 0; p < numPairs; ++p) {
                flattenVariants({args[1 + p * 2]}, ranges[p]);
                if (ranges[p].size() != targetCells.size()) {
                    throw BasicRuntimeError(5, "Calc." + fn + ": all ranges must have equal size.");
                }
            }
            double sum = 0.0;
            double bestMax = -1e308, bestMin = 1e308;
            int64_t cnt = 0;
            for (size_t i = 0; i < targetCells.size(); ++i) {
                bool ok = true;
                for (size_t p = 0; p < numPairs; ++p) {
                    if (!matchesCalcCriteria(ranges[p][i], args[2 + p * 2])) { ok = false; break; }
                }
                if (ok && isNumericVariant(targetCells[i])) {
                    double v = targetCells[i].toDouble();
                    sum += v;
                    if (cnt == 0 || v > bestMax) bestMax = v;
                    if (cnt == 0 || v < bestMin) bestMin = v;
                    ++cnt;
                }
            }
            if (fn == "SUMIFS") return Variant::fromDouble(sum);
            if (fn == "AVERAGEIFS") {
                if (cnt == 0) throw BasicRuntimeError(11, "Calc.AVERAGEIFS division by zero (no matching cells).");
                return Variant::fromDouble(sum / static_cast<double>(cnt));
            }
            if (fn == "MAXIFS") return Variant::fromDouble(cnt == 0 ? 0.0 : bestMax);
            return Variant::fromDouble(cnt == 0 ? 0.0 : bestMin);
        }

        // ====================================================================
        // 4. LOOKUP & REFERENCE FUNCTIONS (VLOOKUP, HLOOKUP, LOOKUP, XLOOKUP, MATCH, INDEX, CHOOSE)
        // ====================================================================
        if (fn == "VLOOKUP") {
            requireArgs(3, 4);
            const Variant &key = args[0];
            GridView table(args[1]);
            int colIdx1 = static_cast<int>(args[2].toInt());
            bool sortedLookup = (args.size() == 4) ? args[3].toBool() : true;
            if (colIdx1 < 1 || colIdx1 > table.cols) {
                throw BasicRuntimeError(9, "Calc.VLOOKUP column index " + std::to_string(colIdx1) + " out of bounds.");
            }
            int foundRow = -1;
            if (!sortedLookup) {
                for (int r = 0; r < table.rows; ++r) {
                    if (matchesCalcCriteria(table.cell(r, 0), key) || variantEquals(table.cell(r, 0), key)) {
                        foundRow = r;
                        break;
                    }
                }
            } else {
                for (int r = 0; r < table.rows; ++r) {
                    int cmp = variantCompare(table.cell(r, 0), key);
                    if (cmp == 0) { foundRow = r; break; }
                    if (cmp < 0) foundRow = r;
                    else break;
                }
            }
            if (foundRow < 0) return Variant::fromError(42, "#N/A");
            return table.cell(foundRow, colIdx1 - 1);
        }

        if (fn == "HLOOKUP") {
            requireArgs(3, 4);
            const Variant &key = args[0];
            GridView table(args[1]);
            int rowIdx1 = static_cast<int>(args[2].toInt());
            bool sortedLookup = (args.size() == 4) ? args[3].toBool() : true;
            if (rowIdx1 < 1 || rowIdx1 > table.rows) {
                throw BasicRuntimeError(9, "Calc.HLOOKUP row index " + std::to_string(rowIdx1) + " out of bounds.");
            }
            int foundCol = -1;
            if (!sortedLookup) {
                for (int c = 0; c < table.cols; ++c) {
                    if (matchesCalcCriteria(table.cell(0, c), key) || variantEquals(table.cell(0, c), key)) {
                        foundCol = c;
                        break;
                    }
                }
            } else {
                for (int c = 0; c < table.cols; ++c) {
                    int cmp = variantCompare(table.cell(0, c), key);
                    if (cmp == 0) { foundCol = c; break; }
                    if (cmp < 0) foundCol = c;
                    else break;
                }
            }
            if (foundCol < 0) return Variant::fromError(42, "#N/A");
            return table.cell(rowIdx1 - 1, foundCol);
        }

        if (fn == "LOOKUP") {
            requireArgs(2, 3);
            const Variant &key = args[0];
            std::vector<Variant> searchVec, resultVec;
            if (args.size() == 3) {
                flattenVariants({args[1]}, searchVec);
                flattenVariants({args[2]}, resultVec);
            } else {
                GridView g(args[1]);
                for (int r = 0; r < g.rows; ++r) {
                    searchVec.push_back(g.cell(r, 0));
                    resultVec.push_back(g.cell(r, g.cols - 1));
                }
            }
            int foundIdx = -1;
            for (size_t i = 0; i < searchVec.size(); ++i) {
                int cmp = variantCompare(searchVec[i], key);
                if (cmp == 0) { foundIdx = static_cast<int>(i); break; }
                if (cmp < 0) foundIdx = static_cast<int>(i);
                else break;
            }
            if (foundIdx < 0 || static_cast<size_t>(foundIdx) >= resultVec.size()) {
                return Variant::fromError(42, "#N/A");
            }
            return resultVec[static_cast<size_t>(foundIdx)];
        }

        if (fn == "XLOOKUP") {
            requireArgs(3, 6);
            const Variant &key = args[0];
            std::vector<Variant> lookVec, retVec;
            flattenVariants({args[1]}, lookVec);
            flattenVariants({args[2]}, retVec);
            if (lookVec.size() != retVec.size()) {
                throw BasicRuntimeError(5, "Calc.XLOOKUP: lookup_array and return_array must have equal length.");
            }
            Variant notFoundVal = (args.size() >= 4) ? args[3] : Variant::fromError(42, "#N/A");
            int matchMode = (args.size() >= 5) ? static_cast<int>(args[4].toInt()) : 0;
            int searchMode = (args.size() >= 6) ? static_cast<int>(args[5].toInt()) : 1;

            int n = static_cast<int>(lookVec.size());
            int start = (searchMode >= 0) ? 0 : (n - 1);
            int step  = (searchMode >= 0) ? 1 : -1;

            int bestIdx = -1;
            for (int i = start; i >= 0 && i < n; i += step) {
                const Variant &cand = lookVec[static_cast<size_t>(i)];
                if (matchMode == 2) {
                    if (matchesCalcCriteria(cand, key)) { bestIdx = i; break; }
                } else if (variantEquals(cand, key)) {
                    bestIdx = i;
                    break;
                } else if (matchMode == -1 && variantCompare(cand, key) < 0) {
                    if (bestIdx < 0 || variantCompare(cand, lookVec[static_cast<size_t>(bestIdx)]) > 0) bestIdx = i;
                } else if (matchMode == 1 && variantCompare(cand, key) > 0) {
                    if (bestIdx < 0 || variantCompare(cand, lookVec[static_cast<size_t>(bestIdx)]) < 0) bestIdx = i;
                }
            }
            if (bestIdx < 0) return notFoundVal;
            return retVec[static_cast<size_t>(bestIdx)];
        }

        if (fn == "MATCH") {
            requireArgs(2, 3);
            const Variant &key = args[0];
            std::vector<Variant> vec;
            flattenVariants({args[1]}, vec);
            int matchType = (args.size() == 3) ? static_cast<int>(args[2].toInt()) : 1;

            int found = -1;
            if (matchType == 0) {
                for (size_t i = 0; i < vec.size(); ++i) {
                    if (matchesCalcCriteria(vec[i], key) || variantEquals(vec[i], key)) {
                        found = static_cast<int>(i);
                        break;
                    }
                }
            } else if (matchType > 0) {
                for (size_t i = 0; i < vec.size(); ++i) {
                    int cmp = variantCompare(vec[i], key);
                    if (cmp == 0) { found = static_cast<int>(i); break; }
                    if (cmp < 0) found = static_cast<int>(i);
                    else break;
                }
            } else {
                for (size_t i = 0; i < vec.size(); ++i) {
                    int cmp = variantCompare(vec[i], key);
                    if (cmp == 0) { found = static_cast<int>(i); break; }
                    if (cmp > 0) found = static_cast<int>(i);
                    else break;
                }
            }
            if (found < 0) return Variant::fromError(42, "#N/A");
            return Variant::fromInt(found + 1);
        }

        if (fn == "INDEX") {
            requireArgs(2, 3);
            GridView g(args[0]);
            int r1 = static_cast<int>(args[1].toInt());
            int c1 = (args.size() == 3) ? static_cast<int>(args[2].toInt()) : 1;
            if (!g.is2D && args.size() == 2) {
                if (r1 < 1 || r1 > g.rows) throw BasicRuntimeError(9, "Calc.INDEX position out of bounds.");
                return g.cell(r1 - 1, 0);
            }
            if (r1 == 0 && c1 >= 1 && c1 <= g.cols) {
                Variant colArr = Variant::makeArray({{0, g.rows - 1}}, args[0].declaredType);
                for (int r = 0; r < g.rows; ++r) colArr.arrayData[static_cast<size_t>(r)] = g.cell(r, c1 - 1);
                return colArr;
            }
            if (c1 == 0 && r1 >= 1 && r1 <= g.rows) {
                Variant rowArr = Variant::makeArray({{0, g.cols - 1}}, args[0].declaredType);
                for (int c = 0; c < g.cols; ++c) rowArr.arrayData[static_cast<size_t>(c)] = g.cell(r1 - 1, c);
                return rowArr;
            }
            if (r1 < 1 || r1 > g.rows || c1 < 1 || c1 > g.cols) {
                throw BasicRuntimeError(9, "Calc.INDEX (row, col) out of bounds.");
            }
            return g.cell(r1 - 1, c1 - 1);
        }

        if (fn == "CHOOSE") {
            requireArgs(2);
            int idx = static_cast<int>(args[0].toInt());
            if (idx < 1 || static_cast<size_t>(idx) >= args.size()) {
                return Variant::fromError(502, "#VALUE!");
            }
            return args[static_cast<size_t>(idx)];
        }

        if (fn == "ROWS") {
            requireArgs(1, 1);
            if (args[0].type != VarType::ARRAY) return Variant::fromInt(1);
            GridView g(args[0]);
            return Variant::fromInt(g.rows);
        }

        if (fn == "COLUMNS") {
            requireArgs(1, 1);
            if (args[0].type != VarType::ARRAY) return Variant::fromInt(1);
            GridView g(args[0]);
            return Variant::fromInt(g.cols);
        }

        // ====================================================================
        // 5. DATABASE FUNCTIONS (DGET, DSUM, DAVERAGE, DCOUNT, DCOUNTA, DMAX, DMIN, DPRODUCT, DSTDEV, DSTDEVP, DVAR, DVARP)
        // ====================================================================
        if (fn == "DGET" || fn == "DSUM" || fn == "DAVERAGE" || fn == "DCOUNT" ||
            fn == "DCOUNTA" || fn == "DMAX" || fn == "DMIN" || fn == "DPRODUCT" ||
            fn == "DSTDEV" || fn == "DSTDEVP" || fn == "DVAR" || fn == "DVARP") {
            requireArgs(3, 4);
            GridView db(args[0]);
            int fieldCol = resolveDatabaseFieldCol(db, args[1]);

            Variant critVar;
            if (args.size() == 4) {
                // Convenience: calc.DGET(db, "Price", "Item", "Bolt")
                critVar = Variant::makeArray({{0, 1}}, VarType::EMPTY);
                critVar.arrayData[0] = args[2];
                critVar.arrayData[1] = args[3];
            } else {
                critVar = args[2];
            }

            std::vector<int> rows = matchDatabaseRows(db, critVar);

            if (fn == "DGET") {
                if (fieldCol < 0) throw BasicRuntimeError(5, "Calc.DGET requires a valid field column.");
                if (rows.empty()) return Variant::fromError(502, "#VALUE!");
                if (rows.size() > 1) return Variant::fromError(504, "Err:504 (multiple records match DGET)");
                return db.cell(rows[0], fieldCol);
            }
            if (fn == "DCOUNT") {
                if (fieldCol < 0) return Variant::fromInt(static_cast<int64_t>(rows.size()));
                int64_t cnt = 0;
                for (int r : rows) {
                    if (isNumericVariant(db.cell(r, fieldCol))) ++cnt;
                }
                return Variant::fromInt(cnt);
            }
            if (fn == "DCOUNTA") {
                if (fieldCol < 0) return Variant::fromInt(static_cast<int64_t>(rows.size()));
                int64_t cnt = 0;
                for (int r : rows) {
                    const Variant &v = db.cell(r, fieldCol);
                    if (v.type != VarType::EMPTY && !v.toString().empty()) ++cnt;
                }
                return Variant::fromInt(cnt);
            }

            if (fieldCol < 0) throw BasicRuntimeError(5, "Calc." + fn + " requires a valid field column.");
            std::vector<double> vals;
            for (int r : rows) {
                const Variant &v = db.cell(r, fieldCol);
                if (isNumericVariant(v)) vals.push_back(v.toDouble());
            }

            if (fn == "DSUM") {
                double s = 0.0;
                for (double v : vals) s += v;
                return Variant::fromDouble(s);
            }
            if (fn == "DAVERAGE") {
                if (vals.empty()) throw BasicRuntimeError(11, "Calc.DAVERAGE division by zero (no matching records).");
                double s = 0.0;
                for (double v : vals) s += v;
                return Variant::fromDouble(s / static_cast<double>(vals.size()));
            }
            if (fn == "DMAX") {
                if (vals.empty()) return Variant::fromInt(0);
                return Variant::fromDouble(*std::max_element(vals.begin(), vals.end()));
            }
            if (fn == "DMIN") {
                if (vals.empty()) return Variant::fromInt(0);
                return Variant::fromDouble(*std::min_element(vals.begin(), vals.end()));
            }
            if (fn == "DPRODUCT") {
                if (vals.empty()) return Variant::fromInt(0);
                double p = 1.0;
                for (double v : vals) p *= v;
                return Variant::fromDouble(p);
            }
            if (fn == "DVAR" || fn == "DVARP" || fn == "DSTDEV" || fn == "DSTDEVP") {
                bool isSample = (fn == "DVAR" || fn == "DSTDEV");
                if (vals.size() < (isSample ? 2 : 1)) {
                    throw BasicRuntimeError(11, "Calc." + fn + ": insufficient matching records.");
                }
                double mean = 0.0;
                for (double v : vals) mean += v;
                mean /= static_cast<double>(vals.size());
                double sq = 0.0;
                for (double v : vals) sq += (v - mean) * (v - mean);
                double var = sq / static_cast<double>(vals.size() - (isSample ? 1 : 0));
                if (fn == "DVAR" || fn == "DVARP") return Variant::fromDouble(var);
                return Variant::fromDouble(std::sqrt(var));
            }
        }

        // ====================================================================
        // 6. STATISTICAL & AGGREGATION FUNCTIONS
        // ====================================================================
        if (fn == "AVERAGE" || fn == "AVERAGEA") {
            std::vector<double> nums = extractNumericList(args, fn == "AVERAGEA");
            if (nums.empty()) throw BasicRuntimeError(11, "Calc." + fn + " division by zero (no numeric values).");
            double s = 0.0;
            for (double v : nums) s += v;
            return Variant::fromDouble(s / static_cast<double>(nums.size()));
        }
        if (fn == "COUNT") {
            std::vector<double> nums = extractNumericList(args);
            return Variant::fromInt(static_cast<int64_t>(nums.size()));
        }
        if (fn == "COUNTA") {
            std::vector<Variant> flat;
            flattenVariants(args, flat);
            int64_t cnt = 0;
            for (const auto &v : flat) {
                if (v.type != VarType::EMPTY && !(v.type == VarType::STRING && v.sVal.empty())) ++cnt;
            }
            return Variant::fromInt(cnt);
        }
        if (fn == "COUNTBLANK") {
            requireArgs(1, 1);
            std::vector<Variant> flat;
            flattenVariants(args, flat);
            int64_t cnt = 0;
            for (const auto &v : flat) {
                if (v.type == VarType::EMPTY || (v.type == VarType::STRING && v.sVal.empty())) ++cnt;
            }
            return Variant::fromInt(cnt);
        }
        if (fn == "MAX" || fn == "MAXA") {
            std::vector<double> nums = extractNumericList(args, fn == "MAXA");
            if (nums.empty()) return Variant::fromInt(0);
            return Variant::fromDouble(*std::max_element(nums.begin(), nums.end()));
        }
        if (fn == "MIN" || fn == "MINA") {
            std::vector<double> nums = extractNumericList(args, fn == "MINA");
            if (nums.empty()) return Variant::fromInt(0);
            return Variant::fromDouble(*std::min_element(nums.begin(), nums.end()));
        }
        if (fn == "MEDIAN") {
            std::vector<double> nums = extractNumericList(args);
            if (nums.empty()) throw BasicRuntimeError(5, "Calc.MEDIAN: no numeric values.");
            std::sort(nums.begin(), nums.end());
            size_t n = nums.size();
            if (n % 2 == 1) return Variant::fromDouble(nums[n / 2]);
            return Variant::fromDouble(0.5 * (nums[n / 2 - 1] + nums[n / 2]));
        }
        if (fn == "MODE") {
            std::vector<double> nums = extractNumericList(args);
            if (nums.empty()) throw BasicRuntimeError(5, "Calc.MODE: no numeric values.");
            std::map<double, int> freq;
            int bestCount = 0;
            double bestVal = nums[0];
            for (double v : nums) {
                int c = ++freq[v];
                if (c > bestCount) { bestCount = c; bestVal = v; }
            }
            if (bestCount <= 1) return Variant::fromError(42, "#N/A");
            return Variant::fromDouble(bestVal);
        }
        if (fn == "LARGE" || fn == "SMALL") {
            requireArgs(2, 2);
            std::vector<double> nums = extractNumericList({args[0]});
            int k = static_cast<int>(args[1].toInt());
            if (k < 1 || static_cast<size_t>(k) > nums.size()) {
                throw BasicRuntimeError(5, "Calc." + fn + ": rank k is out of bounds.");
            }
            if (fn == "LARGE") std::sort(nums.begin(), nums.end(), std::greater<double>());
            else std::sort(nums.begin(), nums.end());
            return Variant::fromDouble(nums[static_cast<size_t>(k - 1)]);
        }
        if (fn == "PERCENTILE" || fn == "QUARTILE") {
            requireArgs(2, 2);
            std::vector<double> nums = extractNumericList({args[0]});
            if (nums.empty()) throw BasicRuntimeError(5, "Calc." + fn + ": empty array.");
            double p = args[1].toDouble();
            if (fn == "QUARTILE") {
                int q = static_cast<int>(args[1].toInt());
                if (q < 0 || q > 4) throw BasicRuntimeError(5, "Calc.QUARTILE: type must be 0..4.");
                p = 0.25 * q;
            }
            if (p < 0.0 || p > 1.0) throw BasicRuntimeError(5, "Calc.PERCENTILE: k must be in [0, 1].");
            std::sort(nums.begin(), nums.end());
            double pos = p * static_cast<double>(nums.size() - 1);
            size_t lo = static_cast<size_t>(std::floor(pos));
            size_t hi = static_cast<size_t>(std::ceil(pos));
            double frac = pos - static_cast<double>(lo);
            return Variant::fromDouble(nums[lo] + frac * (nums[hi] - nums[lo]));
        }
        if (fn == "RANK") {
            requireArgs(2, 3);
            double target = args[0].toDouble();
            std::vector<double> nums = extractNumericList({args[1]});
            bool ascending = (args.size() == 3 && args[2].toInt() != 0);
            if (ascending) std::sort(nums.begin(), nums.end());
            else std::sort(nums.begin(), nums.end(), std::greater<double>());
            for (size_t i = 0; i < nums.size(); ++i) {
                if (std::fabs(nums[i] - target) <= 1e-12) return Variant::fromInt(static_cast<int64_t>(i + 1));
            }
            return Variant::fromError(42, "#N/A");
        }
        if (fn == "VAR" || fn == "VAR.S" || fn == "VARP" || fn == "VAR.P" ||
            fn == "STDEV" || fn == "STDEV.S" || fn == "STDEVP" || fn == "STDEV.P" ||
            fn == "AVEDEV" || fn == "DEVSQ") {
            std::vector<double> nums = extractNumericList(args);
            bool isSample = (fn == "VAR" || fn == "VAR.S" || fn == "STDEV" || fn == "STDEV.S");
            if (nums.size() < (isSample ? 2 : 1)) {
                throw BasicRuntimeError(11, "Calc." + fn + ": insufficient data points.");
            }
            double mean = 0.0;
            for (double v : nums) mean += v;
            mean /= static_cast<double>(nums.size());
            if (fn == "AVEDEV") {
                double ad = 0.0;
                for (double v : nums) ad += std::fabs(v - mean);
                return Variant::fromDouble(ad / static_cast<double>(nums.size()));
            }
            double sq = 0.0;
            for (double v : nums) sq += (v - mean) * (v - mean);
            if (fn == "DEVSQ") return Variant::fromDouble(sq);
            double var = sq / static_cast<double>(nums.size() - (isSample ? 1 : 0));
            if (fn == "VAR" || fn == "VAR.S" || fn == "VARP" || fn == "VAR.P") return Variant::fromDouble(var);
            return Variant::fromDouble(std::sqrt(var));
        }
        if (fn == "GEOMEAN" || fn == "HARMEAN") {
            std::vector<double> nums = extractNumericList(args);
            if (nums.empty()) throw BasicRuntimeError(5, "Calc." + fn + ": no numeric values.");
            if (fn == "GEOMEAN") {
                double logSum = 0.0;
                for (double v : nums) {
                    if (v <= 0.0) throw BasicRuntimeError(5, "Calc.GEOMEAN requires positive numbers.");
                    logSum += std::log(v);
                }
                return Variant::fromDouble(std::exp(logSum / static_cast<double>(nums.size())));
            } else {
                double invSum = 0.0;
                for (double v : nums) {
                    if (v <= 0.0) throw BasicRuntimeError(5, "Calc.HARMEAN requires positive numbers.");
                    invSum += 1.0 / v;
                }
                return Variant::fromDouble(static_cast<double>(nums.size()) / invSum);
            }
        }
        if (fn == "CORREL" || fn == "PEARSON" || fn == "COVAR" || fn == "SLOPE" ||
            fn == "INTERCEPT" || fn == "RSQ" || fn == "FORECAST") {
            size_t offset = (fn == "FORECAST") ? 1 : 0;
            requireArgs(2 + offset, 2 + offset);
            std::vector<double> y = extractNumericList({args[offset]});
            std::vector<double> x = extractNumericList({args[offset + 1]});
            if (x.size() != y.size() || x.empty()) {
                throw BasicRuntimeError(5, "Calc." + fn + ": X and Y arrays must be non-empty and equal length.");
            }
            double n = static_cast<double>(x.size());
            double mx = 0.0, my = 0.0;
            for (size_t i = 0; i < x.size(); ++i) { mx += x[i]; my += y[i]; }
            mx /= n; my /= n;
            double sxx = 0.0, syy = 0.0, sxy = 0.0;
            for (size_t i = 0; i < x.size(); ++i) {
                double dx = x[i] - mx, dy = y[i] - my;
                sxx += dx * dx;
                syy += dy * dy;
                sxy += dx * dy;
            }
            if (fn == "COVAR") return Variant::fromDouble(sxy / n);
            if (std::fabs(sxx) < 1e-18) throw BasicRuntimeError(11, "Calc." + fn + " division by zero (variance is 0).");
            double slope = sxy / sxx;
            double intercept = my - slope * mx;
            if (fn == "SLOPE") return Variant::fromDouble(slope);
            if (fn == "INTERCEPT") return Variant::fromDouble(intercept);
            if (fn == "FORECAST") return Variant::fromDouble(intercept + slope * args[0].toDouble());
            if (std::fabs(syy) < 1e-18) throw BasicRuntimeError(11, "Calc." + fn + " division by zero.");
            double r = sxy / std::sqrt(sxx * syy);
            if (fn == "RSQ") return Variant::fromDouble(r * r);
            return Variant::fromDouble(r);
        }

        // ====================================================================
        // 7. TEXT FUNCTIONS
        // ====================================================================
        if (fn == "CONCAT" || fn == "CONCATENATE") {
            std::vector<Variant> flat;
            flattenVariants(args, flat);
            std::string out;
            for (const auto &v : flat) out += v.toString();
            return Variant::fromString(out);
        }
        if (fn == "TEXTJOIN") {
            requireArgs(3);
            std::string delim = args[0].toString();
            bool ignoreEmpty = args[1].toBool();
            std::vector<Variant> rest(args.begin() + 2, args.end());
            std::vector<Variant> flat;
            flattenVariants(rest, flat);
            std::string out;
            bool first = true;
            for (const auto &v : flat) {
                std::string s = v.toString();
                if (ignoreEmpty && s.empty()) continue;
                if (!first) out += delim;
                first = false;
                out += s;
            }
            return Variant::fromString(out);
        }
        if (fn == "EXACT") {
            requireArgs(2, 2);
            return Variant::fromInt(args[0].toString() == args[1].toString() ? -1 : 0);
        }
        if (fn == "FIND" || fn == "SEARCH") {
            requireArgs(2, 3);
            std::string needle = args[0].toString();
            std::string hay = args[1].toString();
            int64_t start1 = (args.size() == 3) ? args[2].toInt() : 1;
            if (start1 < 1 || static_cast<size_t>(start1) > hay.size() + 1) {
                return Variant::fromError(502, "#VALUE!");
            }
            if (fn == "SEARCH") {
                needle = toUpper(needle);
                hay = toUpper(hay);
            }
            size_t pos = hay.find(needle, static_cast<size_t>(start1 - 1));
            if (pos == std::string::npos) return Variant::fromError(502, "#VALUE!");
            return Variant::fromInt(static_cast<int64_t>(pos + 1));
        }
        if (fn == "SUBSTITUTE") {
            requireArgs(3, 4);
            std::string text = args[0].toString();
            std::string oldStr = args[1].toString();
            std::string newStr = args[2].toString();
            if (oldStr.empty()) return Variant::fromString(text);
            int targetInstance = (args.size() == 4) ? static_cast<int>(args[3].toInt()) : 0;
            if (args.size() == 4 && targetInstance < 1) throw BasicRuntimeError(5, "Calc.SUBSTITUTE instance must be >= 1.");
            std::string out;
            size_t pos = 0;
            int occ = 0;
            while (true) {
                size_t next = text.find(oldStr, pos);
                if (next == std::string::npos) {
                    out += text.substr(pos);
                    break;
                }
                ++occ;
                out += text.substr(pos, next - pos);
                if (targetInstance == 0 || occ == targetInstance) out += newStr;
                else out += oldStr;
                pos = next + oldStr.size();
            }
            return Variant::fromString(out);
        }
        if (fn == "REPLACE") {
            requireArgs(4, 4);
            std::string text = args[0].toString();
            int64_t start1 = args[1].toInt();
            int64_t count = args[2].toInt();
            std::string repl = args[3].toString();
            if (start1 < 1 || count < 0) throw BasicRuntimeError(5, "Calc.REPLACE invalid start or length.");
            size_t idx = std::min<size_t>(text.size(), static_cast<size_t>(start1 - 1));
            size_t rem = std::min<size_t>(text.size() - idx, static_cast<size_t>(count));
            text.replace(idx, rem, repl);
            return Variant::fromString(text);
        }
        if (fn == "PROPER") {
            requireArgs(1, 1);
            std::string s = args[0].toString();
            bool capNext = true;
            for (char &c : s) {
                if (std::isalpha(static_cast<unsigned char>(c))) {
                    c = static_cast<char>(capNext ? std::toupper(static_cast<unsigned char>(c))
                                                  : std::tolower(static_cast<unsigned char>(c)));
                    capNext = false;
                } else {
                    capNext = true;
                }
            }
            return Variant::fromString(s);
        }
        if (fn == "CLEAN") {
            requireArgs(1, 1);
            std::string s = args[0].toString(), out;
            for (unsigned char c : s) {
                if (c >= 32) out.push_back(static_cast<char>(c));
            }
            return Variant::fromString(out);
        }
        if (fn == "REPT") {
            requireArgs(2, 2);
            std::string s = args[0].toString();
            int64_t n = std::max<int64_t>(0, args[1].toInt());
            std::string out;
            out.reserve(s.size() * static_cast<size_t>(n));
            for (int64_t i = 0; i < n; ++i) out += s;
            return Variant::fromString(out);
        }
        if (fn == "CHAR") {
            requireArgs(1, 1);
            return Variant::fromString(std::string(1, static_cast<char>(args[0].toInt() & 0xFF)));
        }
        if (fn == "CODE") {
            requireArgs(1, 1);
            std::string s = args[0].toString();
            return Variant::fromInt(s.empty() ? 0 : static_cast<unsigned char>(s[0]));
        }
        if (fn == "VALUE") {
            requireArgs(1, 1);
            return Variant::fromDouble(args[0].toDouble());
        }
        if (fn == "TEXT") {
            requireArgs(2, 2);
            return Variant::fromString(NumberFormatter::formatCustomOrNamed(args[0], args[1].toString()));
        }
        if (fn == "LEFT") {
            requireArgs(1, 2);
            std::string s = args[0].toString();
            size_t n = (args.size() == 2) ? static_cast<size_t>(std::max<int64_t>(0, args[1].toInt())) : 1;
            return Variant::fromString(s.substr(0, std::min(s.size(), n)));
        }
        if (fn == "RIGHT") {
            requireArgs(1, 2);
            std::string s = args[0].toString();
            size_t n = (args.size() == 2) ? static_cast<size_t>(std::max<int64_t>(0, args[1].toInt())) : 1;
            n = std::min(s.size(), n);
            return Variant::fromString(s.substr(s.size() - n));
        }
        if (fn == "MID") {
            requireArgs(3, 3);
            std::string s = args[0].toString();
            size_t st = static_cast<size_t>(std::max<int64_t>(1, args[1].toInt()) - 1);
            size_t ln = static_cast<size_t>(std::max<int64_t>(0, args[2].toInt()));
            if (st >= s.size()) return Variant::fromString("");
            return Variant::fromString(s.substr(st, ln));
        }
        if (fn == "LEN") {
            requireArgs(1, 1);
            return Variant::fromInt(static_cast<int64_t>(args[0].toString().size()));
        }
        if (fn == "TRIM") {
            requireArgs(1, 1);
            std::string s = trim(args[0].toString()), out;
            bool prevSpace = false;
            for (char c : s) {
                if (c == ' ' || c == '\t') {
                    if (!prevSpace) out.push_back(' ');
                    prevSpace = true;
                } else {
                    out.push_back(c);
                    prevSpace = false;
                }
            }
            return Variant::fromString(out);
        }
        if (fn == "UPPER") { requireArgs(1, 1); return Variant::fromString(toUpper(args[0].toString())); }
        if (fn == "LOWER") {
            requireArgs(1, 1);
            std::string s = args[0].toString();
            for (char &c : s) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
            return Variant::fromString(s);
        }

        // ====================================================================
        // 8. LOGICAL & INFORMATION FUNCTIONS
        // ====================================================================
        if (fn == "IF") {
            requireArgs(2, 3);
            if (args[0].toBool()) return args[1];
            return (args.size() == 3) ? args[2] : Variant::fromInt(0);
        }
        if (fn == "IFS") {
            if (args.size() < 2 || args.size() % 2 != 0) {
                throw BasicRuntimeError(5, "Calc.IFS requires pairs of (condition, value).");
            }
            for (size_t i = 0; i < args.size(); i += 2) {
                if (args[i].toBool()) return args[i + 1];
            }
            return Variant::fromError(42, "#N/A");
        }
        if (fn == "SWITCH") {
            requireArgs(3);
            const Variant &target = args[0];
            size_t i = 1;
            for (; i + 1 < args.size(); i += 2) {
                if (variantEquals(target, args[i])) return args[i + 1];
            }
            if (i < args.size()) return args[i];
            return Variant::fromError(42, "#N/A");
        }
        if (fn == "AND" || fn == "OR" || fn == "XOR") {
            requireArgs(1);
            std::vector<Variant> flat;
            flattenVariants(args, flat);
            if (fn == "AND") {
                for (const auto &v : flat) if (!v.toBool()) return Variant::fromInt(0);
                return Variant::fromInt(-1);
            }
            if (fn == "OR") {
                for (const auto &v : flat) if (v.toBool()) return Variant::fromInt(-1);
                return Variant::fromInt(0);
            }
            int trues = 0;
            for (const auto &v : flat) if (v.toBool()) ++trues;
            return Variant::fromInt((trues % 2 == 1) ? -1 : 0);
        }
        if (fn == "NOT") {
            requireArgs(1, 1);
            return Variant::fromInt(args[0].toBool() ? 0 : -1);
        }
        if (fn == "IFERROR") {
            requireArgs(2, 2);
            return (args[0].type == VarType::ERRVAL) ? args[1] : args[0];
        }
        if (fn == "IFNA") {
            requireArgs(2, 2);
            return (args[0].type == VarType::ERRVAL && (args[0].iVal == 42 || args[0].sVal == "#N/A")) ? args[1] : args[0];
        }
        if (fn == "ISERROR") {
            requireArgs(1, 1);
            return Variant::fromInt(args[0].type == VarType::ERRVAL ? -1 : 0);
        }
        if (fn == "ISNA") {
            requireArgs(1, 1);
            return Variant::fromInt((args[0].type == VarType::ERRVAL && (args[0].iVal == 42 || args[0].sVal == "#N/A")) ? -1 : 0);
        }
        if (fn == "ISNUMBER") {
            requireArgs(1, 1);
            return Variant::fromInt(isNumericVariant(args[0]) ? -1 : 0);
        }
        if (fn == "ISTEXT") {
            requireArgs(1, 1);
            return Variant::fromInt(args[0].type == VarType::STRING ? -1 : 0);
        }
        if (fn == "ISBLANK") {
            requireArgs(1, 1);
            return Variant::fromInt(args[0].type == VarType::EMPTY ? -1 : 0);
        }
        if (fn == "ISEVEN") {
            requireArgs(1, 1);
            int64_t n = static_cast<int64_t>(std::trunc(args[0].toDouble()));
            return Variant::fromInt((n % 2 == 0) ? -1 : 0);
        }
        if (fn == "ISODD") {
            requireArgs(1, 1);
            int64_t n = static_cast<int64_t>(std::trunc(args[0].toDouble()));
            return Variant::fromInt((n % 2 != 0) ? -1 : 0);
        }

        throw BasicRuntimeError(35, "Unimplemented Calc function '" + fn + "'.");
    }
}

// ============================================================================
// LEXER & TOKENIZER
// ============================================================================
enum class TokType {
    END, NUMBER, STRING, IDENT,
    PLUS, MINUS, STAR, SLASH, BSLASH, CARET, AMP,
    EQ, NEQ, LT, GT, LE, GE,
    LPAREN, RPAREN, COMMA, SEMICOLON, COLON, HASH
};

struct Token {
    TokType type = TokType::END;
    std::string text;
    double numVal = 0.0;
    bool isFloat = false;
};

static std::vector<Token> tokenizeLine(const std::string &line, int lineNo = 0) {
    std::vector<Token> tokens;
    size_t i = 0, n = line.size();
    while (i < n) {
        char c = line[i];
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') { ++i; continue; }
        if (c == '\'') break;

        if (std::isdigit(static_cast<unsigned char>(c)) || (c == '.' && i + 1 < n && std::isdigit(static_cast<unsigned char>(line[i + 1])))) {
            size_t start = i;
            bool isFlt = false;
            while (i < n && (std::isdigit(static_cast<unsigned char>(line[i])) || line[i] == '.')) {
                if (line[i] == '.') isFlt = true;
                ++i;
            }
            if (i < n && (line[i] == 'e' || line[i] == 'E')) {
                isFlt = true; ++i;
                if (i < n && (line[i] == '+' || line[i] == '-')) ++i;
                while (i < n && std::isdigit(static_cast<unsigned char>(line[i]))) ++i;
            }
            std::string numStr = line.substr(start, i - start);
            tokens.push_back({TokType::NUMBER, numStr, std::stod(numStr), isFlt});
            continue;
        }

        if (c == '"') {
            ++i;
            std::string s;
            bool closed = false;
            while (i < n) {
                if (line[i] == '"') {
                    if (i + 1 < n && line[i + 1] == '"') { s += '"'; i += 2; continue; }
                    ++i;
                    closed = true;
                    break;
                }
                s += line[i++];
            }
            if (!closed) {
                throw BasicRuntimeError(2, "Unterminated string literal (missing closing '\"').",
                                        lineNo, line, "\"" + s, "\"" + s + "\"");
            }
            tokens.push_back({TokType::STRING, s, 0.0, false});
            continue;
        }

        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            size_t start = i;
            while (i < n && (std::isalnum(static_cast<unsigned char>(line[i])) ||
                             line[i] == '_' || line[i] == '$' || line[i] == '%' ||
                             (line[i] == '.' && i + 1 < n && std::isalpha(static_cast<unsigned char>(line[i + 1]))))) {
                ++i;
            }
            std::string id = line.substr(start, i - start);
            if (toUpper(id) == "REM") break;
            tokens.push_back({TokType::IDENT, id, 0.0, false});
            continue;
        }

        if (i + 1 < n) {
            std::string two = line.substr(i, 2);
            if (two == "<>") { tokens.push_back({TokType::NEQ, two}); i += 2; continue; }
            if (two == "<=") { tokens.push_back({TokType::LE, two}); i += 2; continue; }
            if (two == ">=") { tokens.push_back({TokType::GE, two}); i += 2; continue; }
        }

        TokType tt = TokType::END;
        switch (c) {
            case '+': tt = TokType::PLUS; break;
            case '-': tt = TokType::MINUS; break;
            case '*': tt = TokType::STAR; break;
            case '/': tt = TokType::SLASH; break;
            case '\\': tt = TokType::BSLASH; break;
            case '^': tt = TokType::CARET; break;
            case '&': tt = TokType::AMP; break;
            case '=': tt = TokType::EQ; break;
            case '<': tt = TokType::LT; break;
            case '>': tt = TokType::GT; break;
            case '(': tt = TokType::LPAREN; break;
            case ')': tt = TokType::RPAREN; break;
            case ',': tt = TokType::COMMA; break;
            case ';': tt = TokType::SEMICOLON; break;
            case ':': tt = TokType::COLON; break;
            case '#': tt = TokType::HASH; break;
            default:
                throw BasicRuntimeError(2, std::string("Unexpected character '") + c + "' in statement.",
                                        lineNo, line, std::string(1, c));
        }
        tokens.push_back({tt, std::string(1, c)});
        ++i;
    }
    tokens.push_back({TokType::END, ""});
    return tokens;
}

static bool isReservedNonLabelKeyword(const std::string &u) {
    static const std::unordered_set<std::string> kwSet = {
        "IF", "THEN", "ELSE", "ELSEIF", "ENDIF", "FOR", "NEXT", "DO", "LOOP",
        "WHILE", "WEND", "SELECT", "CASE", "SUB", "FUNCTION", "EXIT", "END",
        "ON", "RESUME", "GOTO", "GOSUB", "RETURN", "PRINT", "WRITE", "DIM",
        "REDIM", "ERASE", "OPTION", "OPEN", "CLOSE", "CALL", "REM", "LINE", "LSET", "RSET", "WAIT",
        "KILL", "NAME", "FILECOPY", "MKDIR", "RMDIR", "CHDIR", "CHDRIVE", "SETATTR",
        "SEEK", "INPUT", "PUT", "GET", "LOCK", "UNLOCK"
    };
    return kwSet.count(u) > 0;
}

// ============================================================================
// INTERPRETER CORE
// ============================================================================
struct ParamDecl {
    std::string name;
    VarType type = VarType::EMPTY;
    bool byVal = false;
};

struct RoutineDecl {
    std::string name;
    bool isFunction = false;
    VarType returnType = VarType::EMPTY;
    std::vector<ParamDecl> params;
    int startLine = 0;
    int endLine = 0;
};

struct FileEntry {
    std::fstream stream;
    std::string mode;
    int colPos = 0;
    int recLen = 128;
    bool isLocked = false;
};

class StarBasicInterpreter {
private:
    std::vector<std::string> sourceLines;
    std::vector<int> sourceLineNumbers;
    std::vector<std::vector<Token>> tokenizedLines;
    std::unordered_map<std::string, RoutineDecl> routines;
    std::vector<std::unordered_map<std::string, Variant>> callStack;
    std::unordered_map<std::string, Variant> globals;
    std::map<int, FileEntry> openFiles;
    FileSystemUtil::DirState dirState;
    int optionBase = 0;
    bool optionExplicit = false;

    int currentErrCode = 0;
    int currentErrLine = 0;
    std::string currentErrMsg;

    std::unordered_map<std::string, std::string> varFormulas;
    std::vector<std::string> formulaOrder;

    void recordVarFormula(const std::string &rawVarName, const std::vector<Token> &toks, size_t startIdx) {
        if (!callStack.empty()) {
            std::string uName = toUpper(rawVarName);
            if (routines.count(uName) && routines[uName].isFunction) return;
        }
        std::string u = toUpper(rawVarName);
        if (!u.empty() && (u.back() == '$' || u.back() == '%')) u.pop_back();

        std::ostringstream oss;
        bool hasIdent = false;
        for (size_t i = startIdx; i < toks.size() && toks[i].type != TokType::END; ++i) {
            if (i > startIdx) oss << " ";
            if (toks[i].type == TokType::STRING) oss << "\"" << toks[i].text << "\"";
            else oss << toks[i].text;
            if (toks[i].type == TokType::IDENT) hasIdent = true;
        }
        std::string rhs = trim(oss.str());
        if (hasIdent && !rhs.empty()) {
            varFormulas[u] = rhs;
            if (std::find(formulaOrder.begin(), formulaOrder.end(), u) == formulaOrder.end()) {
                formulaOrder.push_back(u);
            }
        }
    }

    LinAlg::SolverCallbacks makeSolverCallbacks() {
        LinAlg::SolverCallbacks cb;

        cb.varExists = [this](const std::string &name) -> bool {
            size_t lp = name.find('(');
            if (lp != std::string::npos) return this->varExists(trim(name.substr(0, lp)));
            return this->varExists(name);
        };

        cb.funcExists = [this](const std::string &name) -> bool {
            return routines.count(toUpper(trim(name))) > 0;
        };

        cb.getVar = [this](const std::string &name) -> Variant {
            size_t lp = name.find('('), rp = name.rfind(')');
            if (lp != std::string::npos && rp != std::string::npos && rp > lp) {
                std::string base = trim(name.substr(0, lp));
                int idx = std::stoi(trim(name.substr(lp + 1, rp - lp - 1)));
                return resolveVar(base).at({idx});
            }
            return resolveVar(name);
        };

        cb.setVar = [this](const std::string &name, double val) {
            size_t lp = name.find('('), rp = name.rfind(')');
            if (lp != std::string::npos && rp != std::string::npos && rp > lp) {
                std::string base = trim(name.substr(0, lp));
                int idx = std::stoi(trim(name.substr(lp + 1, rp - lp - 1)));
                Variant &slot = resolveVar(base);
                slot.at({idx}).assignCoerced(Variant::fromDouble(val));
                if (!callStack.empty()) {
                    std::string uBase = toUpper(base);
                    if (!uBase.empty() && (uBase.back() == '$' || uBase.back() == '%')) uBase.pop_back();
                    globals[uBase] = slot;
                }
                return;
            }
            Variant &slot = resolveVar(name);
            slot.assignCoerced(Variant::fromDouble(val));
            if (!callStack.empty()) {
                std::string u = toUpper(trim(name));
                if (!u.empty() && (u.back() == '$' || u.back() == '%')) u.pop_back();
                globals[u] = slot;
            }
        };

        cb.getSymbolFormula = [this](const std::string &name) -> std::string {
            std::string u = toUpper(trim(name));
            if (!u.empty() && (u.back() == '$' || u.back() == '%')) u.pop_back();

            auto rit = routines.find(u);
            if (rit != routines.end() && rit->second.isFunction) {
                std::string singleFormula;
                int stmtCount = 0;
                for (int ln = rit->second.startLine + 1; ln < rit->second.endLine; ++ln) {
                    std::string lineStr = trim(sourceLines[ln]);
                    if (lineStr.empty()) continue;
                    ++stmtCount;
                    size_t eq = lineStr.find('=');
                    if (eq != std::string::npos && toUpper(trim(lineStr.substr(0, eq))) == u) {
                        singleFormula = trim(lineStr.substr(eq + 1));
                    }
                }
                if (stmtCount == 1 && !singleFormula.empty()) return singleFormula;
                return "";
            }

            auto fit = varFormulas.find(u);
            if (fit != varFormulas.end()) return fit->second;
            return "";
        };

        cb.evalExpr = [this](const std::string &expr, const std::vector<std::string> &changingVars) -> double {
            std::unordered_set<std::string> depVars;
            for (const auto &cv : changingVars) {
                size_t lp = cv.find('(');
                depVars.insert(toUpper(trim(lp != std::string::npos ? cv.substr(0, lp) : cv)));
            }
            for (const auto &fVar : formulaOrder) {
                if (depVars.count(fVar) || !varFormulas.count(fVar)) continue;
                const std::string &fStr = varFormulas[fVar];
                if (fStr.find('<') != std::string::npos || fStr.find('>') != std::string::npos ||
                    fStr.find('=') != std::string::npos) continue;
                std::vector<Token> fToks = tokenizeLine(fStr, 0);
                bool depends = false;
                for (const auto &tk : fToks) {
                    if (tk.type == TokType::IDENT && depVars.count(toUpper(tk.text))) {
                        depends = true;
                        break;
                    }
                }
                if (depends) {
                    depVars.insert(fVar);
                    ExprCursor fc{fToks, 0};
                    Variant v = evalExpr(fc);
                    resolveVar(fVar).assignCoerced(v);
                    if (!callStack.empty()) globals[fVar] = resolveVar(fVar);
                }
            }

            std::string t = trim(expr);
            std::string u = toUpper(t);
            auto rit = routines.find(u);
            if (rit != routines.end() && rit->second.isFunction && !rit->second.params.empty() &&
                rit->second.params.size() == changingVars.size()) {
                std::ostringstream callOss;
                callOss << t << "(";
                for (size_t i = 0; i < changingVars.size(); ++i) {
                    if (i > 0) callOss << ", ";
                    callOss << changingVars[i];
                }
                callOss << ")";
                t = callOss.str();
            }
            std::vector<Token> toks = tokenizeLine(t, 0);
            ExprCursor c{toks, 0};
            return evalExpr(c).toDouble();
        };

        cb.setSolverStats = [this](const std::string &status, double objVal, int iters, double elapsedSec) {
            globals["SOLVER.STATUS"] = Variant::fromString(status);
            globals["SOLVER.VALUE"] = Variant::fromDouble(objVal);
            globals["SOLVER.ITERATIONS"] = Variant::fromInt(iters);
            globals["SOLVER.TIME"] = Variant::fromDouble(elapsedSec);
        };

        return cb;
    }

    std::vector<std::string> collectUserSymbols() const {
        std::vector<std::string> syms;
        for (const auto &kv : routines) syms.push_back(kv.first);
        for (const auto &kv : globals) syms.push_back(kv.first);
        if (!callStack.empty()) {
            for (const auto &kv : callStack.back()) syms.push_back(kv.first);
        }
        return syms;
    }

    static void copyPreservedElements(const Variant &oldArr, Variant &newArr,
                                      size_t dimIdx, std::vector<int> &indices) {
        if (dimIdx == newArr.dims.size()) {
            newArr.at(indices).assignCoerced(oldArr.at(indices));
            return;
        }
        int overlapLow = std::max(oldArr.dims[dimIdx].low, newArr.dims[dimIdx].low);
        int overlapHigh = std::min(oldArr.dims[dimIdx].high, newArr.dims[dimIdx].high);
        for (int idx = overlapLow; idx <= overlapHigh; ++idx) {
            indices[dimIdx] = idx;
            copyPreservedElements(oldArr, newArr, dimIdx + 1, indices);
        }
    }

    static bool isSingleLineIfStatement(const std::string &segment, int lineNo) {
        std::vector<Token> toks = tokenizeLine(segment, lineNo);
        if (toks.empty() || toks[0].type != TokType::IDENT || toUpper(toks[0].text) != "IF") return false;
        for (size_t i = 1; i < toks.size(); ++i) {
            if (toks[i].type == TokType::IDENT && toUpper(toks[i].text) == "THEN") {
                return (i + 1 < toks.size() && toks[i + 1].type != TokType::END);
            }
        }
        return false;
    }

    void addLogicalSegment(const std::string &seg, int rawLineNo) {
        std::string t = trim(seg);
        if (t.empty()) return;
        std::vector<Token> toks = tokenizeLine(t, rawLineNo);
        if (toks.empty() || toks[0].type == TokType::END) return;

        if (toks.size() >= 3 && toks[0].type == TokType::IDENT &&
            toks[1].type == TokType::COLON && !isReservedNonLabelKeyword(toUpper(toks[0].text))) {
            std::string labelDef = toks[0].text + ":";
            sourceLines.push_back(labelDef);
            sourceLineNumbers.push_back(rawLineNo);
            tokenizedLines.push_back(tokenizeLine(labelDef, rawLineNo));

            size_t colonPos = t.find(':');
            if (colonPos != std::string::npos && colonPos + 1 < t.size()) {
                addLogicalSegment(t.substr(colonPos + 1), rawLineNo);
            }
            return;
        }

        if (toks[0].type == TokType::NUMBER && !toks[0].isFloat && toks.size() >= 3) {
            std::string labelDef = toks[0].text + ":";
            sourceLines.push_back(labelDef);
            sourceLineNumbers.push_back(rawLineNo);
            tokenizedLines.push_back(tokenizeLine(labelDef, rawLineNo));

            size_t afterNum = t.find(toks[0].text) + toks[0].text.size();
            while (afterNum < t.size() && (t[afterNum] == ' ' || t[afterNum] == '\t')) ++afterNum;
            if (afterNum < t.size() && t[afterNum] == ':') ++afterNum;
            if (afterNum < t.size()) {
                addLogicalSegment(t.substr(afterNum), rawLineNo);
            }
            return;
        }

        sourceLines.push_back(t);
        sourceLineNumbers.push_back(rawLineNo);
        tokenizedLines.push_back(std::move(toks));
    }

    void preprocessSource(const std::string &rawCode) {
        sourceLines.clear();
        sourceLineNumbers.clear();
        tokenizedLines.clear();
        routines.clear();

        std::istringstream iss(rawCode);
        std::string rawLine;
        int rawLineNo = 0;

        while (std::getline(iss, rawLine)) {
            ++rawLineNo;
            std::string stripped;
            bool inQuotes = false;
            for (size_t i = 0; i < rawLine.size(); ++i) {
                char c = rawLine[i];
                if (c == '"') inQuotes = !inQuotes;
                if (!inQuotes && c == '\'') break;
                stripped += c;
            }
            stripped = trim(stripped);
            if (stripped.empty()) continue;

            if (isSingleLineIfStatement(stripped, rawLineNo)) {
                addLogicalSegment(stripped, rawLineNo);
                continue;
            }

            std::vector<std::string> parts;
            std::string current;
            inQuotes = false;
            for (size_t i = 0; i < stripped.size(); ++i) {
                char c = stripped[i];
                if (c == '"') inQuotes = !inQuotes;
                if (!inQuotes && c == ':') {
                    std::string cand = trim(current);
                    std::vector<Token> cToks = tokenizeLine(cand, rawLineNo);
                    if (parts.empty() && cToks.size() == 2 &&
                        ((cToks[0].type == TokType::IDENT && !isReservedNonLabelKeyword(toUpper(cToks[0].text))) ||
                         (cToks[0].type == TokType::NUMBER && !cToks[0].isFloat))) {
                        parts.push_back(cand + ":");
                    } else if (!cand.empty()) {
                        parts.push_back(cand);
                    }
                    current.clear();
                } else {
                    current += c;
                }
            }
            if (!trim(current).empty()) parts.push_back(trim(current));
            for (const auto &p : parts) addLogicalSegment(p, rawLineNo);
        }
    }

    static VarType parseTypeKeyword(const std::string &kw) {
        std::string u = toUpper(kw);
        if (u == "INTEGER" || u == "LONG") return VarType::INTEGER;
        if (u == "DOUBLE" || u == "SINGLE") return VarType::DOUBLE;
        if (u == "STRING") return VarType::STRING;
        if (u == "VARIANT" || u == "OBJECT") return VarType::EMPTY;
        return VarType::EMPTY;
    }

    Variant& resolveVar(const std::string &name) {
        std::string u = toUpper(name);
        if (!u.empty() && (u.back() == '$' || u.back() == '%')) u.pop_back();
        if (!callStack.empty()) {
            auto &local = callStack.back();
            auto it = local.find(u);
            if (it != local.end()) return it->second;
        }
        auto git = globals.find(u);
        if (git != globals.end()) return git->second;
        if (optionExplicit) {
            std::string sug = suggestBestMatch(name, collectUserSymbols());
            std::string msg = "Variable '" + name + "' is not defined.";
            if (!sug.empty()) msg += " Did you mean '" + sug + "'?";
            throw BasicRuntimeError(12, msg, 0, "", name, sug);
        }
        if (!callStack.empty()) return callStack.back()[u];
        return globals[u];
    }

    bool varExists(const std::string &name) const {
        std::string u = toUpper(name);
        if (!u.empty() && (u.back() == '$' || u.back() == '%')) u.pop_back();
        if (!callStack.empty() && callStack.back().count(u)) return true;
        return globals.count(u) > 0;
    }

    int showMsgBox(const std::string &prompt, int buttons, const std::string &title) {
        const char *guiEnv = std::getenv("SBASIC_GUI");
        if (guiEnv && std::string(guiEnv) == "1") {
            std::string cmd = "zenity --info --title=\"" + title + "\" --text=\"" + prompt + "\" 2>/dev/null";
            return (std::system(cmd.c_str()) == 0) ? 1 : 2;
        }
        std::string border(std::max<size_t>(44, std::max(title.size(), prompt.size()) + 6), '=');
        std::cout << "\n+" << border << "+\n| [" << title << "]\n| " << prompt << "\n+" << border << "+\n";
        if (buttons == 4) {
            std::cout << "Select [Y/n]: ";
            std::string ans;
            std::getline(std::cin, ans);
            return (!ans.empty() && (ans[0] == 'n' || ans[0] == 'N')) ? 7 : 6;
        }
        return 1;
    }

    std::string showInputBox(const std::string &prompt, const std::string &title, const std::string &defVal) {
        const char *guiEnv = std::getenv("SBASIC_GUI");
        if (guiEnv && std::string(guiEnv) == "1") {
            std::string cmd = "zenity --entry --title=\"" + title + "\" --text=\"" + prompt + "\" --entry-text=\"" + defVal + "\" 2>/dev/null";
            FILE *pipe = popen(cmd.c_str(), "r");
            if (pipe) {
                char buf[512]; std::string res;
                while (fgets(buf, sizeof(buf), pipe)) res += buf;
                pclose(pipe);
                return trim(res);
            }
        }
        std::cout << "\n[" << title << "] " << prompt;
        if (!defVal.empty()) std::cout << " (default: " << defVal << ")";
        std::cout << ": ";
        if (std::cin.peek() == EOF) { std::cout << defVal << "\n"; return defVal; }
        std::string line;
        std::getline(std::cin, line);
        return line.empty() ? defVal : line;
    }

    struct ExprCursor {
        const std::vector<Token> &toks;
        size_t pos = 0;
        const Token& peek() const { return toks[pos]; }
        const Token& get() { return toks[pos++]; }
        bool match(TokType t) { if (peek().type == t) { ++pos; return true; } return false; }
        bool matchIdent(const std::string &kw) {
            if (peek().type == TokType::IDENT && toUpper(peek().text) == kw) { ++pos; return true; }
            return false;
        }
    };

    Variant evalExpr(ExprCursor &c) { return parseOr(c); }

    Variant parseOr(ExprCursor &c) {
        Variant left = parseAnd(c);
        while (c.matchIdent("OR")) {
            Variant right = parseAnd(c);
            left = Variant::fromInt((left.toBool() || right.toBool()) ? -1 : 0);
        }
        return left;
    }

    Variant parseAnd(ExprCursor &c) {
        Variant left = parseNot(c);
        while (c.matchIdent("AND")) {
            Variant right = parseNot(c);
            left = Variant::fromInt((left.toBool() && right.toBool()) ? -1 : 0);
        }
        return left;
    }

    Variant parseNot(ExprCursor &c) {
        if (c.matchIdent("NOT")) return Variant::fromInt(parseNot(c).toBool() ? 0 : -1);
        return parseCompare(c);
    }

    static bool compareVariants(const Variant &left, const Variant &right, TokType op) {
        if (left.type == VarType::STRING && right.type == VarType::STRING) {
            if (op == TokType::EQ) return (left.sVal == right.sVal);
            if (op == TokType::NEQ) return (left.sVal != right.sVal);
            if (op == TokType::LT) return (left.sVal < right.sVal);
            if (op == TokType::GT) return (left.sVal > right.sVal);
            if (op == TokType::LE) return (left.sVal <= right.sVal);
            if (op == TokType::GE) return (left.sVal >= right.sVal);
        } else {
            double a = left.toDouble(), b = right.toDouble();
            if (op == TokType::EQ) return (std::fabs(a - b) <= 1e-12);
            if (op == TokType::NEQ) return (std::fabs(a - b) > 1e-12);
            if (op == TokType::LT) return (a < b);
            if (op == TokType::GT) return (a > b);
            if (op == TokType::LE) return (a <= b);
            if (op == TokType::GE) return (a >= b);
        }
        return false;
    }

    Variant parseCompare(ExprCursor &c) {
        Variant left = parseConcat(c);
        while (true) {
            TokType t = c.peek().type;
            if (t == TokType::EQ || t == TokType::NEQ || t == TokType::LT ||
                t == TokType::GT || t == TokType::LE || t == TokType::GE) {
                c.get();
                Variant right = parseConcat(c);
                left = Variant::fromInt(compareVariants(left, right, t) ? -1 : 0);
            } else break;
        }
        return left;
    }

    Variant parseConcat(ExprCursor &c) {
        Variant left = parseAddSub(c);
        while (c.match(TokType::AMP)) left = Variant::fromString(left.toString() + parseAddSub(c).toString());
        return left;
    }

    Variant parseAddSub(ExprCursor &c) {
        Variant left = parseMod(c);
        while (c.peek().type == TokType::PLUS || c.peek().type == TokType::MINUS) {
            TokType op = c.get().type;
            Variant right = parseMod(c);
            if (op == TokType::PLUS && left.type == VarType::STRING && right.type == VarType::STRING)
                left = Variant::fromString(left.sVal + right.sVal);
            else if (left.type == VarType::INTEGER && right.type == VarType::INTEGER)
                left = Variant::fromInt(op == TokType::PLUS ? left.iVal + right.iVal : left.iVal - right.iVal);
            else
                left = Variant::fromDouble(op == TokType::PLUS ? left.toDouble() + right.toDouble() : left.toDouble() - right.toDouble());
        }
        return left;
    }

    Variant parseMod(ExprCursor &c) {
        Variant left = parseIntDiv(c);
        while (c.matchIdent("MOD")) {
            int64_t b = parseIntDiv(c).toInt();
            if (b == 0) throw BasicRuntimeError(11, "Division by zero");
            left = Variant::fromInt(left.toInt() % b);
        }
        return left;
    }

    Variant parseIntDiv(ExprCursor &c) {
        Variant left = parseMulDiv(c);
        while (c.match(TokType::BSLASH)) {
            int64_t b = parseMulDiv(c).toInt();
            if (b == 0) throw BasicRuntimeError(11, "Division by zero");
            left = Variant::fromInt(left.toInt() / b);
        }
        return left;
    }

    Variant parseMulDiv(ExprCursor &c) {
        Variant left = parseUnary(c);
        while (c.peek().type == TokType::STAR || c.peek().type == TokType::SLASH) {
            TokType op = c.get().type;
            Variant right = parseUnary(c);
            if (op == TokType::STAR) {
                if (left.type == VarType::INTEGER && right.type == VarType::INTEGER)
                    left = Variant::fromInt(left.iVal * right.iVal);
                else
                    left = Variant::fromDouble(left.toDouble() * right.toDouble());
            } else {
                double b = right.toDouble();
                if (std::fabs(b) < 1e-18) throw BasicRuntimeError(11, "Division by zero");
                left = Variant::fromDouble(left.toDouble() / b);
            }
        }
        return left;
    }

    Variant parseUnary(ExprCursor &c) {
        if (c.match(TokType::PLUS)) return parseUnary(c);
        if (c.match(TokType::MINUS)) {
            Variant v = parseUnary(c);
            return (v.type == VarType::INTEGER) ? Variant::fromInt(-v.iVal) : Variant::fromDouble(-v.toDouble());
        }
        return parsePower(c);
    }

    Variant parsePower(ExprCursor &c) {
        Variant base = parsePrimary(c);
        if (c.match(TokType::CARET)) return Variant::fromDouble(std::pow(base.toDouble(), parseUnary(c).toDouble()));
        return base;
    }

    Variant parsePrimary(ExprCursor &c) {
        if (c.match(TokType::LPAREN)) {
            Variant v = evalExpr(c);
            if (!c.match(TokType::RPAREN)) throw BasicRuntimeError(2, "Missing closing parenthesis ')'.");
            return v;
        }
        if (c.peek().type == TokType::NUMBER) {
            Token t = c.get();
            return t.isFloat ? Variant::fromDouble(t.numVal) : Variant::fromInt(static_cast<int64_t>(t.numVal));
        }
        if (c.peek().type == TokType::STRING) return Variant::fromString(c.get().text);
        if (c.peek().type == TokType::IDENT) {
            std::string id = c.get().text;
            std::string u = toUpper(id);
            if (u == "TRUE") return Variant::fromInt(-1);
            if (u == "FALSE") return Variant::fromInt(0);
            if (u == "PI") return Variant::fromDouble(std::acos(-1.0));

            if ((u == "ERR" || u == "ERR.NUMBER") && c.peek().type != TokType::LPAREN) {
                return Variant::fromInt(currentErrCode);
            }
            if (u == "ERL" && c.peek().type != TokType::LPAREN) {
                return Variant::fromInt(currentErrLine);
            }
            if ((u == "ERROR" || u == "ERROR$" || u == "ERR.DESCRIPTION") && c.peek().type != TokType::LPAREN) {
                if (!currentErrMsg.empty()) return Variant::fromString(currentErrMsg);
                return Variant::fromString(getStandardErrorMessage(currentErrCode));
            }
            if (u == "FREEFILE" && c.peek().type != TokType::LPAREN) {
                int ch = 1; while (openFiles.count(ch)) ++ch;
                return Variant::fromInt(ch);
            }
            if (u == "RND" && c.peek().type != TokType::LPAREN) {
                return Variant::fromDouble(static_cast<double>(std::rand()) / RAND_MAX);
            }
            if ((u == "DATE" || u == "DATE$") && c.peek().type != TokType::LPAREN && !varExists(u)) {
                return DateTimeUtil::currentDate().toVariant();
            }
            if ((u == "TIME" || u == "TIME$") && c.peek().type != TokType::LPAREN && !varExists(u)) {
                return DateTimeUtil::currentTime().toVariant();
            }
            if (u == "NOW" && c.peek().type != TokType::LPAREN && !varExists(u)) {
                return DateTimeUtil::currentNow().toVariant();
            }
            if (u == "TIMER" && c.peek().type != TokType::LPAREN && !varExists(u)) {
                return Variant::fromDouble(DateTimeUtil::currentTimer());
            }
            if ((u == "CURDIR" || u == "CURDIR$") && c.peek().type != TokType::LPAREN && !varExists(u)) {
                std::error_code ec;
                return Variant::fromString(std::filesystem::current_path(ec).string());
            }
            if ((u == "DIR" || u == "DIR$") && c.peek().type != TokType::LPAREN && !varExists(u)) {
                return Variant::fromString(dirState.nextMatch());
            }

            if (c.match(TokType::LPAREN)) {
                std::vector<Variant> args;
                std::vector<std::string> refNames;
                if (!c.match(TokType::RPAREN)) {
                    while (true) {
                        c.match(TokType::HASH);
                        size_t startTok = c.pos;
                        Variant val = evalExpr(c);
                        refNames.push_back((c.pos == startTok + 1 && c.toks[startTok].type == TokType::IDENT) ? c.toks[startTok].text : "");
                        args.push_back(val);
                        if (c.match(TokType::COMMA) || c.match(TokType::SEMICOLON)) continue;
                        if (c.match(TokType::RPAREN)) break;
                        throw BasicRuntimeError(2, "Expected ',' or ')' in call to '" + id + "'.", 0, "", id);
                    }
                }
                return invokeFunctionOrArray(u, args, refNames);
            }
            if (LibOffice::isOfficeRoutine(u) || CalcFunctions::isCalcFunction(u)) {
                return invokeFunctionOrArray(u, {}, {});
            }
            if (!varExists(u)) {
                auto rit = routines.find(u);
                if (rit != routines.end() && rit->second.isFunction && rit->second.params.empty()) {
                    return executeRoutine(rit->second, {}, {});
                }
            }
            return resolveVar(id);
        }
        throw BasicRuntimeError(2, "Unexpected token '" + c.peek().text + "' in expression.",
                                0, "", c.peek().text, suggestBestMatch(c.peek().text, collectUserSymbols()));
    }

    Variant invokeFunctionOrArray(const std::string &u, const std::vector<Variant> &args, const std::vector<std::string> &refNames) {
        if (LibOffice::isOfficeRoutine(u)) {
            return LibOffice::dispatch(u, args, refNames, globals, callStack.empty() ? nullptr : &callStack.back());
        }
        if (CalcFunctions::isCalcFunction(u)) {
            return CalcFunctions::dispatch(u, args);
        }
        if (u == "WAIT") {
            int64_t ms = args.empty() ? 0 : std::max<int64_t>(0, args[0].toInt());
            if (gVerboseMode) std::cout << "[VERBOSE] Waiting " << ms << " ms...\n";
            if (ms > 0) std::this_thread::sleep_for(std::chrono::milliseconds(ms));
            return Variant::fromInt(0);
        }

        if (u == "ERR" || u == "ERR.NUMBER") return Variant::fromInt(currentErrCode);
        if (u == "ERL") return Variant::fromInt(currentErrLine);
        if (u == "ERROR" || u == "ERROR$" || u == "ERR.DESCRIPTION") {
            if (args.empty()) {
                return Variant::fromString(!currentErrMsg.empty() ? currentErrMsg : getStandardErrorMessage(currentErrCode));
            }
            int code = static_cast<int>(args[0].toInt());
            if (code == currentErrCode && !currentErrMsg.empty()) return Variant::fromString(currentErrMsg);
            return Variant::fromString(getStandardErrorMessage(code));
        }
        if (u == "CVERR") {
            int code = args.empty() ? 1 : static_cast<int>(args[0].toInt());
            return Variant::fromError(code, getStandardErrorMessage(code));
        }
        if (u == "ISERROR") {
            if (args.empty()) return Variant::fromInt(0);
            return Variant::fromInt(args[0].type == VarType::ERRVAL ? -1 : 0);
        }

        if (u == "FORMAT" || u == "FORMAT$") {
            if (args.empty()) throw BasicRuntimeError(5, "Format requires at least 1 argument.");
            if (args.size() == 1) return Variant::fromString(args[0].toString());
            return Variant::fromString(NumberFormatter::formatCustomOrNamed(args[0], args[1].toString()));
        }
        if (u == "FORMATNUMBER") {
            if (args.empty()) throw BasicRuntimeError(5, "FormatNumber requires at least 1 argument.");
            int dec = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 2;
            bool lead = (args.size() >= 3) ? (args[2].toInt() != 0) : true;
            bool parens = (args.size() >= 4) ? (args[3].toInt() == -1 || args[3].toInt() == 1) : false;
            bool group = (args.size() >= 5) ? (args[4].toInt() != 0) : true;
            return Variant::fromString(NumberFormatter::formatNumberWithOptions(args[0].toDouble(), dec, lead, parens, group));
        }
        if (u == "FORMATPERCENT") {
            if (args.empty()) throw BasicRuntimeError(5, "FormatPercent requires at least 1 argument.");
            int dec = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 2;
            bool lead = (args.size() >= 3) ? (args[2].toInt() != 0) : true;
            bool parens = (args.size() >= 4) ? (args[3].toInt() == -1 || args[3].toInt() == 1) : false;
            bool group = (args.size() >= 5) ? (args[4].toInt() != 0) : true;
            return Variant::fromString(NumberFormatter::formatNumberWithOptions(args[0].toDouble() * 100.0, dec, lead, parens, group, "", "%"));
        }
        if (u == "FORMATCURRENCY") {
            if (args.empty()) throw BasicRuntimeError(5, "FormatCurrency requires at least 1 argument.");
            int dec = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 2;
            bool lead = (args.size() >= 3) ? (args[2].toInt() != 0) : true;
            bool parens = (args.size() >= 4) ? (args[3].toInt() != 0) : true;
            bool group = (args.size() >= 5) ? (args[4].toInt() != 0) : true;
            return Variant::fromString(NumberFormatter::formatNumberWithOptions(args[0].toDouble(), dec, lead, parens, group, "$", ""));
        }
        if (u == "FORMATSCIENTIFIC") {
            if (args.empty()) throw BasicRuntimeError(5, "FormatScientific requires at least 1 argument.");
            int dec = (args.size() >= 2) ? std::max(0, static_cast<int>(args[1].toInt())) : 2;
            std::string mask = "0." + std::string(static_cast<size_t>(dec), '0') + "E+00";
            if (dec == 0) mask = "0E+00";
            return Variant::fromString(NumberFormatter::applySingleMask(args[0].toDouble(), mask, true));
        }
        if (u == "ROUND") {
            if (args.empty()) throw BasicRuntimeError(5, "Round requires at least 1 argument.");
            double val = args[0].toDouble();
            int dec = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 0;
            double factor = std::pow(10.0, dec);
            return Variant::fromDouble(std::round(val * factor) / factor);
        }
        if (u == "SPACE" || u == "SPACE$" || u == "SPC") {
            int64_t n = args.empty() ? 0 : std::max<int64_t>(0, args[0].toInt());
            return Variant::fromString(std::string(static_cast<size_t>(n), ' '));
        }
        if (u == "STRING" || u == "STRING$") {
            if (args.size() < 2) throw BasicRuntimeError(5, "String$ requires 2 arguments.");
            int64_t n = args[0].toInt();
            if (n < 0) throw BasicRuntimeError(5, "Invalid procedure call: negative length in String$.");
            char ch = ' ';
            if (args[1].type == VarType::STRING) {
                if (args[1].sVal.empty()) throw BasicRuntimeError(5, "Invalid procedure call: empty character argument in String$.");
                ch = args[1].sVal[0];
            } else {
                int64_t code = args[1].toInt();
                if (code < 0) throw BasicRuntimeError(5, "Invalid procedure call: negative ASCII character code in String$.");
                ch = static_cast<char>(static_cast<unsigned char>(code & 0xFF));
            }
            return Variant::fromString(std::string(static_cast<size_t>(n), ch));
        }
        if (u == "LPAD") {
            if (args.size() < 2) throw BasicRuntimeError(5, "LPad requires at least 2 arguments.");
            std::string s = args[0].toString();
            size_t w = static_cast<size_t>(std::max<int64_t>(0, args[1].toInt()));
            char pad = (args.size() >= 3 && !args[2].toString().empty()) ? args[2].toString()[0] : ' ';
            if (s.size() < w) s = std::string(w - s.size(), pad) + s;
            return Variant::fromString(s);
        }
        if (u == "RPAD") {
            if (args.size() < 2) throw BasicRuntimeError(5, "RPad requires at least 2 arguments.");
            std::string s = args[0].toString();
            size_t w = static_cast<size_t>(std::max<int64_t>(0, args[1].toInt()));
            char pad = (args.size() >= 3 && !args[2].toString().empty()) ? args[2].toString()[0] : ' ';
            if (s.size() < w) s = s + std::string(w - s.size(), pad);
            return Variant::fromString(s);
        }
        if (u == "HEX" || u == "HEX$") {
            std::ostringstream oss;
            oss << std::uppercase << std::hex << static_cast<uint64_t>(args.at(0).toInt());
            return Variant::fromString(oss.str());
        }
        if (u == "OCT" || u == "OCT$") {
            std::ostringstream oss;
            oss << std::oct << static_cast<uint64_t>(args.at(0).toInt());
            return Variant::fromString(oss.str());
        }

        if (u == "SIN") return Variant::fromDouble(std::sin(args.at(0).toDouble()));
        if (u == "COS") return Variant::fromDouble(std::cos(args.at(0).toDouble()));
        if (u == "TAN") return Variant::fromDouble(std::tan(args.at(0).toDouble()));
        if (u == "ATN") return Variant::fromDouble(std::atan(args.at(0).toDouble()));
        if (u == "SQR") {
            double v = args.at(0).toDouble();
            if (v < 0.0) throw BasicRuntimeError(5, "Invalid procedure call: Sqr of negative number");
            return Variant::fromDouble(std::sqrt(v));
        }
        if (u == "EXP") return Variant::fromDouble(std::exp(args.at(0).toDouble()));
        if (u == "LOG") {
            double v = args.at(0).toDouble();
            if (v <= 0.0) throw BasicRuntimeError(5, "Invalid procedure call: Log of non-positive number");
            return Variant::fromDouble(std::log(v));
        }
        if (u == "ABS") return Variant::fromDouble(std::fabs(args.at(0).toDouble()));
        if (u == "SGN") { double v = args.at(0).toDouble(); return Variant::fromInt((v > 0) ? 1 : ((v < 0) ? -1 : 0)); }
        if (u == "INT") return Variant::fromInt(static_cast<int64_t>(std::floor(args.at(0).toDouble())));
        if (u == "FIX") return Variant::fromInt(static_cast<int64_t>(std::trunc(args.at(0).toDouble())));
        if (u == "RND") return Variant::fromDouble(static_cast<double>(std::rand()) / RAND_MAX);

        if (u == "CINT" || u == "CLNG") return Variant::fromInt(args.at(0).toInt());
        if (u == "CDBL" || u == "CSNG" || u == "VAL") return Variant::fromDouble(args.at(0).toDouble());
        if (u == "CSTR" || u == "STR" || u == "STR$") return Variant::fromString(args.at(0).toString());
        if (u == "LEN") return Variant::fromInt(static_cast<int64_t>(args.at(0).toString().size()));
        if (u == "UCASE" || u == "UCASE$") return Variant::fromString(toUpper(args.at(0).toString()));
        if (u == "LCASE" || u == "LCASE$") {
            std::string s = args.at(0).toString();
            for (char &ch : s) ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
            return Variant::fromString(s);
        }
        if (u == "TRIM" || u == "TRIM$") return Variant::fromString(trim(args.at(0).toString()));
        if (u == "LTRIM" || u == "LTRIM$") {
            std::string s = args.at(0).toString();
            size_t a = s.find_first_not_of(" \t\r\n");
            return Variant::fromString(a == std::string::npos ? "" : s.substr(a));
        }
        if (u == "RTRIM" || u == "RTRIM$") {
            std::string s = args.at(0).toString();
            size_t b = s.find_last_not_of(" \t\r\n");
            return Variant::fromString(b == std::string::npos ? "" : s.substr(0, b + 1));
        }
        if (u == "LEFT" || u == "LEFT$") {
            std::string s = args.at(0).toString();
            return Variant::fromString(s.substr(0, std::min<size_t>(s.size(), std::max<int64_t>(0, args.at(1).toInt()))));
        }
        if (u == "RIGHT" || u == "RIGHT$") {
            std::string s = args.at(0).toString();
            size_t n = std::min<size_t>(s.size(), std::max<int64_t>(0, args.at(1).toInt()));
            return Variant::fromString(s.substr(s.size() - n));
        }
        if (u == "MID" || u == "MID$") {
            std::string s = args.at(0).toString();
            size_t st = static_cast<size_t>(std::max<int64_t>(1, args.at(1).toInt()) - 1);
            if (st >= s.size()) return Variant::fromString("");
            size_t ln = (args.size() >= 3) ? static_cast<size_t>(std::max<int64_t>(0, args[2].toInt())) : std::string::npos;
            return Variant::fromString(s.substr(st, ln));
        }
        if (u == "INSTR") {
            int64_t startPos = 1;
            std::string text1, text2;
            int compareMode = 0;

            if (args.size() == 2) {
                text1 = args[0].toString();
                text2 = args[1].toString();
            } else if (args.size() == 3) {
                if (args[0].type == VarType::INTEGER || args[0].type == VarType::DOUBLE) {
                    startPos = args[0].toInt();
                    text1 = args[1].toString();
                    text2 = args[2].toString();
                } else {
                    text1 = args[0].toString();
                    text2 = args[1].toString();
                    compareMode = static_cast<int>(args[2].toInt());
                }
            } else if (args.size() >= 4) {
                startPos = args[0].toInt();
                text1 = args[1].toString();
                text2 = args[2].toString();
                compareMode = static_cast<int>(args[3].toInt());
            } else {
                throw BasicRuntimeError(5, "InStr requires 2 to 4 arguments.");
            }

            if (startPos < 1 || static_cast<size_t>(startPos) > text1.size() + 1) return Variant::fromInt(0);
            if (text2.empty()) return Variant::fromInt(startPos);

            std::string hay = (compareMode != 0) ? toUpper(text1) : text1;
            std::string needle = (compareMode != 0) ? toUpper(text2) : text2;
            size_t found = hay.find(needle, static_cast<size_t>(startPos - 1));
            if (found == std::string::npos) return Variant::fromInt(0);
            return Variant::fromInt(static_cast<int64_t>(found + 1));
        }
        if (u == "INSTRREV") {
            if (args.size() < 2 || args.size() > 4)
                throw BasicRuntimeError(5, "InStrRev requires 2 to 4 arguments.");
            std::string text1 = args[0].toString();
            std::string text2 = args[1].toString();
            int64_t startPos = (args.size() >= 3) ? args[2].toInt() : -1;
            int compareMode = (args.size() >= 4) ? static_cast<int>(args[3].toInt()) : 0;

            if (startPos == 0 || startPos < -1)
                throw BasicRuntimeError(5, "Invalid procedure call: Start argument in InStrRev must be -1 or >= 1.");
            if (text1.empty()) return Variant::fromInt(0);

            size_t effStart = (startPos == -1) ? text1.size() : static_cast<size_t>(startPos);
            if (effStart > text1.size()) return Variant::fromInt(0);
            if (text2.empty()) return Variant::fromInt(static_cast<int64_t>(effStart));
            if (text2.size() > effStart) return Variant::fromInt(0);

            std::string haySlice = text1.substr(0, effStart);
            std::string hay = (compareMode != 0) ? toUpper(haySlice) : haySlice;
            std::string needle = (compareMode != 0) ? toUpper(text2) : text2;
            size_t found = hay.rfind(needle);
            if (found == std::string::npos) return Variant::fromInt(0);
            return Variant::fromInt(static_cast<int64_t>(found + 1));
        }
        if (u == "REPLACE" || u == "REPLACE$") {
            if (args.size() < 3 || args.size() > 6)
                throw BasicRuntimeError(5, "Replace requires 3 to 6 arguments.");
            std::string expr = args[0].toString();
            std::string findStr = args[1].toString();
            std::string replStr = args[2].toString();
            int64_t startPos = (args.size() >= 4) ? args[3].toInt() : 1;
            int64_t maxCount = (args.size() >= 5) ? args[4].toInt() : -1;
            int compareMode = (args.size() >= 6) ? static_cast<int>(args[5].toInt()) : 0;

            if (startPos < 1 || maxCount < -1)
                throw BasicRuntimeError(5, "Invalid procedure call: invalid Start or Count in Replace.");
            if (expr.empty() || static_cast<size_t>(startPos) > expr.size())
                return Variant::fromString("");

            std::string work = expr.substr(static_cast<size_t>(startPos - 1));
            if (findStr.empty() || maxCount == 0)
                return Variant::fromString(work);

            std::string searchSpace = (compareMode != 0) ? toUpper(work) : work;
            std::string needle = (compareMode != 0) ? toUpper(findStr) : findStr;

            std::string out;
            size_t pos = 0;
            int64_t count = 0;
            while (maxCount < 0 || count < maxCount) {
                size_t next = searchSpace.find(needle, pos);
                if (next == std::string::npos) break;
                out.append(work, pos, next - pos);
                out.append(replStr);
                pos = next + needle.size();
                ++count;
            }
            out.append(work, pos, std::string::npos);
            return Variant::fromString(out);
        }
        if (u == "STRREVERSE" || u == "STRREVERSE$") {
            if (args.size() != 1) throw BasicRuntimeError(5, "StrReverse requires 1 argument.");
            std::string s = args[0].toString();
            std::reverse(s.begin(), s.end());
            return Variant::fromString(s);
        }
        if (u == "STRCOMP") {
            if (args.size() < 2 || args.size() > 3)
                throw BasicRuntimeError(5, "StrComp requires 2 or 3 arguments.");
            if (args[0].type == VarType::ERRVAL) return args[0];
            if (args[1].type == VarType::ERRVAL) return args[1];
            std::string s1 = args[0].toString();
            std::string s2 = args[1].toString();
            int compareMode = (args.size() >= 3) ? static_cast<int>(args[2].toInt()) : 0;
            if (compareMode != 0) {
                s1 = toUpper(s1);
                s2 = toUpper(s2);
            }
            int res = (s1 < s2) ? -1 : ((s1 > s2) ? 1 : 0);
            return Variant::fromInt(res);
        }
        if (u == "FILTER") {
            if (args.size() < 2 || args.size() > 4)
                throw BasicRuntimeError(5, "Filter requires 2 to 4 arguments.");
            const Variant &srcArr = args[0];
            if (srcArr.type != VarType::ARRAY)
                throw BasicRuntimeError(5, "Filter requires a 1D array as first argument.");
            if (srcArr.dims.size() > 1)
                throw BasicRuntimeError(5, "Filter only supports 1D arrays.");

            std::string matchStr = args[1].toString();
            bool includeMatch = (args.size() >= 3) ? args[2].toBool() : true;
            int compareMode = (args.size() >= 4) ? static_cast<int>(args[3].toInt()) : 0;
            std::string needle = (compareMode != 0) ? toUpper(matchStr) : matchStr;

            std::vector<std::string> matched;
            for (const auto &item : srcArr.arrayData) {
                std::string elem = item.toString();
                std::string hay = (compareMode != 0) ? toUpper(elem) : elem;
                bool found = needle.empty() ? true : (hay.find(needle) != std::string::npos);
                if (found == includeMatch) {
                    matched.push_back(elem);
                }
            }

            if (matched.empty()) {
                return Variant::makeArray({{0, -1}}, VarType::STRING);
            }
            Variant outArr = Variant::makeArray({{0, static_cast<int>(matched.size()) - 1}}, VarType::STRING);
            for (size_t i = 0; i < matched.size(); ++i) {
                outArr.arrayData[i] = Variant::fromString(matched[i]);
                outArr.arrayData[i].declaredType = VarType::STRING;
            }
            return outArr;
        }
        if (u == "SPLIT") {
            if (args.empty()) throw BasicRuntimeError(5, "Split requires at least 1 argument.");
            std::string text = args[0].toString();
            std::string delim = (args.size() >= 2) ? args[1].toString() : " ";
            int64_t maxParts = (args.size() >= 3) ? args[2].toInt() : -1;

            std::vector<std::string> parts;
            if (text.empty()) {
                parts.push_back("");
            } else if (delim.empty() || maxParts == 1) {
                parts.push_back(text);
            } else {
                size_t pos = 0;
                while (true) {
                    if (maxParts > 0 && static_cast<int64_t>(parts.size()) == maxParts - 1) {
                        parts.push_back(text.substr(pos));
                        break;
                    }
                    size_t next = text.find(delim, pos);
                    if (next == std::string::npos) {
                        parts.push_back(text.substr(pos));
                        break;
                    }
                    parts.push_back(text.substr(pos, next - pos));
                    pos = next + delim.size();
                }
            }

            Variant arr = Variant::makeArray({{0, static_cast<int>(parts.size()) - 1}}, VarType::STRING);
            for (size_t i = 0; i < parts.size(); ++i) {
                arr.arrayData[i] = Variant::fromString(parts[i]);
                arr.arrayData[i].declaredType = VarType::STRING;
            }
            return arr;
        }
        if (u == "JOIN") {
            if (args.empty() || args[0].type != VarType::ARRAY)
                throw BasicRuntimeError(5, "Join requires an array as first argument.");
            std::string delim = (args.size() >= 2) ? args[1].toString() : " ";
            const Variant &arr = args[0];
            std::ostringstream oss;
            for (size_t i = 0; i < arr.arrayData.size(); ++i) {
                if (i > 0) oss << delim;
                oss << arr.arrayData[i].toString();
            }
            return Variant::fromString(oss.str());
        }
        if (u == "CHR" || u == "CHR$") return Variant::fromString(std::string(1, static_cast<char>(args.at(0).toInt())));
        if (u == "ASC") { std::string s = args.at(0).toString(); return Variant::fromInt(s.empty() ? 0 : static_cast<unsigned char>(s[0])); }

        if (u == "LBOUND" || u == "UBOUND") {
            const Variant &arr = args.at(0);
            if (arr.type != VarType::ARRAY) throw BasicRuntimeError(9, u + " requires an array.");
            int dim = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) - 1 : 0;
            if (dim < 0 || dim >= static_cast<int>(arr.dims.size())) throw BasicRuntimeError(9, "Invalid dimension in " + u);
            return Variant::fromInt(u == "LBOUND" ? arr.dims[dim].low : arr.dims[dim].high);
        }
        if (u == "ARRAY") {
            Variant arr = Variant::makeArray({{optionBase, optionBase + static_cast<int>(args.size()) - 1}}, VarType::EMPTY);
            for (size_t i = 0; i < args.size(); ++i) arr.arrayData[i] = args[i];
            return arr;
        }

        // --- DATE & TIME FUNCTIONS ---
        if (u == "DATE" || u == "DATE$") {
            return DateTimeUtil::currentDate().toVariant();
        }
        if (u == "TIME" || u == "TIME$") {
            return DateTimeUtil::currentTime().toVariant();
        }
        if (u == "NOW") {
            return DateTimeUtil::currentNow().toVariant();
        }
        if (u == "TIMER") {
            return Variant::fromDouble(DateTimeUtil::currentTimer());
        }
        if (u == "YEAR") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Year requires 1 argument.");
            return Variant::fromInt(DateTimeUtil::parseDateVariant(args[0]).year);
        }
        if (u == "MONTH") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Month requires 1 argument.");
            return Variant::fromInt(DateTimeUtil::parseDateVariant(args[0]).month);
        }
        if (u == "DAY") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Day requires 1 argument.");
            return Variant::fromInt(DateTimeUtil::parseDateVariant(args[0]).day);
        }
        if (u == "HOUR") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Hour requires 1 argument.");
            return Variant::fromInt(DateTimeUtil::parseDateVariant(args[0]).hour);
        }
        if (u == "MINUTE") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Minute requires 1 argument.");
            return Variant::fromInt(DateTimeUtil::parseDateVariant(args[0]).minute);
        }
        if (u == "SECOND") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Second requires 1 argument.");
            return Variant::fromInt(DateTimeUtil::parseDateVariant(args[0]).second);
        }
        if (u == "WEEKDAY") {
            if (args.empty() || args.size() > 2) throw BasicRuntimeError(5, "Weekday requires 1 or 2 arguments.");
            int firstDay = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 1;
            return Variant::fromInt(DateTimeUtil::computeWeekday(DateTimeUtil::parseDateVariant(args[0]), firstDay));
        }
        if (u == "DATESERIAL") {
            if (args.size() != 3) throw BasicRuntimeError(5, "DateSerial requires 3 arguments (year, month, day).");
            return DateTimeUtil::makeDateSerial(static_cast<int>(args[0].toInt()),
                                                static_cast<int>(args[1].toInt()),
                                                static_cast<int>(args[2].toInt())).toVariant();
        }
        if (u == "TIMESERIAL") {
            if (args.size() != 3) throw BasicRuntimeError(5, "TimeSerial requires 3 arguments (hour, minute, second).");
            return DateTimeUtil::makeTimeSerial(static_cast<int>(args[0].toInt()),
                                                static_cast<int>(args[1].toInt()),
                                                static_cast<int>(args[2].toInt())).toVariant();
        }
        if (u == "DATEVALUE") {
            if (args.size() != 1) throw BasicRuntimeError(5, "DateValue requires 1 argument.");
            DateTimeUtil::DateParts dp = DateTimeUtil::parseDateVariant(args[0]);
            dp.hour = 0; dp.minute = 0; dp.second = 0;
            dp.hasDate = true; dp.hasTime = false;
            return dp.toVariant();
        }
        if (u == "TIMEVALUE") {
            if (args.size() != 1) throw BasicRuntimeError(5, "TimeValue requires 1 argument.");
            DateTimeUtil::DateParts dp = DateTimeUtil::parseDateVariant(args[0]);
            dp.year = 1899; dp.month = 12; dp.day = 30;
            dp.hasDate = false; dp.hasTime = true;
            return dp.toVariant();
        }
        if (u == "DATEADD") {
            if (args.size() != 3) throw BasicRuntimeError(5, "DateAdd requires 3 arguments (interval, number, date).");
            DateTimeUtil::DateParts dp = DateTimeUtil::parseDateVariant(args[2]);
            return DateTimeUtil::computeDateAdd(args[0].toString(), args[1].toDouble(), dp).toVariant();
        }
        if (u == "DATEDIFF") {
            if (args.size() < 3 || args.size() > 5) throw BasicRuntimeError(5, "DateDiff requires 3 to 5 arguments.");
            DateTimeUtil::DateParts d1 = DateTimeUtil::parseDateVariant(args[1]);
            DateTimeUtil::DateParts d2 = DateTimeUtil::parseDateVariant(args[2]);
            int firstDay = (args.size() >= 4) ? static_cast<int>(args[3].toInt()) : 1;
            return Variant::fromInt(DateTimeUtil::computeDateDiff(args[0].toString(), d1, d2, firstDay));
        }
        if (u == "DATEPART") {
            if (args.size() < 2 || args.size() > 4) throw BasicRuntimeError(5, "DatePart requires 2 to 4 arguments.");
            DateTimeUtil::DateParts dp = DateTimeUtil::parseDateVariant(args[1]);
            int firstDay = (args.size() >= 3) ? static_cast<int>(args[2].toInt()) : 1;
            return Variant::fromInt(DateTimeUtil::computeDatePart(args[0].toString(), dp, firstDay));
        }
        if (u == "FORMATDATETIME") {
            if (args.empty() || args.size() > 2) throw BasicRuntimeError(5, "FormatDateTime requires 1 or 2 arguments.");
            int fmt = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 0;
            return Variant::fromString(DateTimeUtil::formatDateTimeValue(args[0], fmt));
        }

        // --- FILE SYSTEM & DIRECTORY FUNCTIONS ---
        if (u == "DIR" || u == "DIR$") {
            if (args.empty()) return Variant::fromString(dirState.nextMatch());
            std::string pat = args[0].toString();
            int attr = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 0;
            return Variant::fromString(dirState.startSearch(pat, attr));
        }
        if (u == "CURDIR" || u == "CURDIR$") {
            std::error_code ec;
            return Variant::fromString(std::filesystem::current_path(ec).string());
        }
        if (u == "FILEDATETIME") {
            if (args.size() != 1) throw BasicRuntimeError(5, "FileDateTime requires 1 argument.");
            return Variant::fromString(FileSystemUtil::getFileDateTime(args[0].toString()));
        }
        if (u == "FILELEN") {
            if (args.size() != 1) throw BasicRuntimeError(5, "FileLen requires 1 argument.");
            return Variant::fromInt(FileSystemUtil::getFileLength(args[0].toString()));
        }
        if (u == "GETATTR") {
            if (args.size() != 1) throw BasicRuntimeError(5, "GetAttr requires 1 argument.");
            return Variant::fromInt(FileSystemUtil::getFileAttributes(args[0].toString()));
        }
        if (u == "SETATTR") {
            if (args.size() != 2) throw BasicRuntimeError(5, "SetAttr requires 2 arguments (path, attributes).");
            FileSystemUtil::setFileAttributes(args[0].toString(), static_cast<int>(args[1].toInt()));
            return Variant::fromInt(0);
        }
        if (u == "KILL") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Kill requires 1 argument.");
            std::string pat = args[0].toString();
            std::error_code ec;
            std::filesystem::path p(pat);
            if (pat.find('*') != std::string::npos || pat.find('?') != std::string::npos) {
                std::filesystem::path parent = p.parent_path();
                if (parent.empty()) parent = std::filesystem::current_path(ec);
                std::string mask = p.filename().string();
                int removedCount = 0;
                for (const auto &entry : std::filesystem::directory_iterator(parent, ec)) {
                    if (!entry.is_directory(ec) && FileSystemUtil::wildcardMatch(mask, entry.path().filename().string())) {
                        if (std::filesystem::remove(entry.path(), ec)) ++removedCount;
                    }
                }
                if (removedCount == 0) throw BasicRuntimeError(53, "File not found: " + pat);
            } else {
                if (!std::filesystem::exists(p, ec) || std::filesystem::is_directory(p, ec))
                    throw BasicRuntimeError(53, "File not found: " + pat);
                if (!std::filesystem::remove(p, ec))
                    throw BasicRuntimeError(75, "Path/File access error deleting: " + pat);
            }
            return Variant::fromInt(0);
        }
        if (u == "FILECOPY") {
            if (args.size() != 2) throw BasicRuntimeError(5, "FileCopy requires 2 arguments (source, destination).");
            std::string srcPath = args[0].toString();
            std::string dstPath = args[1].toString();
            std::error_code ec;
            if (!std::filesystem::exists(srcPath, ec) || std::filesystem::is_directory(srcPath, ec))
                throw BasicRuntimeError(53, "File not found: " + srcPath);
            std::filesystem::copy_file(srcPath, dstPath, std::filesystem::copy_options::overwrite_existing, ec);
            if (ec) throw BasicRuntimeError(75, "Path/File access error copying '" + srcPath + "' to '" + dstPath + "'.");
            return Variant::fromInt(0);
        }
        if (u == "MKDIR") {
            if (args.size() != 1) throw BasicRuntimeError(5, "MkDir requires 1 argument.");
            std::string dirPath = args[0].toString();
            std::error_code ec;
            if (std::filesystem::exists(dirPath, ec))
                throw BasicRuntimeError(75, "Path/File access error: directory already exists: " + dirPath);
            if (!std::filesystem::create_directories(dirPath, ec))
                throw BasicRuntimeError(76, "Path not found or cannot create directory: " + dirPath);
            return Variant::fromInt(0);
        }
        if (u == "RMDIR") {
            if (args.size() != 1) throw BasicRuntimeError(5, "RmDir requires 1 argument.");
            std::string dirPath = args[0].toString();
            std::error_code ec;
            if (!std::filesystem::exists(dirPath, ec) || !std::filesystem::is_directory(dirPath, ec))
                throw BasicRuntimeError(76, "Path not found: " + dirPath);
            if (!std::filesystem::remove(dirPath, ec))
                throw BasicRuntimeError(75, "Path/File access error (directory may not be empty): " + dirPath);
            return Variant::fromInt(0);
        }
        if (u == "CHDIR") {
            if (args.size() != 1) throw BasicRuntimeError(5, "ChDir requires 1 argument.");
            std::string dirPath = args[0].toString();
            std::error_code ec;
            std::filesystem::current_path(dirPath, ec);
            if (ec) throw BasicRuntimeError(76, "Path not found: " + dirPath);
            return Variant::fromInt(0);
        }
        if (u == "CHDRIVE") {
            if (args.size() != 1) throw BasicRuntimeError(5, "ChDrive requires 1 argument.");
            return Variant::fromInt(0);
        }

        // --- OPEN FILE POSITION, LENGTH & RAW INPUT FUNCTIONS ---
        if (u == "EOF") {
            int ch = static_cast<int>(args.at(0).toInt());
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel not open.");
            it->second.stream.clear();
            return Variant::fromInt(it->second.stream.peek() == std::char_traits<char>::eof() ? -1 : 0);
        }
        if (u == "LOC") {
            if (args.size() != 1) throw BasicRuntimeError(5, "Loc requires 1 argument (#channel).");
            int ch = static_cast<int>(args[0].toInt());
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel not open.");
            it->second.stream.clear();
            std::streamoff pos = (it->second.mode == "OUTPUT" || it->second.mode == "APPEND")
                                 ? it->second.stream.tellp()
                                 : it->second.stream.tellg();
            if (pos < 0) pos = 0;
            return Variant::fromInt(static_cast<int64_t>(pos));
        }
        if (u == "LOF") {
            if (args.size() != 1) throw BasicRuntimeError(5, "LOF requires 1 argument (#channel).");
            int ch = static_cast<int>(args[0].toInt());
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel not open.");
            it->second.stream.clear();
            it->second.stream.flush();
            std::streampos curG = it->second.stream.tellg();
            it->second.stream.seekg(0, std::ios::end);
            std::streamoff endPos = it->second.stream.tellg();
            if (curG != std::streampos(-1)) it->second.stream.seekg(curG);
            return Variant::fromInt(endPos < 0 ? 0 : static_cast<int64_t>(endPos));
        }
        if (u == "SEEK") {
            if (args.empty() || args.size() > 2) throw BasicRuntimeError(5, "Seek requires 1 or 2 arguments.");
            int ch = static_cast<int>(args[0].toInt());
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel not open.");
            it->second.stream.clear();
            if (args.size() == 1) {
                std::streamoff pos = (it->second.mode == "OUTPUT" || it->second.mode == "APPEND")
                                     ? it->second.stream.tellp()
                                     : it->second.stream.tellg();
                if (pos < 0) pos = 0;
                return Variant::fromInt(static_cast<int64_t>(pos) + 1);
            } else {
                int64_t oneBasedPos = args[1].toInt();
                if (oneBasedPos < 1) throw BasicRuntimeError(5, "Invalid procedure call: Seek position must be >= 1.");
                std::streamoff zeroOffset = static_cast<std::streamoff>(oneBasedPos - 1);
                it->second.stream.seekg(zeroOffset, std::ios::beg);
                it->second.stream.seekp(zeroOffset, std::ios::beg);
                return Variant::fromInt(0);
            }
        }
        if (u == "INPUT" || u == "INPUT$") {
            if (args.size() != 2) throw BasicRuntimeError(5, "Input requires 2 arguments.");
            // Support both Input(count, [#]ch) and Input(#ch, byRefVar)
            if (u == "INPUT" && refNames.size() >= 2 && !refNames[1].empty() && openFiles.count(static_cast<int>(args[0].toInt()))) {
                int ch = static_cast<int>(args[0].toInt());
                auto &st = openFiles[ch].stream;
                st.clear();
                while (st.good() && (st.peek() == ' ' || st.peek() == '\t' || st.peek() == '\r' || st.peek() == '\n')) {
                    st.get();
                }
                if (st.peek() == std::char_traits<char>::eof()) {
                    throw BasicRuntimeError(62, getStandardErrorMessage(62));
                }
                std::string valStr;
                bool wasQuoted = false;
                if (st.peek() == '"') {
                    wasQuoted = true;
                    st.get();
                    while (st.good()) {
                        int c = st.get();
                        if (c == std::char_traits<char>::eof() || c == '"') break;
                        valStr.push_back(static_cast<char>(c));
                    }
                    while (st.good() && (st.peek() == ' ' || st.peek() == '\t')) st.get();
                    if (st.peek() == ',') {
                        st.get();
                    } else if (st.peek() == '\r') {
                        st.get();
                        if (st.peek() == '\n') st.get();
                    } else if (st.peek() == '\n') {
                        st.get();
                    }
                } else {
                    while (st.good()) {
                        int c = st.peek();
                        if (c == std::char_traits<char>::eof()) break;
                        if (c == ',') { st.get(); break; }
                        if (c == '\r') { st.get(); if (st.peek() == '\n') st.get(); break; }
                        if (c == '\n') { st.get(); break; }
                        valStr.push_back(static_cast<char>(st.get()));
                    }
                    valStr = trim(valStr);
                }
                Variant &dst = resolveVar(refNames[1]);
                if (!wasQuoted && dst.declaredType == VarType::EMPTY) {
                    try {
                        size_t idx = 0;
                        double d = std::stod(valStr, &idx);
                        if (idx == valStr.size()) {
                            if (valStr.find('.') == std::string::npos && valStr.find('e') == std::string::npos && valStr.find('E') == std::string::npos)
                                dst.assignCoerced(Variant::fromInt(static_cast<int64_t>(d)));
                            else
                                dst.assignCoerced(Variant::fromDouble(d));
                            return dst;
                        }
                    } catch (...) {}
                }
                dst.assignCoerced(Variant::fromString(valStr));
                return dst;
            }

            int64_t count = args[0].toInt();
            int ch = static_cast<int>(args[1].toInt());
            if (count < 0) throw BasicRuntimeError(5, "Invalid procedure call: negative byte count in Input$.");
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel not open.");
            it->second.stream.clear();
            std::string buf(static_cast<size_t>(count), '\0');
            it->second.stream.read(&buf[0], static_cast<std::streamsize>(count));
            std::streamsize got = it->second.stream.gcount();
            if (got < count) throw BasicRuntimeError(62, getStandardErrorMessage(62));
            return Variant::fromString(buf);
        }
        if (u == "FREEFILE") { int ch = 1; while (openFiles.count(ch)) ++ch; return Variant::fromInt(ch); }
        if (u == "MSGBOX") {
            std::string p = args.at(0).toString();
            int b = (args.size() >= 2) ? static_cast<int>(args[1].toInt()) : 0;
            std::string t = (args.size() >= 3) ? args[2].toString() : "sBasic";
            return Variant::fromInt(showMsgBox(p, b, t));
        }
        if (u == "INPUTBOX") {
            std::string p = args.at(0).toString();
            std::string t = (args.size() >= 2) ? args[1].toString() : "Input";
            std::string d = (args.size() >= 3) ? args[2].toString() : "";
            return Variant::fromString(showInputBox(p, t, d));
        }

        if (u == "MATMUL") return LinAlg::matMul(args.at(0), args.at(1));
        if (u == "MATINV") return LinAlg::matInv(args.at(0));
        if (u == "MATTRANSPOSE") return LinAlg::matTranspose(args.at(0));
        if (u == "MATEIG") {
            Variant eigVals, eigVecs;
            LinAlg::jacobiEigen(args.at(0), eigVals, eigVecs);
            if (args.size() == 2 && !refNames[1].empty()) resolveVar(refNames[1]) = eigVecs;
            return eigVals;
        }
        if (u == "MATEIGVEC") {
            Variant eigVals, eigVecs;
            LinAlg::jacobiEigen(args.at(0), eigVals, eigVecs);
            return eigVecs;
        }
        if (u == "LINEARSOLVER" || u == "LINSOLVER" || u == "SIMPLEXSOLVER" ||
            u == "NONLINEARSOLVER" || u == "NLPSOLVER" || u == "SOLVER") {
            if (args.empty()) throw BasicRuntimeError(5, u + " requires a specification string.");
            std::string combinedSpec;
            for (size_t i = 0; i < args.size(); ++i) {
                if (i > 0) combinedSpec += ", ";
                combinedSpec += args[i].toString();
            }
            auto cb = makeSolverCallbacks();
            if (u == "LINEARSOLVER" || u == "LINSOLVER" || u == "SIMPLEXSOLVER") {
                return LinAlg::linearSolver(combinedSpec, cb);
            }
            if (u == "NONLINEARSOLVER" || u == "NLPSOLVER") {
                return LinAlg::nonlinearSolver(combinedSpec, cb);
            }
            try {
                return LinAlg::linearSolver(combinedSpec, cb);
            } catch (...) {
                return LinAlg::nonlinearSolver(combinedSpec, cb);
            }
        }

        auto rit = routines.find(u);
        if (rit != routines.end()) return executeRoutine(rit->second, args, refNames);

        if (varExists(u)) {
            Variant &arr = resolveVar(u);
            if (arr.type == VarType::ARRAY) {
                std::vector<int> idx;
                for (const auto &a : args) idx.push_back(static_cast<int>(a.toInt()));
                return arr.at(idx);
            }
        }

        std::string sug = suggestBestMatch(u, collectUserSymbols());
        std::string msg = "Unknown command, function, or array '" + u + "'.";
        if (!sug.empty()) msg += " Did you mean '" + sug + "'?";
        throw BasicRuntimeError(35, msg, 0, "", u, sug);
    }

    enum class ErrorMode { NONE, RESUME_NEXT, GOTO_LABEL };

    struct ForFrame {
        int forLine = 0;
        int nextLine = 0;
        std::string varName;
        double limitVal = 0.0;
        double stepVal = 1.0;
    };

    struct ExecContext {
        int startLine = 0;
        int endLine = 0;
        int pc = 0;
        bool pcJumped = false;
        bool exitRoutine = false;
        std::unordered_map<std::string, int> labelMap;
        std::vector<ForFrame> forStack;
        std::vector<int> gosubStack;

        ErrorMode errMode = ErrorMode::NONE;
        std::string errLabel;
        bool inErrorHandler = false;
        int errorResumePc = -1;
    };

    static bool isMultiLineIf(const std::vector<Token> &toks) {
        if (toks.size() < 2 || toks[0].type != TokType::IDENT || toUpper(toks[0].text) != "IF") return false;
        return (toks[toks.size() - 2].type == TokType::IDENT && toUpper(toks[toks.size() - 2].text) == "THEN");
    }

    int findMatchingEndIf(int ifLine, int limitLine, int &elseLine, std::vector<int> &elseIfLines) {
        int depth = 0; elseLine = -1; elseIfLines.clear();
        for (int ln = ifLine + 1; ln < limitLine; ++ln) {
            const auto &toks = tokenizedLines[ln];
            if (toks.empty() || toks[0].type == TokType::END) continue;
            std::string kw = toUpper(toks[0].text);
            if (isMultiLineIf(toks)) ++depth;
            else if ((kw == "END" && toks.size() >= 2 && toUpper(toks[1].text) == "IF") || kw == "ENDIF") {
                if (depth == 0) return ln;
                --depth;
            } else if (depth == 0 && kw == "ELSEIF") elseIfLines.push_back(ln);
            else if (depth == 0 && kw == "ELSE") elseLine = ln;
        }
        throw BasicRuntimeError(2, "Missing 'End If' for multi-line 'If' block.",
                                sourceLineNumbers[ifLine], sourceLines[ifLine], "If", "End If");
    }

    int findEnclosingEndIfFromBranch(int branchLine, int limitLine) {
        int depth = 0;
        for (int ln = branchLine + 1; ln < limitLine; ++ln) {
            const auto &toks = tokenizedLines[ln];
            if (toks.empty() || toks[0].type == TokType::END) continue;
            std::string kw = toUpper(toks[0].text);
            if (isMultiLineIf(toks)) ++depth;
            else if ((kw == "END" && toks.size() >= 2 && toUpper(toks[1].text) == "IF") || kw == "ENDIF") {
                if (depth == 0) return ln;
                --depth;
            }
        }
        throw BasicRuntimeError(2, "Missing 'End If'.", sourceLineNumbers[branchLine], sourceLines[branchLine], "", "End If");
    }

    int findMatchingEndSelect(int selectLine, int limitLine, std::vector<int> &caseLines, int &caseElseLine) {
        int depth = 0; caseLines.clear(); caseElseLine = -1;
        for (int ln = selectLine + 1; ln < limitLine; ++ln) {
            const auto &toks = tokenizedLines[ln];
            if (toks.empty() || toks[0].type == TokType::END) continue;
            std::string kw = toUpper(toks[0].text);
            if (kw == "SELECT" && toks.size() >= 2 && toUpper(toks[1].text) == "CASE") {
                ++depth;
            } else if ((kw == "END" && toks.size() >= 2 && toUpper(toks[1].text) == "SELECT") || kw == "ENDSELECT") {
                if (depth == 0) return ln;
                --depth;
            } else if (depth == 0 && kw == "CASE") {
                if (toks.size() >= 2 && toUpper(toks[1].text) == "ELSE") caseElseLine = ln;
                else caseLines.push_back(ln);
            }
        }
        throw BasicRuntimeError(2, "Missing 'End Select' for 'Select Case' block.",
                                sourceLineNumbers[selectLine], sourceLines[selectLine], "Select", "End Select");
    }

    int findEnclosingEndSelectFromCase(int caseLine, int limitLine) {
        int depth = 0;
        for (int ln = caseLine + 1; ln < limitLine; ++ln) {
            const auto &toks = tokenizedLines[ln];
            if (toks.empty() || toks[0].type == TokType::END) continue;
            std::string kw = toUpper(toks[0].text);
            if (kw == "SELECT" && toks.size() >= 2 && toUpper(toks[1].text) == "CASE") ++depth;
            else if ((kw == "END" && toks.size() >= 2 && toUpper(toks[1].text) == "SELECT") || kw == "ENDSELECT") {
                if (depth == 0) return ln;
                --depth;
            }
        }
        throw BasicRuntimeError(2, "Missing 'End Select'.", sourceLineNumbers[caseLine], sourceLines[caseLine], "Case", "End Select");
    }

    int findMatchingLoopEnd(int startLine, int limitLine, const std::string &openKw, const std::string &closeKw) {
        int depth = 0;
        for (int ln = startLine + 1; ln < limitLine; ++ln) {
            const auto &toks = tokenizedLines[ln];
            if (toks.empty() || toks[0].type == TokType::END) continue;
            std::string kw = toUpper(toks[0].text);
            if (kw == openKw) ++depth;
            else if (kw == closeKw) { if (depth == 0) return ln; --depth; }
        }
        throw BasicRuntimeError(2, "Missing '" + closeKw + "' for '" + openKw + "' loop.",
                                sourceLineNumbers[startLine], sourceLines[startLine], openKw, closeKw);
    }

    int findMatchingLoopStart(int closeLine, int startLimit, const std::string &openKw, const std::string &closeKw) {
        int depth = 0;
        for (int ln = closeLine - 1; ln >= startLimit; --ln) {
            const auto &toks = tokenizedLines[ln];
            if (toks.empty() || toks[0].type == TokType::END) continue;
            std::string kw = toUpper(toks[0].text);
            if (kw == closeKw) ++depth;
            else if (kw == openKw) { if (depth == 0) return ln; --depth; }
        }
        throw BasicRuntimeError(2, "'" + closeKw + "' without matching '" + openKw + "'.",
                                sourceLineNumbers[closeLine], sourceLines[closeLine], closeKw, openKw);
    }

    bool evalSelectCaseLine(const std::vector<Token> &caseToks, const Variant &testVal) {
        ExprCursor c{caseToks, 1};
        while (c.peek().type != TokType::END) {
            c.matchIdent("IS");
            TokType op = c.peek().type;
            if (op == TokType::EQ || op == TokType::NEQ || op == TokType::LT ||
                op == TokType::GT || op == TokType::LE || op == TokType::GE) {
                c.get();
                Variant rhs = evalExpr(c);
                if (compareVariants(testVal, rhs, op)) return true;
            } else {
                Variant v1 = evalExpr(c);
                if (c.matchIdent("TO")) {
                    Variant v2 = evalExpr(c);
                    if (compareVariants(testVal, v1, TokType::GE) && compareVariants(testVal, v2, TokType::LE)) {
                        return true;
                    }
                } else {
                    if (compareVariants(testVal, v1, TokType::EQ)) return true;
                }
            }
            if (!c.match(TokType::COMMA)) break;
        }
        return false;
    }

    static std::vector<std::vector<Token>> splitTokensByColon(const std::vector<Token> &toks) {
        std::vector<std::vector<Token>> groups;
        std::vector<Token> cur;
        for (const auto &t : toks) {
            if (t.type == TokType::END) break;
            if (t.type == TokType::COLON) {
                if (!cur.empty()) {
                    cur.push_back({TokType::END, ""});
                    groups.push_back(std::move(cur));
                    cur.clear();
                }
            } else {
                cur.push_back(t);
            }
        }
        if (!cur.empty()) {
            cur.push_back({TokType::END, ""});
            groups.push_back(std::move(cur));
        }
        return groups;
    }

    void jumpToLabel(ExecContext &ctx, const std::string &rawLabel, bool isErrorJump = false) {
        std::string lbl = toUpper(rawLabel);
        auto it = ctx.labelMap.find(lbl);
        if (it == ctx.labelMap.end()) {
            std::vector<std::string> knownLabels;
            for (const auto &kv : ctx.labelMap) knownLabels.push_back(kv.first);
            std::string sug = suggestBestMatch(rawLabel, knownLabels);
            throw BasicRuntimeError(35, "Label '" + rawLabel + "' is not defined.",
                                    sourceLineNumbers[ctx.pc], sourceLines[ctx.pc], rawLabel, sug);
        }
        if (gVerboseMode) {
            std::cout << "[VERBOSE] Jumping to label '" << rawLabel
                      << "' (line " << sourceLineNumbers[it->second] << ")\n";
        }
        ctx.pc = it->second;
        ctx.pcJumped = true;
        if (!isErrorJump) {
            while (!ctx.forStack.empty() &&
                   (ctx.pc < ctx.forStack.back().forLine || ctx.pc > ctx.forStack.back().nextLine)) {
                ctx.forStack.pop_back();
            }
        }
    }

    Variant executeRoutine(const RoutineDecl &r, const std::vector<Variant> &args, const std::vector<std::string> &refNames) {
        if (args.size() != r.params.size()) {
            throw BasicRuntimeError(5, "Argument count mismatch calling '" + r.name + "': expected " +
                                    std::to_string(r.params.size()) + ", got " + std::to_string(args.size()) + ".");
        }
        if (gVerboseMode) {
            std::cout << "[VERBOSE] Entering " << (r.isFunction ? "Function " : "Sub ") << r.name << "\n";
        }
        auto normSym = [](std::string s) {
            s = toUpper(s);
            if (!s.empty() && (s.back() == '$' || s.back() == '%')) s.pop_back();
            return s;
        };
        std::unordered_map<std::string, Variant> frame;
        for (size_t i = 0; i < r.params.size(); ++i) {
            Variant pv; pv.declaredType = r.params[i].type; pv.assignCoerced(args[i]);
            frame[normSym(r.params[i].name)] = pv;
        }
        if (r.isFunction) { Variant rv; rv.declaredType = r.returnType; frame[normSym(r.name)] = rv; }
        callStack.push_back(std::move(frame));
        try {
            executeBlock(r.startLine + 1, r.endLine);
        } catch (...) {
            callStack.pop_back();
            throw;
        }
        auto finished = std::move(callStack.back());
        callStack.pop_back();
        if (callStack.empty() && r.name == "MAIN") {
            for (const auto &kv : finished) {
                globals[kv.first] = kv.second;
            }
        }
        for (size_t i = 0; i < r.params.size(); ++i) {
            if (!r.params[i].byVal && i < refNames.size() && !refNames[i].empty())
                resolveVar(refNames[i]).assignCoerced(finished[normSym(r.params[i].name)]);
        }
        if (gVerboseMode) {
            std::cout << "[VERBOSE] Exiting " << (r.isFunction ? "Function " : "Sub ") << r.name << "\n";
        }
        return r.isFunction ? finished[normSym(r.name)] : Variant();
    }

    void executeBlock(int startLine, int endLine) {
        ExecContext ctx;
        ctx.startLine = startLine;
        ctx.endLine = endLine;
        ctx.pc = startLine;

        for (int ln = startLine; ln < endLine; ++ln) {
            const auto &toks = tokenizedLines[ln];
            if (toks.size() >= 2 && toks[1].type == TokType::COLON &&
                (toks[0].type == TokType::IDENT || toks[0].type == TokType::NUMBER)) {
                ctx.labelMap[toUpper(toks[0].text)] = ln;
            }
        }

        while (ctx.pc < ctx.endLine && !ctx.exitRoutine) {
            ctx.pcJumped = false;
            int curPc = ctx.pc;
            const auto &toks = tokenizedLines[curPc];
            if (toks.empty() || toks[0].type == TokType::END) { ++ctx.pc; continue; }

            if (toks.size() >= 2 && toks[1].type == TokType::COLON &&
                (toks[0].type == TokType::IDENT || toks[0].type == TokType::NUMBER) &&
                toks[2].type == TokType::END) {
                ++ctx.pc;
                continue;
            }

            if (gVerboseMode) {
                std::cout << "[TRACE Line " << std::setw(3) << sourceLineNumbers[curPc]
                          << "] " << sourceLines[curPc] << "\n";
            }

            try {
                std::string kw = (toks[0].type == TokType::IDENT) ? toUpper(toks[0].text) : "";

                if (kw == "OPTION" && toks.size() >= 2) {
                    std::string optType = toUpper(toks[1].text);
                    if (optType == "BASE" && toks.size() >= 3) {
                        optionBase = static_cast<int>(toks[2].numVal);
                    } else if (optType == "EXPLICIT") {
                        optionExplicit = true;
                    }
                    ++ctx.pc; continue;
                }

                if (kw == "IF") {
                    ExprCursor c{toks, 1};
                    Variant cond = evalExpr(c);
                    if (!c.matchIdent("THEN")) {
                        std::string bad = c.peek().text;
                        throw BasicRuntimeError(2, "Expected 'Then' after 'If' condition.",
                                                sourceLineNumbers[curPc], sourceLines[curPc],
                                                bad.empty() ? "If" : bad, "Then");
                    }

                    if (c.peek().type != TokType::END) {
                        std::vector<Token> tToks, eToks;
                        bool inE = false;
                        while (c.peek().type != TokType::END) {
                            if (c.peek().type == TokType::IDENT && toUpper(c.peek().text) == "ELSE") {
                                inE = true; c.get(); continue;
                            }
                            if (!inE) tToks.push_back(c.get());
                            else eToks.push_back(c.get());
                        }
                        const auto &chosen = cond.toBool() ? tToks : eToks;
                        if (!chosen.empty()) {
                            if (chosen.size() == 1 &&
                                (chosen[0].type == TokType::NUMBER ||
                                 (chosen[0].type == TokType::IDENT && ctx.labelMap.count(toUpper(chosen[0].text))))) {
                                jumpToLabel(ctx, chosen[0].text);
                            } else {
                                auto subStmts = splitTokensByColon(chosen);
                                for (const auto &st : subStmts) {
                                    executeSingleTokens(st, ctx, curPc);
                                    if (ctx.pcJumped || ctx.exitRoutine) break;
                                }
                            }
                        }
                        if (!ctx.pcJumped) ++ctx.pc;
                        continue;
                    }

                    int elseLn = -1;
                    std::vector<int> elseIfs;
                    int endIfLn = findMatchingEndIf(curPc, ctx.endLine, elseLn, elseIfs);
                    if (cond.toBool()) {
                        ctx.pc = curPc + 1;
                    } else {
                        bool branchFound = false;
                        for (int eLn : elseIfs) {
                            ExprCursor ec{tokenizedLines[eLn], 1};
                            Variant eCond = evalExpr(ec);
                            if (eCond.toBool()) {
                                ctx.pc = eLn + 1;
                                branchFound = true;
                                break;
                            }
                        }
                        if (!branchFound) {
                            ctx.pc = (elseLn != -1) ? (elseLn + 1) : (endIfLn + 1);
                        }
                    }
                    continue;
                }

                if (kw == "ELSEIF" || kw == "ELSE") {
                    int endIfLn = findEnclosingEndIfFromBranch(curPc, ctx.endLine);
                    ctx.pc = endIfLn + 1;
                    continue;
                }

                if (kw == "ENDIF" || (kw == "END" && toks.size() >= 2 && toUpper(toks[1].text) == "IF")) {
                    ++ctx.pc;
                    continue;
                }

                if (kw == "SELECT") {
                    if (toks.size() < 2 || toUpper(toks[1].text) != "CASE") {
                        throw BasicRuntimeError(2, "Expected 'Case' after 'Select'.",
                                                sourceLineNumbers[curPc], sourceLines[curPc],
                                                toks.size() >= 2 ? toks[1].text : "Select", "Select Case");
                    }
                    ExprCursor c{toks, 2};
                    Variant testVal = evalExpr(c);
                    std::vector<int> caseLines;
                    int caseElseLn = -1;
                    int endSelectLn = findMatchingEndSelect(curPc, ctx.endLine, caseLines, caseElseLn);

                    bool matched = false;
                    for (int cLn : caseLines) {
                        if (evalSelectCaseLine(tokenizedLines[cLn], testVal)) {
                            ctx.pc = cLn + 1;
                            matched = true;
                            break;
                        }
                    }
                    if (!matched) {
                        ctx.pc = (caseElseLn != -1) ? (caseElseLn + 1) : (endSelectLn + 1);
                    }
                    continue;
                }

                if (kw == "CASE") {
                    int endSelectLn = findEnclosingEndSelectFromCase(curPc, ctx.endLine);
                    ctx.pc = endSelectLn + 1;
                    continue;
                }

                if (kw == "ENDSELECT" || (kw == "END" && toks.size() >= 2 && toUpper(toks[1].text) == "SELECT")) {
                    ++ctx.pc;
                    continue;
                }

                if (kw == "FOR") {
                    int nextLn = findMatchingLoopEnd(curPc, ctx.endLine, "FOR", "NEXT");
                    ExprCursor c{toks, 1};
                    std::string lv = toUpper(c.get().text);
                    if (!c.match(TokType::EQ)) {
                        throw BasicRuntimeError(2, "Expected '=' after loop variable in 'For' statement.",
                                                sourceLineNumbers[curPc], sourceLines[curPc], lv, "For " + lv + " = ... To ...");
                    }
                    Variant st = evalExpr(c);
                    if (!c.matchIdent("TO")) {
                        throw BasicRuntimeError(2, "Expected 'To' in 'For' statement.",
                                                sourceLineNumbers[curPc], sourceLines[curPc], c.peek().text, "To");
                    }
                    Variant en = evalExpr(c);
                    Variant stp = c.matchIdent("STEP") ? evalExpr(c) : Variant::fromInt(1);

                    resolveVar(lv).assignCoerced(st);
                    double cur = resolveVar(lv).toDouble();
                    double lim = en.toDouble();
                    double sp = stp.toDouble();

                    while (!ctx.forStack.empty() && ctx.forStack.back().forLine >= curPc) {
                        ctx.forStack.pop_back();
                    }

                    if ((sp >= 0.0 && cur > lim + 1e-12) || (sp < 0.0 && cur < lim - 1e-12)) {
                        ctx.pc = nextLn + 1;
                    } else {
                        ctx.forStack.push_back({curPc, nextLn, lv, lim, sp});
                        ctx.pc = curPc + 1;
                    }
                    continue;
                }

                if (kw == "NEXT") {
                    if (ctx.forStack.empty()) {
                        throw BasicRuntimeError(2, "'Next' without matching 'For'.",
                                                sourceLineNumbers[curPc], sourceLines[curPc], "Next", "For ... Next");
                    }
                    ForFrame &ff = ctx.forStack.back();
                    Variant &v = resolveVar(ff.varName);
                    v.assignCoerced(Variant::fromDouble(v.toDouble() + ff.stepVal));
                    double cur = v.toDouble();
                    if ((ff.stepVal >= 0.0 && cur <= ff.limitVal + 1e-12) ||
                        (ff.stepVal < 0.0 && cur >= ff.limitVal - 1e-12)) {
                        ctx.pc = ff.forLine + 1;
                    } else {
                        int afterNext = ff.nextLine + 1;
                        ctx.forStack.pop_back();
                        ctx.pc = afterNext;
                    }
                    continue;
                }

                if (kw == "WHILE") {
                    int wendLn = findMatchingLoopEnd(curPc, ctx.endLine, "WHILE", "WEND");
                    ExprCursor c{toks, 1};
                    if (!evalExpr(c).toBool()) ctx.pc = wendLn + 1;
                    else ctx.pc = curPc + 1;
                    continue;
                }

                if (kw == "WEND") {
                    int whileLn = findMatchingLoopStart(curPc, ctx.startLine, "WHILE", "WEND");
                    ctx.pc = whileLn;
                    continue;
                }

                if (kw == "DO") {
                    int loopLn = findMatchingLoopEnd(curPc, ctx.endLine, "DO", "LOOP");
                    if (toks.size() > 2) {
                        ExprCursor c{toks, 1};
                        bool isUntil = c.matchIdent("UNTIL");
                        if (!isUntil && !c.matchIdent("WHILE")) {
                            std::string bad = c.peek().text;
                            throw BasicRuntimeError(2, "Expected 'While' or 'Until' after 'Do'.",
                                                    sourceLineNumbers[curPc], sourceLines[curPc],
                                                    bad, suggestBestMatch(bad, {"While", "Until"}));
                        }
                        bool ok = evalExpr(c).toBool();
                        if ((isUntil && ok) || (!isUntil && !ok)) {
                            ctx.pc = loopLn + 1;
                            continue;
                        }
                    }
                    ctx.pc = curPc + 1;
                    continue;
                }

                if (kw == "LOOP") {
                    int doLn = findMatchingLoopStart(curPc, ctx.startLine, "DO", "LOOP");
                    if (toks.size() > 2) {
                        ExprCursor c{toks, 1};
                        bool isUntil = c.matchIdent("UNTIL");
                        if (!isUntil && !c.matchIdent("WHILE")) {
                            std::string bad = c.peek().text;
                            throw BasicRuntimeError(2, "Expected 'While' or 'Until' after 'Loop'.",
                                                    sourceLineNumbers[curPc], sourceLines[curPc],
                                                    bad, suggestBestMatch(bad, {"While", "Until"}));
                        }
                        bool ok = evalExpr(c).toBool();
                        if ((isUntil && ok) || (!isUntil && !ok)) {
                            ctx.pc = curPc + 1;
                            continue;
                        }
                    }
                    ctx.pc = doLn;
                    continue;
                }

                executeSingleTokens(toks, ctx, curPc);
                if (!ctx.pcJumped) ++ctx.pc;

            } catch (BasicRuntimeError &bre) {
                if (bre.line <= 0) bre.line = sourceLineNumbers[curPc];
                if (bre.sourceCodeLine.empty()) bre.sourceCodeLine = sourceLines[curPc];
                handleRuntimeError(ctx, curPc, bre);
            } catch (const std::exception &ex) {
                int code = classifyExceptionToErrorCode(ex.what());
                BasicRuntimeError bre(code, ex.what(), sourceLineNumbers[curPc], sourceLines[curPc]);
                handleRuntimeError(ctx, curPc, bre);
            }
        }
    }

    void handleRuntimeError(ExecContext &ctx, int faultPc, const BasicRuntimeError &bre) {
        currentErrCode = bre.code;
        currentErrLine = bre.line;
        currentErrMsg = bre.what();

        if (ctx.errMode == ErrorMode::RESUME_NEXT) {
            if (gVerboseMode) {
                std::cout << "[VERBOSE] Error #" << bre.code << " (" << bre.what()
                          << ") handled by 'On Error Resume Next' at line " << bre.line << "\n";
            }
            ctx.pc = faultPc + 1;
            ctx.pcJumped = true;
            return;
        }
        if (ctx.errMode == ErrorMode::GOTO_LABEL && !ctx.inErrorHandler && !ctx.errLabel.empty()) {
            if (gVerboseMode) {
                std::cout << "[VERBOSE] Error #" << bre.code << " (" << bre.what()
                          << ") caught at line " << bre.line << " -> jumping to handler '" << ctx.errLabel << "'\n";
            }
            ctx.errorResumePc = faultPc;
            ctx.inErrorHandler = true;
            jumpToLabel(ctx, ctx.errLabel, true);
            return;
        }
        throw bre;
    }

    void executeSingleTokens(const std::vector<Token> &toks, ExecContext &ctx, int curPc) {
        if (toks.empty() || toks[0].type == TokType::END) return;
        std::string kw = (toks[0].type == TokType::IDENT) ? toUpper(toks[0].text) : "";

        if (kw == "WAIT") {
            ExprCursor c{toks, 1};
            int64_t ms = (c.peek().type == TokType::END) ? 0 : std::max<int64_t>(0, evalExpr(c).toInt());
            if (gVerboseMode) {
                std::cout << "[VERBOSE] Waiting " << ms << " ms...\n";
            }
            if (ms > 0) {
                std::this_thread::sleep_for(std::chrono::milliseconds(ms));
            }
            return;
        }

        if (kw == "OPTION" && toks.size() >= 2) {
            std::string optType = toUpper(toks[1].text);
            if (optType == "BASE" && toks.size() >= 3) {
                optionBase = static_cast<int>(toks[2].numVal);
            } else if (optType == "EXPLICIT") {
                optionExplicit = true;
            }
            return;
        }

        if (kw == "ERASE") {
            ExprCursor c{toks, 1};
            if (c.peek().type == TokType::END) {
                throw BasicRuntimeError(2, "Missing array variable name after 'Erase'.");
            }
            while (c.peek().type != TokType::END) {
                if (c.peek().type != TokType::IDENT) {
                    throw BasicRuntimeError(2, "Expected array variable name in 'Erase'.");
                }
                std::string rawName = c.get().text;
                Variant &arr = resolveVar(rawName);
                if (arr.type != VarType::ARRAY) {
                    throw BasicRuntimeError(13, "Type mismatch in Erase: '" + rawName + "' is not an array.");
                }
                if (arr.iVal == 1 && !arr.dims.empty()) {
                    Variant initElem;
                    initElem.declaredType = arr.declaredType;
                    if (arr.declaredType == VarType::INTEGER) initElem.type = VarType::INTEGER;
                    else if (arr.declaredType == VarType::DOUBLE) initElem.type = VarType::DOUBLE;
                    else if (arr.declaredType == VarType::STRING) initElem.type = VarType::STRING;
                    std::fill(arr.arrayData.begin(), arr.arrayData.end(), initElem);
                } else {
                    arr.dims.clear();
                    arr.arrayData.clear();
                    arr.arrayData.shrink_to_fit();
                    arr.iVal = 0;
                }
                if (!c.match(TokType::COMMA)) break;
            }
            return;
        }

        if (kw == "DIM" || kw == "REDIM" || kw == "GLOBAL" || kw == "PUBLIC" || kw == "PRIVATE") {
            ExprCursor c{toks, 1};
            bool preserve = c.matchIdent("PRESERVE");
            while (c.peek().type != TokType::END) {
                std::string rawName = c.get().text;
                std::string varName = toUpper(rawName);
                VarType suffixType = VarType::EMPTY;
                if (!varName.empty() && varName.back() == '$') {
                    suffixType = VarType::STRING;
                    varName.pop_back();
                } else if (!varName.empty() && varName.back() == '%') {
                    suffixType = VarType::INTEGER;
                    varName.pop_back();
                }

                bool hasParens = false;
                std::vector<DimBound> bounds;
                if (c.match(TokType::LPAREN)) {
                    hasParens = true;
                    while (!c.match(TokType::RPAREN) && c.peek().type != TokType::END) {
                        int v1 = static_cast<int>(evalExpr(c).toInt());
                        if (c.matchIdent("TO")) bounds.push_back({v1, static_cast<int>(evalExpr(c).toInt())});
                        else bounds.push_back({optionBase, v1});
                        c.match(TokType::COMMA);
                    }
                }

                VarType dt = c.matchIdent("AS") ? parseTypeKeyword(c.get().text) : suffixType;

                Variant *targetSlot = nullptr;
                if (kw == "GLOBAL" || kw == "PUBLIC") {
                    targetSlot = &globals[varName];
                } else if (kw == "REDIM") {
                    if (!callStack.empty() && callStack.back().count(varName)) {
                        targetSlot = &callStack.back()[varName];
                    } else if (globals.count(varName)) {
                        targetSlot = &globals[varName];
                    } else if (!callStack.empty()) {
                        targetSlot = &callStack.back()[varName];
                    } else {
                        targetSlot = &globals[varName];
                    }
                } else {
                    if (!callStack.empty()) targetSlot = &callStack.back()[varName];
                    else targetSlot = &globals[varName];
                }

                if (dt == VarType::EMPTY && targetSlot->declaredType != VarType::EMPTY) {
                    dt = targetSlot->declaredType;
                }

                Variant newVar;
                if (!bounds.empty()) {
                    if (preserve && targetSlot->type == VarType::ARRAY &&
                        !targetSlot->dims.empty() && targetSlot->dims.size() != bounds.size()) {
                        throw BasicRuntimeError(9, "Subscript out of range: cannot change number of dimensions with ReDim Preserve.");
                    }
                    newVar = Variant::makeArray(bounds, dt);
                    newVar.iVal = (kw == "REDIM") ? 0 : 1;
                    if (preserve && targetSlot->type == VarType::ARRAY &&
                        targetSlot->dims.size() == bounds.size() && !targetSlot->arrayData.empty()) {
                        std::vector<int> idx(bounds.size(), 0);
                        copyPreservedElements(*targetSlot, newVar, 0, idx);
                    }
                } else if (hasParens || kw == "REDIM") {
                    newVar.type = VarType::ARRAY;
                    newVar.declaredType = dt;
                    newVar.iVal = 0;
                } else {
                    newVar.declaredType = dt;
                    if (dt == VarType::INTEGER) newVar.type = VarType::INTEGER;
                    else if (dt == VarType::DOUBLE) newVar.type = VarType::DOUBLE;
                    else if (dt == VarType::STRING) newVar.type = VarType::STRING;
                }

                *targetSlot = std::move(newVar);
                c.match(TokType::COMMA);
            }
            return;
        }

        if (kw == "ON") {
            ExprCursor c{toks, 1};
            c.matchIdent("LOCAL");
            if (!c.matchIdent("ERROR")) {
                throw BasicRuntimeError(2, "Expected 'Error' after 'On'.",
                                        sourceLineNumbers[curPc], sourceLines[curPc], c.peek().text, "On Error GoTo ...");
            }
            if (c.matchIdent("RESUME")) {
                c.matchIdent("NEXT");
                ctx.errMode = ErrorMode::RESUME_NEXT;
                ctx.errLabel.clear();
                ctx.inErrorHandler = false;
                return;
            }
            if (c.matchIdent("GOTO")) {
                if (c.peek().type == TokType::NUMBER && static_cast<int>(c.peek().numVal) == 0) {
                    c.get();
                    ctx.errMode = ErrorMode::NONE;
                    ctx.errLabel.clear();
                    ctx.inErrorHandler = false;
                } else {
                    std::string target = c.get().text;
                    ctx.errMode = ErrorMode::GOTO_LABEL;
                    ctx.errLabel = toUpper(target);
                }
                return;
            }
            throw BasicRuntimeError(2, "Invalid 'On Error' syntax. Expected 'GoTo <label>', 'GoTo 0', or 'Resume Next'.",
                                    sourceLineNumbers[curPc], sourceLines[curPc], c.peek().text, "On Error GoTo");
        }

        if (kw == "RESUME") {
            if (!ctx.inErrorHandler || ctx.errorResumePc < 0) {
                throw BasicRuntimeError(20, getStandardErrorMessage(20));
            }
            ExprCursor c{toks, 1};
            currentErrCode = 0;
            currentErrLine = 0;
            currentErrMsg.clear();
            ctx.inErrorHandler = false;

            if (c.peek().type == TokType::END ||
                (c.peek().type == TokType::NUMBER && static_cast<int>(c.peek().numVal) == 0)) {
                ctx.pc = ctx.errorResumePc;
                ctx.pcJumped = true;
            } else if (c.matchIdent("NEXT")) {
                ctx.pc = ctx.errorResumePc + 1;
                ctx.pcJumped = true;
            } else {
                std::string target = c.get().text;
                jumpToLabel(ctx, target);
            }
            return;
        }

        if (kw == "ERROR" && toks.size() >= 2 && toks[1].type != TokType::EQ && toks[1].type != TokType::LPAREN) {
            ExprCursor c{toks, 1};
            int code = static_cast<int>(evalExpr(c).toInt());
            throw BasicRuntimeError(code, getStandardErrorMessage(code));
        }

        if (kw == "ERR.CLEAR") {
            currentErrCode = 0;
            currentErrLine = 0;
            currentErrMsg.clear();
            return;
        }
        if (kw == "ERR.RAISE") {
            ExprCursor c{toks, 1};
            bool hp = c.match(TokType::LPAREN);
            int code = static_cast<int>(evalExpr(c).toInt());
            std::string src, desc;
            if (c.match(TokType::COMMA)) {
                src = evalExpr(c).toString();
                if (c.match(TokType::COMMA)) desc = evalExpr(c).toString();
            }
            if (hp) c.match(TokType::RPAREN);
            throw BasicRuntimeError(code, desc.empty() ? getStandardErrorMessage(code) : desc);
        }

        if (kw == "GOTO") {
            if (toks.size() < 2 || toks[1].type == TokType::END) {
                throw BasicRuntimeError(2, "Missing target label after 'GoTo'.");
            }
            jumpToLabel(ctx, toks[1].text);
            return;
        }

        if (kw == "GOSUB") {
            if (toks.size() < 2 || toks[1].type == TokType::END) {
                throw BasicRuntimeError(2, "Missing target label after 'GoSub'.");
            }
            ctx.gosubStack.push_back(ctx.pc + 1);
            jumpToLabel(ctx, toks[1].text, true);
            return;
        }
        if (kw == "RETURN") {
            if (ctx.gosubStack.empty()) {
                throw BasicRuntimeError(3, getStandardErrorMessage(3));
            }
            ctx.pc = ctx.gosubStack.back();
            ctx.gosubStack.pop_back();
            ctx.pcJumped = true;
            return;
        }

        if (kw == "EXIT" && toks.size() >= 2) {
            std::string t = toUpper(toks[1].text);
            if (t == "FOR") {
                if (ctx.forStack.empty()) throw BasicRuntimeError(2, "'Exit For' outside 'For' loop.");
                int afterNext = ctx.forStack.back().nextLine + 1;
                ctx.forStack.pop_back();
                ctx.pc = afterNext;
                ctx.pcJumped = true;
                return;
            }
            if (t == "DO") {
                int loopLn = findMatchingLoopEnd(ctx.pc, ctx.endLine, "DO", "LOOP");
                ctx.pc = loopLn + 1;
                ctx.pcJumped = true;
                return;
            }
            if (t == "SUB" || t == "FUNCTION") {
                ctx.exitRoutine = true;
                return;
            }
            throw BasicRuntimeError(2, "Invalid 'Exit' target '" + toks[1].text + "'.",
                                    sourceLineNumbers[curPc], sourceLines[curPc], toks[1].text,
                                    suggestBestMatch("Exit " + toks[1].text));
        }

        if (kw == "END" && toks.size() == 2 && toks[1].type == TokType::END) {
            ctx.exitRoutine = true;
            return;
        }

        if (kw == "LSET" || kw == "RSET") {
            ExprCursor c{toks, 1};
            std::string targetVar = c.get().text;
            if (!c.match(TokType::EQ)) throw BasicRuntimeError(2, "Expected '=' in " + kw);
            std::string valStr = evalExpr(c).toString();
            Variant &dst = resolveVar(targetVar);
            size_t width = dst.toString().size();
            if (width == 0) {
                dst.assignCoerced(Variant::fromString(valStr));
            } else if (valStr.size() >= width) {
                dst.assignCoerced(Variant::fromString(valStr.substr(0, width)));
            } else {
                std::string pad(width - valStr.size(), ' ');
                dst.assignCoerced(Variant::fromString(kw == "LSET" ? (valStr + pad) : (pad + valStr)));
            }
            return;
        }

        if (kw == "RUN") {
            ExprCursor c{toks, 1};
            std::string scriptPath = evalExpr(c).toString();
            StarBasicInterpreter childInterp;
            childInterp.loadAndRunFile(scriptPath);
            return;
        }

        if (kw == "KILL" || kw == "MKDIR" || kw == "RMDIR" || kw == "CHDIR" || kw == "CHDRIVE") {
            ExprCursor c{toks, 1};
            bool hp = c.match(TokType::LPAREN);
            Variant arg1 = evalExpr(c);
            if (hp) c.match(TokType::RPAREN);
            invokeFunctionOrArray(kw, {arg1}, {""});
            return;
        }

        if (kw == "FILECOPY" || kw == "SETATTR") {
            ExprCursor c{toks, 1};
            bool hp = c.match(TokType::LPAREN);
            Variant arg1 = evalExpr(c);
            c.match(TokType::COMMA);
            Variant arg2 = evalExpr(c);
            if (hp) c.match(TokType::RPAREN);
            invokeFunctionOrArray(kw, {arg1, arg2}, {"", ""});
            return;
        }

        if (kw == "NAME") {
            ExprCursor c{toks, 1};
            std::string oldPath = evalExpr(c).toString();
            if (!c.matchIdent("AS")) {
                throw BasicRuntimeError(2, "Expected 'As' in 'Name oldpath As newpath' statement.");
            }
            std::string newPath = evalExpr(c).toString();
            std::error_code ec;
            if (!std::filesystem::exists(oldPath, ec))
                throw BasicRuntimeError(53, "File not found: " + oldPath);
            if (std::filesystem::exists(newPath, ec))
                throw BasicRuntimeError(58, "File already exists: " + newPath);
            std::filesystem::rename(oldPath, newPath, ec);
            if (ec) throw BasicRuntimeError(75, "Path/File access error renaming '" + oldPath + "' to '" + newPath + "'.");
            return;
        }

        if (kw == "SEEK") {
            ExprCursor c{toks, 1};
            bool hp = c.match(TokType::LPAREN);
            c.match(TokType::HASH);
            Variant chVar = evalExpr(c);
            if (c.match(TokType::COMMA)) {
                Variant posVar = evalExpr(c);
                if (hp) c.match(TokType::RPAREN);
                invokeFunctionOrArray("SEEK", {chVar, posVar}, {"", ""});
            } else {
                if (hp) c.match(TokType::RPAREN);
            }
            return;
        }

        if (kw == "LOCK" || kw == "UNLOCK") {
            ExprCursor c{toks, 1};
            c.match(TokType::HASH);
            int ch = static_cast<int>(evalExpr(c).toInt());
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel #" + std::to_string(ch) + " is not open.");
            while (c.peek().type != TokType::END) c.get();
            it->second.isLocked = (kw == "LOCK");
            return;
        }

        if (kw == "INPUT") {
            ExprCursor c{toks, 1};
            bool hp = c.match(TokType::LPAREN);
            if (c.match(TokType::HASH) || (hp && c.peek().type != TokType::STRING)) {
                c.match(TokType::HASH);
                int ch = static_cast<int>(evalExpr(c).toInt());
                auto it = openFiles.find(ch);
                if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel #" + std::to_string(ch) + " is not open.");
                auto &st = it->second.stream;
                st.clear();
                while (c.match(TokType::COMMA)) {
                    std::string vn = c.get().text;
                    std::vector<int> idx;
                    if (c.match(TokType::LPAREN)) {
                        while (!c.match(TokType::RPAREN) && c.peek().type != TokType::END) {
                            idx.push_back(static_cast<int>(evalExpr(c).toInt()));
                            c.match(TokType::COMMA);
                        }
                    }
                    while (st.good() && (st.peek() == ' ' || st.peek() == '\t' || st.peek() == '\r' || st.peek() == '\n')) {
                        st.get();
                    }
                    if (st.peek() == std::char_traits<char>::eof()) {
                        throw BasicRuntimeError(62, getStandardErrorMessage(62));
                    }
                    std::string valStr;
                    bool wasQuoted = false;
                    if (st.peek() == '"') {
                        wasQuoted = true;
                        st.get();
                        while (st.good()) {
                            int chIn = st.get();
                            if (chIn == std::char_traits<char>::eof() || chIn == '"') break;
                            valStr.push_back(static_cast<char>(chIn));
                        }
                        while (st.good() && (st.peek() == ' ' || st.peek() == '\t')) st.get();
                        if (st.peek() == ',') {
                            st.get();
                        } else if (st.peek() == '\r') {
                            st.get();
                            if (st.peek() == '\n') st.get();
                        } else if (st.peek() == '\n') {
                            st.get();
                        }
                    } else {
                        while (st.good()) {
                            int chIn = st.peek();
                            if (chIn == std::char_traits<char>::eof()) break;
                            if (chIn == ',') { st.get(); break; }
                            if (chIn == '\r') { st.get(); if (st.peek() == '\n') st.get(); break; }
                            if (chIn == '\n') { st.get(); break; }
                            valStr.push_back(static_cast<char>(st.get()));
                        }
                        valStr = trim(valStr);
                    }
                    Variant &dst = idx.empty() ? resolveVar(vn) : resolveVar(vn).at(idx);
                    bool assignedNumeric = false;
                    if (!wasQuoted && dst.declaredType == VarType::EMPTY) {
                        try {
                            size_t pIdx = 0;
                            double d = std::stod(valStr, &pIdx);
                            if (pIdx == valStr.size()) {
                                if (valStr.find('.') == std::string::npos && valStr.find('e') == std::string::npos && valStr.find('E') == std::string::npos)
                                    dst.assignCoerced(Variant::fromInt(static_cast<int64_t>(d)));
                                else
                                    dst.assignCoerced(Variant::fromDouble(d));
                                assignedNumeric = true;
                            }
                        } catch (...) {}
                    }
                    if (!assignedNumeric) dst.assignCoerced(Variant::fromString(valStr));
                }
                if (hp) c.match(TokType::RPAREN);
                return;
            } else {
                // Console Input ["prompt";|,] var
                if (c.peek().type == TokType::STRING) {
                    std::cout << c.get().text;
                    if (!c.match(TokType::SEMICOLON)) c.match(TokType::COMMA);
                }
                while (c.peek().type != TokType::END) {
                    std::string vn = c.get().text;
                    std::string lineIn;
                    std::getline(std::cin, lineIn);
                    resolveVar(vn).assignCoerced(Variant::fromString(trim(lineIn)));
                    if (!c.match(TokType::COMMA)) break;
                }
                return;
            }
        }

        if (kw == "PUT" || kw == "GET") {
            ExprCursor c{toks, 1};
            c.match(TokType::HASH);
            int ch = static_cast<int>(evalExpr(c).toInt());
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel #" + std::to_string(ch) + " is not open.");
            auto &fe = it->second;
            fe.stream.clear();

            if (!c.match(TokType::COMMA)) throw BasicRuntimeError(2, "Expected ',' after channel in " + kw);
            int64_t pos = -1;
            if (c.peek().type != TokType::COMMA) {
                pos = evalExpr(c).toInt();
                if (pos < 1) throw BasicRuntimeError(5, "Invalid procedure call: position in " + kw + " must be >= 1.");
            }
            if (!c.match(TokType::COMMA)) throw BasicRuntimeError(2, "Expected ',' before variable in " + kw);

            std::string vn = c.get().text;
            std::vector<int> idx;
            if (c.match(TokType::LPAREN)) {
                while (!c.match(TokType::RPAREN) && c.peek().type != TokType::END) {
                    idx.push_back(static_cast<int>(evalExpr(c).toInt()));
                    c.match(TokType::COMMA);
                }
            }
            Variant &target = idx.empty() ? resolveVar(vn) : resolveVar(vn).at(idx);

            if (pos >= 1) {
                std::streamoff byteOff = (fe.mode == "RANDOM")
                                         ? static_cast<std::streamoff>((pos - 1) * fe.recLen)
                                         : static_cast<std::streamoff>(pos - 1);
                if (kw == "PUT") fe.stream.seekp(byteOff, std::ios::beg);
                else fe.stream.seekg(byteOff, std::ios::beg);
            }

            if (kw == "PUT") {
                VarType effType = (target.declaredType != VarType::EMPTY) ? target.declaredType : target.type;
                if (effType == VarType::INTEGER) {
                    int64_t v = target.toInt();
                    fe.stream.write(reinterpret_cast<const char*>(&v), sizeof(int64_t));
                } else if (effType == VarType::DOUBLE) {
                    double v = target.toDouble();
                    fe.stream.write(reinterpret_cast<const char*>(&v), sizeof(double));
                } else {
                    std::string s = target.toString();
                    if (fe.mode == "RANDOM") {
                        uint32_t len = static_cast<uint32_t>(s.size());
                        fe.stream.write(reinterpret_cast<const char*>(&len), sizeof(uint32_t));
                    }
                    if (!s.empty()) fe.stream.write(s.data(), static_cast<std::streamsize>(s.size()));
                }
                fe.stream.flush();
            } else {
                VarType effType = (target.declaredType != VarType::EMPTY) ? target.declaredType : target.type;
                if (effType == VarType::INTEGER) {
                    int64_t v = 0;
                    fe.stream.read(reinterpret_cast<char*>(&v), sizeof(int64_t));
                    target.assignCoerced(Variant::fromInt(v));
                } else if (effType == VarType::DOUBLE) {
                    double v = 0.0;
                    fe.stream.read(reinterpret_cast<char*>(&v), sizeof(double));
                    target.assignCoerced(Variant::fromDouble(v));
                } else {
                    if (fe.mode == "RANDOM") {
                        uint32_t len = 0;
                        fe.stream.read(reinterpret_cast<char*>(&len), sizeof(uint32_t));
                        std::string s(len, '\0');
                        if (len > 0) fe.stream.read(&s[0], static_cast<std::streamsize>(len));
                        target.assignCoerced(Variant::fromString(s));
                    } else {
                        size_t readLen = target.sVal.size();
                        if (readLen > 0) {
                            std::string s(readLen, '\0');
                            fe.stream.read(&s[0], static_cast<std::streamsize>(readLen));
                            s.resize(static_cast<size_t>(fe.stream.gcount()));
                            target.assignCoerced(Variant::fromString(s));
                        } else {
                            std::ostringstream oss;
                            oss << fe.stream.rdbuf();
                            target.assignCoerced(Variant::fromString(oss.str()));
                        }
                    }
                }
            }
            return;
        }

        if (kw == "OPEN") {
            ExprCursor c{toks, 1};
            std::string fn = evalExpr(c).toString();
            std::string mode = "RANDOM";
            if (c.matchIdent("FOR")) {
                mode = toUpper(c.get().text);
                if (mode == "BINARY" || mode == "RANDOM" || mode == "INPUT" || mode == "OUTPUT" || mode == "APPEND") {
                    while (c.peek().type == TokType::IDENT && toUpper(c.peek().text) != "AS") {
                        c.get();
                    }
                }
            }
            c.matchIdent("AS");
            c.match(TokType::HASH);
            int ch = static_cast<int>(evalExpr(c).toInt());
            int recLen = 128;
            if (c.matchIdent("LEN")) {
                c.match(TokType::EQ);
                recLen = std::max(1, static_cast<int>(evalExpr(c).toInt()));
            }
            if (openFiles.count(ch)) throw BasicRuntimeError(55, "File already open on channel #" + std::to_string(ch));
            FileEntry fe;
            fe.mode = mode;
            fe.recLen = recLen;
            std::ios_base::openmode om;
            if (mode == "OUTPUT") {
                om = std::ios::out | std::ios::trunc;
            } else if (mode == "APPEND") {
                om = std::ios::out | std::ios::app;
            } else if (mode == "BINARY" || mode == "RANDOM") {
                if (!fileExists(fn)) {
                    std::ofstream createFile(fn, std::ios::out | std::ios::binary);
                }
                om = std::ios::in | std::ios::out | std::ios::binary;
            } else {
                om = std::ios::in;
            }
            fe.stream.open(fn, om);
            if (!fe.stream.is_open()) throw BasicRuntimeError(53, "File not found or cannot open: " + fn);
            openFiles[ch] = std::move(fe);
            return;
        }

        if (kw == "CLOSE") {
            ExprCursor c{toks, 1};
            if (c.peek().type == TokType::END) openFiles.clear();
            else {
                while (c.peek().type != TokType::END) {
                    c.match(TokType::HASH);
                    openFiles.erase(static_cast<int>(evalExpr(c).toInt()));
                    c.match(TokType::COMMA);
                }
            }
            return;
        }

        if (kw == "LINE" && toks.size() >= 2 && toUpper(toks[1].text) == "INPUT") {
            ExprCursor c{toks, 2};
            c.match(TokType::HASH);
            int ch = static_cast<int>(evalExpr(c).toInt());
            c.match(TokType::COMMA);
            std::string vn = c.get().text;
            std::vector<int> idx;
            if (c.match(TokType::LPAREN)) {
                while (!c.match(TokType::RPAREN)) {
                    idx.push_back(static_cast<int>(evalExpr(c).toInt()));
                    c.match(TokType::COMMA);
                }
            }
            auto it = openFiles.find(ch);
            if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel #" + std::to_string(ch) + " is not open.");
            if (it->second.stream.peek() == std::char_traits<char>::eof()) {
                throw BasicRuntimeError(62, getStandardErrorMessage(62));
            }
            std::string ln;
            std::getline(it->second.stream, ln);
            if (!ln.empty() && ln.back() == '\r') ln.pop_back();
            if (idx.empty()) resolveVar(vn).assignCoerced(Variant::fromString(ln));
            else resolveVar(vn).at(idx).assignCoerced(Variant::fromString(ln));
            return;
        }

        if (kw == "PRINT" || kw == "WRITE") {
            ExprCursor c{toks, 1};
            std::ostream *out = &std::cout;
            int localCol = 0;
            int *colPtr = &localCol;

            if (c.match(TokType::HASH)) {
                int ch = static_cast<int>(evalExpr(c).toInt());
                auto it = openFiles.find(ch);
                if (it == openFiles.end()) throw BasicRuntimeError(52, "File channel #" + std::to_string(ch) + " is not open.");
                out = &it->second.stream;
                colPtr = &it->second.colPos;
                if (!c.match(TokType::COMMA)) c.match(TokType::SEMICOLON);
            }

            if (kw == "WRITE") {
                bool firstItem = true;
                while (c.peek().type != TokType::END) {
                    if (c.match(TokType::COMMA) || c.match(TokType::SEMICOLON)) continue;
                    if (!firstItem) *out << ",";
                    firstItem = false;
                    Variant v = evalExpr(c);
                    if (v.type == VarType::STRING) *out << "\"" << v.sVal << "\"";
                    else *out << v.toString();
                }
                *out << "\n";
                *colPtr = 0;
                return;
            }

            bool noNl = false;
            while (c.peek().type != TokType::END) {
                if (c.match(TokType::SEMICOLON)) { noNl = true; continue; }
                if (c.match(TokType::COMMA)) {
                    *out << "\t";
                    *colPtr += 8 - (*colPtr % 8);
                    noNl = true;
                    continue;
                }
                if (c.peek().type == TokType::IDENT && toUpper(c.peek().text) == "TAB" &&
                    c.pos + 1 < c.toks.size() && c.toks[c.pos + 1].type == TokType::LPAREN) {
                    c.get();
                    c.get();
                    int targetCol = std::max(1, static_cast<int>(evalExpr(c).toInt())) - 1;
                    c.match(TokType::RPAREN);
                    if (targetCol > *colPtr) {
                        std::string pad(static_cast<size_t>(targetCol - *colPtr), ' ');
                        *out << pad;
                        *colPtr = targetCol;
                    }
                    noNl = false;
                    continue;
                }
                std::string piece = evalExpr(c).toString();
                *out << piece;
                *colPtr += static_cast<int>(piece.size());
                noNl = false;
            }
            if (!noNl) {
                *out << "\n";
                *colPtr = 0;
            }
            return;
        }

        if (kw == "CALL") {
            ExprCursor c{toks, 1};
            evalExpr(c);
            return;
        }

        int pDepth = 0, eqPos = -1;
        for (size_t i = 0; i < toks.size(); ++i) {
            if (toks[i].type == TokType::LPAREN) ++pDepth;
            else if (toks[i].type == TokType::RPAREN) --pDepth;
            else if (toks[i].type == TokType::EQ && pDepth == 0) { eqPos = static_cast<int>(i); break; }
        }
        if (eqPos > 0) {
            std::string lhs = toUpper(toks[0].text);
            if ((lhs == "DATE" || lhs == "DATE$") && eqPos == 1) {
                ExprCursor rc{toks, static_cast<size_t>(eqPos + 1)};
                DateTimeUtil::setSystemDate(evalExpr(rc));
                return;
            }
            if ((lhs == "TIME" || lhs == "TIME$") && eqPos == 1) {
                ExprCursor rc{toks, static_cast<size_t>(eqPos + 1)};
                DateTimeUtil::setSystemTime(evalExpr(rc));
                return;
            }
            if ((lhs == "ERR" || lhs == "ERR.NUMBER") && eqPos == 1) {
                ExprCursor rc{toks, static_cast<size_t>(eqPos + 1)};
                currentErrCode = static_cast<int>(evalExpr(rc).toInt());
                if (currentErrCode == 0) {
                    currentErrLine = 0;
                    currentErrMsg.clear();
                } else {
                    currentErrMsg = getStandardErrorMessage(currentErrCode);
                }
                return;
            }
            if ((lhs == "MID" || lhs == "MID$") && eqPos > 1) {
                ExprCursor lc{toks, 1};
                lc.match(TokType::LPAREN);
                std::string targetVar = lc.get().text;
                lc.match(TokType::COMMA);
                int64_t startPos = evalExpr(lc).toInt();
                int64_t maxLen = -1;
                if (lc.match(TokType::COMMA)) maxLen = evalExpr(lc).toInt();
                lc.match(TokType::RPAREN);

                ExprCursor rc{toks, static_cast<size_t>(eqPos + 1)};
                std::string repl = evalExpr(rc).toString();
                std::string orig = resolveVar(targetVar).toString();
                if (startPos >= 1 && static_cast<size_t>(startPos - 1) < orig.size()) {
                    size_t st = static_cast<size_t>(startPos - 1);
                    size_t avail = orig.size() - st;
                    size_t n = (maxLen >= 0) ? std::min<size_t>(avail, static_cast<size_t>(maxLen)) : avail;
                    n = std::min<size_t>(n, repl.size());
                    orig.replace(st, n, repl.substr(0, n));
                    resolveVar(targetVar).assignCoerced(Variant::fromString(orig));
                }
                return;
            }

            ExprCursor rc{toks, static_cast<size_t>(eqPos + 1)};
            Variant rVal = evalExpr(rc);
            if (eqPos == 1) {
                resolveVar(lhs).assignCoerced(rVal);
                recordVarFormula(lhs, toks, static_cast<size_t>(eqPos + 1));
            }
            else {
                ExprCursor lc{toks, 1};
                lc.match(TokType::LPAREN);
                std::vector<int> idx;
                while (!lc.match(TokType::RPAREN)) {
                    idx.push_back(static_cast<int>(evalExpr(lc).toInt()));
                    lc.match(TokType::COMMA);
                }
                resolveVar(lhs).at(idx).assignCoerced(rVal);
            }
            return;
        }

        if (toks[0].type == TokType::IDENT) {
            std::string rawCmd = toks[0].text;
            std::string subName = toUpper(rawCmd);
            ExprCursor c{toks, 1};
            std::vector<Variant> args;
            std::vector<std::string> refs;
            bool hp = c.match(TokType::LPAREN);
            while (c.peek().type != TokType::END && !(hp && c.peek().type == TokType::RPAREN)) {
                if (c.match(TokType::HASH)) {}
                size_t st = c.pos;
                args.push_back(evalExpr(c));
                refs.push_back((c.pos == st + 1 && c.toks[st].type == TokType::IDENT) ? c.toks[st].text : "");
                if (!c.match(TokType::COMMA)) c.match(TokType::SEMICOLON);
            }
            if (hp) c.match(TokType::RPAREN);
            invokeFunctionOrArray(subName, args, refs);
        }
    }

public:
    void loadAndRun(const std::string &code) {
        std::srand(static_cast<unsigned int>(std::time(nullptr)));
        optionBase = 0;
        optionExplicit = false;
        varFormulas.clear();
        formulaOrder.clear();
        LibOffice::syncGlobals(&globals);
        preprocessSource(code);

        std::vector<bool> insideRoutine(tokenizedLines.size(), false);
        for (size_t i = 0; i < tokenizedLines.size(); ++i) {
            const auto &toks = tokenizedLines[i];
            if (toks.empty() || toks[0].type != TokType::IDENT) continue;
            std::string kw = toUpper(toks[0].text);
            if (kw == "SUB" || kw == "FUNCTION") {
                RoutineDecl r;
                r.isFunction = (kw == "FUNCTION");
                r.name = toUpper(toks[1].text);
                r.startLine = static_cast<int>(i);
                ExprCursor c{toks, 2};
                if (c.match(TokType::LPAREN)) {
                    while (!c.match(TokType::RPAREN) && c.peek().type != TokType::END) {
                        ParamDecl p;
                        if (c.matchIdent("BYVAL")) p.byVal = true;
                        else if (c.matchIdent("BYREF")) p.byVal = false;
                        p.name = c.get().text;
                        if (c.match(TokType::LPAREN)) c.match(TokType::RPAREN);
                        if (c.matchIdent("AS")) p.type = parseTypeKeyword(c.get().text);
                        r.params.push_back(p);
                        c.match(TokType::COMMA);
                    }
                }
                if (r.isFunction && c.matchIdent("AS")) r.returnType = parseTypeKeyword(c.get().text);
                size_t j = i + 1;
                for (; j < tokenizedLines.size(); ++j) {
                    const auto &jt = tokenizedLines[j];
                    if (jt.size() >= 2 && toUpper(jt[0].text) == "END" && toUpper(jt[1].text) == kw) {
                        r.endLine = static_cast<int>(j);
                        break;
                    }
                }
                if (j == tokenizedLines.size()) {
                    throw BasicRuntimeError(2, "Missing 'End " + kw + "' for '" + r.name + "'.",
                                            sourceLineNumbers[i], sourceLines[i], kw, "End " + kw);
                }
                for (size_t k = i; k <= j; ++k) insideRoutine[k] = true;
                routines[r.name] = r;
                i = j;
            }
        }

        bool topLevelRan = false;
        std::vector<std::string> topLevelSrc;
        std::vector<std::vector<Token>> topLevelTokens;
        std::vector<int> topLevelLineNums;
        for (size_t i = 0; i < tokenizedLines.size(); ++i) {
            if (!insideRoutine[i] && !tokenizedLines[i].empty() && tokenizedLines[i][0].type != TokType::END) {
                std::string kw = toUpper(tokenizedLines[i][0].text);
                if (kw != "OPTION" && kw != "DIM" && kw != "GLOBAL" && kw != "PUBLIC" && kw != "PRIVATE") topLevelRan = true;
                topLevelSrc.push_back(sourceLines[i]);
                topLevelTokens.push_back(tokenizedLines[i]);
                topLevelLineNums.push_back(sourceLineNumbers[i]);
            }
        }
        if (!topLevelTokens.empty()) {
            int baseLine = static_cast<int>(tokenizedLines.size());
            for (size_t i = 0; i < topLevelTokens.size(); ++i) {
                sourceLines.push_back(std::move(topLevelSrc[i]));
                tokenizedLines.push_back(std::move(topLevelTokens[i]));
                sourceLineNumbers.push_back(topLevelLineNums[i]);
            }
            int endTopLine = static_cast<int>(tokenizedLines.size());
            executeBlock(baseLine, endTopLine);
        }
        if (!topLevelRan && routines.count("MAIN")) {
            executeRoutine(routines["MAIN"], {}, {});
        }
    }

    void loadAndRunFile(const std::string &filepath) {
        std::string sourceCode = readSourceFile(filepath);
        loadAndRun(sourceCode);
    }

    static std::string formatReplValue(const Variant &v) {
        if (v.type == VarType::INTEGER) return std::to_string(v.iVal);
        if (v.type == VarType::DOUBLE) {
            if (std::fabs(v.dVal - std::llround(v.dVal)) < 1e-12) {
                return std::to_string(static_cast<int64_t>(std::llround(v.dVal)));
            }
            double rounded = std::round(v.dVal * 1000.0) / 1000.0;
            if (rounded == 0.0 && v.dVal != 0.0) {
                return v.toString();
            }
            std::ostringstream oss;
            oss << std::fixed << std::setprecision(3) << rounded;
            std::string s = oss.str();
            while (!s.empty() && s.back() == '0') s.pop_back();
            if (!s.empty() && s.back() == '.') s.pop_back();
            return s;
        }
        return v.toString();
    }

    static void printReplBox(const std::string &content, const std::string &title = "sBasic") {
        std::string border(std::max<size_t>(44, std::max(title.size(), content.size()) + 6), '=');
        std::cout << "+" << border << "+\n"
                  << "| [" << title << "]\n"
                  << "| " << content << "\n"
                  << "+" << border << "+\n";
    }

    static bool isReplExitCommand(const std::string &s) {
        std::string u = toUpper(trim(s));
        return (u == "QUIT" || u == "QUITE" || u == "Q" || u == "EXIT");
    }

    static bool isSimpleLiteralTokens(const std::vector<Token> &toks, size_t startIdx) {
        size_t rem = 0;
        for (size_t i = startIdx; i < toks.size() && toks[i].type != TokType::END; ++i) ++rem;
        if (rem == 1) {
            TokType t = toks[startIdx].type;
            if (t == TokType::NUMBER || t == TokType::STRING) return true;
            if (t == TokType::IDENT) {
                std::string u = toUpper(toks[startIdx].text);
                return (u == "TRUE" || u == "FALSE");
            }
        } else if (rem == 2) {
            TokType t0 = toks[startIdx].type;
            TokType t1 = toks[startIdx + 1].type;
            if ((t0 == TokType::PLUS || t0 == TokType::MINUS) && t1 == TokType::NUMBER) return true;
        }
        return false;
    }

    static bool isStatementKeywordForRepl(const std::string &u) {
        static const std::unordered_set<std::string> stmtKws = {
            "PRINT", "WRITE", "LINE", "INPUT", "OPEN", "CLOSE", "DIM", "REDIM",
            "GLOBAL", "PUBLIC", "PRIVATE", "OPTION", "SUB", "FUNCTION", "IF",
            "SELECT", "FOR", "NEXT", "DO", "LOOP", "WHILE", "WEND", "EXIT",
            "GOTO", "GOSUB", "RETURN", "ON", "RESUME", "ERROR", "ERR.CLEAR",
            "ERR.RAISE", "WAIT", "RUN", "CALL", "LSET", "RSET", "END", "REM",
            "ERASE", "KILL", "NAME", "FILECOPY", "MKDIR", "RMDIR", "CHDIR",
            "CHDRIVE", "SETATTR", "SEEK", "PUT", "GET", "LOCK", "UNLOCK", "MSGBOX"
        };
        return stmtKws.count(u) > 0;
    }

    void executeInteractiveChunk(const std::string &chunk, int baseRawLineNo) {
        size_t prevSize = tokenizedLines.size();

        std::istringstream iss(chunk);
        std::string rawLine;
        int offset = 0;
        while (std::getline(iss, rawLine)) {
            int lineNo = baseRawLineNo + (offset++);
            std::string stripped;
            bool inQuotes = false;
            for (char c : rawLine) {
                if (c == '"') inQuotes = !inQuotes;
                if (!inQuotes && c == '\'') break;
                stripped += c;
            }
            stripped = trim(stripped);
            if (stripped.empty()) continue;

            if (isSingleLineIfStatement(stripped, lineNo)) {
                addLogicalSegment(stripped, lineNo);
                continue;
            }

            std::vector<std::string> parts;
            std::string current;
            inQuotes = false;
            for (char c : stripped) {
                if (c == '"') inQuotes = !inQuotes;
                if (!inQuotes && c == ':') {
                    std::string cand = trim(current);
                    std::vector<Token> cToks = tokenizeLine(cand, lineNo);
                    if (parts.empty() && cToks.size() == 2 &&
                        ((cToks[0].type == TokType::IDENT && !isReservedNonLabelKeyword(toUpper(cToks[0].text))) ||
                         (cToks[0].type == TokType::NUMBER && !cToks[0].isFloat))) {
                        parts.push_back(cand + ":");
                    } else if (!cand.empty()) {
                        parts.push_back(cand);
                    }
                    current.clear();
                } else {
                    current += c;
                }
            }
            if (!trim(current).empty()) parts.push_back(trim(current));
            for (const auto &p : parts) addLogicalSegment(p, lineNo);
        }

        size_t newSize = tokenizedLines.size();
        if (newSize <= prevSize) return;

        std::vector<bool> insideRoutine(newSize, false);
        for (size_t i = prevSize; i < newSize; ++i) {
            const auto &toks = tokenizedLines[i];
            if (toks.empty() || toks[0].type != TokType::IDENT) continue;
            std::string kw = toUpper(toks[0].text);
            if (kw == "SUB" || kw == "FUNCTION") {
                RoutineDecl r;
                r.isFunction = (kw == "FUNCTION");
                r.name = toUpper(toks[1].text);
                r.startLine = static_cast<int>(i);
                ExprCursor c{toks, 2};
                if (c.match(TokType::LPAREN)) {
                    while (!c.match(TokType::RPAREN) && c.peek().type != TokType::END) {
                        ParamDecl p;
                        if (c.matchIdent("BYVAL")) p.byVal = true;
                        else if (c.matchIdent("BYREF")) p.byVal = false;
                        p.name = c.get().text;
                        if (c.match(TokType::LPAREN)) c.match(TokType::RPAREN);
                        if (c.matchIdent("AS")) p.type = parseTypeKeyword(c.get().text);
                        r.params.push_back(p);
                        c.match(TokType::COMMA);
                    }
                }
                if (r.isFunction && c.matchIdent("AS")) r.returnType = parseTypeKeyword(c.get().text);
                size_t j = i + 1;
                for (; j < newSize; ++j) {
                    const auto &jt = tokenizedLines[j];
                    if (jt.size() >= 2 && toUpper(jt[0].text) == "END" && toUpper(jt[1].text) == kw) {
                        r.endLine = static_cast<int>(j);
                        break;
                    }
                }
                if (j == newSize) {
                    throw BasicRuntimeError(2, "Missing 'End " + kw + "' for '" + r.name + "'.",
                                            sourceLineNumbers[i], sourceLines[i], kw, "End " + kw);
                }
                for (size_t k = i; k <= j; ++k) insideRoutine[k] = true;
                routines[r.name] = r;
                i = j;
            }
        }

        std::vector<std::string> topLevelSrc;
        std::vector<std::vector<Token>> topLevelTokens;
        std::vector<int> topLevelLineNums;
        for (size_t i = prevSize; i < newSize; ++i) {
            if (!insideRoutine[i] && !tokenizedLines[i].empty() && tokenizedLines[i][0].type != TokType::END) {
                topLevelSrc.push_back(sourceLines[i]);
                topLevelTokens.push_back(tokenizedLines[i]);
                topLevelLineNums.push_back(sourceLineNumbers[i]);
            }
        }
        if (!topLevelTokens.empty()) {
            int baseLine = static_cast<int>(tokenizedLines.size());
            for (size_t i = 0; i < topLevelTokens.size(); ++i) {
                sourceLines.push_back(std::move(topLevelSrc[i]));
                tokenizedLines.push_back(std::move(topLevelTokens[i]));
                sourceLineNumbers.push_back(topLevelLineNums[i]);
            }
            int endTopLine = static_cast<int>(tokenizedLines.size());
            executeBlock(baseLine, endTopLine);
        }
    }

    void runInteractiveSession() {
        std::srand(static_cast<unsigned int>(std::time(nullptr)));
        LibOffice::syncGlobals(&globals);

        std::cout << ">start coding below or quit>\n";

        int replLineNo = 0;
        std::string accumulatedLine;
        std::string multiLineBlock;
        int blockDepth = 0;

        while (true) {
            std::cout << ">";
            std::cout.flush();

            std::string rawLine;
            if (!std::getline(std::cin, rawLine)) {
                std::cout << "\n";
                break;
            }
            ++replLineNo;

            std::string t = trim(rawLine);
            if (!t.empty() && t.back() == '_') {
                accumulatedLine += t.substr(0, t.size() - 1) + " ";
                continue;
            }
            std::string fullLine = trim(accumulatedLine + t);
            accumulatedLine.clear();

            if (fullLine.empty()) continue;
            if (blockDepth == 0 && isReplExitCommand(fullLine)) break;

            try {
                std::vector<Token> toks = tokenizeLine(fullLine, replLineNo);
                if (toks.empty() || toks[0].type == TokType::END) continue;

                std::string kw = (toks[0].type == TokType::IDENT) ? toUpper(toks[0].text) : "";
                std::string kw2 = (toks.size() >= 2 && toks[1].type == TokType::IDENT) ? toUpper(toks[1].text) : "";

                if (kw == "SUB" || kw == "FUNCTION" || kw == "FOR" || kw == "WHILE" || kw == "DO" ||
                    (kw == "SELECT" && kw2 == "CASE") || isMultiLineIf(toks)) {
                    ++blockDepth;
                } else if (blockDepth > 0) {
                    if (kw == "NEXT" || kw == "WEND" || kw == "LOOP" || kw == "ENDIF" || kw == "ENDSELECT" ||
                        (kw == "END" && (kw2 == "SUB" || kw2 == "FUNCTION" || kw2 == "IF" || kw2 == "SELECT"))) {
                        --blockDepth;
                    }
                }

                if (blockDepth > 0) {
                    multiLineBlock += fullLine + "\n";
                    continue;
                }
                if (!multiLineBlock.empty()) {
                    multiLineBlock += fullLine + "\n";
                    std::string chunkToRun = std::move(multiLineBlock);
                    multiLineBlock.clear();
                    executeInteractiveChunk(chunkToRun, replLineNo);
                    continue;
                }

                bool hasTopLevelColon = false;
                int pDepth = 0, eqPos = -1;
                for (size_t i = 0; i < toks.size() && toks[i].type != TokType::END; ++i) {
                    if (toks[i].type == TokType::LPAREN) ++pDepth;
                    else if (toks[i].type == TokType::RPAREN) --pDepth;
                    else if (pDepth == 0) {
                        if (toks[i].type == TokType::COLON) hasTopLevelColon = true;
                        else if (toks[i].type == TokType::EQ && eqPos < 0) eqPos = static_cast<int>(i);
                    }
                }

                if (hasTopLevelColon || isSingleLineIfStatement(fullLine, replLineNo)) {
                    executeInteractiveChunk(fullLine, replLineNo);
                    continue;
                }

                if (eqPos > 0 && !isStatementKeywordForRepl(kw)) {
                    executeInteractiveChunk(fullLine, replLineNo);
                    if (kw != "DATE" && kw != "DATE$" && kw != "TIME" && kw != "TIME$" &&
                        kw != "ERR" && kw != "ERR.NUMBER" && kw != "MID" && kw != "MID$" &&
                        !isSimpleLiteralTokens(toks, static_cast<size_t>(eqPos + 1))) {
                        std::vector<Token> lhsToks(toks.begin(), toks.begin() + eqPos);
                        lhsToks.push_back({TokType::END, ""});
                        ExprCursor lc{lhsToks, 0};
                        Variant val = evalExpr(lc);

                        size_t eqCharPos = fullLine.find('=');
                        std::string lhsText = (eqCharPos != std::string::npos)
                                              ? trim(fullLine.substr(0, eqCharPos))
                                              : toks[0].text;
                        printReplBox(lhsText + "=" + formatReplValue(val));
                    }
                    continue;
                }

                bool isKnownSub = (routines.count(kw) && !routines[kw].isFunction);
                if (isStatementKeywordForRepl(kw) || isKnownSub) {
                    executeInteractiveChunk(fullLine, replLineNo);
                    continue;
                }

                ExprCursor ec{toks, 0};
                Variant val = evalExpr(ec);
                if (ec.peek().type == TokType::END) {
                    if (val.type != VarType::EMPTY) {
                        printReplBox(fullLine + "=" + formatReplValue(val));
                    }
                } else {
                    executeInteractiveChunk(fullLine, replLineNo);
                }

            } catch (BasicRuntimeError &bre) {
                if (bre.line <= 0) bre.line = replLineNo;
                if (bre.sourceCodeLine.empty()) bre.sourceCodeLine = fullLine;
                printDiagnosticReport(bre);
                multiLineBlock.clear();
                blockDepth = 0;
            } catch (const std::exception &ex) {
                int code = classifyExceptionToErrorCode(ex.what());
                BasicRuntimeError bre(code, ex.what(), replLineNo, fullLine);
                printDiagnosticReport(bre);
                multiLineBlock.clear();
                blockDepth = 0;
            }
        }
    }
};

static void printGplBanner() {
    std::cout << "//*      This program is free software; you can redistribute it and/or      */\n"
              << "//*      modify it under the terms of the GNU General Public License as     */\n"
              << "//*      published by the Free Software Foundation; version 2 of            */\n"
              << "//*      the License.                                                       */\n"
              << "//*                                                                         */\n"
              << "//*      This program is distributed in the hope that it will be useful,    */\n"
              << "//*      but WITHOUT ANY WARRANTY; without even the implied warranty of     */\n"
              << "//*      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the       */\n"
              << "//*      GNU General Public License for more details.                       */\n"
              << "//*      https://sourceforge.net/projects/somewhatbasic/                    */\n";
}

static void printHelp(const std::string &rawTopic = "") {
    std::string topic = toUpper(trim(rawTopic));
    bool showAll = (topic.empty() || topic == "ALL");

    std::cout << "====================================================================\n"
              << "  StarBasic (sbasic) - Standalone sBasic Interpreter\n"
              << "====================================================================\n"
              << "Usage:\n"
              << "  ./sbasic [options] [script.bas ...]\n\n"
              << "Command-Line Options:\n"
              << "  -v, --verbose         Enable line-by-line execution trace\n"
              << "  -q, --quiet           Disable execution trace (default)\n"
              << "  -h, -help, --help     Show full reference (or filter by topic:\n"
              << "                        ops, flow, math, str, arr, io, ods, doc)\n"
              << "  SBASIC_GUI=1          Env var to use Zenity GUI dialogs for MsgBox/InputBox\n";

    if (showAll || topic == "OPS" || topic == "OPERANDS" || topic == "OPERATORS") {
        std::cout << "\n--- OPERATORS, TYPES & CONSTANTS (topic: ops) ---\n"
                  << "  Arithmetic  : +  -  *  / (float div)  \\ (integer div)  ^ (power)  Mod\n"
                  << "  String      : & (concatenation)  + (string concatenation)\n"
                  << "  Comparison  : =  <>  <  >  <=  >=\n"
                  << "  Logical     : Not  And  Or\n"
                  << "  Constants   : True (-1)  False (0)  Pi (3.14159265358979...)\n"
                  << "  Data Types  : Integer (%), Long, Double, Single, String ($), Variant, Object\n";
    }

    if (showAll || topic == "FLOW" || topic == "STATEMENTS") {
        std::cout << "\n--- STATEMENTS & CONTROL FLOW (topic: flow) ---\n"
                  << "  Declarations: Option Explicit, Option Base 0|1\n"
                  << "                Dim, ReDim [Preserve], Erase, Global, Public, Private\n"
                  << "                v(n) Or v(low To high) As <Type>\n"
                  << "  Procedures  : Sub name([ByVal|ByRef] p As Type) ... End Sub\n"
                  << "                Function name(...) As Type ... End Function\n"
                  << "                Call name(...), Exit Sub, Exit Function, End\n"
                  << "  Branching   : If cond Then ... ElseIf cond Then ... Else ... End If\n"
                  << "                Select Case expr / Case v1, v2 / Case x To y / Case Is > v / Case Else / End Select\n"
                  << "  Loops       : For i = a To b [Step s] ... Next [i]  (Exit For)\n"
                  << "                Do [While|Until cond] ... Loop [While|Until cond]  (Exit Do)\n"
                  << "                While cond ... Wend\n"
                  << "  Jumps       : GoTo label, GoSub label ... Return\n"
                  << "  Errors      : On [Local] Error GoTo <label|0>, On Error Resume Next\n"
                  << "                Resume [Next|label], Error <code>, Err.Raise(code, [src], [desc]), Err.Clear\n"
                  << "                Err / Err.Number, Erl (error line), Error$ / Err.Description\n"
                  << "  Preprocessor: #Include \"file.bas\", Run \"file.bas\", Wait <ms>, Rem / '\n";
    }

    if (showAll || topic == "MATH") {
        std::cout << "\n--- MATH & NUMERIC FUNCTIONS (topic: math) ---\n"
                  << "  Trig / Exp  : Sin(x), Cos(x), Tan(x), Atn(x), Exp(x), Log(x), Sqr(x)\n"
                  << "  Rounding    : Abs(x), Sgn(x), Int(x), Fix(x), Round(x, [decimals]), Rnd()\n"
                  << "  Conversion  : CInt(x), CLng(x), CDbl(x), CSng(x), CStr(x), Val(s), Str$(x)\n"
                  << "                Hex$(n), Oct$(n), CVErr(code), IsError(v)\n";
    }

    if (showAll || topic == "STR" || topic == "STRING") {
        std::cout << "\n--- STRING & FORMATTING FUNCTIONS (topic: str) ---\n"
                  << "  Substrings  : Len(s), Left$(s, n), Right$(s, n), Mid$(s, start, [len])\n"
                  << "                Mid(var, start, [len]) = replacement, StrReverse(s)\n"
                  << "  Search/Repl : InStr([start,] hay, needle, [cmp]), InStrRev(hay, needle, [start], [cmp])\n"
                  << "                Replace(s, find, repl, [start], [count], [cmp]), StrComp(s1, s2, [cmp])\n"
                  << "  Case/Trim   : UCase$(s), LCase$(s), Trim$(s), LTrim$(s), RTrim$(s), LPad, RPad\n"
                  << "                LSet var = s, RSet var = s, Space$(n), Spc(n), String$(n, chOrCode)\n"
                  << "  Arrays/Char : Split(s, [delim], [max]), Join(arr, [delim]), Filter(arr, match, [inc], [cmp])\n"
                  << "                Chr$(code), Asc(s)\n"
                  << "  Formatting  : Format$(val, mask), FormatNumber(v, [dec], [lead], [parens], [group])\n"
                  << "                FormatPercent(...), FormatCurrency(...), FormatScientific(v, [dec])\n";
    }

    if (showAll || topic == "ARR" || topic == "LINALG" || topic == "MATRIX" || topic == "SOLVER") {
        std::cout << "\n--- ARRAYS, LINEAR ALGEBRA & SOLVERS (topic: arr / solver) ---\n"
                  << "  Arrays      : Array(v1, v2, ...), LBound(arr, [dim]), UBound(arr, [dim])\n"
                  << "                Erase arr1 [, arr2], ReDim [Preserve] arr(low To high)\n"
                  << "  Matrices    : MatMul(A, B), MatInv(A), MatTranspose(A)\n"
                  << "                MatEig(A, [ByRef eigVecs]), MatEigVec(A)\n"
                  << "  Solvers     : LinearSolver(\"GetMax|GetMin, By Changing=a,b,c, LimitingConditions=..., TimeLimit=5 min, ...\")\n"
                  << "                NonlinearSolver(\"GetMax|GetMin, By Changing=a,b,c, LimitingConditions=..., Options ...\")\n"
                  << "                Solver(\"...\") (auto-detects linear vs. nonlinear)\n";
    }

    if (showAll || topic == "DATE" || topic == "TIME") {
        std::cout << "\n--- DATE & TIME FUNCTIONS (topic: date) ---\n"
                  << "  System Clock: Date / Date$, Time / Time$, Now, Timer\n"
                  << "  Components  : Year(d), Month(d), Day(d), Hour(t), Minute(t), Second(t), Weekday(d, [first])\n"
                  << "  Construction: DateSerial(y, m, d), TimeSerial(h, m, s), DateValue(s), TimeValue(s)\n"
                  << "  Arithmetic  : DateAdd(interval, n, d), DateDiff(interval, d1, d2), DatePart(interval, d)\n"
                  << "  Formatting  : FormatDateTime(d, [namedFormat 0..4])\n";
    }

    if (showAll || topic == "IO" || topic == "FILE") {
        std::cout << "\n--- FILE SYSTEM, BINARY/RANDOM I/O & DIALOGS (topic: io) ---\n"
                  << "  File I/O    : Open path For Input|Output|Append|Binary|Random As #ch [Len=n], Close [#ch]\n"
                  << "                Print [#ch,] expr [; | , | Tab(col)], Write [#ch,] expr1, expr2, ...\n"
                  << "                Line Input #ch, var, Input #ch, var, Input$(n, #ch)\n"
                  << "                Put #ch, [pos], var, Get #ch, [pos], var, Lock #ch, Unlock #ch\n"
                  << "                FreeFile(), EOF(ch), Loc(ch), LOF(ch), Seek(ch [, pos])\n"
                  << "  File System : Dir([pattern], [attr]), Kill path, Name old As new, FileCopy src, dst\n"
                  << "                MkDir path, RmDir path, ChDir path, ChDrive drv, CurDir$()\n"
                  << "                FileDateTime(path), FileLen(path), GetAttr(path), SetAttr path, attr\n"
                  << "  Dialogs     : MsgBox(prompt, [buttons], [title]), InputBox(prompt, [title], [default])\n";
    }

    if (showAll || topic == "CALC" || topic == "MATH") {
        std::cout << "\n--- LIBREOFFICE CALC SPREADSHEET FUNCTIONS (prefix: calc.* | topic: calc) ---\n"
                  << "  Hyperbolic  : calc.SINH(x), calc.COSH(x), calc.TANH(x), calc.COTH(x), calc.SECH(x), calc.CSCH(x)\n"
                  << "                calc.ASINH(x), calc.ACOSH(x), calc.ATANH(x), calc.ACOTH(x), calc.ASECH(x), calc.ACSCH(x)\n"
                  << "  Trig & Math : calc.ASIN(x), calc.ACOS(x), calc.ATAN(x), calc.ATAN2(x, y), calc.COT(x), calc.ACOT(x)\n"
                  << "                calc.RADIANS(deg), calc.DEGREES(rad), calc.PI(), calc.LN(x), calc.LOG(x, [base]), calc.LOG10(x)\n"
                  << "                calc.POWER(x, y), calc.SQRT(x), calc.SQRTPI(x), calc.FACT(n), calc.FACTDOUBLE(n)\n"
                  << "                calc.COMBIN(n, k), calc.COMBINA(n, k), calc.PERMUT(n, k), calc.GCD(...), calc.LCM(...)\n"
                  << "                calc.ROUND(x, [d]), calc.ROUNDUP(x, [d]), calc.ROUNDDOWN(x, [d]), calc.MROUND(x, mult)\n"
                  << "                calc.CEILING(x, [sig]), calc.FLOOR(x, [sig]), calc.TRUNC(x, [d]), calc.EVEN(x), calc.ODD(x)\n"
                  << "                calc.MOD(n, d), calc.QUOTIENT(n, d), calc.RAND(), calc.RANDBETWEEN(low, high)\n"
                  << "  Array Math  : calc.SUM(...), calc.PRODUCT(...), calc.SUMSQ(...), calc.SUMPRODUCT(arr1, arr2, ...)\n"
                  << "                calc.SUMX2MY2(x, y), calc.SUMX2PY2(x, y), calc.SUMXMY2(x, y), calc.SUMIF(...), calc.SUMIFS(...)\n"
                  << "  Lookup/Ref  : calc.VLOOKUP(key, table2D, col, [sorted]), calc.HLOOKUP(key, table2D, row, [sorted])\n"
                  << "                calc.LOOKUP(key, searchVec, [resVec]), calc.XLOOKUP(key, lookVec, retVec, [notFound], [mode], [dir])\n"
                  << "                calc.MATCH(key, vec, [type]), calc.INDEX(arr, row, [col]), calc.CHOOSE(idx, v1, v2, ...)\n"
                  << "                calc.ROWS(arr), calc.COLUMNS(arr)\n"
                  << "  Database    : calc.DGET(db2D, field, crit), calc.DSUM(db2D, field, crit), calc.DAVERAGE(db2D, field, crit)\n"
                  << "                calc.DCOUNT(db2D, field, crit), calc.DCOUNTA(db2D, field, crit), calc.DMAX(db2D, field, crit)\n"
                  << "                calc.DMIN(db2D, field, crit), calc.DPRODUCT(db2D, field, crit), calc.DSTDEV(db2D, field, crit)\n"
                  << "                calc.DSTDEVP(db2D, field, crit), calc.DVAR(db2D, field, crit), calc.DVARP(db2D, field, crit)\n"
                  << "  Statistics  : calc.AVERAGE(...), calc.AVERAGEIF(...), calc.AVERAGEIFS(...), calc.MEDIAN(...), calc.MODE(...)\n"
                  << "                calc.COUNT(...), calc.COUNTA(...), calc.COUNTBLANK(arr), calc.COUNTIF(...), calc.COUNTIFS(...)\n"
                  << "                calc.MAX(...), calc.MIN(...), calc.MAXIFS(...), calc.MINIFS(...), calc.LARGE(arr, k), calc.SMALL(arr, k)\n"
                  << "                calc.PERCENTILE(arr, k), calc.QUARTILE(arr, q), calc.RANK(v, arr, [order])\n"
                  << "                calc.VAR(...), calc.VARP(...), calc.STDEV(...), calc.STDEVP(...), calc.GEOMEAN(...), calc.HARMEAN(...)\n"
                  << "                calc.CORREL(y, x), calc.COVAR(y, x), calc.SLOPE(y, x), calc.INTERCEPT(y, x), calc.RSQ(y, x), calc.FORECAST(x0, y, x)\n"
                  << "  Text & Logic: calc.CONCAT(...), calc.TEXTJOIN(delim, skipEmpty, ...), calc.EXACT(s1, s2), calc.FIND(f, s, [st])\n"
                  << "                calc.SEARCH(f, s, [st]), calc.SUBSTITUTE(s, old, new, [n]), calc.REPLACE(s, st, len, new)\n"
                  << "                calc.PROPER(s), calc.CLEAN(s), calc.REPT(s, n), calc.VALUE(s), calc.TEXT(v, mask)\n"
                  << "                calc.IF(c, t, [f]), calc.IFS(c1, v1, ...), calc.SWITCH(e, v1, r1, ..., [def])\n"
                  << "                calc.AND(...), calc.OR(...), calc.XOR(...), calc.NOT(v), calc.IFERROR(v, alt), calc.IFNA(v, alt)\n";
    }

    if (showAll || topic == "ODS" || topic == "CALC") {
        std::cout << "\n--- SPREADSHEETS: .ODS / .XLSX / .CSV (topic: ods) ---\n"
                  << "  Workbook    : ODS.New / Workbook.New, ODS.Open(path), ODS.SaveAs(path), Workbook.Save\n"
                  << "  Sheets      : Sheets.getCount(), Sheets.hasByName(name), Sheets.getByName(name)\n"
                  << "                Sheets.getByIndex(idx), Sheets.getName()\n"
                  << "                Sheets.insertNewByName(name, pos), Sheets.removeByName(name)\n"
                  << "                Sheets.copyByName(src, dst), Sheet.PrintTable\n"
                  << "  Ranges/Cells: Sheet.getCellByPosition(col, row), Sheet.getCellByName(\"A1\")\n"
                  << "                Sheet.getCellRangeByPosition(l, t, r, b), Sheet.getCellRangeByName(\"A1:C5\")\n"
                  << "                Sheet.clearContents(), getValue(), setValue(v), getString(), setString(s)\n"
                  << "                getCellValue(c, r), setCellValue(c, r, val)\n"
                  << "                getData() / getDataArray(), setDataArray(arr)\n"
                  << "                SearchString(txt), SearchNext(txt), getCellAddress()\n";
    }

    if (showAll || topic == "DOC" || topic == "ODT" || topic == "DOCX") {
        std::cout << "\n--- DOCUMENTS: .ODT / .DOCX / .TXT (topic: doc) ---\n"
                  << "  Document    : DOC.New / ODT.New, DOC.Open(path), DOC.SaveAs(path), DOC.Save\n"
                  << "  Paragraphs  : DOC.AppendHeading(text, [level 1..6])\n"
                  << "                DOC.AppendParagraph(text, [styleOrLevel])\n"
                  << "                DOC.InsertParagraph(idx, text, [styleOrLevel]), DOC.RemoveParagraph(idx)\n"
                  << "                DOC.GetParagraphCount(), DOC.GetParagraph(idx), DOC.SetParagraph(idx, text)\n"
                  << "  Styles      : DOC.GetHeadingLevel(idx), DOC.SetHeadingLevel(idx, level)\n"
                  << "                DOC.GetParagraphStyle(idx), DOC.SetParagraphStyle(idx, styleName)\n"
                  << "                Built-in styles: \"Title\", \"Subtitle\", \"Heading 1\"..\"Heading 6\",\n"
                  << "                                 \"Text Body\", \"Quotations\", \"Preformatted Text\"\n"
                  << "  Full Text   : DOC.GetText(), DOC.SetText(text), DOC.SearchText(s, [startIdx])\n"
                  << "                DOC.SearchNext(s), DOC.ReplaceAll(find, repl), DOC.PrintDocument\n";
    }
    std::cout << "====================================================================\n";
}

int main(int argc, char *argv[]) {
    StarBasicInterpreter interp;
    std::vector<std::string> scriptFiles;
    bool forceInteractive = false;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-q" || arg == "--quiet") {
            gVerboseMode = false;
        } else if (arg == "-v" || arg == "--verbose") {
            gVerboseMode = true;
        } else if (arg == "-i" || arg == "--interactive") {
            forceInteractive = true;
        } else if (arg == "-h" || arg == "-help" || arg == "--help" || arg == "-?") {
            std::string topic = (i + 1 < argc) ? argv[i + 1] : "";
            printHelp(topic);
            return 0;
        } else {
            scriptFiles.push_back(arg);
        }
    }

    try {
        if (!scriptFiles.empty()) {
            for (const auto &f : scriptFiles) {
                interp.loadAndRunFile(f);
            }
            if (forceInteractive && SBASIC_ISATTY) {
                interp.runInteractiveSession();
            }
        } else {
            printGplBanner();
            std::cout << ">Enter path to file:";
            std::cout.flush();
            std::string filepath;
            if (std::getline(std::cin, filepath)) {
                filepath = trim(filepath);
                std::string uPath = toUpper(filepath);
                if (uPath == "Q" || uPath == "QUIT" || uPath == "QUITE" || uPath == "EXIT") {
                    return 0;
                }
                if (!filepath.empty()) {
                    try {
                        interp.loadAndRunFile(filepath);
                    } catch (const BasicRuntimeError &bre) {
                        printDiagnosticReport(bre);
                    } catch (const std::exception &ex) {
                        std::cerr << "\n[StarBasic Error] " << ex.what() << "\n";
                    }
                }
                if (SBASIC_ISATTY) {
                    interp.runInteractiveSession();
                }
            }
        }
    } catch (const BasicRuntimeError &bre) {
        printDiagnosticReport(bre);
        return 1;
    } catch (const std::exception &ex) {
        std::cerr << "\n[StarBasic Error] " << ex.what() << "\n";
        return 1;
    }
    return 0;
}
