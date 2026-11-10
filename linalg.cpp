// linalg.cpp - Linear Algebra & Optimization Engine for Standalone LibreOffice Basic (StarBasic) Interpreter

#include <sstream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <iomanip>
#include <stdexcept>
#include <cstdint>
#include <functional>
#include <chrono>
#include <random>
#include <limits>
#include <cctype>
#include <iostream>

// ============================================================================
// VARIANT & MULTI-DIMENSIONAL ARRAY TYPE SYSTEM (ABI-compatible with sbasic.cpp)
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
// LINEAR ALGEBRA & SOLVER ENGINE
// ============================================================================
namespace LinAlg {

    Variant matMul(const Variant &A, const Variant &B) {
        if (A.type != VarType::ARRAY || B.type != VarType::ARRAY)
            throw std::runtime_error("MatMul requires two arrays.");

        if (A.dims.size() == 2 && B.dims.size() == 2) {
            int m = A.dims[0].size(), kA = A.dims[1].size();
            int kB = B.dims[0].size(), n = B.dims[1].size();
            if (kA != kB) throw std::runtime_error("MatMul inner dimensions do not match.");

            Variant C = Variant::makeArray({A.dims[0], B.dims[1]}, VarType::DOUBLE);
            for (int i = 0; i < m; ++i) {
                for (int j = 0; j < n; ++j) {
                    double sum = 0.0;
                    for (int p = 0; p < kA; ++p) {
                        sum += A.at({A.dims[0].low + i, A.dims[1].low + p}).toDouble() *
                               B.at({B.dims[0].low + p, B.dims[1].low + j}).toDouble();
                    }
                    C.at({C.dims[0].low + i, C.dims[1].low + j}).assignCoerced(Variant::fromDouble(sum));
                }
            }
            return C;
        } else if (A.dims.size() == 2 && B.dims.size() == 1) {
            int m = A.dims[0].size(), kA = A.dims[1].size(), kB = B.dims[0].size();
            if (kA != kB) throw std::runtime_error("MatMul matrix-vector dimensions do not match.");

            Variant C = Variant::makeArray({A.dims[0]}, VarType::DOUBLE);
            for (int i = 0; i < m; ++i) {
                double sum = 0.0;
                for (int p = 0; p < kA; ++p) {
                    sum += A.at({A.dims[0].low + i, A.dims[1].low + p}).toDouble() *
                           B.at({B.dims[0].low + p}).toDouble();
                }
                C.at({C.dims[0].low + i}).assignCoerced(Variant::fromDouble(sum));
            }
            return C;
        }
        throw std::runtime_error("MatMul supports (2D x 2D) or (2D x 1D) arrays.");
    }

    Variant matInv(const Variant &A) {
        if (A.type != VarType::ARRAY || A.dims.size() != 2 || A.dims[0].size() != A.dims[1].size())
            throw std::runtime_error("MatInv requires a 2D square matrix.");
        int n = A.dims[0].size();

        std::vector<std::vector<double>> aug(n, std::vector<double>(2 * n, 0.0));
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j)
                aug[i][j] = A.at({A.dims[0].low + i, A.dims[1].low + j}).toDouble();
            aug[i][n + i] = 1.0;
        }

        for (int col = 0; col < n; ++col) {
            int pivot = col;
            double maxAbs = std::fabs(aug[col][col]);
            for (int row = col + 1; row < n; ++row) {
                if (std::fabs(aug[row][col]) > maxAbs) { maxAbs = std::fabs(aug[row][col]); pivot = row; }
            }
            if (maxAbs < 1e-14) throw std::runtime_error("MatInv error: Matrix is singular.");
            if (pivot != col) std::swap(aug[col], aug[pivot]);

            double div = aug[col][col];
            for (int j = 0; j < 2 * n; ++j) aug[col][j] /= div;

            for (int row = 0; row < n; ++row) {
                if (row != col) {
                    double factor = aug[row][col];
                    if (std::fabs(factor) > 0.0) {
                        for (int j = 0; j < 2 * n; ++j) aug[row][j] -= factor * aug[col][j];
                    }
                }
            }
        }

        Variant inv = Variant::makeArray(A.dims, VarType::DOUBLE);
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < n; ++j)
                inv.at({A.dims[0].low + i, A.dims[1].low + j}).assignCoerced(Variant::fromDouble(aug[i][n + j]));
        return inv;
    }

    Variant matTranspose(const Variant &A) {
        if (A.type != VarType::ARRAY || A.dims.size() != 2)
            throw std::runtime_error("MatTranspose requires a 2D matrix.");
        Variant T = Variant::makeArray({A.dims[1], A.dims[0]}, VarType::DOUBLE);
        int m = A.dims[0].size(), n = A.dims[1].size();
        for (int i = 0; i < m; ++i)
            for (int j = 0; j < n; ++j)
                T.at({T.dims[0].low + j, T.dims[1].low + i})
                 .assignCoerced(Variant::fromDouble(A.at({A.dims[0].low + i, A.dims[1].low + j}).toDouble()));
        return T;
    }

    void jacobiEigen(const Variant &A, Variant &eigValsOut, Variant &eigVecsOut) {
        if (A.type != VarType::ARRAY || A.dims.size() != 2 || A.dims[0].size() != A.dims[1].size())
            throw std::runtime_error("MatEig requires a 2D square matrix.");

        int n = A.dims[0].size();
        std::vector<std::vector<double>> S(n, std::vector<double>(n, 0.0));
        std::vector<std::vector<double>> V(n, std::vector<double>(n, 0.0));

        for (int i = 0; i < n; ++i) {
            V[i][i] = 1.0;
            for (int j = 0; j < n; ++j)
                S[i][j] = A.at({A.dims[0].low + i, A.dims[1].low + j}).toDouble();
        }

        const int maxIter = 200;
        for (int iter = 0; iter < maxIter; ++iter) {
            double maxOff = 0.0;
            int p = 0, q = 1;
            for (int i = 0; i < n; ++i) {
                for (int j = i + 1; j < n; ++j) {
                    if (std::fabs(S[i][j]) > maxOff) { maxOff = std::fabs(S[i][j]); p = i; q = j; }
                }
            }
            if (maxOff < 1e-13) break;

            double diff = S[q][q] - S[p][p];
            double t;
            if (std::fabs(S[p][q]) < 1e-30 * std::fabs(diff)) {
                t = S[p][q] / diff;
            } else {
                double phi = diff / (2.0 * S[p][q]);
                t = 1.0 / (std::fabs(phi) + std::sqrt(phi * phi + 1.0));
                if (phi < 0.0) t = -t;
            }
            double c = 1.0 / std::sqrt(1.0 + t * t);
            double s = t * c;
            double tau = s / (1.0 + c);

            double h = t * S[p][q];
            S[p][p] -= h; S[q][q] += h; S[p][q] = S[q][p] = 0.0;

            for (int j = 0; j < n; ++j) {
                if (j != p && j != q) {
                    double g = S[j][p], hj = S[j][q];
                    S[j][p] = S[p][j] = g - s * (hj + g * tau);
                    S[j][q] = S[q][j] = hj + s * (g - hj * tau);
                }
            }
            for (int j = 0; j < n; ++j) {
                double g = V[j][p], hj = V[j][q];
                V[j][p] = g - s * (hj + g * tau);
                V[j][q] = hj + s * (g - hj * tau);
            }
        }

        std::vector<int> order(n);
        for (int i = 0; i < n; ++i) order[i] = i;
        std::sort(order.begin(), order.end(), [&](int a, int b) { return S[a][a] < S[b][b]; });

        eigValsOut = Variant::makeArray({A.dims[0]}, VarType::DOUBLE);
        eigVecsOut = Variant::makeArray(A.dims, VarType::DOUBLE);
        for (int idx = 0; idx < n; ++idx) {
            int col = order[idx];
            eigValsOut.at({A.dims[0].low + idx}).assignCoerced(Variant::fromDouble(S[col][col]));
            for (int row = 0; row < n; ++row)
                eigVecsOut.at({A.dims[0].low + row, A.dims[1].low + idx}).assignCoerced(Variant::fromDouble(V[row][col]));
        }
    }

    // ========================================================================
    // LIBREOFFICE-STYLE LINEAR & NONLINEAR SOLVER ENGINE FOR SBASIC
    // ========================================================================

    struct SolverCallbacks {
        std::function<double(const std::string &expr, const std::vector<std::string> &changingVars)> evalExpr;
        std::function<Variant(const std::string &name)> getVar;
        std::function<void(const std::string &name, double val)> setVar;
        std::function<bool(const std::string &name)> varExists;
        std::function<bool(const std::string &name)> funcExists;
        std::function<std::string(const std::string &name)> getSymbolFormula;
        std::function<void(const std::string &status, double objVal, int iters, double elapsedSec)> setSolverStats;
    };

    namespace SolverDetail {

        static std::string sTrim(const std::string &s) {
            size_t a = s.find_first_not_of(" \t\r\n");
            if (a == std::string::npos) return "";
            size_t b = s.find_last_not_of(" \t\r\n");
            return s.substr(a, b - a + 1);
        }

        static std::string sUpper(std::string s) {
            for (char &c : s) c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
            return s;
        }

        static std::string normalizeKey(const std::string &s) {
            std::string out;
            for (char c : s) {
                if (std::isalnum(static_cast<unsigned char>(c))) {
                    out.push_back(static_cast<char>(std::toupper(static_cast<unsigned char>(c))));
                }
            }
            return out;
        }

        enum class GoalType { MAXIMIZE, MINIMIZE, TARGET_VALUE };
        enum class RelOp { LE, GE, EQ, FUNC_BOOL };

        struct ParsedConstraint {
            std::string rawText;
            std::string lhsExpr;
            std::string rhsExpr;
            RelOp op = RelOp::LE;
        };

        struct SolverSpec {
            GoalType goal = GoalType::MAXIMIZE;
            double targetValue = 0.0;
            std::string objectiveExpr;
            std::vector<std::string> changingVars;
            std::vector<double> lowerBounds;
            std::vector<double> upperBounds;
            std::vector<bool> isInteger;
            std::vector<bool> isBinary;
            std::vector<ParsedConstraint> constraints;

            // Options (LibreOffice Linear & DEPS/SCO Nonlinear defaults)
            double timeLimitSec = 100.0;
            bool nonNegative = false;
            bool assumeLinear = false;
            bool allInteger = false;
            bool allBinary = false;
            double tolerance = 1e-9;
            int maxIterations = 20000;

            // Nonlinear DEPS / SCO / Local Polish options
            std::string algorithm = "DEPS"; // DEPS, SCO, DE, PSO, NELDERMEAD
            int swarmSize = 70;
            int learningCycles = 2000;
            double deFactor = 0.5;
            double crossover = 0.9;
            double cognitive = 1.49445;
            double social = 1.49445;
            double inertia = 0.729;
            int stagnationLimit = 70;
            double stagnationTol = 1e-7;
            double defaultBoxMin = -1000.0;
            double defaultBoxMax = 1000.0;
            bool polish = true;
            bool verbose = false;
            std::string returnMode = "VALUE"; // VALUE, REPORT, ARRAY
        };

        static std::vector<std::string> splitTopLevel(const std::string &spec) {
            std::vector<std::string> out;
            std::string cur;
            int parenDepth = 0;
            bool inQuotes = false;
            for (size_t i = 0; i < spec.size(); ++i) {
                char c = spec[i];
                if (c == '"') inQuotes = !inQuotes;
                if (!inQuotes) {
                    if (c == '(' || c == '[') ++parenDepth;
                    else if (c == ')' || c == ']') parenDepth = std::max(0, parenDepth - 1);
                    else if ((c == ',' || c == ';') && parenDepth == 0) {
                        std::string t = sTrim(cur);
                        if (!t.empty()) out.push_back(t);
                        cur.clear();
                        continue;
                    }
                }
                cur.push_back(c);
            }
            std::string t = sTrim(cur);
            if (!t.empty()) out.push_back(t);
            return out;
        }

        static double parseTimeDuration(const std::string &raw) {
            std::string s = sTrim(raw);
            if (s.empty()) return 100.0;
            size_t idx = 0;
            double val = 100.0;
            try { val = std::stod(s, &idx); } catch (...) { return 100.0; }
            std::string unit = normalizeKey(s.substr(idx));
            if (unit == "MS" || unit == "MILLI" || unit == "MILLISECONDS") return val / 1000.0;
            if (unit == "M" || unit == "MIN" || unit == "MINS" || unit == "MINUTE" || unit == "MINUTES") return val * 60.0;
            if (unit == "H" || unit == "HR" || unit == "HRS" || unit == "HOUR" || unit == "HOURS") return val * 3600.0;
            return val;
        }

        static bool parseBoolToken(const std::string &raw, bool defVal = true) {
            std::string u = normalizeKey(raw);
            if (u.empty()) return defVal;
            if (u == "TRUE" || u == "YES" || u == "ON" || u == "1") return true;
            if (u == "FALSE" || u == "NO" || u == "OFF" || u == "0") return false;
            return defVal;
        }

        static bool isSolverSectionOrOptionKey(const std::string &normKey) {
            static const std::vector<std::string> keys = {
                "GETMAX", "GETMIN", "MAX", "MIN", "MAXIMIZE", "MINIMIZE",
                "GETVALUE", "VALUEOF", "TARGETVALUE",
                "TARGET", "OBJECTIVE", "FUNC", "FUNCTION", "FORMULA", "GOAL",
                "BYCHANGING", "CHANGING", "VARIABLES", "VARS", "CHANGINGCELLS",
                "LIMITINGCONDITIONS", "CONDITIONS", "CONSTRAINTS", "SUBJECTTO", "ST", "BOUNDS",
                "TIMELIMIT", "TIMEOUT", "MAXTIME",
                "OPTIONS", "OPTION",
                "NONNEGATIVE", "ASSUMENONNEGATIVE", "ASSUMELINEAR",
                "INTEGER", "ALLINTEGER", "ASSUMEINTEGER", "BINARY", "ALLBINARY",
                "TOLERANCE", "EPSILON", "EPSILONLEVEL",
                "MAXITERATIONS", "ITERATIONS", "BRANCHANDBOUNDDEPTH",
                "ALGORITHM", "METHOD", "ENGINE",
                "SWARMSIZE", "POPULATION", "POPULATIONSIZE", "LIBRARYSIZE",
                "LEARNINGCYCLES", "CYCLES", "GENERATIONS",
                "DEFACTOR", "SCALINGFACTOR", "CROSSOVER", "CR",
                "COGNITIVE", "SOCIAL", "INERTIA",
                "STAGNATIONLIMIT", "STAGNATIONTOLERANCE",
                "DEFAULTMIN", "DEFAULTMAX", "MINBOUND", "MAXBOUND",
                "POLISH", "ENHANCEWITHLOCALSEARCH", "VERBOSE", "REPORT", "RETURN", "OUTPUT"
            };
            return std::find(keys.begin(), keys.end(), normKey) != keys.end();
        }

        static bool splitRelational(const std::string &expr, std::string &lhs, RelOp &op, std::string &rhs) {
            int pDepth = 0;
            for (size_t i = 0; i < expr.size(); ++i) {
                char c = expr[i];
                if (c == '(' || c == '[') ++pDepth;
                else if (c == ')' || c == ']') pDepth = std::max(0, pDepth - 1);
                else if (pDepth == 0) {
                    if (i + 1 < expr.size()) {
                        std::string two = expr.substr(i, 2);
                        if (two == "<=") { lhs = sTrim(expr.substr(0, i)); rhs = sTrim(expr.substr(i + 2)); op = RelOp::LE; return true; }
                        if (two == ">=") { lhs = sTrim(expr.substr(0, i)); rhs = sTrim(expr.substr(i + 2)); op = RelOp::GE; return true; }
                        if (two == "==") { lhs = sTrim(expr.substr(0, i)); rhs = sTrim(expr.substr(i + 2)); op = RelOp::EQ; return true; }
                    }
                    if (c == '=') { lhs = sTrim(expr.substr(0, i)); rhs = sTrim(expr.substr(i + 1)); op = RelOp::EQ; return true; }
                    if (c == '<') { lhs = sTrim(expr.substr(0, i)); rhs = sTrim(expr.substr(i + 1)); op = RelOp::LE; return true; }
                    if (c == '>') { lhs = sTrim(expr.substr(0, i)); rhs = sTrim(expr.substr(i + 1)); op = RelOp::GE; return true; }
                }
            }
            return false;
        }

        static int findChangingVarIndex(const SolverSpec &spec, const std::string &varName) {
            std::string u = sUpper(sTrim(varName));
            for (size_t i = 0; i < spec.changingVars.size(); ++i) {
                if (sUpper(spec.changingVars[i]) == u) return static_cast<int>(i);
            }
            return -1;
        }

        static void parseSingleConditionItem(const std::string &rawItem,
                                             SolverSpec &spec,
                                             const SolverCallbacks &cb,
                                             int depth = 0) {
            if (depth > 6) return;
            std::string item = sTrim(rawItem);
            if (item.empty()) return;

            std::string uItem = normalizeKey(item);
            if (uItem == "NONNEGATIVE" || uItem == "ASSUMENONNEGATIVE") {
                spec.nonNegative = true;
                return;
            }
            if (uItem == "ALLINTEGER" || uItem == "ASSUMEINTEGER") {
                spec.allInteger = true;
                return;
            }
            if (uItem == "ALLBINARY") {
                spec.allBinary = true;
                return;
            }

            size_t lp = item.find('('), rp = item.rfind(')');
            if (lp != std::string::npos && rp == item.size() - 1 && lp > 0) {
                std::string fn = normalizeKey(item.substr(0, lp));
                std::string arg = sTrim(item.substr(lp + 1, rp - lp - 1));
                int vIdx = findChangingVarIndex(spec, arg);
                if (vIdx >= 0) {
                    if (fn == "INT" || fn == "INTEGER") { spec.isInteger[vIdx] = true; return; }
                    if (fn == "BIN" || fn == "BINARY") {
                        spec.isInteger[vIdx] = true;
                        spec.isBinary[vIdx] = true;
                        spec.lowerBounds[vIdx] = std::max(spec.lowerBounds[vIdx], 0.0);
                        spec.upperBounds[vIdx] = std::min(spec.upperBounds[vIdx], 1.0);
                        return;
                    }
                    if (fn == "FREE") {
                        spec.lowerBounds[vIdx] = -1e30;
                        spec.upperBounds[vIdx] = 1e30;
                        return;
                    }
                }
            }

            {
                int pDepth = 0;
                std::vector<size_t> opPos;
                std::vector<int> opLen;
                std::vector<RelOp> ops;
                for (size_t i = 0; i < item.size(); ++i) {
                    char c = item[i];
                    if (c == '(' || c == '[') ++pDepth;
                    else if (c == ')' || c == ']') pDepth = std::max(0, pDepth - 1);
                    else if (pDepth == 0) {
                        if (i + 1 < item.size() && (item.substr(i, 2) == "<=" || item.substr(i, 2) == ">=")) {
                            opPos.push_back(i);
                            opLen.push_back(2);
                            ops.push_back(item.substr(i, 2) == "<=" ? RelOp::LE : RelOp::GE);
                            ++i;
                        } else if (c == '<' || c == '>') {
                            opPos.push_back(i);
                            opLen.push_back(1);
                            ops.push_back(c == '<' ? RelOp::LE : RelOp::GE);
                        }
                    }
                }
                if (opPos.size() == 2) {
                    std::string e1 = sTrim(item.substr(0, opPos[0]));
                    std::string e2 = sTrim(item.substr(opPos[0] + opLen[0], opPos[1] - (opPos[0] + opLen[0])));
                    std::string e3 = sTrim(item.substr(opPos[1] + opLen[1]));
                    std::string op1Str = (ops[0] == RelOp::LE) ? "<=" : ">=";
                    std::string op2Str = (ops[1] == RelOp::LE) ? "<=" : ">=";
                    parseSingleConditionItem(e1 + " " + op1Str + " " + e2, spec, cb, depth + 1);
                    parseSingleConditionItem(e2 + " " + op2Str + " " + e3, spec, cb, depth + 1);
                    return;
                }
            }

            std::string lhs, rhs;
            RelOp op;
            if (splitRelational(item, lhs, op, rhs)) {
                int vIdx = findChangingVarIndex(spec, lhs);
                std::string uRhs = normalizeKey(rhs);
                if (vIdx >= 0 && op == RelOp::EQ) {
                    if (uRhs == "INT" || uRhs == "INTEGER") { spec.isInteger[vIdx] = true; return; }
                    if (uRhs == "BIN" || uRhs == "BINARY") {
                        spec.isInteger[vIdx] = true;
                        spec.isBinary[vIdx] = true;
                        spec.lowerBounds[vIdx] = std::max(spec.lowerBounds[vIdx], 0.0);
                        spec.upperBounds[vIdx] = std::min(spec.upperBounds[vIdx], 1.0);
                        return;
                    }
                    if (uRhs == "NONNEGATIVE") {
                        spec.lowerBounds[vIdx] = std::max(spec.lowerBounds[vIdx], 0.0);
                        return;
                    }
                }
                spec.constraints.push_back({item, lhs, rhs, op});
                return;
            }

            if (cb.varExists && cb.varExists(item)) {
                Variant v = cb.getVar(item);
                if (v.type == VarType::ARRAY) {
                    for (const auto &el : v.arrayData) {
                        for (const auto &sub : splitTopLevel(el.toString())) {
                            parseSingleConditionItem(sub, spec, cb, depth + 1);
                        }
                    }
                    return;
                }
                if (v.type == VarType::STRING && !sTrim(v.sVal).empty()) {
                    for (const auto &sub : splitTopLevel(v.sVal)) {
                        parseSingleConditionItem(sub, spec, cb, depth + 1);
                    }
                    return;
                }
            }

            if (cb.getSymbolFormula) {
                std::string formula = sTrim(cb.getSymbolFormula(item));
                if (!formula.empty() && formula != item) {
                    for (const auto &sub : splitTopLevel(formula)) {
                        parseSingleConditionItem(sub, spec, cb, depth + 1);
                    }
                    return;
                }
            }

            spec.constraints.push_back({item, item, "0", RelOp::FUNC_BOOL});
        }

        static void applyOptionKeyValue(const std::string &normKey, const std::string &valStr, SolverSpec &spec) {
            if (normKey == "TIMELIMIT" || normKey == "TIMEOUT" || normKey == "MAXTIME") {
                spec.timeLimitSec = std::max(0.01, parseTimeDuration(valStr));
            } else if (normKey == "NONNEGATIVE" || normKey == "ASSUMENONNEGATIVE") {
                spec.nonNegative = parseBoolToken(valStr, true);
            } else if (normKey == "ASSUMELINEAR") {
                spec.assumeLinear = parseBoolToken(valStr, true);
            } else if (normKey == "INTEGER" || normKey == "ALLINTEGER" || normKey == "ASSUMEINTEGER") {
                spec.allInteger = parseBoolToken(valStr, true);
            } else if (normKey == "BINARY" || normKey == "ALLBINARY") {
                spec.allBinary = parseBoolToken(valStr, true);
            } else if (normKey == "TOLERANCE" || normKey == "EPSILON") {
                try { spec.tolerance = std::max(1e-14, std::stod(valStr)); } catch (...) {}
            } else if (normKey == "EPSILONLEVEL") {
                int lvl = 1;
                try { lvl = std::stoi(valStr); } catch (...) {}
                if (lvl <= 0) spec.tolerance = 1e-12;
                else if (lvl == 1) spec.tolerance = 1e-9;
                else if (lvl == 2) spec.tolerance = 1e-7;
                else spec.tolerance = 1e-5;
            } else if (normKey == "MAXITERATIONS" || normKey == "ITERATIONS" || normKey == "BRANCHANDBOUNDDEPTH") {
                try { spec.maxIterations = std::max(10, std::stoi(valStr)); } catch (...) {}
            } else if (normKey == "ALGORITHM" || normKey == "METHOD" || normKey == "ENGINE") {
                spec.algorithm = normalizeKey(valStr);
            } else if (normKey == "SWARMSIZE" || normKey == "POPULATION" || normKey == "POPULATIONSIZE" || normKey == "LIBRARYSIZE") {
                try { spec.swarmSize = std::max(10, std::stoi(valStr)); } catch (...) {}
            } else if (normKey == "LEARNINGCYCLES" || normKey == "CYCLES" || normKey == "GENERATIONS") {
                try { spec.learningCycles = std::max(10, std::stoi(valStr)); } catch (...) {}
            } else if (normKey == "DEFACTOR" || normKey == "SCALINGFACTOR") {
                try { spec.deFactor = std::stod(valStr); } catch (...) {}
            } else if (normKey == "CROSSOVER" || normKey == "CR") {
                try { spec.crossover = std::stod(valStr); } catch (...) {}
            } else if (normKey == "COGNITIVE") {
                try { spec.cognitive = std::stod(valStr); } catch (...) {}
            } else if (normKey == "SOCIAL") {
                try { spec.social = std::stod(valStr); } catch (...) {}
            } else if (normKey == "INERTIA") {
                try { spec.inertia = std::stod(valStr); } catch (...) {}
            } else if (normKey == "STAGNATIONLIMIT") {
                try { spec.stagnationLimit = std::max(5, std::stoi(valStr)); } catch (...) {}
            } else if (normKey == "STAGNATIONTOLERANCE") {
                try { spec.stagnationTol = std::max(1e-14, std::stod(valStr)); } catch (...) {}
            } else if (normKey == "DEFAULTMIN" || normKey == "MINBOUND") {
                try { spec.defaultBoxMin = std::stod(valStr); } catch (...) {}
            } else if (normKey == "DEFAULTMAX" || normKey == "MAXBOUND") {
                try { spec.defaultBoxMax = std::stod(valStr); } catch (...) {}
            } else if (normKey == "POLISH" || normKey == "ENHANCEWITHLOCALSEARCH") {
                spec.polish = parseBoolToken(valStr, true);
            } else if (normKey == "VERBOSE" || normKey == "REPORT") {
                spec.verbose = parseBoolToken(valStr, true);
            } else if (normKey == "RETURN" || normKey == "OUTPUT") {
                spec.returnMode = normalizeKey(valStr);
            }
        }

        static void addChangingVariableToken(const std::string &rawVar, SolverSpec &spec, const SolverCallbacks &cb) {
            std::string tok = sTrim(rawVar);
            if (tok.empty()) return;

            double lb = -1e30, ub = 1e30;
            size_t lbPos = tok.find('['), rbPos = tok.rfind(']');
            if (lbPos != std::string::npos && rbPos == tok.size() - 1 && lbPos > 0) {
                std::string inside = tok.substr(lbPos + 1, rbPos - lbPos - 1);
                tok = sTrim(tok.substr(0, lbPos));
                size_t sep = inside.find(',');
                if (sep == std::string::npos) sep = inside.find(':');
                if (sep != std::string::npos) {
                    try { lb = std::stod(sTrim(inside.substr(0, sep))); } catch (...) {}
                    try { ub = std::stod(sTrim(inside.substr(sep + 1))); } catch (...) {}
                }
            }

            if (tok.find('(') == std::string::npos && cb.varExists && cb.varExists(tok)) {
                Variant v = cb.getVar(tok);
                if (v.type == VarType::ARRAY && v.dims.size() == 1) {
                    for (int idx = v.dims[0].low; idx <= v.dims[0].high; ++idx) {
                        spec.changingVars.push_back(tok + "(" + std::to_string(idx) + ")");
                        spec.lowerBounds.push_back(lb);
                        spec.upperBounds.push_back(ub);
                        spec.isInteger.push_back(v.declaredType == VarType::INTEGER);
                        spec.isBinary.push_back(false);
                    }
                    return;
                }
            }

            bool declInt = false;
            if (tok.find('(') == std::string::npos && cb.varExists && cb.varExists(tok)) {
                Variant v = cb.getVar(tok);
                if (v.declaredType == VarType::INTEGER) declInt = true;
            }
            spec.changingVars.push_back(tok);
            spec.lowerBounds.push_back(lb);
            spec.upperBounds.push_back(ub);
            spec.isInteger.push_back(declInt);
            spec.isBinary.push_back(false);
        }

        static SolverSpec parseSolverString(const std::string &rawSpec, const SolverCallbacks &cb) {
            SolverSpec spec;
            std::vector<std::string> segments = splitTopLevel(rawSpec);

            enum class Section { HEADER, BY_CHANGING, LIMITING_CONDITIONS, OPTIONS };
            Section currentSec = Section::HEADER;
            std::vector<std::string> rawConditions;

            for (const auto &seg : segments) {
                std::string s = sTrim(seg);
                if (s.empty()) continue;

                std::string uSeg = sUpper(s);
                if (uSeg.rfind("OPTIONS ", 0) == 0 || uSeg == "OPTIONS") {
                    currentSec = Section::OPTIONS;
                    std::string rest = sTrim(s.substr(7));
                    if (!rest.empty() && (rest[0] == '=' || rest[0] == ':')) rest = sTrim(rest.substr(1));
                    if (!rest.empty()) {
                        size_t eq = rest.find('=');
                        if (eq != std::string::npos) {
                            applyOptionKeyValue(normalizeKey(rest.substr(0, eq)), sTrim(rest.substr(eq + 1)), spec);
                        } else {
                            applyOptionKeyValue(normalizeKey(rest), "True", spec);
                        }
                    }
                    continue;
                }

                size_t sepPos = std::string::npos;
                int pDepth = 0;
                for (size_t i = 0; i < s.size(); ++i) {
                    char c = s[i];
                    if (c == '(' || c == '[') ++pDepth;
                    else if (c == ')' || c == ']') pDepth = std::max(0, pDepth - 1);
                    else if (pDepth == 0 && (c == '=' || c == ':')) {
                        if (c == '=' && i > 0 && (s[i - 1] == '<' || s[i - 1] == '>' || s[i - 1] == '!')) continue;
                        if (c == '=' && i + 1 < s.size() && s[i + 1] == '=') continue;
                        sepPos = i;
                        break;
                    }
                }

                if (sepPos != std::string::npos) {
                    std::string rawKey = sTrim(s.substr(0, sepPos));
                    std::string rawVal = sTrim(s.substr(sepPos + 1));
                    std::string normKey = normalizeKey(rawKey);

                    if (isSolverSectionOrOptionKey(normKey) && findChangingVarIndex(spec, rawKey) < 0) {
                        if (normKey == "GETMAX" || normKey == "MAX" || normKey == "MAXIMIZE") {
                            spec.goal = GoalType::MAXIMIZE;
                            if (!rawVal.empty()) spec.objectiveExpr = rawVal;
                            continue;
                        }
                        if (normKey == "GETMIN" || normKey == "MIN" || normKey == "MINIMIZE") {
                            spec.goal = GoalType::MINIMIZE;
                            if (!rawVal.empty()) spec.objectiveExpr = rawVal;
                            continue;
                        }
                        if (normKey == "GETVALUE" || normKey == "VALUEOF" || normKey == "TARGETVALUE") {
                            spec.goal = GoalType::TARGET_VALUE;
                            try { spec.targetValue = std::stod(rawVal); } catch (...) {}
                            continue;
                        }
                        if (normKey == "GOAL") {
                            std::string g = normalizeKey(rawVal);
                            if (g == "GETMIN" || g == "MIN" || g == "MINIMIZE") spec.goal = GoalType::MINIMIZE;
                            else if (g == "GETMAX" || g == "MAX" || g == "MAXIMIZE") spec.goal = GoalType::MAXIMIZE;
                            else {
                                spec.goal = GoalType::TARGET_VALUE;
                                try { spec.targetValue = std::stod(rawVal); } catch (...) {}
                            }
                            continue;
                        }
                        if (normKey == "TARGET" || normKey == "OBJECTIVE" || normKey == "FUNC" ||
                            normKey == "FUNCTION" || normKey == "FORMULA") {
                            spec.objectiveExpr = rawVal;
                            continue;
                        }
                        if (normKey == "BYCHANGING" || normKey == "CHANGING" || normKey == "VARIABLES" ||
                            normKey == "VARS" || normKey == "CHANGINGCELLS") {
                            currentSec = Section::BY_CHANGING;
                            if (!rawVal.empty()) addChangingVariableToken(rawVal, spec, cb);
                            continue;
                        }
                        if (normKey == "LIMITINGCONDITIONS" || normKey == "CONDITIONS" || normKey == "CONSTRAINTS" ||
                            normKey == "SUBJECTTO" || normKey == "ST" || normKey == "BOUNDS") {
                            currentSec = Section::LIMITING_CONDITIONS;
                            if (!rawVal.empty()) rawConditions.push_back(rawVal);
                            continue;
                        }
                        if (normKey == "OPTIONS" || normKey == "OPTION") {
                            currentSec = Section::OPTIONS;
                            if (!rawVal.empty()) applyOptionKeyValue(normalizeKey(rawVal), "True", spec);
                            continue;
                        }
                        applyOptionKeyValue(normKey, rawVal, spec);
                        continue;
                    }
                }

                std::string normSeg = normalizeKey(s);
                if (normSeg == "GETMAX" || normSeg == "MAX" || normSeg == "MAXIMIZE") {
                    spec.goal = GoalType::MAXIMIZE;
                    if (spec.objectiveExpr.empty() &&
                        ((cb.funcExists && cb.funcExists(s)) || (cb.varExists && cb.varExists(s)))) {
                        spec.objectiveExpr = s;
                    }
                    continue;
                }
                if (normSeg == "GETMIN" || normSeg == "MIN" || normSeg == "MINIMIZE") {
                    spec.goal = GoalType::MINIMIZE;
                    if (spec.objectiveExpr.empty() &&
                        ((cb.funcExists && cb.funcExists(s)) || (cb.varExists && cb.varExists(s)))) {
                        spec.objectiveExpr = s;
                    }
                    continue;
                }
                if (normSeg == "NONNEGATIVE" || normSeg == "ASSUMENONNEGATIVE" ||
                    normSeg == "ASSUMELINEAR" || normSeg == "ALLINTEGER" ||
                    normSeg == "ASSUMEINTEGER" || normSeg == "ALLBINARY" ||
                    normSeg == "VERBOSE" || normSeg == "REPORT") {
                    applyOptionKeyValue(normSeg, "True", spec);
                    continue;
                }

                if (currentSec == Section::BY_CHANGING) {
                    addChangingVariableToken(s, spec, cb);
                } else if (currentSec == Section::LIMITING_CONDITIONS) {
                    rawConditions.push_back(s);
                } else if (currentSec == Section::OPTIONS) {
                    applyOptionKeyValue(normSeg, "True", spec);
                } else {
                    if (spec.objectiveExpr.empty()) spec.objectiveExpr = s;
                }
            }

            if (spec.changingVars.empty()) {
                throw std::runtime_error("Solver error: No 'By Changing' decision variables specified.");
            }

            if (spec.objectiveExpr.empty()) {
                std::string fallbackName = (spec.goal == GoalType::MINIMIZE) ? "GetMin" : "GetMax";
                if ((cb.funcExists && cb.funcExists(fallbackName)) || (cb.varExists && cb.varExists(fallbackName))) {
                    spec.objectiveExpr = fallbackName;
                } else {
                    throw std::runtime_error("Solver error: No objective expression, variable, or function specified.");
                }
            }
            if (cb.varExists && cb.varExists(spec.objectiveExpr)) {
                Variant v = cb.getVar(spec.objectiveExpr);
                if (v.type == VarType::STRING && !sTrim(v.sVal).empty()) {
                    spec.objectiveExpr = sTrim(v.sVal);
                } else if (cb.getSymbolFormula) {
                    std::string f = sTrim(cb.getSymbolFormula(spec.objectiveExpr));
                    if (!f.empty()) spec.objectiveExpr = f;
                }
            } else if (cb.getSymbolFormula) {
                std::string f = sTrim(cb.getSymbolFormula(spec.objectiveExpr));
                if (!f.empty()) spec.objectiveExpr = f;
            }

            for (const auto &rc : rawConditions) {
                parseSingleConditionItem(rc, spec, cb, 0);
            }

            for (size_t j = 0; j < spec.changingVars.size(); ++j) {
                if (spec.nonNegative) {
                    spec.lowerBounds[j] = std::max(spec.lowerBounds[j], 0.0);
                }
                if (spec.allInteger) {
                    spec.isInteger[j] = true;
                }
                if (spec.allBinary) {
                    spec.isInteger[j] = true;
                    spec.isBinary[j] = true;
                    spec.lowerBounds[j] = std::max(spec.lowerBounds[j], 0.0);
                    spec.upperBounds[j] = std::min(spec.upperBounds[j], 1.0);
                }
            }

            return spec;
        }

        static void setDecisionVector(const SolverSpec &spec, const SolverCallbacks &cb, const std::vector<double> &x) {
            for (size_t j = 0; j < spec.changingVars.size(); ++j) {
                cb.setVar(spec.changingVars[j], x[j]);
            }
        }

        static std::vector<double> getDecisionVector(const SolverSpec &spec, const SolverCallbacks &cb) {
            std::vector<double> x(spec.changingVars.size(), 0.0);
            for (size_t j = 0; j < spec.changingVars.size(); ++j) {
                if (cb.varExists && cb.varExists(spec.changingVars[j])) {
                    x[j] = cb.getVar(spec.changingVars[j]).toDouble();
                }
            }
            return x;
        }

        // ====================================================================
        // TWO-PHASE SIMPLEX + BRANCH-AND-BOUND MILP SOLVER
        // ====================================================================
        enum class LPStatus { OPTIMAL, INFEASIBLE, UNBOUNDED };

        struct LinearRow {
            std::vector<double> a;
            RelOp op = RelOp::LE;
            double b = 0.0;
        };

        struct LPResult {
            LPStatus status = LPStatus::INFEASIBLE;
            double objVal = 0.0;
            std::vector<double> x;
            int iterations = 0;
        };

        static LPResult solveSimplexLP(int nOrig,
                                       const std::vector<double> &cOrig,
                                       double c0,
                                       GoalType goal,
                                       double targetValue,
                                       const std::vector<LinearRow> &rowsOrig,
                                       const std::vector<double> &lb,
                                       const std::vector<double> &ub,
                                       double eps,
                                       int maxIters) {
            std::vector<double> cMax(nOrig, 0.0);
            std::vector<LinearRow> rows = rowsOrig;
            if (goal == GoalType::MAXIMIZE) {
                cMax = cOrig;
            } else if (goal == GoalType::MINIMIZE) {
                for (int j = 0; j < nOrig; ++j) cMax[j] = -cOrig[j];
            } else {
                LinearRow eqRow;
                eqRow.a = cOrig;
                eqRow.op = RelOp::EQ;
                eqRow.b = targetValue - c0;
                rows.push_back(eqRow);
            }

            std::vector<int> posCol(nOrig, -1), negCol(nOrig, -1);
            int nStruct = 0;
            for (int j = 0; j < nOrig; ++j) {
                if (lb[j] > ub[j] + eps) return {LPStatus::INFEASIBLE, 0.0, {}, 0};
                posCol[j] = nStruct++;
                if (lb[j] <= -1e20) {
                    negCol[j] = nStruct++;
                }
            }

            struct StdRow {
                std::vector<double> a;
                RelOp op;
                double rhs;
            };
            std::vector<StdRow> stdRows;

            for (const auto &r : rows) {
                StdRow sr;
                sr.a.assign(nStruct, 0.0);
                double shift = 0.0;
                for (int j = 0; j < nOrig; ++j) {
                    double aij = r.a[j];
                    if (negCol[j] == -1) {
                        sr.a[posCol[j]] = aij;
                        shift += aij * lb[j];
                    } else {
                        sr.a[posCol[j]] = aij;
                        sr.a[negCol[j]] = -aij;
                    }
                }
                sr.op = r.op;
                sr.rhs = r.b - shift;
                stdRows.push_back(sr);
            }

            for (int j = 0; j < nOrig; ++j) {
                if (ub[j] < 1e20) {
                    StdRow sr;
                    sr.a.assign(nStruct, 0.0);
                    if (negCol[j] == -1) {
                        sr.a[posCol[j]] = 1.0;
                        sr.op = RelOp::LE;
                        sr.rhs = ub[j] - lb[j];
                    } else {
                        sr.a[posCol[j]] = 1.0;
                        sr.a[negCol[j]] = -1.0;
                        sr.op = RelOp::LE;
                        sr.rhs = ub[j];
                    }
                    stdRows.push_back(sr);
                }
            }

            std::vector<double> cStruct(nStruct, 0.0);
            for (int j = 0; j < nOrig; ++j) {
                cStruct[posCol[j]] = cMax[j];
                if (negCol[j] != -1) cStruct[negCol[j]] = -cMax[j];
            }

            int m = static_cast<int>(stdRows.size());
            int nSlack = 0, nArt = 0;
            for (int i = 0; i < m; ++i) {
                if (stdRows[i].rhs < 0.0) {
                    for (double &v : stdRows[i].a) v = -v;
                    stdRows[i].rhs = -stdRows[i].rhs;
                    if (stdRows[i].op == RelOp::LE) stdRows[i].op = RelOp::GE;
                    else if (stdRows[i].op == RelOp::GE) stdRows[i].op = RelOp::LE;
                }
                if (stdRows[i].op == RelOp::LE) ++nSlack;
                else if (stdRows[i].op == RelOp::GE) { ++nSlack; ++nArt; }
                else ++nArt;
            }

            int totalCols = nStruct + nSlack + nArt;
            int artStart = nStruct + nSlack;
            std::vector<std::vector<double>> T(m + 2, std::vector<double>(totalCols + 1, 0.0));
            std::vector<int> basis(m, -1);

            int curSlack = nStruct;
            int curArt = artStart;
            for (int i = 0; i < m; ++i) {
                for (int j = 0; j < nStruct; ++j) T[i][j] = stdRows[i].a[j];
                T[i][totalCols] = stdRows[i].rhs;
                if (stdRows[i].op == RelOp::LE) {
                    T[i][curSlack] = 1.0;
                    basis[i] = curSlack++;
                } else if (stdRows[i].op == RelOp::GE) {
                    T[i][curSlack++] = -1.0;
                    T[i][curArt] = 1.0;
                    basis[i] = curArt++;
                } else {
                    T[i][curArt] = 1.0;
                    basis[i] = curArt++;
                }
            }

            for (int j = 0; j < nStruct; ++j) T[m][j] = -cStruct[j];

            for (int j = artStart; j < totalCols; ++j) T[m + 1][j] = 1.0;
            for (int i = 0; i < m; ++i) {
                if (basis[i] >= artStart) {
                    for (int j = 0; j <= totalCols; ++j) T[m + 1][j] -= T[i][j];
                }
            }

            int totalIters = 0;
            auto pivot = [&](int pRow, int pCol) {
                double pv = T[pRow][pCol];
                for (int j = 0; j <= totalCols; ++j) T[pRow][j] /= pv;
                for (int i = 0; i <= m + 1; ++i) {
                    if (i != pRow) {
                        double factor = T[i][pCol];
                        if (std::fabs(factor) > 0.0) {
                            for (int j = 0; j <= totalCols; ++j) T[i][j] -= factor * T[pRow][j];
                        }
                    }
                }
                basis[pRow] = pCol;
                ++totalIters;
            };

            auto runSimplexPhase = [&](int objRow, int maxColLimit) -> LPStatus {
                while (totalIters < maxIters) {
                    int enterCol = -1;
                    double mostNeg = -eps;
                    for (int j = 0; j < maxColLimit; ++j) {
                        if (T[objRow][j] < mostNeg) {
                            mostNeg = T[objRow][j];
                            enterCol = j;
                            break;
                        }
                    }
                    if (enterCol == -1) return LPStatus::OPTIMAL;

                    int leaveRow = -1;
                    double minRatio = std::numeric_limits<double>::infinity();
                    for (int i = 0; i < m; ++i) {
                        double aij = T[i][enterCol];
                        if (aij > eps) {
                            double ratio = T[i][totalCols] / aij;
                            if (ratio < minRatio - eps ||
                                (std::fabs(ratio - minRatio) <= eps && leaveRow != -1 && basis[i] < basis[leaveRow])) {
                                minRatio = ratio;
                                leaveRow = i;
                            }
                        }
                    }
                    if (leaveRow == -1) return LPStatus::UNBOUNDED;
                    pivot(leaveRow, enterCol);
                }
                return LPStatus::OPTIMAL;
            };

            if (nArt > 0) {
                runSimplexPhase(m + 1, totalCols);
                if (-T[m + 1][totalCols] > std::max(1e-6, eps * 100.0)) {
                    return {LPStatus::INFEASIBLE, 0.0, {}, totalIters};
                }
                for (int i = 0; i < m; ++i) {
                    if (basis[i] >= artStart) {
                        for (int j = 0; j < artStart; ++j) {
                            if (std::fabs(T[i][j]) > eps) {
                                pivot(i, j);
                                break;
                            }
                        }
                    }
                }
            }

            LPStatus st2 = runSimplexPhase(m, artStart);
            if (st2 == LPStatus::UNBOUNDED) {
                return {LPStatus::UNBOUNDED, 0.0, {}, totalIters};
            }

            std::vector<double> yVal(nStruct, 0.0);
            for (int i = 0; i < m; ++i) {
                if (basis[i] >= 0 && basis[i] < nStruct) {
                    yVal[basis[i]] = std::max(0.0, T[i][totalCols]);
                }
            }

            std::vector<double> xSol(nOrig, 0.0);
            double finalObj = c0;
            for (int j = 0; j < nOrig; ++j) {
                if (negCol[j] == -1) xSol[j] = lb[j] + yVal[posCol[j]];
                else xSol[j] = yVal[posCol[j]] - yVal[negCol[j]];
                finalObj += cOrig[j] * xSol[j];
            }
            return {LPStatus::OPTIMAL, finalObj, xSol, totalIters};
        }

    } // namespace SolverDetail

    // ========================================================================
    // PUBLIC LINEAR SOLVER ENTRY POINT
    // ========================================================================
    Variant linearSolver(const std::string &specString, const SolverCallbacks &cb) {
        using namespace SolverDetail;
        auto startTime = std::chrono::steady_clock::now();

        SolverSpec spec = parseSolverString(specString, cb);
        int n = static_cast<int>(spec.changingVars.size());
        std::vector<double> origX = getDecisionVector(spec, cb);

        std::vector<double> baseX(n, 0.0);
        setDecisionVector(spec, cb, baseX);
        double f0 = 0.0;
        try {
            f0 = cb.evalExpr(spec.objectiveExpr, spec.changingVars);
            if (!std::isfinite(f0)) throw std::runtime_error("Non-finite at 0");
        } catch (...) {
            baseX = origX;
            setDecisionVector(spec, cb, baseX);
            f0 = cb.evalExpr(spec.objectiveExpr, spec.changingVars);
        }

        std::vector<double> c(n, 0.0);
        double c0 = f0;
        for (int j = 0; j < n; ++j) {
            std::vector<double> probeX = baseX;
            probeX[j] += 1.0;
            setDecisionVector(spec, cb, probeX);
            double fj = cb.evalExpr(spec.objectiveExpr, spec.changingVars);
            c[j] = fj - f0;
            c0 -= c[j] * baseX[j];
        }

        if (!spec.assumeLinear) {
            std::vector<double> testX = baseX;
            double predicted = c0;
            for (int j = 0; j < n; ++j) {
                testX[j] += 2.345 * (j + 1);
                predicted += c[j] * testX[j];
            }
            setDecisionVector(spec, cb, testX);
            double actual = cb.evalExpr(spec.objectiveExpr, spec.changingVars);
            if (std::fabs(actual - predicted) > 1e-4 * (1.0 + std::fabs(actual))) {
                setDecisionVector(spec, cb, origX);
                throw std::runtime_error("LinearSolver error: Objective '" + spec.objectiveExpr +
                                         "' is nonlinear. Use NonlinearSolver(...) instead.");
            }
        }

        std::vector<LinearRow> rows;
        std::vector<double> lb = spec.lowerBounds;
        std::vector<double> ub = spec.upperBounds;

        for (const auto &con : spec.constraints) {
            if (con.op == RelOp::FUNC_BOOL) {
                throw std::runtime_error("LinearSolver error: Condition '" + con.rawText +
                                         "' has no linear comparison operator (<=, >=, =).");
            }
            setDecisionVector(spec, cb, baseX);
            double g0 = cb.evalExpr("(" + con.lhsExpr + ") - (" + con.rhsExpr + ")", spec.changingVars);

            LinearRow row;
            row.a.assign(n, 0.0);
            row.op = con.op;
            double bVal = -g0;
            int nonZeroCount = 0;
            int singleVarIdx = -1;

            for (int j = 0; j < n; ++j) {
                std::vector<double> probeX = baseX;
                probeX[j] += 1.0;
                setDecisionVector(spec, cb, probeX);
                double gj = cb.evalExpr("(" + con.lhsExpr + ") - (" + con.rhsExpr + ")", spec.changingVars);
                row.a[j] = gj - g0;
                bVal += row.a[j] * baseX[j];
                if (std::fabs(row.a[j]) > 1e-12) {
                    ++nonZeroCount;
                    singleVarIdx = j;
                }
            }
            row.b = bVal;

            if (nonZeroCount == 1 && singleVarIdx >= 0) {
                double coeff = row.a[singleVarIdx];
                double boundVal = row.b / coeff;
                RelOp effOp = row.op;
                if (coeff < 0.0) {
                    if (effOp == RelOp::LE) effOp = RelOp::GE;
                    else if (effOp == RelOp::GE) effOp = RelOp::LE;
                }
                if (effOp == RelOp::GE) {
                    lb[singleVarIdx] = std::max(lb[singleVarIdx], boundVal);
                    continue;
                } else if (effOp == RelOp::LE) {
                    ub[singleVarIdx] = std::min(ub[singleVarIdx], boundVal);
                    continue;
                }
            }
            rows.push_back(row);
        }

        bool hasIntegers = false;
        for (int j = 0; j < n; ++j) {
            if (spec.isInteger[j] || spec.isBinary[j]) {
                hasIntegers = true;
                break;
            }
        }

        LPResult bestRes;
        int totalSimplexIters = 0;

        if (!hasIntegers) {
            bestRes = solveSimplexLP(n, c, c0, spec.goal, spec.targetValue,
                                     rows, lb, ub, spec.tolerance, spec.maxIterations);
            totalSimplexIters = bestRes.iterations;
        } else {
            struct BBNode {
                std::vector<double> lb;
                std::vector<double> ub;
            };
            std::vector<BBNode> stack;
            stack.push_back({lb, ub});

            bool foundIncumbent = false;
            double bestObj = (spec.goal == GoalType::MINIMIZE)
                             ? std::numeric_limits<double>::infinity()
                             : -std::numeric_limits<double>::infinity();
            std::vector<double> bestX(n, 0.0);

            int nodesVisited = 0;
            while (!stack.empty() && nodesVisited < spec.maxIterations) {
                auto now = std::chrono::steady_clock::now();
                double elapsed = std::chrono::duration<double>(now - startTime).count();
                if (elapsed > spec.timeLimitSec) break;

                BBNode node = std::move(stack.back());
                stack.pop_back();
                ++nodesVisited;

                LPResult lp = solveSimplexLP(n, c, c0, spec.goal, spec.targetValue,
                                             rows, node.lb, node.ub, spec.tolerance, spec.maxIterations);
                totalSimplexIters += lp.iterations;
                if (lp.status != LPStatus::OPTIMAL) continue;

                if (foundIncumbent) {
                    if (spec.goal == GoalType::MAXIMIZE && lp.objVal <= bestObj + spec.tolerance) continue;
                    if (spec.goal == GoalType::MINIMIZE && lp.objVal >= bestObj - spec.tolerance) continue;
                }

                int branchVar = -1;
                double maxFracDist = -1.0;
                for (int j = 0; j < n; ++j) {
                    if (spec.isInteger[j] || spec.isBinary[j]) {
                        double val = lp.x[j];
                        double nearest = std::round(val);
                        double frac = std::fabs(val - nearest);
                        if (frac > std::max(1e-6, spec.tolerance * 10.0)) {
                            if (frac > maxFracDist) {
                                maxFracDist = frac;
                                branchVar = j;
                            }
                        }
                    }
                }

                if (branchVar == -1) {
                    for (int j = 0; j < n; ++j) {
                        if (spec.isInteger[j] || spec.isBinary[j]) lp.x[j] = std::round(lp.x[j]);
                    }
                    double intObj = c0;
                    for (int j = 0; j < n; ++j) intObj += c[j] * lp.x[j];

                    if (!foundIncumbent ||
                        (spec.goal == GoalType::MAXIMIZE && intObj > bestObj) ||
                        (spec.goal == GoalType::MINIMIZE && intObj < bestObj) ||
                        (spec.goal == GoalType::TARGET_VALUE)) {
                        foundIncumbent = true;
                        bestObj = intObj;
                        bestX = lp.x;
                    }
                } else {
                    double v = lp.x[branchVar];
                    double fl = std::floor(v);
                    double cl = std::ceil(v);
                    if (cl <= node.ub[branchVar] + spec.tolerance) {
                        BBNode rightChild = node;
                        rightChild.lb[branchVar] = std::max(rightChild.lb[branchVar], cl);
                        stack.push_back(std::move(rightChild));
                    }
                    if (fl >= node.lb[branchVar] - spec.tolerance) {
                        BBNode leftChild = node;
                        leftChild.ub[branchVar] = std::min(leftChild.ub[branchVar], fl);
                        stack.push_back(std::move(leftChild));
                    }
                }
            }

            if (foundIncumbent) {
                bestRes = {LPStatus::OPTIMAL, bestObj, bestX, totalSimplexIters};
            } else {
                bestRes = {LPStatus::INFEASIBLE, 0.0, origX, totalSimplexIters};
            }
        }

        double elapsedSec = std::chrono::duration<double>(std::chrono::steady_clock::now() - startTime).count();

        if (bestRes.status == LPStatus::INFEASIBLE) {
            setDecisionVector(spec, cb, origX);
            if (cb.setSolverStats) cb.setSolverStats("INFEASIBLE", 0.0, totalSimplexIters, elapsedSec);
            throw std::runtime_error("LinearSolver: No feasible solution found matching LimitingConditions.");
        }
        if (bestRes.status == LPStatus::UNBOUNDED) {
            setDecisionVector(spec, cb, origX);
            if (cb.setSolverStats) cb.setSolverStats("UNBOUNDED", 0.0, totalSimplexIters, elapsedSec);
            throw std::runtime_error("LinearSolver: Objective function is unbounded.");
        }

        for (int j = 0; j < n; ++j) {
            if (std::fabs(bestRes.x[j] - std::round(bestRes.x[j])) < 1e-11) {
                bestRes.x[j] = std::round(bestRes.x[j]);
            }
        }
        setDecisionVector(spec, cb, bestRes.x);
        double verifiedObj = cb.evalExpr(spec.objectiveExpr, spec.changingVars);
        if (cb.setSolverStats) cb.setSolverStats("OPTIMAL", verifiedObj, totalSimplexIters, elapsedSec);

        if (spec.verbose) {
            std::cout << "[LinearSolver] Status=OPTIMAL, Objective=" << verifiedObj
                      << ", Iterations=" << totalSimplexIters << ", Time=" << elapsedSec << "s\n";
        }

        if (spec.returnMode == "ARRAY") {
            Variant arr = Variant::makeArray({{0, n}}, VarType::DOUBLE);
            arr.arrayData[0] = Variant::fromDouble(verifiedObj);
            for (int j = 0; j < n; ++j) arr.arrayData[j + 1] = Variant::fromDouble(bestRes.x[j]);
            return arr;
        }
        return Variant::fromDouble(verifiedObj);
    }

    // ========================================================================
    // PUBLIC NONLINEAR SOLVER ENTRY POINT (DEPS / SCO + NELDER-MEAD POLISH)
    // ========================================================================
    Variant nonlinearSolver(const std::string &specString, const SolverCallbacks &cb) {
        using namespace SolverDetail;
        auto startTime = std::chrono::steady_clock::now();

        SolverSpec spec = parseSolverString(specString, cb);
        int n = static_cast<int>(spec.changingVars.size());
        std::vector<double> origX = getDecisionVector(spec, cb);

        std::vector<double> lb = spec.lowerBounds;
        std::vector<double> ub = spec.upperBounds;
        for (const auto &con : spec.constraints) {
            int vIdx = findChangingVarIndex(spec, con.lhsExpr);
            if (vIdx >= 0 && (con.op == RelOp::LE || con.op == RelOp::GE)) {
                try {
                    size_t idx = 0;
                    double val = std::stod(con.rhsExpr, &idx);
                    if (idx == con.rhsExpr.size()) {
                        if (con.op == RelOp::GE) lb[vIdx] = std::max(lb[vIdx], val);
                        else ub[vIdx] = std::min(ub[vIdx], val);
                    }
                } catch (...) {}
            }
        }

        std::vector<double> initMin(n), initMax(n);
        for (int j = 0; j < n; ++j) {
            initMin[j] = (lb[j] > -1e20) ? lb[j] : std::min(spec.defaultBoxMin, origX[j] - 10.0);
            initMax[j] = (ub[j] <  1e20) ? ub[j] : std::max(spec.defaultBoxMax, origX[j] + 10.0);
            if (initMin[j] > initMax[j]) std::swap(initMin[j], initMax[j]);
        }

        struct EvalCandidate {
            std::vector<double> x;
            double obj = 0.0;
            double fitness = 0.0;
            double violation = 0.0;
        };

        auto projectPoint = [&](std::vector<double> &pt) {
            for (int j = 0; j < n; ++j) {
                if (pt[j] < lb[j]) pt[j] = lb[j];
                if (pt[j] > ub[j]) pt[j] = ub[j];
                if (spec.isBinary[j]) {
                    pt[j] = (pt[j] >= 0.5) ? 1.0 : 0.0;
                } else if (spec.isInteger[j]) {
                    pt[j] = std::round(pt[j]);
                    if (pt[j] < lb[j]) pt[j] = std::ceil(lb[j]);
                    if (pt[j] > ub[j]) pt[j] = std::floor(ub[j]);
                }
            }
        };

        auto evaluatePoint = [&](const std::vector<double> &rawPt) -> EvalCandidate {
            EvalCandidate cand;
            cand.x = rawPt;
            projectPoint(cand.x);
            setDecisionVector(spec, cb, cand.x);

            try {
                cand.obj = cb.evalExpr(spec.objectiveExpr, spec.changingVars);
                if (!std::isfinite(cand.obj)) {
                    cand.obj = 0.0;
                    cand.fitness = 1e30;
                    cand.violation = 1e30;
                    return cand;
                }
            } catch (...) {
                cand.obj = 0.0;
                cand.fitness = 1e30;
                cand.violation = 1e30;
                return cand;
            }

            if (spec.goal == GoalType::MAXIMIZE) cand.fitness = -cand.obj;
            else if (spec.goal == GoalType::MINIMIZE) cand.fitness = cand.obj;
            else cand.fitness = std::fabs(cand.obj - spec.targetValue);

            double viol = 0.0;
            for (const auto &con : spec.constraints) {
                try {
                    if (con.op == RelOp::FUNC_BOOL) {
                        double v = cb.evalExpr(con.lhsExpr, spec.changingVars);
                        if (std::fabs(v) < 1e-12) viol += 1.0;
                    } else {
                        double diff = cb.evalExpr("(" + con.lhsExpr + ") - (" + con.rhsExpr + ")", spec.changingVars);
                        if (!std::isfinite(diff)) {
                            viol += 1e15;
                        } else if (con.op == RelOp::LE) {
                            if (diff > spec.tolerance) viol += diff;
                        } else if (con.op == RelOp::GE) {
                            if (-diff > spec.tolerance) viol += -diff;
                        } else if (con.op == RelOp::EQ) {
                            if (std::fabs(diff) > std::max(1e-6, spec.tolerance * 100.0)) viol += std::fabs(diff);
                        }
                    }
                } catch (...) {
                    viol += 1e15;
                }
            }
            cand.violation = viol;
            return cand;
        };

        auto isBetter = [&](const EvalCandidate &a, const EvalCandidate &b) -> bool {
            bool aFeas = (a.violation <= spec.tolerance);
            bool bFeas = (b.violation <= spec.tolerance);
            if (aFeas && bFeas) return a.fitness < b.fitness;
            if (aFeas != bFeas) return aFeas;
            return a.violation < b.violation;
        };

        std::mt19937_64 rng(42);
        std::uniform_real_distribution<double> uni01(0.0, 1.0);

        int popSize = std::max(15, spec.swarmSize);
        std::vector<EvalCandidate> currentPop(popSize);
        std::vector<EvalCandidate> pBest(popSize);
        std::vector<std::vector<double>> velocity(popSize, std::vector<double>(n, 0.0));

        for (int i = 0; i < popSize; ++i) {
            std::vector<double> pt(n, 0.0);
            if (i == 0) {
                pt = origX;
            } else if (i == 1) {
                pt.assign(n, 0.0);
            } else if (i < popSize / 3) {
                double radius = std::pow(10.0, -1.0 + 3.0 * (static_cast<double>(i) / (popSize / 3.0)));
                for (int j = 0; j < n; ++j) {
                    pt[j] = origX[j] + (uni01(rng) * 2.0 - 1.0) * radius;
                }
            } else {
                for (int j = 0; j < n; ++j) {
                    pt[j] = initMin[j] + uni01(rng) * (initMax[j] - initMin[j]);
                }
            }
            currentPop[i] = evaluatePoint(pt);
            pBest[i] = currentPop[i];
        }

        EvalCandidate gBest = pBest[0];
        for (int i = 1; i < popSize; ++i) {
            if (isBetter(pBest[i], gBest)) gBest = pBest[i];
        }

        int stagnationCount = 0;
        int cyclesDone = 0;
        bool useDE = (spec.algorithm != "PSO" && spec.algorithm != "NELDERMEAD");
        bool usePSO = (spec.algorithm != "DE" && spec.algorithm != "NELDERMEAD");

        if (spec.algorithm != "NELDERMEAD") {
            for (int cycle = 0; cycle < spec.learningCycles; ++cycle) {
                auto now = std::chrono::steady_clock::now();
                if (std::chrono::duration<double>(now - startTime).count() > spec.timeLimitSec) break;
                ++cyclesDone;

                EvalCandidate prevBest = gBest;

                for (int i = 0; i < popSize; ++i) {
                    std::vector<double> trial = currentPop[i].x;

                    if (spec.algorithm == "SCO") {
                        int i1 = static_cast<int>(rng() % popSize);
                        int i2 = static_cast<int>(rng() % popSize);
                        const EvalCandidate &model = isBetter(pBest[i1], pBest[i2]) ? pBest[i1] : pBest[i2];
                        for (int j = 0; j < n; ++j) {
                            double center = 0.5 * (currentPop[i].x[j] + model.x[j]);
                            double span = std::fabs(model.x[j] - currentPop[i].x[j]);
                            trial[j] = center + (uni01(rng) * 2.0 - 1.0) * span;
                        }
                    } else if (useDE && (!usePSO || uni01(rng) < 0.5)) {
                        int r1 = static_cast<int>(rng() % popSize);
                        int r2 = static_cast<int>(rng() % popSize);
                        int r3 = static_cast<int>(rng() % popSize);
                        int jRand = static_cast<int>(rng() % n);
                        for (int j = 0; j < n; ++j) {
                            if (j == jRand || uni01(rng) < spec.crossover) {
                                const std::vector<double> &baseVec = (uni01(rng) < 0.5) ? gBest.x : pBest[r1].x;
                                trial[j] = baseVec[j] + spec.deFactor * (pBest[r2].x[j] - pBest[r3].x[j]);
                            }
                        }
                    } else {
                        for (int j = 0; j < n; ++j) {
                            double r1 = uni01(rng), r2 = uni01(rng);
                            velocity[i][j] = spec.inertia * velocity[i][j]
                                           + spec.cognitive * r1 * (pBest[i].x[j] - currentPop[i].x[j])
                                           + spec.social * r2 * (gBest.x[j] - currentPop[i].x[j]);
                            trial[j] = currentPop[i].x[j] + velocity[i][j];
                        }
                    }

                    EvalCandidate cand = evaluatePoint(trial);
                    currentPop[i] = cand;
                    if (isBetter(cand, pBest[i])) {
                        pBest[i] = cand;
                        if (isBetter(cand, gBest)) gBest = cand;
                    }
                }

                double improvement = std::fabs(gBest.fitness - prevBest.fitness) +
                                     std::fabs(gBest.violation - prevBest.violation);
                if (gBest.violation <= spec.tolerance && improvement < spec.stagnationTol) {
                    ++stagnationCount;
                    if (stagnationCount >= spec.stagnationLimit) break;
                } else {
                    stagnationCount = 0;
                }
            }
        }

        if (spec.polish || spec.algorithm == "NELDERMEAD") {
            std::vector<EvalCandidate> simplex(n + 1, gBest);
            for (int k = 1; k <= n; ++k) {
                std::vector<double> pt = gBest.x;
                if (!spec.isInteger[k - 1] && !spec.isBinary[k - 1]) {
                    double step = std::max(0.05, 0.05 * std::fabs(pt[k - 1]));
                    pt[k - 1] += step;
                }
                simplex[k] = evaluatePoint(pt);
            }

            for (int iter = 0; iter < 600; ++iter) {
                auto now = std::chrono::steady_clock::now();
                if (std::chrono::duration<double>(now - startTime).count() > spec.timeLimitSec) break;

                std::sort(simplex.begin(), simplex.end(), isBetter);
                if (isBetter(simplex[0], gBest)) gBest = simplex[0];

                double maxSpread = 0.0;
                for (int j = 0; j < n; ++j) {
                    maxSpread = std::max(maxSpread, std::fabs(simplex.back().x[j] - simplex[0].x[j]));
                }
                if (maxSpread < 1e-12) break;

                std::vector<double> centroid(n, 0.0);
                for (int i = 0; i < n; ++i) {
                    for (int j = 0; j < n; ++j) centroid[j] += simplex[i].x[j] / n;
                }

                auto combine = [&](double coeff) {
                    std::vector<double> pt(n);
                    for (int j = 0; j < n; ++j) {
                        pt[j] = centroid[j] + coeff * (centroid[j] - simplex.back().x[j]);
                    }
                    return evaluatePoint(pt);
                };

                EvalCandidate xr = combine(1.0);
                if (isBetter(xr, simplex[0])) {
                    EvalCandidate xe = combine(2.0);
                    simplex.back() = isBetter(xe, xr) ? xe : xr;
                } else if (isBetter(xr, simplex[n - 1])) {
                    simplex.back() = xr;
                } else {
                    EvalCandidate xc = combine(isBetter(xr, simplex.back()) ? 0.5 : -0.5);
                    if (isBetter(xc, simplex.back())) {
                        simplex.back() = xc;
                    } else {
                        for (int i = 1; i <= n; ++i) {
                            for (int j = 0; j < n; ++j) {
                                simplex[i].x[j] = 0.5 * (simplex[0].x[j] + simplex[i].x[j]);
                            }
                            simplex[i] = evaluatePoint(simplex[i].x);
                        }
                    }
                }
            }

            double stepSize = 0.1;
            for (int iter = 0; iter < 400 && stepSize > 1e-12; ++iter) {
                bool improved = false;
                for (int j = 0; j < n; ++j) {
                    double delta = (spec.isInteger[j] || spec.isBinary[j]) ? 1.0 : stepSize;
                    for (double dir : {1.0, -1.0}) {
                        std::vector<double> pt = gBest.x;
                        pt[j] += dir * delta;
                        EvalCandidate c = evaluatePoint(pt);
                        if (isBetter(c, gBest)) {
                            gBest = c;
                            improved = true;
                        }
                    }
                }
                if (!improved) stepSize *= 0.25;
            }
        }

        double elapsedSec = std::chrono::duration<double>(std::chrono::steady_clock::now() - startTime).count();

        if (gBest.violation > std::max(1e-4, spec.tolerance * 1000.0)) {
            setDecisionVector(spec, cb, origX);
            if (cb.setSolverStats) cb.setSolverStats("INFEASIBLE", gBest.obj, cyclesDone, elapsedSec);
            throw std::runtime_error("NonlinearSolver: Could not find a feasible solution satisfying LimitingConditions.");
        }

        for (int j = 0; j < n; ++j) {
            if (std::fabs(gBest.x[j] - std::round(gBest.x[j])) < 1e-9) {
                gBest.x[j] = std::round(gBest.x[j]);
            }
        }
        setDecisionVector(spec, cb, gBest.x);
        double finalObj = cb.evalExpr(spec.objectiveExpr, spec.changingVars);
        if (cb.setSolverStats) cb.setSolverStats("OPTIMAL", finalObj, cyclesDone, elapsedSec);

        if (spec.verbose) {
            std::cout << "[NonlinearSolver] Status=OPTIMAL, Objective=" << finalObj
                      << ", Cycles=" << cyclesDone << ", Time=" << elapsedSec << "s\n";
        }

        if (spec.returnMode == "ARRAY") {
            Variant arr = Variant::makeArray({{0, n}}, VarType::DOUBLE);
            arr.arrayData[0] = Variant::fromDouble(finalObj);
            for (int j = 0; j < n; ++j) arr.arrayData[j + 1] = Variant::fromDouble(gBest.x[j]);
            return arr;
        }
        return Variant::fromDouble(finalObj);
    }

} // namespace LinAlg
