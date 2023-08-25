#include <cctype>
#include <filesystem>
#include <iostream>
#include <string>
#include <vector>
#include <cstdlib>

#include <unistd.h>
#include <sys/wait.h>

#include <llvm/TableGen/Record.h>
#include <llvm/TableGen/Main.h>

#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ManagedStatic.h>

#include <mlir/TableGen/Operator.h>
#include <mlir/TableGen/Attribute.h>
#include <mlir/TableGen/Argument.h>

// TODO: Auto generate haddock document
// TODO: Just make this better (It's too bodgy right now)

std::vector<std::string> tokenize(std::string s, std::string del) {
    int start, end = -1*del.size();
    std::vector<std::string> result;
    do {
        start = end + del.size();
        end = s.find(del, start);
        std::string i = s.substr(start, end - start); 
        if (i.length() > 0) result.push_back(i);
    } while (end != -1);
    return result;
}

static inline void getNamesFromOp(const mlir::tblgen::Operator& op, std::string& dialect, std::string& opname, std::string& gfname) {
    opname = op.getOperationName();
    dialect = op.getDialectName().str();
    gfname = opname.substr(1 + dialect.length());
}

static inline std::string getAttrType(std::string attrType) {
    // std::string t = "";
    // for (auto s : tokenize(attrType, "::")) 
    //     t += s + ".";
    // t.pop_back();
    return tokenize(attrType, "::").back();
}

static inline std::string canonizeName(std::string name, std::string ifempty = "") {
    if (name.empty()) 
        return ifempty;
    
    auto parts = tokenize(name, "_");
    auto cannon = parts[0];
    for (int i = 1; i < parts.size(); ++i) {
        auto part = parts[i];
        part[0] = toupper(part[0]);
        cannon.append(part);
    }
    cannon = "_" + cannon;
    return cannon;
} 

static inline std::string ifempty(std::string str, std::string ifempty = "undefined") {
    if (str.empty())
        return ifempty;
    return str;
}

static inline std::vector<std::string> emitTypeSig(const mlir::tblgen::Operator &op, llvm::raw_ostream &os) {
    std::vector<std::string> args;
    for (auto attribute : op.getAttributes()) {
        if (attribute.attr.isDerivedAttr())
            continue;

        if (attribute.attr.isOptional())
            os << "Maybe ";
        os << getAttrType(attribute.attr.getStorageType().str()) << " -> ";
        args.push_back(ifempty(attribute.name.str(), "unnamed_attr"));
    }
    for (auto operand : op.getOperands()) {
        if (operand.isVariadicOfVariadic() || operand.isVariadic()) {
            os << "[Value]";
        } else if (operand.isOptional()) {
            os << "Maybe Value";
        } else {
            os << "Value";
        }
        os << " -> ";
        args.push_back(ifempty(operand.name.str(), "unnamed_operand"));
    }
    for (auto region : op.getRegions()) {
        if (region.isVariadic()) {
            os << "[RegionM ()]";
        } else {
            os << "RegionM ()";
        }
        os << " -> ";
        args.push_back(ifempty(region.name.str(), "unnamed_region"));
    }
    for (auto block : op.getSuccessors()) {
        if (block.isVariadic()) {
            os << "[Block]";
        } else {
            os << "Block";
        }
        os << " -> ";
        args.push_back(ifempty(block.name.str(), "unnamed_block"));
    }

    auto numResults = op.getNumResults();
    if (numResults == 0) {
        os << "BlockM ()";
    } else {
        if (op.getNumVariableLengthResults() > 0) {
            os << "[Type] -> BlockM [Value]";
            args.push_back("result_types");
        } else {
            if (numResults == 1) {
                os << "Type -> BlockM Value";
                args.push_back("result_type");
            } else {
                std::string temp = "(";
                os << "(";
                for (int i = 0; i < numResults; ++i) {
                    os << "Type";
                    temp += ifempty(op.getResult(i).name.str(), "unnamed_result");
                    if (i < numResults - 1) {
                        os << ", ";
                        temp += ", ";
                    }
                }
                temp += ")";
                args.push_back(temp);

                os << ") -> BlockM (";
                for (int i = 0; i < numResults; ++i) {
                    os << "Value";
                    if (i < numResults - 1) 
                        os << ", ";
                }
                os << ")";
            }
        }
    }
    return args;
}



static inline void emitImpl(const mlir::tblgen::Operator &op, llvm::raw_ostream &os) {
    for (auto attribute : op.getAttributes()) {
        if (attribute.attr.isDerivedAttr())
            continue;
        auto name = attribute.name.str();
        os << canonizeName(name, "_unnamedAttr") << " ";
    }
    for (auto operand : op.getOperands()) {
        auto name = operand.name.str();
        os << canonizeName(name, "_unnamedOper") << " ";
    }
    for (auto region : op.getRegions()) {
        auto name = region.name.str();
        os << canonizeName(name, "_unnamedRegion") << " ";
    }
    for (auto block : op.getSuccessors()) {
        auto name = block.name.str();
        os << canonizeName(name, "_unnamedBlock") << " ";
    }

    auto numResults = op.getNumResults();
    if (numResults > 0) {
        if (op.getNumVariableLengthResults() > 0) {
            os << "_resultTypes ";
        } else {
            if (numResults == 1) {
                os << canonizeName(op.getResult(0).name.str(), "_resultType") << " ";
            } else {
                os << "(";
                for (int i = 0; i < numResults; ++i) {
                    auto name = op.getResult(i).name.str();
                    os << canonizeName(name, "_unnamedResultType");
                    if (i < numResults - 1) {
                        os << ", ";
                    }
                }
                os << ") ";
            }
        }
    }
    os << "= BlockM $ \\ctx blk -> do" << "\n  ";

    // Attrs
    os << "attrs <- sequence $ ([";
    bool tail = false;
    for (auto attr : op.getAttributes()) {
        if (attr.attr.isDerivedAttr() || attr.attr.isOptional())
            continue;
        if (tail) {
            os << ", ";
        }
        tail = true;
        os << "\"" << attr.name << "\" <#= " << canonizeName(attr.name.str(), "_unnamedAttr");
    }
    os << "] ++ catMaybes [";
    tail = false;
    for (auto attr : op.getAttributes()) {
        if (attr.attr.isDerivedAttr() || !attr.attr.isOptional())
            continue;
        if (tail) {
            os << ", ";
        }
        tail = true;
        os << "\"" << attr.name << "\" <?= " << canonizeName(attr.name.str(), "_unnamedAttr");
    }
    os << "]) <*> [ctx]" << "\n  ";
    
    // Opers
    os << "let opers = concat [";
    tail = false;
    for (auto operand : op.getOperands()) {
        if (tail) {
            os << ", ";
        }
        tail = true;

        auto canon = canonizeName(operand.name.str(), "_unnamedOper");
        if (operand.isVariadicOfVariadic() || operand.isVariadic()) {
            os << canon;
        } else if (operand.isOptional()) {
            os << "maybeToList " << canon;
        } else {
            os << "[" << canon << "]";
        }
    }
    os << "]" << "\n  ";

    // Regions
    os << "rgns <- sequence $ newRegion <$> (concat [";
    tail = false;
    for (auto region : op.getRegions()) {
        if (tail) os << ", ";
        tail = true;
        auto canon = canonizeName(region.name.str(), "_unnamedRegion");
        if (region.isVariadic()) {
            os << canon;
        } else {
            os << "[" << canon << "]";
        }
    }
    os << "]) <*> [ctx]" << "\n  ";

    // Block
    os << "let blks = concat [";
    tail = false;
    for (auto block : op.getSuccessors()) {
        if (tail) os << ", ";
        tail = true;

        auto canon = canonizeName(block.name.str(), "_unnamedBlock");
        if (block.isVariadic()) {
            os << canon;
        } else {
            os << "[" << canon << "]";
        }
    }

    os << "]" << "\n  ";

    // Types
    os << "results <- ";
    if (numResults > 0) {
        if (op.getNumVariableLengthResults() > 0) {
            os << "sequence $ getType <$> _resultTypes <*> [ctx]" << "\n  ";
        } else {
            if (numResults == 1) {
                auto canon = canonizeName(op.getResult(0).name.str(), "_resultType");
                os << "singleton <$> getType " << canon << " ctx\n  ";
            } else {
                os << "sequence $ getType <$> [";
                for (int i = 0; i < numResults; ++i) {
                    os << canonizeName(op.getResult(i).name.str(), "_unnamedResultType");
                    if (i < numResults - 1) 
                        os << ", ";
                }
                os << "] <*> [ctx]\n  ";
            }
        }
    } else {
        os << "return []\n  ";
    }
    // creating operation
    os << "loc <- mlirLocationUnknownGet ctx" << "\n  "
       << "op  <- mlirOperationCreate \"" << op.getOperationName() << "\" loc results opers rgns blks attrs False" << "\n  "
       << "mlirBlockAppendOwnedOperation blk op" << "\n  ";
    
    // Results
    if (numResults > 0) {
        if (op.getNumVariableLengthResults() > 0) {
            os << "mlirOperationGetAllResults op" << "\n";
        } else {
            if (numResults == 1) {
                auto canon = canonizeName(op.getResult(0).name.str(), "resultType");
                os << "mlirOperationGetResult op 0\n";
            } else {
                os << "(";
                for (int i = 0; i < numResults - 1; i++)
                    os << ",";
                os << ") <$> ";
                for (int i = 0; i < numResults; ++i) {
                    os << "mlirOperationGetResult op " << i;
                    if (i < numResults - 1) 
                        os << " <*> ";
                }
                os << "\n";
            }
        }
    } else {
        os << "return ()\n";
    }

}

static inline void emitHaddock0(const mlir::tblgen::Operator &op, llvm::raw_ostream &os) {
    os << "{-|\n";
    os << op.getSummary() << "\n";
    os << "Trait: ";
    bool tail = false;
    for (auto trait : op.getTraits()) {
        if (trait.getDef().isAnonymous())
            continue;
        if (tail)
            os << ", ";
        tail = true;
        os << trait.getDef().getName();
    }
    os << "\n-}\n";
}

static inline void emitHaddock1(const std::vector<std::string> &args, llvm::raw_ostream &os) {
    for (auto arg : args) 
        os << arg << "  ";
} 


static inline void emitOp(const mlir::tblgen::Operator &op, llvm::raw_ostream &os) {
    auto funcName = op.getCppClassName().str();
    funcName = "_" + funcName;
    emitHaddock0(op, os);
    os << funcName << " :: ";
    auto args = emitTypeSig(op, os);
    os << "\n-- ^" << funcName;
    emitHaddock1(args, os);
    os << "\n" << funcName << " ";
    emitImpl(op, os);
    os << "\n\n";
}

bool Generator(std::string hsModuleName, llvm::cl::list<std::string> &importedModules, llvm::raw_ostream &os, const llvm::RecordKeeper &recordKeeper) {
    std::filesystem::path tdfile(recordKeeper.getInputFilename());
    os << "-- This file is autogenerated" << "\n"
       << "module " << hsModuleName << " where" << "\n"
//       << "import Foreign" << "\n"
//       << "import Foreign.C" << "\n"
       << "import Data.Maybe" << "\n"
       << "import Data.List" << "\n"
       << "import MLIR.IR" << "\n"
       << "import MLIR.BuiltinAttributes" << "\n"
//       << "import MLIR.BuiltinTypes" << "\n"
       << "import MLIR.C.IR hiding (Type, Attribute, Region)" << "\n"
       << "\n";
    for (auto i : importedModules) {
        os << "import " << i << "\n";
    } 
    os << "\n";

    for (auto record : recordKeeper.getAllDerivedDefinitions("Op")) {
        mlir::tblgen::Operator op(record);
        emitOp(op, os);
    }

    return false;
}



// Would be nice if this could be call multiple times in one program
// From the llvm mailing list, I found that somebody managed this with llc. Cannot replicate ):
int main_entry(int argc, char **argv) {
    llvm::InitLLVM llvm(argc, argv);
    
    llvm::cl::opt<std::string> hsModuleName("m", llvm::cl::desc("The module name"), llvm::cl::value_desc("modulename"), llvm::cl::Required);
    llvm::cl::list<std::string> includedModules("i", llvm::cl::desc("Modules to be imported in hs file"), llvm::cl::ZeroOrMore);

    llvm::cl::ParseCommandLineOptions(argc, argv);
    
    return llvm::TableGenMain(*argv, [&](llvm::raw_ostream &os, const llvm::RecordKeeper &recordKeeper){
        return Generator(hsModuleName.getValue(), includedModules, os, recordKeeper);
    });

    return 0;
}


// NOTE: There is nothing that I can do to make asan not pissed off
extern "C" void hs_generator(int argc, char **argv) {
    pid_t child = fork();
    if (child == 0) { // Is child
        main_entry(argc, argv);
        _exit(EXIT_SUCCESS); // Trick to silence asan (might cause issue)
    } else if (child == -1) { // Failure
        std::cerr << "Failed to fork\n";
        exit(EXIT_FAILURE);
    } else { // Is parent
        if (child != waitpid(child, nullptr, 0)) {
            std::cerr << "Failed in waiting child process\n";
            exit(EXIT_FAILURE);
        }
    }
}
