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

#define OPERANDS_LIST_NAME "operands"
#define RESULTS_LIST_NAME "results"

// NOTE: FUCKKKKKK
std::string stripNamespace(std::string str) {
    int i = str.length();
    for (; i > 0 && str[i - 1] != ':'; --i);
    return str.substr(i);
}

struct AttributeInfo {
    bool isOptional;
    std::string type;
    std::string name;
    std::string hsname;
};

struct OperandInfo {
    bool isOptional;
    bool isVariadic;
    std::string hsname;
};

struct SuccessorInfo {
    bool isVariadic;
    std::string hsname;
};

struct ResultInfo {
    std::string hsname;
};

struct RegionInfo {
    bool isVariadic;
    std::string hsname;
};

struct OperationInfo {
    bool simpleOperands;
    bool simpleResults;
    std::string hsname;
    std::string name;

    std::vector<AttributeInfo> attributes;
    std::vector<OperandInfo> operands;
    std::vector<SuccessorInfo> successors;
    std::vector<ResultInfo> results;
    std::vector<RegionInfo> regions;
};

OperationInfo gatherOperationInfo(mlir::tblgen::Operator op) {
    OperationInfo info = {
        .simpleOperands = op.getNumVariableLengthOperands() <= 1,
        .simpleResults  = op.getNumVariableLengthResults() == 0,

        .hsname = "_" + stripNamespace(op.getCppClassName().str()),
        .name   = op.getOperationName(),
        
        .attributes = std::vector<AttributeInfo>(op.getNumAttributes()),
        .operands   = std::vector<OperandInfo>(op.getNumOperands()),
        .successors = std::vector<SuccessorInfo>(op.getNumSuccessors()),
        .results    = std::vector<ResultInfo>(op.getNumResults()),
        .regions    = std::vector<RegionInfo>(op.getNumRegions()),
    };

    for (int i = 0; i < info.attributes.size(); ++i) {
        auto attribute = op.getAttribute(i);
        info.attributes[i] = (AttributeInfo){
            .isOptional = attribute.attr.isOptional(),
            .type = ((std::string)"_") + (attribute.attr.isOptional() ? "Maybe" : "") + stripNamespace(attribute.attr.getStorageType().str()),
            .name = attribute.name.str(),
            .hsname = "_" + attribute.name.str(),
        };
    }

    for (int i = 0; i < info.operands.size(); ++i) {
        auto operand = op.getOperand(i);
        info.operands[i] = (OperandInfo){
            .isOptional = operand.isOptional(),
            .isVariadic = operand.isVariadic(),
            .hsname     = "_" + (operand.name.size() ? operand.name.str() : "arg" + std::to_string(i)),
        };
    }

    for (int i = 0; i < info.successors.size(); ++i) {
        auto successor = op.getSuccessor(i);
        info.successors[i] = (SuccessorInfo){
            .isVariadic = successor.isVariadic(),
            .hsname     = "_" + (successor.name.size() ? successor.name.str() : "suc" + std::to_string(i)),
        };
    }

    for (int i = 0; i < info.regions.size(); ++i) {
        auto region = op.getRegion(i);
        info.regions[i] = (RegionInfo){
            .isVariadic = region.isVariadic(),
            .hsname = "_" + (region.name.size() ? region.name.str() : "reg" + std::to_string(i)),
        };
    }

    for (int i = 0; i < info.results.size(); ++i) {
        auto result = op.getResult(i);
        info.results[i] = (ResultInfo){
            .hsname = "_" + (result.name.size() ? result.name.str() : "res" + std::to_string(i)),
        };
    }

    return info;
}

void generateSignature(llvm::raw_ostream &os, const OperationInfo &info) {
    os << info.hsname << " :: ";
    for (auto i : info.attributes) os << i.type << " -> ";
    if (info.simpleOperands) {
        for (auto i : info.operands) {
            if (i.isOptional) {
                os << "Maybe Value -> ";
            } else if (i.isVariadic) {
                os << "[Value] -> ";
            } else {
                os << "Value -> ";
            }
        }
    } else {
        os << "[Value] -> ";
    }
    for (auto i : info.successors) {
        if (i.isVariadic) {
            os << "[Block] -> ";
        } else {
            os << "Block -> ";
        }
    }
    for (auto i : info.regions) {
        if (i.isVariadic) {
            os << "[RegionM ()] -> ";
        } else {
            os << "RegionM () -> ";
        }
    }
    if (info.simpleResults) {
        if (info.results.size()) {
            os << "(AnyType";
            for (int i = 1; i < info.results.size(); ++i)
                os << ", AnyType";
            os << ") -> ";
        }  
    } else {
        os << "[AnyType] -> ";
    }
    os << "BlockM ";
    if (info.simpleResults) {
        os << "(";
        for (int i = 0; i < info.results.size(); ++i) 
            os << (i ? ", " : "") << "Value";
        os << ")";
    } else {
        os << "[Value]";
    }
    os << "\n";
}

void generateBinding(llvm::raw_ostream &os, const OperationInfo &info) {
    os << info.hsname << " ";
    for (auto i : info.attributes) os << i.hsname << " ";
    if (info.simpleOperands) {
        for (auto i : info.operands) os << i.hsname << " ";
    } else {
        os << OPERANDS_LIST_NAME " ";
    }
    for (auto i : info.successors) os << i.hsname << " ";
    for (auto i : info.regions) os << i.hsname << " ";
    if (info.simpleResults) {
        if (info.results.size()) {
            os << "(" << info.results[0].hsname;
            for (int i = 1; i < info.results.size(); ++i)
                os << ", " << info.results[i].hsname;
            os << ") ";
        }
    } else {
        os << RESULTS_LIST_NAME " ";
    }
    os << "= BlockM $ \\ c b -> do\n";

    os << "  let successors = ";
    for (auto i : info.successors) os << i.hsname << (i.isVariadic ? "++" : ":");
    os << "[]" << "\n";

    if (info.simpleOperands) {
        os << "      " OPERANDS_LIST_NAME " = ";
        for (auto i : info.operands) {
            os << i.hsname;
            if (i.isVariadic) {
                os << "++";
            } else if (i.isOptional) {
                os << "?:";
            } else {
                os << ":";
            } 
        }
        os << "[]" << "\n";
    }

    os << "      loc = locationUnknownGet c" << "\n";

    
    os << "  attributes <- sequence (";
    for (auto i : info.attributes) {
        os << "(\"" << i.name << "\" " << (i.isOptional ? "<?=" : "<#=") << i.hsname << ")" << (i.isOptional ? "?:" : ":");
    }
    os << "[] <*> [c])" << "\n";

    os << "  types <- forM ("; 
    if (info.simpleResults) {
        for (auto i : info.results) {
            os << i.hsname << ":";
        }
        os << "[]";
    } else {
        os << RESULTS_LIST_NAME;
    }
    os << " :: [AnyType]) (`typeGet` c)" << "\n";

    os << "  regions <- forM (";
    for (auto i : info.regions) os << i.hsname << (i.isVariadic ? "++" : ":");
    os << "[]) (`runRegionM` c)" << "\n";
    
    os << "  op <- operationCreate \"" << info.name << "\" loc types " OPERANDS_LIST_NAME " regions successors attributes" << "\n"
       << "  blockAppendOwnedOperation b op" << "\n";

    if (info.simpleResults) {
        switch (info.results.size()) {
        case 0:
            os << "  return ()";
            break;
        case 1:
            os << "  operationGetResult op 0";
            break;
        default:
            os << "  (";
            for (int i = 1; i < info.results.size(); ++i) {
                os << ",";
            }
            os << ") <$> (operationGetResult op 0) ";
            for (int i = 1; i < info.results.size(); ++i) {
                os << "<*> (operationGetResult op " << i << ") ";
            }
            break;
        }
    } else {
        os << "  operationGetAllResults op";
    }
    os << "\n";

}

int main_entry(int argc, char **argv) {
    llvm::InitLLVM llvm(argc, argv);
    
    llvm::cl::opt<std::string> preprocessor("p", llvm::cl::desc("preprocessor"), llvm::cl::value_desc(""), llvm::cl::Required);
    llvm::cl::opt<std::string> hsModuleName("m", llvm::cl::desc("The module name"), llvm::cl::value_desc("modulename"), llvm::cl::Required);
    llvm::cl::list<std::string> includedModules("i", llvm::cl::desc("Modules to be imported in hs file"), llvm::cl::ZeroOrMore);

    llvm::cl::ParseCommandLineOptions(argc, argv);
    
    return llvm::TableGenMain(*argv, [&](llvm::raw_ostream &os, const llvm::RecordKeeper &recordKeeper){
        os << "{-# OPTIONS_GHC -pgmPgcc  -optP-E #-}" << "\n"
           << "{-# LANGUAGE CPP #-}" << "\n"
           << "#include <" << preprocessor << ">" << "\n"
           << "module " << hsModuleName << " where\n"
           << "import Control.Monad (forM)" << "\n"
           << "import MLIR" << "\n"
           << "import MLIR.AutogenUtils" << "\n"
           << "import MLIR.C.IR (Block)" << "\n\n";
        for (auto m : includedModules) os << "import " << m << "\n";
        os << "\n";

        for (auto record : recordKeeper.getAllDerivedDefinitions("Op")) {
            OperationInfo info = gatherOperationInfo(mlir::tblgen::Operator(record));
            generateSignature(os, info);
            generateBinding(os, info);
        }
        return false;
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
