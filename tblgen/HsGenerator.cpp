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

// NOTE: FUCKKKKKK
std::string stripNamespace(std::string str) {
    int i = str.length();
    for (; i > 0 && str[i - 1] != ':'; --i);
    return str.substr(i);
}

struct AttributeInfo {
    bool isOptional;
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

    std::vector<AttributeInfo> attributes;
    std::vector<OperandInfo> operands;
    std::vector<SuccessorInfo> successors;
    std::vector<ResultInfo> results;
    std::vector<RegionInfo> regions;
};

OperationInfo gatherOperationInfo(mlir::tblgen::Operator &op) {
    OperationInfo info = {
        .simpleOperands = op.getNumVariableLengthOperands() <= 1,
        .simpleResults  = op.getNumVariableLengthResults() == 0,
        
        .attributes = std::vector<AttributeInfo>(op.getNumAttributes()),
        .operands   = std::vector<OperandInfo>(op.getNumOperands()),
        .successors = std::vector<SuccessorInfo>(op.getNumSuccessors()),
        .results    = std::vector<ResultInfo>(op.getNumResults()),
        .regions    = std::vector<RegionInfo>(op.getNumRegions()),
    };
}



int main_entry(int argc, char **argv) {
    llvm::InitLLVM llvm(argc, argv);
    
    llvm::cl::opt<std::string> preprocessor("p", llvm::cl::desc("preprocessor"), llvm::cl::value_desc(""), llvm::cl::Required);
    llvm::cl::opt<std::string> hsModuleName("m", llvm::cl::desc("The module name"), llvm::cl::value_desc("modulename"), llvm::cl::Required);
    llvm::cl::list<std::string> includedModules("i", llvm::cl::desc("Modules to be imported in hs file"), llvm::cl::ZeroOrMore);

    llvm::cl::ParseCommandLineOptions(argc, argv);
    
    return llvm::TableGenMain(*argv, [&](llvm::raw_ostream &os, const llvm::RecordKeeper &recordKeeper){
        os << "{-# GHC_OPTIONS -pgmPgcc  -optP-E #-}" << "\n"
           << "{-# LANGUAGE CPP #-}" << "\n"
           << "#include <" << preprocessor << ">" << "\n"
           << "module " << hsModuleName << " where\n"
           << "import MLIR" << "\n"
           << "import MLIR.AutogenUtils" << "\n\n";
        for (auto m : includedModules) os << m << "\n";
        os << "\n";

        for (auto record : recordKeeper.getAllDerivedDefinitions("Op")) {
            

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
