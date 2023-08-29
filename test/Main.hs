module Main (main) where
import MLIR.TblGen

main :: IO ()
main = do
  hsGenerate ["/home/cha0s/.local/include"] 
             "/home/cha0s/.local/include/mlir/Dialect/ControlFlow/IR/ControlFlowOps.td"
             "header.h"
             "MLIR.Dialect.ControlFlow"
             []
             "ControlFlow.hs"
  
  
  hsGenerate ["/home/cha0s/.local/include"] 
             "/home/cha0s/.local/include/mlir/Dialect/Func/IR/FuncOps.td"
             "header.h"
             "MLIR.Dialect.Func"
             []
             "Func.hs"

  hsGenerate ["/home/cha0s/.local/include"] 
             "/home/cha0s/.local/include/mlir/Dialect/Arith/IR/ArithOps.td"
             "header.h"
             "MLIR.Dialect.Arith"
             []
             "Arith.hs"
