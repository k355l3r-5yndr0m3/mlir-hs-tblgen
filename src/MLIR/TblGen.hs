{-# LANGUAGE ForeignFunctionInterface, ViewPatterns #-}
module MLIR.TblGen (
  hsGenerate
) where
import Control.Monad
import Data.Functor

import Foreign (Ptr, withArrayLen, free)
import Foreign.C (CInt(..), CString, newCString)


hsGenerate :: [FilePath] -> FilePath -> String -> String -> [String] -> FilePath -> IO ()
hsGenerate includeDirs tableGenFile header moduleName importedModule outputFile = hsGenerator args
  where args = "hsGenerator":mconcat[includeDirs <&> (\x -> '-':'I':x),
                                     mconcat (importedModule <&> \m -> ["-i", m]),
                                     ["-m", moduleName, "-o", outputFile, "-p", header, tableGenFile]]

foreign import ccall unsafe "hs_generator" 
  hsGenerator' :: CInt -> Ptr CString -> IO ()
hsGenerator :: [String] -> IO ()
hsGenerator args = do 
  args' <- forM args newCString
  withArrayLen args' $ \(fromIntegral -> argc) argv ->
    hsGenerator' argc argv
  forM_ args' free


