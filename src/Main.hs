module Main(main) where

import System.Environment
import Lexer
import Parser
import Sentence
import System.IO
import Utils

main :: IO ()
main = do
  (fileName:rest) <- getArgs
  fHandle <- openFile fileName ReadMode
  thmString <- hGetContents fHandle
  let thm = processTheoremFile thmString
  case thm of
    Failed errMsg -> putStrLn errMsg
    Succeeded t -> do
      putStr $ show t
      putStrLn $ "\n\nis " ++ (show $ checkTheorem t)
  
processTheoremFile thmFileContents =
  (toTokens thmFileContents >>= parseTheoremToks)