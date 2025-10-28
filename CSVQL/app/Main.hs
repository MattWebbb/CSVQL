module Main where
import CSVQueryTokens
import CSVQueryGrammar
import CSVQueryTypeChecker 
import CSVQueryEvaluator 

import System.Environment
import Control.Exception
import System.IO
import Data.List (sort, intercalate)

main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs
           sourceText <- readFile fileName
           
           let parsedProg = parseCSVQuery (alexScanTokens sourceText)
           let typeCheck = typeOfOutput parsedProg

           typeCheck `seq` do
             let environment = evaluateOutput parsedProg
             let output = getAssignmentOfVariable "output" environment
             printGridAsCSV output

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

printGridAsCSV :: IO EnvironmentVariable -> IO ()
printGridAsCSV io = do
  EnvGrid ioGrid <- io
  gridIOStrings <- ioGrid
  gridStrings <- mapM (mapM id) gridIOStrings
  let isEmpty = null gridStrings || all null gridStrings
  if isEmpty
    then return ()
    else putStr $ intercalate "\n" $ map (intercalate ",") (sort gridStrings)