module CSVQueryEvaluator where
import CSVQueryGrammar
import Control.Applicative (liftA3, liftA2)
import System.Directory (doesFileExist)
import Data.Char (isSpace)
import Data.List
import System.IO

data EnvironmentVariable = EnvInt Int | EnvBool Bool | EnvString (IO String) | EnvGrid (IO [[IO String]])
type Environment = [(String, IO EnvironmentVariable)]

getAssignmentOfVariable :: String -> Environment -> IO EnvironmentVariable
getAssignmentOfVariable variable [] = error ("Couldn't find assignment for " ++ variable) -- Should never occur
getAssignmentOfVariable variable ((currentVariable, currentAssignment) : environment) 
  | variable == currentVariable = currentAssignment
  | otherwise = getAssignmentOfVariable variable environment

addVariableAssignment :: String -> IO EnvironmentVariable -> Environment -> Environment
addVariableAssignment variable variableAssignment environment = [(variable, variableAssignment)] ++ environment

evaluateOutput :: Grammar -> Environment
evaluateOutput (SingleExpression expression) = addVariableAssignment "output" (evaluateExpression [] expression) []
evaluateOutput (MultipleExpression assignments expression) = addVariableAssignment "output" (evaluateExpression updatedEnvironment expression) updatedEnvironment
  where
    updatedEnvironment = handleAssignments [] assignments

handleAssignments :: Environment -> Assignment -> Environment
handleAssignments environment (SingleAssignment _ variable expression) = addVariableAssignment variable (evaluateExpression environment expression) environment
handleAssignments environment (MultipleAssignment assignments _ variable expression) = addVariableAssignment variable (evaluateExpression updatedEnvironment expression) (environment ++ updatedEnvironment)
  where
    updatedEnvironment = handleAssignments environment assignments


evaluateExpression :: Environment -> Expression -> IO EnvironmentVariable
evaluateExpression environment (Var variable) = getAssignmentOfVariable variable environment
evaluateExpression environment (Int int) = return (EnvInt int)
evaluateExpression environment (BooleanTrue) = return (EnvBool True)
evaluateExpression environment (BooleanFalse) = return (EnvBool False)
evaluateExpression environment (String str) = return (EnvString (return (removeQuotationMarks str)))
evaluateExpression environment (Grid grid) = return (EnvGrid (checkGridIO (evaluateGridIO environment grid)))
evaluateExpression environment (Read grid) = return (EnvGrid (checkGridIO (readGridIO (evaluateExpression environment grid))))
evaluateExpression environment (Empty) = return (EnvGrid (return([[]])))

evaluateExpression environment (AddRows expression1 expression2) = evaluateAddRowsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (AddColumns expression1 expression2) = evaluateAddColumnsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (InsertRows expression1 expression2 expression3) = evaluateInsertRowsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2) (evaluateExpression environment expression3)
evaluateExpression environment (InsertColumns expression1 expression2 expression3) = evaluateInsertColumnsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2) (evaluateExpression environment expression3)
evaluateExpression environment (DropRow expression1 expression2) = evaluateDropRowIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (DropColumn expression1 expression2) = evaluateDropColumnIO (evaluateExpression environment expression1) (evaluateExpression environment expression2) 
evaluateExpression environment (ReverseRow expression1 expression2) = evaluateReverseRowIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (ReverseColumn expression1 expression2) = evaluateReverseColumnIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)

evaluateExpression environment (RepeatRow expectedType expression1 expression2 expression3 expression4) = evaluateRepeatRowIO environment expectedType expression1 (evaluateExpression environment expression2) (evaluateExpression environment expression3) expression4
evaluateExpression environment (RepeatColumn expectedType expression1 expression2 expression3 expression4) = evaluateRepeatColIO environment expectedType expression1 (evaluateExpression environment expression2) (evaluateExpression environment expression3) expression4
evaluateExpression environment (RepeatWhile expectedType expression1 expression2 expression3 expression4) = evaluateRepeatWhileIO environment expectedType expression1 (evaluateExpression environment expression2) expression3 expression4

evaluateExpression environment (ContainsString expression1 expression2) = evaluateContainsStringIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (ContainsCell expression1 expression2) = evaluateContainsCellIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (ContainsRows expression1 expression2) = evaluateContainsRowsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (ContainsColumns expression1 expression2) = evaluateContainsColumnsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (ContainsCharacters expression1 expression2) = evaluateContainsCharactersIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)

evaluateExpression environment (GetString expression1 expression2 expression3) = evaluateGetStringIO (evaluateExpression environment expression1) (evaluateExpression environment expression2) (evaluateExpression environment expression3)
evaluateExpression environment (GetCell expression1 expression2 expression3) = evaluateGetCellIO (evaluateExpression environment expression1) (evaluateExpression environment expression2) (evaluateExpression environment expression3)
evaluateExpression environment (GetRow expression1 expression2) = evaluateGetRowIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (GetColumn expression1 expression2) = evaluateGetColumnIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)

evaluateExpression environment (GetCharacter expression1 expression2) = evaluateGetCharacterIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Concatenate expression1 expression2) = evaluateConcatenateIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (IntegerToString expression1) = evaluateIntegerToStringIO (evaluateExpression environment expression1)

evaluateExpression environment (And expression1 expression2) = evaluateAndIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Or expression1 expression2) = evaluateOrIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Equals expression1 expression2) = evaluateEqualsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (NotEquals expression1 expression2) = evaluateNotEqualsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (LessThan expression1 expression2) = evaluateLessThanIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (GreaterThan expression1 expression2) = evaluateGreaterThanIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (LessThanOrEquals expression1 expression2) = evaluateLessThanOrEqualsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (GreaterThanOrEquals expression1 expression2) = evaluateGreaterThanOrEqualsIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)

evaluateExpression environment (Plus expression1 expression2) = evaluatePlusIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Minus expression1 expression2) = evaluateMinusIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Multiply expression1 expression2) = evaluateMultiplyIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Divide expression1 expression2) = evaluateDivideIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Mod expression1 expression2) = evaluateModIO (evaluateExpression environment expression1) (evaluateExpression environment expression2)
evaluateExpression environment (Negate expression1) = evaluateNegateIO (evaluateExpression environment expression1)
evaluateExpression environment (NumRows expression1) = evaluateNumRowsIO (evaluateExpression environment expression1)
evaluateExpression environment (NumColumns expression1) = evaluateNumColumnsIO (evaluateExpression environment expression1)
evaluateExpression environment (NumCharacters expression1) = evaluateNumCharactersIO (evaluateExpression environment expression1)

evaluateExpression environment (IfThenElse expression1 expression2 expression3) = evaluateIfThenElseIO (evaluateExpression environment expression1) (evaluateExpression environment expression2) (evaluateExpression environment expression3)

-- Grid functions

evaluateAddRowsIO :: IO EnvironmentVariable ->  IO EnvironmentVariable -> IO EnvironmentVariable
evaluateAddRowsIO ioGrid1 ioGrid2 = do
  EnvGrid ioGrid3 <- ioGrid1
  EnvGrid ioGrid4 <- ioGrid2
  grid1 <- ioGrid3
  grid2 <- ioGrid4
  let newGrid = evaluateAddRows grid1 grid2
  return (EnvGrid (return newGrid))
evaluateAddRows ::  [[IO String]] -> [[IO String]] -> [[IO String]]
evaluateAddRows [[]] [[]] = [[]]
evaluateAddRows grid1 [[]] = grid1
evaluateAddRows [[]] grid2 = grid2
evaluateAddRows grid1 grid2
  | checkGridBool (grid1 ++ grid2) = (grid1 ++ grid2)
  | otherwise = error ("Incompatible grid sizes in function addRows")

checkGridBool :: [[IO String]] -> Bool
checkGridBool [[]] = True
checkGridBool (x:xs) = (all (\row -> length row == length x) xs)

evaluateAddColumnsIO :: IO EnvironmentVariable ->  IO EnvironmentVariable -> IO EnvironmentVariable
evaluateAddColumnsIO ioGrid1 ioGrid2 = do
  EnvGrid ioGrid3 <- ioGrid1
  EnvGrid ioGrid4 <- ioGrid2
  grid1 <- ioGrid3
  grid2 <- ioGrid4
  let newGrid = evaluateAddColumns grid1 grid2
  return (EnvGrid (return newGrid))
evaluateAddColumns :: [[IO String]] -> [[IO String]] -> [[IO String]]
evaluateAddColumns [[]] [[]] = [[]]
evaluateAddColumns grid1 [[]] = grid1
evaluateAddColumns [[]] grid2 = grid2
evaluateAddColumns grid1 grid2 = (addColumns grid1 grid2)

addColumns :: [[IO String]] -> [[IO String]] -> [[IO String]]
addColumns [] [] = []
addColumns (row1 : grid1) (row2 : grid2) = [row1 ++ row2] ++ (addColumns grid1 grid2)
addColumns _ _ = error "Incompatible grid sizes in function addCols"

evaluateInsertRowsIO :: IO EnvironmentVariable ->  IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateInsertRowsIO ioGrid1 ioGrid2 ioInt1 = do
  EnvGrid ioGrid3 <- ioGrid1
  EnvGrid ioGrid4 <- ioGrid2
  grid1 <- ioGrid3
  grid2 <- ioGrid4
  EnvInt int1 <- ioInt1
  let newGrid = evaluateInsertRows grid1 grid2 (fromIntegral int1)
  return (EnvGrid (return newGrid))
evaluateInsertRows :: [[IO String]] -> [[IO String]] -> Int -> [[IO String]]
evaluateInsertRows grid1 [[]] int1
  | int1 < 0 || int1 > (length (grid1)) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function insertRows")
  | otherwise = grid1
evaluateInsertRows [[]] grid2 int1 
  | int1 /= 0 = error ("Index " ++ show int1 ++ " out of range for grid size (0,0) in function insertRows")
  | otherwise = grid2
evaluateInsertRows grid1 grid2 int1
  | int1 < 0 || int1 > (length (grid1)) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function insertRows")
  | length (grid1 !! 0) /= length (grid2 !! 0) = error ("Incompatible grid sizes in function insertRows")
  | otherwise = (take int1 grid1 ++ grid2 ++ drop int1 grid1)

evaluateInsertColumnsIO :: IO EnvironmentVariable ->  IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateInsertColumnsIO ioGrid1 ioGrid2 ioInt1 = do
  EnvGrid ioGrid3 <- ioGrid1
  EnvGrid ioGrid4 <- ioGrid2
  grid1 <- ioGrid3
  grid2 <- ioGrid4
  EnvInt int1 <- ioInt1
  let newGrid = evaluateInsertColumns grid1 grid2 (fromIntegral int1)
  return (EnvGrid (return newGrid))
evaluateInsertColumns :: [[IO String]] -> [[IO String]] -> Int -> [[IO String]]
evaluateInsertColumns grid1 [[]] int1
  | int1 < 0 || int1 > length (grid1 !! 0) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function insertCols")
  | otherwise = grid1
evaluateInsertColumns [[]] grid2 int1
  | int1 /= 0 = error ("Index " ++ show int1 ++ " out of range for grid size (0,0) in function insertCols")
  | otherwise = grid2
evaluateInsertColumns grid1 grid2 int1
  | int1 < 0 || int1 > length (grid1 !! 0) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function insertCols")
  | length grid1 /= length grid2 = error ("Incompatible grid sizes in function insertCols")
  | otherwise = (insertColumns grid1 grid2 int1)

insertColumns :: [[IO String]] -> [[IO String]] -> Int -> [[IO String]]
insertColumns (row1 : []) (row2: []) int1 = [take int1 row1 ++ row2 ++ drop int1 row1]
insertColumns (row1 : grid1) (row2: grid2) int1 = [take int1 row1 ++ row2 ++ drop int1 row1] ++ (insertColumns grid1 grid2 int1) 

evaluateDropRowIO :: IO EnvironmentVariable ->  IO EnvironmentVariable ->  IO EnvironmentVariable
evaluateDropRowIO ioGrid1 ioInt1 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  grid1 <- ioGrid
  let newGrid = evaluateDropRow grid1 (fromIntegral int1)
  return (EnvGrid (return newGrid))
evaluateDropRow :: [[IO String]] -> Int -> [[IO String]]
evaluateDropRow [[]] int1 = error ("Index " ++ show int1 ++ " out of range for grid size (0,0) in function dropRow")
evaluateDropRow grid1 int1
  | int1 < 0 || int1 >= (length grid1) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function dropRow")
  | otherwise = (dropIndex int1 grid1)

evaluateDropColumnIO :: IO EnvironmentVariable ->  IO EnvironmentVariable ->  IO EnvironmentVariable
evaluateDropColumnIO ioGrid1 ioInt1 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  grid1 <- ioGrid
  let newGrid = evaluateDropColumn grid1 (fromIntegral int1)
  return (EnvGrid (return newGrid))
evaluateDropColumn :: [[IO String]] -> Int -> [[IO String]]
evaluateDropColumn [[]] int1  = error ("Index " ++ show int1 ++ " out of range for grid size (0,0) in function dropCol")
evaluateDropColumn grid1 int1
  | int1 < 0 || int1 >= length (grid1 !! 0) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function dropCol")
  | otherwise = (dropColumn grid1 int1)

dropColumn :: [[IO String]] -> Int -> [[IO String]]
dropColumn (row1 : []) int1 = [dropIndex int1 row1]
dropColumn (row1 : grid1) int1 = [dropIndex int1 row1] ++ dropColumn grid1 int1 

dropIndex :: Int -> [a] -> [a]
dropIndex n xs = take n xs ++ drop (n + 1) xs

evaluateReverseRowIO :: IO EnvironmentVariable ->  IO EnvironmentVariable ->  IO EnvironmentVariable
evaluateReverseRowIO ioGrid1 ioInt1 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  grid1 <- ioGrid
  let newGrid = evaluateReverseRow grid1 (fromIntegral int1)
  return (EnvGrid (return newGrid))
evaluateReverseRow :: [[IO String]] -> Int -> [[IO String]]
evaluateReverseRow [[]] int1 = error ("Index " ++ show int1 ++ " out of range for grid size (0,0) in function reverseRow")
evaluateReverseRow grid1 int1
  | int1 < 0 || int1 >= (length grid1) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function reverseRow")
  | otherwise = (take int1 grid1 ++ [reverse (grid1 !! int1)] ++ drop (int1 + 1) grid1)

evaluateReverseColumnIO :: IO EnvironmentVariable ->  IO EnvironmentVariable ->  IO EnvironmentVariable
evaluateReverseColumnIO ioGrid1 ioInt1 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  grid1 <- ioGrid
  let newGrid = evaluateReverseColumn grid1 (fromIntegral int1)
  return (EnvGrid (return newGrid))
evaluateReverseColumn :: [[IO String]] -> Int -> [[IO String]]
evaluateReverseColumn [[]] int1  = error ("Index " ++ show int1 ++ " out of range for grid size (0,0) in function reverseCol")
evaluateReverseColumn grid1 int1
  | int1 < 0 || int1 >= (length (grid1 !! 0)) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function reverseCol")
  | otherwise = (reverseColumn int1 grid1)

reverseColumn :: Int -> [[a]] -> [[a]]
reverseColumn x xs = zipWith (\row newVal -> replaceAt x newVal row) xs (reverse (map (!! x) xs))
  where replaceAt n newVal row = take n row ++ [newVal] ++ drop (n + 1) row

-- Repeat statements ----------------------------------------------

evaluateRepeatRowIO :: Environment -> Type -> String -> IO EnvironmentVariable -> IO EnvironmentVariable -> Expression -> IO EnvironmentVariable
evaluateRepeatRowIO environment expectedType variable ioStart ioEnd expression = do
  EnvInt start <- ioStart
  EnvInt end <- ioEnd
  loop <- evaluateRepeatRow environment expectedType variable start end expression
  return (loop)
evaluateRepeatRow :: Environment -> Type -> String -> Int -> Int -> Expression -> IO EnvironmentVariable
evaluateRepeatRow environment expectedType variable start end expression 
  | start >= end = return (EnvGrid (return [[]]))
  | otherwise = evaluateAddRowsIO (evaluateExpression updatedEnvironment expression) (evaluateRepeatRowIO updatedEnvironment expectedType variable (return (EnvInt (start + 1))) (return (EnvInt (end))) expression)
  where 
    updatedEnvironment = addVariableAssignment variable (return (EnvInt start)) environment

evaluateRepeatColIO :: Environment -> Type -> String -> IO EnvironmentVariable -> IO EnvironmentVariable -> Expression -> IO EnvironmentVariable
evaluateRepeatColIO environment expectedType variable ioStart ioEnd expression = do
  EnvInt start <- ioStart
  EnvInt end <- ioEnd
  loop <- evaluateRepeatCol environment expectedType variable start end expression
  return (loop)
evaluateRepeatCol :: Environment -> Type -> String -> Int -> Int -> Expression -> IO EnvironmentVariable
evaluateRepeatCol environment expectedType variable start end expression 
  | start >= end = return (EnvGrid (return [[]]))
  | otherwise = evaluateAddColumnsIO (evaluateExpression updatedEnvironment expression) (evaluateRepeatColIO updatedEnvironment expectedType variable (return (EnvInt (start + 1))) (return (EnvInt (end))) expression)
  where 
    updatedEnvironment = addVariableAssignment variable (return (EnvInt start)) environment

evaluateRepeatWhileIO :: Environment -> Type -> String -> IO EnvironmentVariable -> Expression -> Expression -> IO EnvironmentVariable
evaluateRepeatWhileIO environment expectedType variable initial condition expression = do
  let updatedEnvironment = addVariableAssignment variable initial environment
  EnvBool evaluatedCondition <- evaluateExpression updatedEnvironment condition
  if (evaluatedCondition == False)
    then do
      initial
    else do
      loop <- evaluateRepeatWhileIO (addVariableAssignment variable initial environment) expectedType variable (evaluateExpression updatedEnvironment expression) condition expression
      return loop
 
-- Contains ----------------------------------------------

evaluateContainsStringIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateContainsStringIO ioGrid1 ioStr1 = do
  EnvGrid ioGrid <- ioGrid1
  grid1 <- ioGrid
  EnvString str1 <- ioStr1
  str2 <- str1
  result <- evaluateContainsString grid1 str2
  return (EnvBool result)

evaluateContainsString :: [[IO String]] -> String -> IO Bool
evaluateContainsString grid1 str1 = do
  strings <- mapM (mapM id) grid1
  return $ any (== str1) (concat strings)

evaluateContainsCellIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateContainsCellIO ioGrid1 ioGrid2 = do
  EnvGrid ioGrid3 <- ioGrid1
  EnvGrid ioGrid4 <- ioGrid2
  grid1 <- ioGrid3
  grid2 <- ioGrid4
  result <- evaluateContainsCell grid1 grid2
  return (EnvBool result)

evaluateContainsCell :: [[IO String]] -> [[IO String]] -> IO Bool
evaluateContainsCell grid1 [[strIO]] = do
  str1 <- strIO
  grid <- mapM (mapM id) grid1
  return $ any (== str1) (concat grid)
evaluateContainsCell _ _ = error "Incompatible grid sizes in function containsCell"

evaluateContainsRowsIO :: IO EnvironmentVariable ->  IO EnvironmentVariable ->  IO EnvironmentVariable
evaluateContainsRowsIO ioGrid1 ioGrid2 = do
  EnvGrid ioGrid3 <- ioGrid1
  EnvGrid ioGrid4 <- ioGrid2
  grid1 <- ioGrid3
  grid2 <- ioGrid4
  result <- evaluateContainsRows grid1 grid2
  return (EnvBool result)
evaluateContainsRows :: [[IO String]] -> [[IO String]] -> IO Bool
evaluateContainsRows ioGrid1 ioGrid2 = do
  grid1 <- mapM (mapM id) ioGrid1
  grid2 <- mapM (mapM id) ioGrid2
  if length (grid1 !! 0) /= length (grid2 !! 0) then do
    error ("Incompatible grid sizes in function containsRow")
  else do
    return (grid2 `isInfixOf` grid1)

evaluateContainsColumnsIO :: IO EnvironmentVariable ->  IO EnvironmentVariable ->  IO EnvironmentVariable
evaluateContainsColumnsIO ioGrid1 ioGrid2 = do
  EnvGrid ioGrid3 <- ioGrid1
  EnvGrid ioGrid4 <- ioGrid2
  grid1 <- ioGrid3
  grid2 <- ioGrid4
  result <- evaluateContainsColumns grid1 grid2
  return (EnvBool result)
evaluateContainsColumns :: [[IO String]] -> [[IO String]] -> IO Bool
evaluateContainsColumns ioGrid1 ioGrid2 = do
  grid1 <- mapM (mapM id) ioGrid1
  grid2 <- mapM (mapM id) ioGrid2
  if (length grid1 /= length grid2) then do 
    error ("Incompatible grid sizes in function containsCol")
  else do
    return (and (zipWith isInfixOf grid2 grid1))
    
evaluateContainsCharactersIO :: IO EnvironmentVariable ->  IO EnvironmentVariable ->  IO EnvironmentVariable
evaluateContainsCharactersIO ioStr1 ioStr2 = do
  EnvString str1 <- ioStr1
  EnvString str2 <- ioStr2
  str3 <- str1
  str4 <- str2
  return (EnvBool (evaluateContainsCharacters str3 str4))
evaluateContainsCharacters :: String -> String -> Bool
evaluateContainsCharacters str1 str2 = (isInfixOf str2 str1)

-- Get String / Grid ----------------------------------------------

evaluateGetStringIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateGetStringIO ioGrid1 ioInt1 ioInt2 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  EnvInt int2 <- ioInt2
  grid1 <- ioGrid
  return (EnvString (evaluateGetString grid1 int1 int2))
evaluateGetString :: [[IO String]] -> Int -> Int -> IO String
evaluateGetString [[]] int1 int2 =  error ("Index (" ++ show int1 ++ "," ++ show int2 ++ ") out of range for grid size (0,0) in function getStr")
evaluateGetString grid1 int1 int2 
  | int1 < 0 || int1 >= (length grid1) || int2 < 0 || int2 >= length(grid1 !! 0) = error ("Index (" ++ show int1 ++ "," ++ show int2 ++ ") out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function getStr")
  | otherwise = (grid1 !! int1 !! int2)

evaluateGetCellIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateGetCellIO ioGrid1 ioInt1 ioInt2 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  EnvInt int2 <- ioInt2
  grid1 <- ioGrid
  return (EnvGrid (return (evaluateGetCell grid1 int1 int2)))
evaluateGetCell :: [[IO String]] -> Int -> Int -> [[IO String]]
evaluateGetCell [[]] int1 int2 =  error ("Index (" ++ show int1 ++ "," ++ show int2 ++ ") out of range for grid size (0,0) in function getCell")
evaluateGetCell grid1 int1 int2 
  | int1 < 0 || int1 >= (length grid1) || int2 < 0 || int2 >= length(grid1 !! 0) = error ("Index (" ++ show int1 ++ "," ++ show int2 ++ ") out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function getCell")
  | otherwise = [[grid1 !! int1 !! int2]]

evaluateGetRowIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateGetRowIO ioGrid1 ioInt1 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  grid1 <- ioGrid
  return (EnvGrid (return (evaluateGetRow grid1 int1)))
evaluateGetRow :: [[IO String]] -> Int -> [[IO String]]
evaluateGetRow [[]] int1 = error ("Index (" ++ show int1 ++ ") out of range for grid size (0,0) in function getRow")
evaluateGetRow grid1 int1
  | int1 < 0 || int1 >= (length grid1) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function getRow")
  | otherwise = [grid1 !! int1]

evaluateGetColumnIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateGetColumnIO ioGrid1 ioInt1 = do
  EnvGrid ioGrid <- ioGrid1
  EnvInt int1 <- ioInt1
  grid1 <- ioGrid
  return (EnvGrid (return (evaluateGetColumn grid1 int1)))
evaluateGetColumn :: [[IO String]] -> Int -> [[IO String]]
evaluateGetColumn [[]] int1 = error ("Index (" ++ show int1 ++ ") out of range for grid size (0,0) in function getCol")
evaluateGetColumn grid1 int1
  | int1 < 0 || int1 >= (length (grid1 !! 0)) = error ("Index " ++ show int1 ++ " out of range for grid size (" ++ show (length grid1) ++ "," ++ show (length (grid1 !! 0)) ++ ") in function getCol")
  | otherwise = [[row !! int1] | row <- grid1, int1 >= 0, int1 < length row]

-- String functions

evaluateConcatenateIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateConcatenateIO ioStr1 ioStr2 = do
  EnvString str1 <- ioStr1
  EnvString str2 <- ioStr2
  str3 <- str1
  str4 <- str2
  return (EnvString (return (evaluateConcatenate str3 str4)))
evaluateConcatenate :: String -> String -> String
evaluateConcatenate str1 str2 = str1 ++ str2

evaluateGetCharacterIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateGetCharacterIO ioStr1 ioInt1 = do
  EnvString str1 <- ioStr1
  str3 <- str1
  EnvInt int1 <- ioInt1
  return (EnvString (return (evaluateGetCharacter str3 int1)))
evaluateGetCharacter :: String -> Int -> String
evaluateGetCharacter str1 int1
  | int1 < 0 || int1 >= (length str1) = error ("Index " ++ show int1 ++ " out of range for String length " ++ show (length str1) ++ " in function getChar")
  | otherwise = [str1 !! int1]

evaluateIntegerToStringIO :: IO EnvironmentVariable -> IO EnvironmentVariable
evaluateIntegerToStringIO ioInt1 = do
  EnvInt int1 <- ioInt1
  return (EnvString (return (show int1)))

-- Boolean ----------------------------------------------

evaluateAndIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateAndIO = liftA2 evaluateAnd
evaluateAnd :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateAnd (EnvBool bool1) (EnvBool bool2) = (EnvBool (bool1 && bool2))

evaluateOrIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateOrIO = liftA2 evaluateOr
evaluateOr :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateOr (EnvBool bool1) (EnvBool bool2) = (EnvBool (bool1 || bool2))

evaluateEqualsIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateEqualsIO ioVal1 ioVal2 = do
  val1 <- ioVal1
  val2 <- ioVal2
  eq <- evaluateEquals val1 val2
  return eq
evaluateEquals :: EnvironmentVariable -> EnvironmentVariable -> IO EnvironmentVariable
evaluateEquals (EnvInt int1) (EnvInt int2) = return (EnvBool (int1 == int2))
evaluateEquals (EnvBool bool1) (EnvBool bool2) = return (EnvBool (bool1 == bool2))
evaluateEquals (EnvString ioStr1) (EnvString ioStr2) = do
  str1 <- ioStr1
  str2 <- ioStr2
  return (EnvBool (str1 == str2))
evaluateEquals (EnvGrid ioGrid1) (EnvGrid ioGrid2) = do
  grid1 <- ioGrid1
  grid2 <- ioGrid2
  grid3 <- mapM (mapM id) grid1
  grid4 <- mapM (mapM id) grid2
  return (EnvBool (grid3 == grid4))

evaluateNotEqualsIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateNotEqualsIO ioVal1 ioVal2 = do
  val1 <- ioVal1
  val2 <- ioVal2
  eq <- evaluateEquals val1 val2
  return eq
evaluateNotEquals :: EnvironmentVariable -> EnvironmentVariable -> IO EnvironmentVariable
evaluateNotEquals (EnvInt int1) (EnvInt int2) = return (EnvBool (int1 /= int2))
evaluateNotEquals (EnvBool bool1) (EnvBool bool2) = return (EnvBool (bool1 /= bool2))
evaluateNotEquals (EnvString ioStr1) (EnvString ioStr2) = do
  str1 <- ioStr1
  str2 <- ioStr2
  return (EnvBool (str1 /= str2))
evaluateNotEquals (EnvGrid ioGrid1) (EnvGrid ioGrid2) = do
  grid1 <- ioGrid1
  grid2 <- ioGrid2
  grid3 <- mapM (mapM id) grid1
  grid4 <- mapM (mapM id) grid2
  return (EnvBool (grid3 /= grid4))

evaluateLessThanIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateLessThanIO = liftA2 evaluateLessThan
evaluateLessThan :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateLessThan (EnvInt int1) (EnvInt int2) = (EnvBool (int1 < int2))

evaluateGreaterThanIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateGreaterThanIO = liftA2 evaluateGreaterThan
evaluateGreaterThan :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateGreaterThan (EnvInt int1) (EnvInt int2) = (EnvBool (int1 > int2))

evaluateLessThanOrEqualsIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateLessThanOrEqualsIO = liftA2 evaluateLessThanOrEquals
evaluateLessThanOrEquals :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateLessThanOrEquals (EnvInt int1) (EnvInt int2) = (EnvBool (int1 <= int2))

evaluateGreaterThanOrEqualsIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateGreaterThanOrEqualsIO = liftA2 evaluateGreaterThanOrEquals
evaluateGreaterThanOrEquals :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateGreaterThanOrEquals (EnvInt int1) (EnvInt int2) = (EnvBool (int1 >= int2))

-- Arithmatic

evaluatePlusIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluatePlusIO = liftA2 evaluatePlus
evaluatePlus :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluatePlus (EnvInt int1) (EnvInt int2) = (EnvInt (int1 + int2))

evaluateMinusIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateMinusIO = liftA2 evaluateMinus
evaluateMinus :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateMinus (EnvInt int1) (EnvInt int2) = (EnvInt (int1 - int2))

evaluateMultiplyIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateMultiplyIO = liftA2 evaluateMultiply
evaluateMultiply :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateMultiply (EnvInt int1) (EnvInt int2) = (EnvInt (int1 * int2))

evaluateDivideIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateDivideIO = liftA2 evaluateDivide
evaluateDivide :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateDivide (EnvInt int1) (EnvInt 0) = error "Division by zero in function /"
evaluateDivide (EnvInt int1) (EnvInt int2) = (EnvInt (int1 `div` int2))

evaluateModIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateModIO = liftA2 evaluateMod
evaluateMod :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateMod (EnvInt int1) (EnvInt 0) = error "Division by zero in function %"
evaluateMod (EnvInt int1) (EnvInt int2) = (EnvInt (int1 `mod` int2))

evaluateNegateIO :: IO EnvironmentVariable -> IO EnvironmentVariable
evaluateNegateIO = fmap evaluateNegate
evaluateNegate :: EnvironmentVariable -> EnvironmentVariable
evaluateNegate (EnvInt int1) = (EnvInt (-int1))

-- Get Sizes ----------------------------------------------

evaluateNumRowsIO :: IO EnvironmentVariable -> IO EnvironmentVariable
evaluateNumRowsIO ioGrid1 = do
  EnvGrid ioGrid <- ioGrid1
  grid1 <- ioGrid
  return (EnvInt (evaluateNumRows grid1))
evaluateNumRows :: [[IO String]] -> Int
evaluateNumRows [[]] = 0 
evaluateNumRows grid1 = length grid1

evaluateNumColumnsIO :: IO EnvironmentVariable -> IO EnvironmentVariable
evaluateNumColumnsIO ioGrid1 = do
  EnvGrid ioGrid <- ioGrid1
  grid1 <- ioGrid
  return (EnvInt (evaluateNumColumns grid1))
evaluateNumColumns :: [[IO String]] -> Int
evaluateNumColumns [[]] = 0 
evaluateNumColumns grid1 = length (grid1 !! 0) 

evaluateNumCharactersIO :: IO EnvironmentVariable -> IO EnvironmentVariable
evaluateNumCharactersIO ioStr = do
  EnvString str <- ioStr
  str2 <- str
  return (EnvInt (evaluateNumCharacters str2))
evaluateNumCharacters :: String -> Int
evaluateNumCharacters str1 = length str1

-- If statements ----------------------------------------------

evaluateIfThenElseIO :: IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable -> IO EnvironmentVariable
evaluateIfThenElseIO = liftA3 evaluateIfThenElse
evaluateIfThenElse :: EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable -> EnvironmentVariable
evaluateIfThenElse (EnvBool True) envVar1 envVar2 = envVar1
evaluateIfThenElse (EnvBool False) envVar1 envVar2 = envVar2

-- Grid Construction

evaluateGridIO :: Environment -> Arr -> IO [[IO String]]
evaluateGridIO environment arr = return (evaluateGrid environment arr)

evaluateGrid :: Environment -> Arr -> [[IO String]]
evaluateGrid environment (SingleArray row) = [evaluateRow environment row]
evaluateGrid environment (MultipleArray row rest) = [evaluateRow environment row] ++ (evaluateGrid environment rest)

evaluateRow :: Environment -> Row -> [IO String]
evaluateRow environment (SingleRow expression) = [evaluateCell(evaluateExpression environment expression)]
evaluateRow environment (MultipleRow expression rest) = [evaluateCell(evaluateExpression environment expression)] ++ (evaluateRow environment rest)

evaluateCell :: IO EnvironmentVariable -> IO String
evaluateCell ioVal = do
  val <- ioVal
  case val of
    EnvInt int -> do
      return (show int)
    EnvString ioStr -> do
      str <- ioStr
      return (removeWhitespace str)

-- File Reading

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr (\char (x:xs) -> if char == delimiter then "":x:xs else (char:x):xs) [""]

readGridIO :: IO EnvironmentVariable -> IO [[IO String]]
readGridIO ioFileName = do
  EnvString fileName <- ioFileName
  fileName2 <- fileName
  file <- readGrid fileName2
  return file

readGrid :: String -> IO [[IO String]]
readGrid fileName = do
  fileExists <- doesFileExist fileName
  if not fileExists
    then error $ "File " ++ fileName ++ " does not exist"
    else do
      content <- readFile fileName
      let baseRows = lines content
          hasTrailingNewline = not (null content) && last content == '\n'
          rows = if hasTrailingNewline then baseRows ++ [""] else baseRows
      if null rows
        then return [[]]
        else return (map (\row -> map (\s -> return (removeWhitespace s)) (splitBy ',' row)) rows)

-- Validate Grid

checkGridIO :: IO [[IO String]] -> IO [[IO String]]
checkGridIO = fmap checkGrid

checkGrid :: [[IO String]] -> [[IO String]]
checkGrid [] = []
checkGrid (x:xs) = if (all (\row -> length row == length x) xs) then (x:xs) else (error ("Invalid grid construction"))

-- Helper functions

removeQuotationMarks :: String -> String
removeQuotationMarks str = delete '"' (delete '"' str)

removeWhitespace :: String -> String
removeWhitespace str = reverse (dropWhile isSpace (reverse (dropWhile isSpace str)))
