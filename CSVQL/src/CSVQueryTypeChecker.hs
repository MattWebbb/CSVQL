module CSVQueryTypeChecker where
import CSVQueryGrammar

type Environment = [(String, Type)]

getTypeOfVariable :: String -> Environment -> Type
getTypeOfVariable variable [] = error ("Couldn't find assignment for " ++ variable)
getTypeOfVariable variable ((currentVariable, currentType) : environment) 
  | variable == currentVariable = currentType
  | otherwise = getTypeOfVariable variable environment

addVariableType :: String -> Type -> Environment -> Environment
addVariableType variable variableType environment = [(variable, variableType)] ++ environment

-- Grammar

typeOfOutput :: Grammar -> Type
typeOfOutput (SingleExpression expression) 
  | TypeGrid == actualType = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid) with actual type (" ++ displayType actualType ++ ") in assignment output") 
  where 
    actualType = typeOfExpression [] expression

typeOfOutput (MultipleExpression assignments expression) 
  | TypeGrid == actualType = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid) with actual type (" ++ displayType actualType ++ ") in assignment output") 
  where 
   updatedEnvironment = handleAssignments [] assignments
   actualType = typeOfExpression updatedEnvironment expression

-- Assignments

handleAssignments :: Environment -> Assignment -> Environment
handleAssignments environment (SingleAssignment expectedType variable expression) 
  | expectedType == actualType = addVariableType variable expectedType environment
  | otherwise = error ("Couldn't match expected type (" ++ displayType expectedType ++ ") with actual type (" ++ displayType actualType ++ ") in assignment " ++ variable)
  where
    actualType = typeOfExpression environment expression

handleAssignments environment (MultipleAssignment remainingAssignments expectedType variable expression)
  | expectedType == actualType = addVariableType variable expectedType updatedEnvironment
  | otherwise = error ("Couldn't match expected type (" ++ displayType expectedType ++ ") with actual type (" ++ displayType actualType ++ ") in assignment " ++ variable)
  where
    updatedEnvironment = handleAssignments environment remainingAssignments
    actualType = typeOfExpression updatedEnvironment expression

-- Expression

typeOfExpression :: Environment -> Expression -> Type

typeOfExpression environment (Var variable) = getTypeOfVariable variable environment 
typeOfExpression environment (Int int) = TypeInt

typeOfExpression environment (BooleanTrue) = TypeBool
typeOfExpression environment (BooleanFalse) = TypeBool

typeOfExpression environment (String str) = TypeString
typeOfExpression environment (Grid grid) = typeOfGrid environment grid
typeOfExpression environment (Empty) = TypeGrid

-- GRID functions --

typeOfExpression environment (Read expression1)
  | (TypeString) == (actualType1) = TypeGrid
  | otherwise = error ("Couldn't match expected type (String) with actual type (" ++ displayType actualType1 ++ ") in function read")
  where 
    actualType1 = typeOfExpression environment expression1

typeOfExpression environment (AddRows expression1 expression2) 
  | (TypeGrid, TypeGrid) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function addRows")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (AddColumns expression1 expression2) 
  | (TypeGrid, TypeGrid) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function addCols")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (InsertRows expression1 expression2 expression3) 
  | (TypeGrid, TypeGrid, TypeInt) == (actualType1, actualType2, actualType3) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ", " ++ displayType actualType3 ++ ") in function insertRows")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2
    actualType3 = typeOfExpression environment expression3

typeOfExpression environment (InsertColumns expression1 expression2 expression3) 
  | (TypeGrid, TypeGrid, TypeInt) == (actualType1, actualType2, actualType3) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ", " ++ displayType actualType3 ++ ") in function insertCols")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2
    actualType3 = typeOfExpression environment expression3

typeOfExpression environment (DropRow expression1 expression2) 
  | (TypeGrid, TypeInt) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function dropRow")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (DropColumn expression1 expression2) 
  | (TypeGrid, TypeInt) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function dropCol")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (ReverseRow expression1 expression2) 
  | (TypeGrid, TypeInt) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function ReverseRow")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (ReverseColumn expression1 expression2) 
  | (TypeGrid, TypeInt) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function ReverseCol")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

-- REPEAT STATEMENTS --


typeOfExpression environment (RepeatRow expectedType variable expression1 expression2 expression3) 
  | (TypeInt, TypeInt, TypeGrid) == (actualType1, actualType2, actualType3) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Int, Int, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ", " ++ displayType actualType3 ++ ") in function repeatRow")
  where 
    updatedEnvironment = handleAssignments environment (SingleAssignment expectedType variable expression1)
    actualType1 = getTypeOfVariable variable updatedEnvironment
    actualType2 = typeOfExpression updatedEnvironment expression2
    actualType3 = typeOfExpression updatedEnvironment expression3

typeOfExpression environment (RepeatColumn expectedType variable expression1 expression2 expression3) 
  | (TypeInt, TypeInt, TypeGrid) == (actualType1, actualType2, actualType3) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Int, Int, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ", " ++ displayType actualType3 ++ ") in function repeatCol")
  where 
    updatedEnvironment = handleAssignments environment (SingleAssignment expectedType variable expression1)
    actualType1 = getTypeOfVariable variable updatedEnvironment
    actualType2 = typeOfExpression updatedEnvironment expression2
    actualType3 = typeOfExpression updatedEnvironment expression3

typeOfExpression environment (RepeatWhile expectedType variable expression1 expression2 expression3) 
  | (TypeInt, TypeBool, TypeInt) == (actualType1, actualType2, actualType3) = TypeInt
  | (TypeBool, TypeBool, TypeBool) == (actualType1, actualType2, actualType3) = TypeBool
  | (TypeString, TypeBool, TypeString) == (actualType1, actualType2, actualType3) = TypeString
  | (TypeGrid, TypeBool, TypeGrid) == (actualType1, actualType2, actualType3) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Int, Bool, Int) or (Bool, Bool, Bool) or (String, Bool, String) or (Grid, Bool, Grid) with actual type (" ++ displayType actualType1 ++ "," ++ displayType actualType2 ++ "," ++ displayType actualType3 ++ ") in function repeatWhile")
  where 
    updatedEnvironment = handleAssignments environment (SingleAssignment expectedType variable expression1)
    actualType1 = getTypeOfVariable variable updatedEnvironment
    actualType2 = typeOfExpression updatedEnvironment expression2
    actualType3 = typeOfExpression updatedEnvironment expression3

-- CONTAINS --

typeOfExpression environment (ContainsString expression1 expression2) 
  | (TypeGrid, TypeString) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Grid, String) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function containsStr")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (ContainsCell expression1 expression2) 
  | (TypeGrid, TypeGrid) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Grid, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function containsCell")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (ContainsColumns expression1 expression2) 
  | (TypeGrid, TypeGrid) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Grid, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function containsCol")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (ContainsRows expression1 expression2) 
  | (TypeGrid, TypeGrid) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Grid, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function containsRow")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (ContainsCharacters expression1 expression2) 
  | (TypeString, TypeString) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (String, String) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function containsString")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

-- GET INFORMATION --

typeOfExpression environment (GetString expression1 expression2 expression3) 
  | (TypeGrid, TypeInt, TypeInt) == (actualType1, actualType2, actualType3) = TypeString
  | otherwise = error ("Couldn't match expected type (Grid, Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ", " ++ displayType actualType3 ++ ") in function getStr")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2
    actualType3 = typeOfExpression environment expression3

typeOfExpression environment (GetCell expression1 expression2 expression3) 
  | (TypeGrid, TypeInt, TypeInt) == (actualType1, actualType2, actualType3) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ", " ++ displayType actualType3 ++ ") in function getCell")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2
    actualType3 = typeOfExpression environment expression3

typeOfExpression environment (GetRow expression1 expression2) 
  | (TypeGrid, TypeInt) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function getRow")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (GetColumn expression1 expression2) 
  | (TypeGrid, TypeInt) == (actualType1, actualType2) = TypeGrid
  | otherwise = error ("Couldn't match expected type (Grid, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function getCol")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (GetCharacter expression1 expression2) 
  | (TypeString, TypeInt) == (actualType1, actualType2) = TypeString
  | otherwise = error ("Couldn't match expected type (String, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function getChar")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

-- STRING OPERATIONS

typeOfExpression environment (Concatenate expression1 expression2) 
  | (TypeString, TypeString) == (actualType1, actualType2) = TypeString
  | otherwise = error ("Couldn't match expected type (String, String) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function ++")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (IntegerToString expression1) 
  | (TypeInt) == (actualType1) = TypeString
  | otherwise = error ("Couldn't match expected type (Int) with actual type (" ++ displayType actualType1 ++ ") in function intToStr")
  where 
    actualType1 = typeOfExpression environment expression1

-- BOOLEAN --

typeOfExpression environment (And expression1 expression2) 
  | (TypeBool, TypeBool) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Bool, Bool) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function &&")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (Or expression1 expression2) 
  | (TypeBool, TypeBool) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Bool, Bool) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function ||")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (Equals expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeBool
  | (TypeBool, TypeBool) == (actualType1, actualType2) = TypeBool
  | (TypeString, TypeString) == (actualType1, actualType2) = TypeBool
  | (TypeGrid, TypeGrid) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Int, Int) or (Bool, Bool) or (String, String) (Grid, Grid) with actual typ (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function ==")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (NotEquals expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeBool
  | (TypeBool, TypeBool) == (actualType1, actualType2) = TypeBool
  | (TypeString, TypeString) == (actualType1, actualType2) = TypeBool
  | (TypeGrid, TypeGrid) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Int, Int) or (Bool, Bool) or (String, String) (Grid, Grid) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function !=")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (LessThan expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function <")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (GreaterThan expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function >")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (LessThanOrEquals expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function <=")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (GreaterThanOrEquals expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeBool
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function >=")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

-- ARITH --

typeOfExpression environment (Plus expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeInt
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function +")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (Minus expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeInt
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function -")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (Multiply expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeInt
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function *")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (Divide expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeInt
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function /")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (Mod expression1 expression2) 
  | (TypeInt, TypeInt) == (actualType1, actualType2) = TypeInt
  | otherwise = error ("Couldn't match expected type (Int, Int) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ") in function %")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2

typeOfExpression environment (Negate expression1) 
  | (TypeInt) == (actualType1) = TypeInt
  | otherwise = error ("Couldn't match expected type (Int) with actual type (" ++ displayType actualType1 ++ ") in function -")
  where 
    actualType1 = typeOfExpression environment expression1

-- NUM

typeOfExpression environment (NumRows expression1) 
  | (TypeGrid) == (actualType1) = TypeInt
  | otherwise = error ("Couldn't match expected type (Grid) with actual type (" ++ displayType actualType1 ++ ") in function numRows")
  where 
    actualType1 = typeOfExpression environment expression1

typeOfExpression environment (NumColumns expression1) 
  | (TypeGrid) == (actualType1) = TypeInt
  | otherwise = error ("Couldn't match expected type (Grid) with actual type (" ++ displayType actualType1 ++ ") in function numCols")
  where 
    actualType1 = typeOfExpression environment expression1

typeOfExpression environment (NumCharacters expression1) 
  | (TypeString) == (actualType1) = TypeInt
  | otherwise = error ("Couldn't match expected type (String) with actual type (" ++ displayType actualType1 ++ ") in function numChars")
  where 
    actualType1 = typeOfExpression environment expression1

-- IF THEN ELSE --

typeOfExpression environment (IfThenElse expression1 expression2 expression3) 
  | (TypeBool) == (actualType1) && (TypeGrid, TypeGrid) == (actualType2, actualType3) = TypeGrid
  | (TypeBool) == (actualType1) && (TypeInt, TypeInt) == (actualType2, actualType3) = TypeInt
  | (TypeBool) == (actualType1) && (TypeString, TypeString) == (actualType2, actualType3) = TypeString
  | (TypeBool) == (actualType1) && (TypeBool, TypeBool) == (actualType2, actualType3) = TypeBool
  | otherwise = error ("Couldn't match expected type (Bool, Grid, Grid) or (Bool, Int, Int) or (Bool, String, String) or (Bool, Bool, Bool) with actual type (" ++ displayType actualType1 ++ ", " ++ displayType actualType2 ++ ", " ++ displayType actualType3 ++ ") in function if-then-else")
  where 
    actualType1 = typeOfExpression environment expression1
    actualType2 = typeOfExpression environment expression2
    actualType3 = typeOfExpression environment expression3

-- GRID --

typeOfGrid :: Environment -> Arr -> Type
typeOfGrid environment (SingleArray row) = checkRow environment row
typeOfGrid environment (MultipleArray row rest) = if checkRow environment row == typeOfGrid environment rest then TypeGrid else TypeGrid

checkRow :: Environment -> Row -> Type
checkRow environment (SingleRow expression) 
  | (actualType) == (TypeString) = TypeGrid
  | (actualType) == (TypeInt) = TypeGrid
  | otherwise = error ("Could not match expectedtype (String) or (Int) with actual type (" ++ displayType actualType ++ ") in function []")
  where
    actualType = typeOfExpression environment expression
checkRow environment (MultipleRow expression rest)
  | (actualType) == (TypeString) && checkRest == TypeGrid = TypeGrid
  | (actualType) == (TypeInt) && checkRest == TypeGrid = TypeGrid
  | otherwise = error ("Could not match expectedtype (String) or (Int) with actual type (" ++ displayType actualType ++ ") in function []")
  where
    actualType = typeOfExpression environment expression
    checkRest = checkRow environment rest

displayType :: Type -> String
displayType TypeInt = "Int"
displayType TypeBool = "Bool"
displayType TypeString = "String"
displayType TypeGrid = "Grid"
