{
module CSVQueryGrammar where
import CSVQueryTokens
}

%name parseCSVQuery
%tokentype { CSVQueryToken }
%error { parseError }
%token
    
    Int         { TokenTypeInt _ }
    Bool        { TokenTypeBool _ }
    String      { TokenTypeString _ }
    Grid        { TokenTypeGrid _ }

    '('        { TokenLParen _ }
    ')'        { TokenRParen _ }
    '['        { TokenLSquare _ }
    ']'        { TokenRSquare _ }

    '='        { TokenAssign _ }
    ','        { TokenComma _ }
    ';'        { TokenSemicolon _ }

    '<'        { TokenLessThan _ }
    '>'        { TokenGreaterThan _ }
    '<='       { TokenLessThanOrEqual _ }
    '>='       { TokenGreaterThanOrEqual _ }
    '=='       { TokenEquals _ }
    '!='       { TokenNotEquals _ }
    '+'        { TokenPlus _ }
    '-'        { TokenMinus _ }
    '*'        { TokenMultiply _ }
    '/'        { TokenDivide _ }
    '%'        { TokenMod _ }

    if         { TokenIf _ }  
    then       { TokenThen _ }
    else       { TokenElse _ }
    '&&'       { TokenAnd _ }
    '||'       { TokenOr _ }
    true       { TokenTrue _ }
    false      { TokenFalse _ }

    read       { TokenRead _ }

    addRows    { TokenAddRows _ }
    addCols    { TokenAddCols _ }
    insertRows { TokenInsertRows _ }
    insertCols { TokenInsertCols _ }
    dropRow    { TokenDropRow _ }
    dropCol    { TokenDropCol _ }
    reverseRow { TokenReverseRow _ }
    reverseCol { TokenReverseCol _ }

    getStr     { TokenGetStr _ }
    getCell    { TokenGetCell _ }
    getRow     { TokenGetRow _ }
    getCol     { TokenGetCol _ }

    containsStr   { TokenContainsStr _ }
    containsCell  { TokenContainsCell _ }
    containsRows  { TokenContainsRows _ }
    containsCols  { TokenContainsCols _ }
    containsChars { TokenContainsChars _ }

    getChar    { TokenGetChar _ }
    '++'       { TokenConcat _ }
    intToStr   { TokenIntToStr _ }

    numRows    { TokenNumRows _ }
    numCols    { TokenNumCols _ }
    numChars   { TokenNumChars _ }

    repeatRow  { TokenRepeatRow _ }
    repeatCol  { TokenRepeatCol _ }
    repeatWhile { TokenRepeatWhile _ }

    output     { TokenOutput _ }

    int        { TokenInt _ $$ }
    string     { TokenString _ $$ }
    var        { TokenVar _ $$ }



%nonassoc '<' '>' '<=' '>=' '==' '!='
%nonassoc '=' ',' ';'
%nonassoc if then else
%nonassoc '(' ')' '[' ']'


%left '/' '%' '*' '+' '-' '++'
%left '&&' '||'
%left addRows addCols insertRows insertCols dropRow dropCol reverseRow reverseCol
%left getStr getCell getRow getCol getChar containsStr containsCell containsRows containsCols containsChars intToStr numRows numCols numChars
%left repeatRow repeatCol repeatWhile
%left read
%left NEG


%%
Grammar : output '=' Expression ';'                                     { SingleExpression $3 }
        | Assignment output '=' Expression ';'                          { MultipleExpression $1 $4 }

Assignment : Type var '=' Expression ';'                                { SingleAssignment $1 $2 $4 }
           | Assignment Type var '=' Expression ';'                     { MultipleAssignment $1 $2 $3 $5 }

Type : Int                                                              { TypeInt }
     | Bool                                                             { TypeBool }
     | String                                                           { TypeString }
     | Grid                                                             { TypeGrid }

Arr : '[' Row ']'                                                       { SingleArray $2 }
     | '[' Row ']' ',' Arr                                              { MultipleArray $2 $5 } 
   
Row : Expression                                                        { SingleRow $1 }
    | Expression ',' Row                                                { MultipleRow $1 $3 }

Expression : '(' Expression ')'                                         { $2 }
           
           | var                                                        { Var $1 }
           | int                                                        { Int $1 }
           | true                                                       { BooleanTrue }
           | false                                                      { BooleanFalse }
           | string                                                     { String $1 }
           | '[' Arr ']'                                                { Grid $2 }
           | '[' '[' ']' ']'                                            { Empty }

           | read Expression                                            { Read $2 }
           
           | addRows Expression Expression                              { AddRows $2 $3 }
           | addCols Expression Expression                              { AddColumns $2 $3 }
           | insertRows Expression Expression Expression                { InsertRows $2 $3 $4 }
           | insertCols Expression Expression Expression                { InsertColumns $2 $3 $4 }
           | dropRow Expression Expression                              { DropRow $2 $3 }
           | dropCol Expression Expression                              { DropColumn $2 $3 }
           | reverseRow Expression Expression                           { ReverseRow $2 $3 }
           | reverseCol Expression Expression                           { ReverseColumn $2 $3 }
          
           | getStr Expression Expression Expression                    { GetString $2 $3 $4 }
           | getCell Expression Expression Expression                   { GetCell $2 $3 $4 }
           | getRow Expression Expression                               { GetRow $2 $3 }
           | getCol Expression Expression                               { GetColumn $2 $3 }

           | containsStr Expression Expression                          { ContainsString $2 $3 }
           | containsCell Expression Expression                         { ContainsCell $2 $3 }
           | containsRows Expression Expression                         { ContainsRows $2 $3 }
           | containsCols Expression Expression                         { ContainsColumns $2 $3 }
           | containsChars Expression Expression                        { ContainsCharacters $2 $3 }

           | getChar Expression Expression                              { GetCharacter $2 $3 }
           | Expression '++' Expression                                 { Concatenate $1 $3 }
           | intToStr Expression                                        { IntegerToString $2 }

           | Expression '&&' Expression                                 { And $1 $3 }
           | Expression '||' Expression                                 { Or $1 $3 }
           | Expression '==' Expression                                 { Equals $1 $3 }
           | Expression '!=' Expression                                 { NotEquals $1 $3 }
           | Expression '<' Expression                                  { LessThan $1 $3 }
           | Expression '>' Expression                                  { GreaterThan $1 $3 }
           | Expression '<=' Expression                                 { LessThanOrEquals $1 $3 }
           | Expression '>=' Expression                                 { GreaterThanOrEquals $1 $3 }
           | Expression '+' Expression                                  { Plus $1 $3 }
           | Expression '-' Expression                                  { Minus $1 $3 }
           | Expression '*' Expression                                  { Multiply $1 $3 }
           | Expression '/' Expression                                  { Divide $1 $3 }
           | Expression '%' Expression                                  { Mod $1 $3 }
           | '-' Expression %prec NEG                                   { Negate $2 } 
 
           | numRows Expression                                         { NumRows $2 }
           | numCols Expression                                         { NumColumns $2 }
           | numChars Expression                                        { NumCharacters $2 }

           | repeatRow '('Type var '=' Expression ',' Expression ')' Expression { RepeatRow $3 $4 $6 $8 $10 }
           | repeatCol '(' Type var '=' Expression ',' Expression ')' Expression { RepeatColumn $3 $4 $6 $8 $10 }
           | repeatWhile '(' Type var '=' Expression  ',' Expression ')' Expression { RepeatWhile $3 $4 $6 $8 $10 }
           
           | if '(' Expression ')' then '(' Expression ')' else '(' Expression ')' { IfThenElse $3 $7 $11 }
      


{

parseError :: [CSVQueryToken] -> a
parseError [] = error "Unidentified Parse Error"
parseError xs = error ("Parse error at " ++ show (tokenPosn (xs !! 0)))

data Grammar = SingleExpression Expression | MultipleExpression Assignment Expression deriving Show
data Assignment = SingleAssignment Type String Expression | MultipleAssignment Assignment Type String Expression deriving Show
data Type = TypeInt | TypeBool | TypeString | TypeGrid deriving (Show,Eq)

data Arr = SingleArray Row | MultipleArray Row Arr deriving (Show, Eq)
data Row = SingleRow Expression | MultipleRow Expression Row deriving (Show, Eq)
            
data Expression = Var String                   
    | Int Int              
    | BooleanTrue                
    | BooleanFalse
    | String String
    | Grid Arr   
    | Empty   

    | Read Expression   

    | AddRows Expression Expression
    | AddColumns Expression Expression
    | InsertRows Expression Expression Expression
    | InsertColumns Expression Expression Expression
    | DropRow Expression Expression
    | DropColumn Expression Expression
    | ReverseRow Expression Expression
    | ReverseColumn Expression Expression

    | GetString Expression Expression Expression
    | GetCell Expression Expression Expression
    | GetRow Expression Expression
    | GetColumn Expression Expression

    | ContainsString Expression Expression
    | ContainsCell Expression Expression
    | ContainsRows Expression Expression
    | ContainsColumns Expression Expression
    | ContainsCharacters Expression Expression

    | GetCharacter Expression Expression
    | Concatenate Expression Expression
    | IntegerToString Expression

    | And Expression Expression                
    | Or Expression Expression       
    | Equals Expression Expression              
    | NotEquals Expression Expression          
    | LessThan Expression Expression            
    | GreaterThan Expression Expression
    | LessThanOrEquals Expression Expression
    | GreaterThanOrEquals Expression Expression
    | Plus Expression Expression
    | Minus Expression Expression
    | Multiply Expression Expression
    | Divide Expression Expression
    | Mod Expression Expression
    | Negate Expression

    | NumRows Expression
    | NumColumns Expression
    | NumCharacters Expression

    | RepeatRow Type String Expression Expression Expression
    | RepeatColumn Type String Expression Expression Expression
    | RepeatWhile Type String Expression Expression Expression

    | IfThenElse Expression Expression Expression
    deriving (Show, Eq)

}