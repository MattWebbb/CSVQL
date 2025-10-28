{
module CSVQueryTokens where
}

%wrapper "posn"
$digit = 0-9        --digits
$alpha = [a-zA-Z]   --alphabet

tokens :-
$white+        ;
"--".*         ;

--Type Tokens
Int            { \p s -> TokenTypeInt p }
Bool           { \p s -> TokenTypeBool p }
String         { \p s -> TokenTypeString p }
Grid           { \p s -> TokenTypeGrid p }

-- Bracket Tokens
\(             { \p s -> TokenLParen p }
\)             { \p s -> TokenRParen p }
\[             { \p s -> TokenLSquare p }
\]             { \p s -> TokenRSquare p }

-- General Tokens
\=             { \p s -> TokenAssign p }
\,             { \p s -> TokenComma p }
\;             { \p s -> TokenSemicolon p }

-- Arithmetic Tokens
\<             { \p s -> TokenLessThan p }
\>             { \p s -> TokenGreaterThan p }
\<=            { \p s -> TokenLessThanOrEqual p }
\>=            { \p s -> TokenGreaterThanOrEqual p }
\==            { \p s -> TokenEquals p }
\!=            { \p s -> TokenNotEquals p }
\+             { \p s -> TokenPlus p }
\-             { \p s -> TokenMinus p }
\*             { \p s -> TokenMultiply p }
\/             { \p s -> TokenDivide p }
\%             { \p s -> TokenMod p }

-- Boolean Tokens
if             { \p s -> TokenIf p }
then           { \p s -> TokenThen p }
else           { \p s -> TokenElse p }
true           { \p s -> TokenTrue p }
false          { \p s -> TokenFalse p }
"&&"           { \p s -> TokenAnd p }
"||"           { \p s -> TokenOr p }

-- Language Specific Tokens
read           { \p s -> TokenRead p }

addRows        { \p s -> TokenAddRows p }
addCols        { \p s -> TokenAddCols p }
insertRows     { \p s -> TokenInsertRows p }
insertCols     { \p s -> TokenInsertCols p }
dropRow        { \p s -> TokenDropRow p }
dropCol        { \p s -> TokenDropCol p }
reverseRow     { \p s -> TokenReverseRow p }
reverseCol     { \p s -> TokenReverseCol p }

getStr         { \p s -> TokenGetStr p }
getCell        { \p s -> TokenGetCell p }
getRow         { \p s -> TokenGetRow p }
getCol         { \p s -> TokenGetCol p }

containsStr    { \p s -> TokenContainsStr p }
containsCell   { \p s -> TokenContainsCell p }
containsRows   { \p s -> TokenContainsRows p }
containsCols   { \p s -> TokenContainsCols p }
containsChars  { \p s -> TokenContainsChars p }

"++"           { \p s -> TokenConcat p }
getChar        { \p s -> TokenGetChar p }
intToStr       { \p s -> TokenIntToStr p }

numRows        { \p s -> TokenNumRows p }
numCols        { \p s -> TokenNumCols p }
numChars       { \p s -> TokenNumChars p }

output         { \p s -> TokenOutput p }

repeatRows     { \p s -> TokenRepeatRow p }
repeatCols     { \p s -> TokenRepeatCol p }
repeatWhile    { \p s -> TokenRepeatWhile p }

$digit+                                  { \p s -> TokenInt p (read s) }
\" [$alpha $digit \_ \’ \- \. ]* \"      { \p s -> TokenString p s }
$alpha [$alpha $digit \_ \’ \- ]*        { \p s -> TokenVar p s }


{

data CSVQueryToken =
    TokenInt AlexPosn Int            |
    TokenVar AlexPosn String         |
    TokenString AlexPosn String      |

    TokenTypeInt AlexPosn            |
    TokenTypeBool AlexPosn           |
    TokenTypeString AlexPosn         |
    TokenTypeGrid AlexPosn           |

    TokenLParen AlexPosn             |
    TokenRParen AlexPosn             |
    TokenLSquare AlexPosn            |
    TokenRSquare AlexPosn            |

    TokenAssign AlexPosn             |
    TokenComma AlexPosn              |
    TokenSemicolon AlexPosn          |

    TokenLessThan AlexPosn           |
    TokenGreaterThan AlexPosn        |
    TokenLessThanOrEqual AlexPosn    |
    TokenGreaterThanOrEqual AlexPosn |
    TokenEquals AlexPosn             |
    TokenNotEquals AlexPosn          |
    TokenPlus AlexPosn               |
    TokenMinus AlexPosn              |
    TokenMultiply AlexPosn           |
    TokenDivide AlexPosn             |
    TokenMod AlexPosn                |

    TokenIf AlexPosn                 |
    TokenThen AlexPosn               |
    TokenElse AlexPosn               |
    TokenTrue AlexPosn               |
    TokenFalse AlexPosn              |
    TokenAnd AlexPosn                |
    TokenOr AlexPosn                 |

    TokenRead AlexPosn               |

    TokenAddRows AlexPosn            |
    TokenAddCols AlexPosn            |   
    TokenInsertRows AlexPosn         |
    TokenInsertCols AlexPosn         |
    TokenDropRow AlexPosn            |
    TokenDropCol AlexPosn            |
    TokenReverseRow AlexPosn         |
    TokenReverseCol AlexPosn         |

    TokenGetStr AlexPosn             |
    TokenGetCell AlexPosn            |
    TokenGetCol AlexPosn             |
    TokenGetRow AlexPosn             |

    TokenContainsStr AlexPosn        |
    TokenContainsCell AlexPosn       |
    TokenContainsCols AlexPosn       |
    TokenContainsRows AlexPosn       |
    TokenContainsChars AlexPosn      |

    TokenConcat  AlexPosn            |
    TokenGetChar AlexPosn            |
    TokenIntToStr AlexPosn           |

    TokenNumRows AlexPosn            |
    TokenNumCols AlexPosn            |
    TokenNumChars AlexPosn           |

    TokenRepeatRow AlexPosn          |
    TokenRepeatCol AlexPosn          |
    TokenRepeatWhile AlexPosn        |

    TokenOutput AlexPosn             
    deriving (Eq,Show)

tokenPosn :: CSVQueryToken -> String
tokenPosn (TokenInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenTypeInt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeString (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeGrid (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSquare (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSquare (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenAssign (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemicolon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThanOrEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThanOrEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMod (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenRead (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenAddRows (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddCols (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInsertRows (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInsertCols (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDropRow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDropCol (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReverseRow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReverseCol (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenGetStr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGetCell (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGetCol (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGetRow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenContainsStr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenContainsCell (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenContainsCols (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenContainsRows (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenContainsChars (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenConcat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGetChar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntToStr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenNumRows (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNumCols (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNumChars (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenRepeatRow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRepeatCol (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRepeatWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenOutput (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}