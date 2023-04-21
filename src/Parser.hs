module Parser where

import Types
import Control.Applicative ((<$>), (<*))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (GenLanguageDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as P


-- -- | Language definition
identifier  = P.identifier whileLexer
integer     = P.integer whileLexer
parens      = P.parens whileLexer
reservedOp  = P.reservedOp whileLexer
reserved    = P.reserved whileLexer
semi        = P.semi whileLexer
symbol      = P.symbol whileLexer
semiSep     = P.semiSep whileLexer
whiteSpace  = P.whiteSpace whileLexer

whileLexer = P.makeTokenParser whileStyle

whileStyle :: Monad m => GenLanguageDef String u m
whileStyle = P.LanguageDef
    { P.commentStart   = ""
    , P.commentEnd     = ""
    , P.commentLine    = "#"
    , P.nestedComments = True
    , P.identStart     = letter
    , P.identLetter    = alphaNum
    , P.opStart        = P.opLetter whileStyle
    , P.opLetter       = oneOf ":!#$%&*+./<=>?@^|-~"
    , P.reservedOpNames= ["&", ":=", "-", "+", "*", "/", "!", "skip"]
    , P.reservedNames  = ["try", "catch", "while", "do", "if", "then", "else"]
    , P.caseSensitive  = True
    }



-- | Parse Statement
statement :: Parser Stm
statement
    =   parens statement
    <|> try stmIf
    <|> try stmSkip
    <|> try stmWhile
    <|> try stmAssignment
    <|> try stmTryCatch


-- | Parse Assignment
stmAssignment :: Parser Stm
stmAssignment = do
    var <- identifier
    reservedOp ":="
    Ass var <$> arithmeticExpr


-- | Parse Skip
stmSkip :: Parser Stm
stmSkip = reservedOp "skip" >> return Skip


-- | Parse if then else
stmIf :: Parser Stm
stmIf = do
    reserved "if"
    spaces
    cond <- booleanExpr
    spaces
    reserved "then"
    spaces
    s1 <- foldr1 Comp <$> sepBy1 statement semi
    spaces
    reserved "else"
    spaces
    s2 <- foldr1 Comp <$> sepBy1 statement semi
    return $ If cond s1 s2


-- | Parse while
stmWhile :: Parser Stm
stmWhile = do
    reserved "while"
    spaces
    check <- booleanExpr
    spaces
    reserved "do"
    spaces
    prog <- foldr1 Comp <$> sepBy1 statement semi
    return $ While check prog


-- | Parse try catch
stmTryCatch :: Parser Stm
stmTryCatch = do
    reserved "try"
    spaces
    s1 <- foldr1 Comp <$> sepBy1 statement semi
    spaces
    reserved "catch"
    spaces
    s2 <- foldr1 Comp <$> sepBy1 statement semi
    return $ Try s1 s2


-- | Arithmetic expressions
arithmeticAtom
    =   Aconst . An <$> integer
    <|> Avar <$> identifier
    <|> parens arithmeticExpr

arithmeticOperation =
    [ [binaryOp "*" Amul AssocLeft
    ,  binaryOp "/" Adiv AssocLeft]
    , [binaryOp "+" Aadd AssocLeft
    ,  binaryOp "-" Asub AssocLeft]
    ]

arithmeticExpr = buildExpressionParser arithmeticOperation arithmeticAtom


-- | Boolean expressions
binaryOp name fun = Infix body
    where
        body = reservedOp name >> return fun

prefixOp name fun = Prefix $ reservedOp name >> return fun


data WrapAtom = BexpW Bexp | AexpW Aexp

booleanAtom
    =   (try (reserved "true") >> truthVal (Bconst (Bb True)))
    <|> (try (reserved "false") >> truthVal (Bconst (Bb False)))
    <|> try (AexpW <$> arithmeticExpr)
    <|> parens booleanExpr'
    where truthVal = return . BexpW

booleanOperation =
    [ [prefixOp "!" bneg]
    , [binaryOp "=" beq AssocLeft   
    , binaryOp "<=" bleq AssocLeft   
    , binaryOp "&" band AssocLeft ]
    ]
    where
        bneg (BexpW b) = BexpW $ Bneg b
        bneg _ = error "Invalid operand for negation"
        beq (AexpW a1) (AexpW a2) = BexpW $ Beq a1 a2
        beq _ _ = error "Invalid operands for equality check"
        bleq (AexpW a1) (AexpW a2) = BexpW $ Bleq a1 a2
        bleq _ _ = error "Invalid operands for less than or equal check"
        band (BexpW b1) (BexpW b2) = BexpW $ Band b1 b2
        band _ _ = error "Parse error: failed to extract boolean"

booleanExpr = do
    result <- booleanExpr'
    case result of
      (BexpW val) -> return val
      _ -> error "Parse error: failed to extract boolean"

booleanExpr' = buildExpressionParser booleanOperation booleanAtom


-- | Parse program
parseString :: String -> Stm
parseString input =
    case parse program "" input of
        Left _    -> error "Could not parse input"
        Right res -> res

program :: Parser Stm
program = do
    whiteSpace
    res <- statement `sepBy` symbol ";"
    eof
    if length res == 1
        then return $ head res
        else return $ foldr1 Comp res
