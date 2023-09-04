module Parser where

import Data.Char (isAlpha, isDigit)
import Prelude hiding (Integer, const)

newtype Program = Program Expression deriving (Show)

data Expression
  = ExpressionApply Apply
  | ExpressionCall Name Arrow Expression
  deriving (Show)

data Apply
  = ApplyBasic Basic
  | ApplyApply Apply Basic
  deriving (Show)

data Basic
  = BasicInteger Integer
  | BasicName Name
  | BasicExpression OpeningBrace Expression ClosingBrace
  | BasicPairs OpeningCurlyBrace Pairs ClosingCurlyBrace
  deriving (Show)

data Pairs
  = PairsAssignment Name Equals Expression
  | PairsSeq Pairs Colon Name Equals Expression
  deriving (Show)

newtype Name = Name CharacterSequence deriving (Show)

newtype Integer = Integer DigitSequence deriving (Show)

newtype Arrow = Arrow (Char, Char) deriving (Show)

newtype OpeningBrace = OpeningBrace Char deriving (Show)

newtype ClosingBrace = ClosingBrace Char deriving (Show)

newtype OpeningCurlyBrace = OpeningCurlyBrace Char deriving (Show)

newtype ClosingCurlyBrace = ClosingCurlyBrace Char deriving (Show)

newtype Equals = Equals Char deriving (Show)

newtype Colon = Colon Char deriving (Show)

newtype Space = Space Char deriving (Show)

newtype NewLine = NewLine Char deriving (Show)

newtype Character = Character Char deriving (Show)

newtype CharacterSequence = CharacterSequence String deriving (Show, Eq)

newtype Digit = Digit Char deriving (Show, Eq)

newtype DigitSequence = DigitSequence String deriving (Show, Eq)

type Parser a b = [a] -> [(b, [a])]

-- none and succeed are input-independent primite parsers
none :: Parser a b
none _ = []

succeed :: b -> Parser a b
succeed val input = [(val, input)]

-- parser recognizes single objects (e.g. character "+")
token :: Eq a => a -> Parser a a
token t = spot (== t)

-- parser recognizes a single object with some property defined by predicate function p
spot :: (a -> Bool) -> Parser a a
spot p (x : xs)
  | p x = [(x, xs)]
  | otherwise = []
spot _ [] = []

-- parser combinators
-- combine two parsers, success of either results in success of whole expression
alt :: Parser a b -> Parser a b -> Parser a b
alt p1 p2 input = p1 input ++ p2 input

-- combine two parsers sequentually
(>*>) :: Parser a b -> Parser a c -> Parser a (b, c)
(>*>) p1 p2 input = [((y, z), rem2) | (y, rem1) <- p1 input, (z, rem2) <- p2 rem1]

-- transform obtained result
build :: Parser a b -> (b -> c) -> Parser a c
build p f input = [(f x, r) | (x, r) <- p input]

parse :: String -> Maybe Program
parse input = parsedOrError ([found | (found, []) <- programParser (filter (/= '\n') input)])

parsedOrError :: [Program] -> Maybe Program
parsedOrError [] = Nothing
parsedOrError p = Just (head p)

-- specific parsers
programParser :: Parser Char Program
programParser = expressionParser `build` makeProgram

makeProgram :: Expression -> Program
makeProgram = Program

expressionParser :: Parser Char Expression
expressionParser = (applyParser `build` makeExpressionApply) `alt` ((nameParser >*> spaceParser >*> arrowParser >*> spaceParser >*> expressionParser) `build` makeExpressionCall)

makeExpressionApply :: Apply -> Expression
makeExpressionApply = ExpressionApply

makeExpressionCall :: ((((Name, Space), Arrow), Space), Expression) -> Expression
makeExpressionCall ((((n, _), a), _), e) = ExpressionCall n a e

applyParser :: Parser Char Apply
applyParser = (basicParser `build` makeApplyBasic) `alt` ((basicParser >*> spaceParser >*> applyParser) `build` makeApplyApply)

makeApplyBasic :: Basic -> Apply
makeApplyBasic = ApplyBasic

makeApplyApply :: ((Basic, Space), Apply) -> Apply
makeApplyApply ((b, _), a) = ApplyApply a b

basicParser :: Parser Char Basic
basicParser =
  (integerParser `build` makeBasicInteger)
    `alt` (nameParser `build` makeBasicName)
    `alt` ((openingBraceParser >*> spaceParser >*> expressionParser >*> spaceParser >*> closingBraceParser) `build` makeBasicExpression)
    `alt` ((openingCurlyBraceParser >*> spaceParser >*> pairsParser >*> spaceParser >*> closingCurlyBraceParser) `build` makeBasicPairs)

makeBasicInteger :: Integer -> Basic
makeBasicInteger = BasicInteger

makeBasicName :: Name -> Basic
makeBasicName = BasicName

makeBasicExpression :: ((((OpeningBrace, Space), Expression), Space), ClosingBrace) -> Basic
makeBasicExpression ((((ob, _), e), _), cb) = BasicExpression ob e cb

makeBasicPairs :: ((((OpeningCurlyBrace, Space), Pairs), Space), ClosingCurlyBrace) -> Basic
makeBasicPairs ((((ocb, _), p), _), ccb) = BasicPairs ocb p ccb

pairsParser :: Parser Char Pairs
pairsParser =
  ((nameParser >*> spaceParser >*> equalsParser >*> spaceParser >*> expressionParser) `build` makePairsAssignment)
    `alt` ((nameParser >*> spaceParser >*> equalsParser >*> spaceParser >*> expressionParser >*> colonParser >*> spaceParser >*> pairsParser) `build` makePairsSeq)

makePairsAssignment :: ((((Name, Space), Equals), Space), Expression) -> Pairs
makePairsAssignment ((((n, _), eq), _), e) = PairsAssignment n eq e

makePairsSeq :: (((((((Name, Space), Equals), Space), Expression), Colon), Space), Pairs) -> Pairs
makePairsSeq (((((((n, _), eq), _), e), c), _), p) = PairsSeq p c n eq e

nameParser :: Parser Char Name
nameParser = characterSequenceParser `build` makeName

makeName :: CharacterSequence -> Name
makeName = Name

integerParser :: Parser Char Integer
integerParser = digitSequenceParser `build` makeInteger

makeInteger :: DigitSequence -> Integer
makeInteger = Integer

equalsParser :: Parser Char Equals
equalsParser = token '=' `build` makeEquals

makeEquals :: Char -> Equals
makeEquals = Equals

arrowParser :: Parser Char Arrow
arrowParser = (token '-' >*> token '>') `build` makeArrow

makeArrow :: (Char, Char) -> Arrow
makeArrow = Arrow

--- Basic parsers
characterSequenceParser :: Parser Char CharacterSequence
characterSequenceParser = ((characterParser >*> characterSequenceParser) `build` makeCharacaterSequence) `alt` succeed (CharacterSequence [])

characterParser :: Parser Char Character
characterParser = spot isAlpha `build` Character

makeCharacaterSequence :: (Character, CharacterSequence) -> CharacterSequence
makeCharacaterSequence (Character c, CharacterSequence cs) = CharacterSequence (c : cs)

digitSequenceParser :: Parser Char DigitSequence
digitSequenceParser = ((digitParser >*> digitSequenceParser) `build` makeDigitSequence) `alt` succeed (DigitSequence [])

digitParser :: Parser Char Digit
digitParser = spot isDigit `build` Digit

makeDigitSequence :: (Digit, DigitSequence) -> DigitSequence
makeDigitSequence (Digit c, DigitSequence cs) = DigitSequence (c : cs)

newLineParser :: Parser Char NewLine
newLineParser = token '\n' `build` makeNewLine

makeNewLine :: Char -> NewLine
makeNewLine = NewLine

openingBraceParser :: Parser Char OpeningBrace
openingBraceParser = token '(' `build` makeOpeningBrace

makeOpeningBrace :: Char -> OpeningBrace
makeOpeningBrace = OpeningBrace

closingBraceParser :: Parser Char ClosingBrace
closingBraceParser = token ')' `build` makeClosingBrace

makeClosingBrace :: Char -> ClosingBrace
makeClosingBrace = ClosingBrace

openingCurlyBraceParser :: Parser Char OpeningCurlyBrace
openingCurlyBraceParser = token '{' `build` makeOpeningCurlyBrace

makeOpeningCurlyBrace :: Char -> OpeningCurlyBrace
makeOpeningCurlyBrace = OpeningCurlyBrace

closingCurlyBraceParser :: Parser Char ClosingCurlyBrace
closingCurlyBraceParser = token '}' `build` makeClosingCurlyBrace

makeClosingCurlyBrace :: Char -> ClosingCurlyBrace
makeClosingCurlyBrace = ClosingCurlyBrace

colonParser :: Parser Char Colon
colonParser = token ',' `build` makeColon

makeColon :: Char -> Colon
makeColon = Colon

spaceParser :: Parser Char Space
spaceParser = token ' ' `build` makeSpace

makeSpace :: Char -> Space
makeSpace = Space