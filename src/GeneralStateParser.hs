-- This is a parser library for parsing
module GeneralStateParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Possible parse errors
data ParseError = FatalError String Int | FailError String Int
  deriving (Eq)

instance Show ParseError where
  show :: ParseError -> String
  show (FatalError msg line) =
    "Fatal error at line " ++ show line ++ ": " ++ msg
  show (FailError msg line) =
    "Error at line " ++ show line ++ ": " ++ msg

-- | Parser type definition parameterized over state s
newtype StateParser s a = SP { doParse :: s -> Either ParseError (a, s) }

instance Functor (StateParser s) where
  fmap :: (a -> b) -> StateParser s a -> StateParser s b
  fmap f p = SP $ \s -> case doParse p s of
    Left err -> Left err
    Right (a, s') -> Right (f a, s')

instance Applicative (StateParser s) where
  pure :: a -> StateParser s a
  pure x = SP $ \s -> Right (x, s)
  (<*>) :: StateParser s (a -> b) -> StateParser s a -> StateParser s b
  p1 <*> p2 = SP $ \s -> case doParse p1 s of
    Left err -> Left err
    Right (f, s') -> case doParse p2 s' of
      Left err -> Left err
      Right (a, s'') -> Right (f a, s'')

instance ParserState s => Alternative (StateParser s) where
  empty :: StateParser s a
  empty = SP $ \s -> Left (FailError "No parses" (getLineNum s))
  (<|>) :: StateParser s a -> StateParser s a -> StateParser s a
  p1 <|> p2 = SP $ \s -> case doParse p1 s of
    Left (FatalError msg line) -> Left (FatalError msg line) -- Stop on fatal error
    Left (FailError _ _) -> doParse p2 s                  -- Try p2 on failure
    success -> success                                        -- Return success

instance Monad (StateParser s) where
  return :: a -> StateParser s a
  return = pure
  (>>=) :: StateParser s a -> (a -> StateParser s b) -> StateParser s b
  p >>= f = SP $ \s -> case doParse p s of
    Left err -> Left err
    Right (a, s') -> doParse (f a) s'

-- | Class representing the parser state
class ParserState s where
  getInput :: s -> String
  setInput :: String -> s -> s
  getLineNum :: s -> Int
  setLineNum :: Int -> s -> s

-- | Throw a fatal error with a message
fatalError :: (ParserState s, Show s) => String -> StateParser s a
fatalError msg = SP $ \state ->
  Left (FatalError (msg ++ "\n" ++ show state) (getLineNum state))

-- | Throw a fatal error with expected string information
fatalErrorWithExpectStr :: (ParserState s, Show s) => String -> String -> Int -> StateParser s a
fatalErrorWithExpectStr msg expectStr len = SP $ \state ->
  Left (FatalError (msg ++ " expected: `" ++ expectStr ++ "`, but got `"
    ++ peekStr len state ++ "`\n" ++ show state) (getLineNum state))

-- | Throw a recoverable failure error with a message
failError :: ParserState s => String -> StateParser s a
failError msg = SP $ \state -> Left (FailError msg (getLineNum state))

-- | Throw a recoverable failure error with expected string information
failErrorWithExpectStr :: ParserState s => String -> String -> Int -> StateParser s a
failErrorWithExpectStr msg expectStr len = SP $ \state ->
  Left (FailError (msg ++ ", expected: `" ++ expectStr
    ++ "`, but got `" ++ peekStr len state ++ "`") (getLineNum state))

-- | Upgrade a FailError to a FatalError with a custom message
updateFailToFatal :: ParserState s => String -> StateParser s a -> StateParser s a
updateFailToFatal errorMsg parser = SP $ \state -> case doParse parser state of
  Left (FailError msg _) -> Left (FatalError (errorMsg ++ ": " ++ msg)
    (getLineNum state))  -- Upgrade to FatalError
  result -> result          -- Return other results

-- | Combine two parsers: upgrade to FatalError if the first succeeds
upgradeToFatalIfFirstSucceeds :: ParserState s => String -> StateParser s () -> StateParser s a -> StateParser s a
upgradeToFatalIfFirstSucceeds errorMsg conditionParser mainParser = do
  _ <- conditionParser
  SP $ \state -> case doParse mainParser state of
    Left (FailError msg _) -> Left (FatalError (errorMsg ++ ": " ++ msg)
      (getLineNum state))  -- Upgrade to FatalError
    result -> result          -- Return other results

-- | Strip whitespace from both ends of a parser's result
wsP :: ParserState s => StateParser s a -> StateParser s a
wsP p = many space *> p <* many space

-- | Strip leading and trailing whitespace from a string
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Strip whitespace and ensure the result is not empty
stripP :: ParserState s => StateParser s String -> StateParser s String
stripP p = do
  result <- p
  let stripped = strip result
  if null stripped
    then failError "empty string after stripping"
    else return stripped

-- | Get the next character from the input
get :: ParserState s => StateParser s Char
get = SP $ \s -> case getInput s of
  (c:cs) ->
    let newLineNum = getLineNum s + if c == '\n' then 1 else 0
        newState = setInput cs . setLineNum newLineNum $ s
    in Right (c, newState)
  [] ->
    Left (FailError ("Unexpected EOF at line " ++ show (getLineNum s))
      (getLineNum s))

-- | Succeed only at the end of the input
eof :: ParserState s => StateParser s ()
eof = SP $ \s -> case getInput s of
  [] -> Right ((), s)  -- Success if no input remains
  _  -> Left (FailError ("Expected EOF at line "
    ++ show (getLineNum s) ++ " but got `" ++ peekStr 5 s ++ "`...")
    (getLineNum s))

-- | Filter parsing results based on a predicate
filter :: (Show a, ParserState s) => (a -> Bool) -> StateParser s a -> StateParser s a
filter f p = SP $ \s -> case doParse p s of
  Left err -> Left err  -- Error already includes line number
  Right (c, s') ->
    if f c
      then Right (c, s')
      else Left (FailError ("Value " ++ show c
        ++ " does not satisfy the predicate at line "
        ++ show (getLineNum s')) (getLineNum s'))

-- | Return the next character if it satisfies the predicate
satisfy :: ParserState s => (Char -> Bool) -> StateParser s Char
satisfy p = GeneralStateParser.filter p get

-- | Parsers for specific types of characters
alpha, digit, upper, lower, space :: ParserState s => StateParser s Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpaceOrTab

isSpaceOrTab :: Char -> Bool
isSpaceOrTab = (`elem` " \t\r")

-- | Return the next character if it matches the given character
char :: ParserState s => Char -> StateParser s Char
char c = satisfy (== c)

-- | Delimiters used in parsing
delims :: Set Char
delims = Set.fromList ['(', ')', '[', ']', '{', '}', ' ', ',']

-- | Check if a character is a delimiter
isDelim :: Char -> Bool
isDelim c = Set.member c delims

-- | Peek the next n characters without consuming them
peek :: ParserState s => Int -> StateParser s String
peek n = SP $ \s ->
  if null (getInput s)
    then Right ("", s)
    else Right (take n (getInput s), s)

-- | Peek the next n characters from the current parse state
peekStr :: ParserState s => Int -> s -> String
peekStr n state = take n (getInput state)

-- | Parse an integer with optional negative sign
intP :: (ParserState s, Show s) => StateParser s Int
intP = do
  firstChar <- peek 1
  if null firstChar
    then fatalError "Unexpected EOF in intP"
    else do
      sign <- optional (char '-')
      digits <- some digit <|> fatalErrorWithExpectStr "intP()"
        "a digit" 1
      nextChar <- peek 1
      let numStr = maybe "" (:[]) sign ++ digits
      if null nextChar || isSpace (head nextChar) || isDelim (head nextChar)
        then case readMaybe numStr of
          Just x  -> return x
          Nothing -> fatalError $ "Invalid integer: " ++ numStr
        else fatalError $ "Invalid integer: " ++ numStr ++ nextChar

-- | Parse a string surrounded by specific open and close parsers
between :: ParserState s => StateParser s open -> StateParser s a -> StateParser s close -> StateParser s a
between open p close = open *> p <* close

-- | Parse a list of items separated by a separator
sepBy :: ParserState s => StateParser s a -> StateParser s sep -> StateParser s [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse one or more items separated by a separator
sepBy1 :: ParserState s => StateParser s a -> StateParser s sep -> StateParser s [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Count occurrences of a character in a string
count :: Eq a => a -> [a] -> Int
count x = length . Prelude.filter (== x)

-- | Parse a specific string and update the line number
string :: ParserState s => String -> StateParser s String
string str = SP $ \s ->
  if str `isPrefixOf` getInput s
    then Right (str, setInput (drop (length str) (getInput s))
                           . setLineNum (getLineNum s + count '\n' str) $ s)
    else Left (FailError ("Expected string " ++ show str
      ++ " at line " ++ show (getLineNum s)) (getLineNum s))

-- | Parse a string with leading and trailing whitespace
stringP :: ParserState s => String -> StateParser s ()
stringP s = wsP (string s) *> pure ()

-- | Parse content within parentheses
parens :: ParserState s => StateParser s a -> StateParser s a
parens x = between (stringP "(") x (stringP ")")

-- | Parse content within brackets
brackets :: ParserState s => StateParser s a -> StateParser s a
brackets x = between (stringP "[") x (stringP "]")

-- | Parse a constant string and return a fixed value
constP :: ParserState s => String -> a -> StateParser s a
constP s x = wsP (string s) *> pure x
  <|> (do
        s' <- peek (length s)
        failErrorWithExpectStr "constP()" s (length s))

-- | Expect a string without consuming input if it fails
expect :: ParserState s => String -> StateParser s () -> StateParser s ()
expect str p = SP $ \state -> case doParse p state of
  Right (_, _) -> Right ((), state)  -- Only check presence
  Left (FailError _ _) -> Left (FailError ("Expected " ++ str)
    (getLineNum state))
  Left fatalError -> Left fatalError

-- | Expect a specific string
expectString :: ParserState s => String -> StateParser s ()
expectString str = expect str (void $ string str)

-- | Expect a specific string, ignoring case
expectStringIgnoreCase :: ParserState s => String -> StateParser s ()
expectStringIgnoreCase str = expect str (void $ stringIgnoreCase str)

-- | Parse a character, ignoring case
charIgnoreCase :: ParserState s => Char -> StateParser s Char
charIgnoreCase c = satisfy (\x -> toLower x == toLower c)

-- | Parse a string, ignoring case
stringIgnoreCase :: ParserState s => String -> StateParser s String
stringIgnoreCase = traverse charIgnoreCase

-- | Parse a constant string, ignoring case, and return a fixed value
constIgnoreCaseP :: ParserState s => String -> a -> StateParser s a
constIgnoreCaseP s x = wsP (stringIgnoreCase s) *> pure x

-- | Advance the line number based on the number of newlines in a string
advanceLine :: String -> Int -> Int
advanceLine s currentLine = currentLine + length (Prelude.filter (== '\n') s)

-- | Parse a newline character (handles different newline formats)
newline :: ParserState s => StateParser s ()
newline = void (char '\n')

-- | Parse an empty line with optional whitespace
emptyLine :: ParserState s => StateParser s ()
emptyLine = wsP newline

-- | Parse a comment line starting with "//"
commentLine :: ParserState s => StateParser s ()
commentLine = wsP (string "//") *> many (satisfy (/= '\n')) *> newline

-- | Skip over empty lines and comment lines
skipCommentOrEmptyLines :: ParserState s => StateParser s ()
skipCommentOrEmptyLines = void $ many (emptyLine <|> commentLine)

-- | Parse any character
anyChar :: ParserState s => StateParser s Char
anyChar = get

-- | Parse many characters until a terminating parser succeeds
manyTill :: ParserState s => StateParser s a -> StateParser s end -> StateParser s [a]
manyTill p end = go
  where
    go = (end *> pure []) <|> (:) <$> p <*> go