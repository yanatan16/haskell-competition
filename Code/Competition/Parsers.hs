module Code.Competition.Parsers (
  probfile,
  grid, nmgrid,
  ints, intn, int,
  floats, floatn, float,
  andSpace,
  eol
) where


-- Parsing
import Text.ParserCombinators.Parsec (GenParser, count, char, space, digit, many, eof, newline, sepEndBy1, spaces, optional)

------------------------
-- A grid of n rows and m columns (no spaces separated)
------------------------

grid :: Int -> Int -> GenParser Char st a -> GenParser Char st [[a]]
grid n m p = count n pline
  where
    pline = do
      ret <- count m p
      eol
      return ret

-- A grid with n and m at the top
nmgrid :: GenParser Char st a -> GenParser Char st [[a]]
nmgrid p = do
  [n,m] <- intn 2
  grid n m p

------------------------
-- A whole problem file
------------------------

probfile :: GenParser Char st prob -> GenParser Char st [prob]
probfile probparse = do
  [t] <- intn 1
  probs <- probparse `sepEndBy1` newline
  eof
  return probs

------------------------
-- Ints
------------------------

ints :: GenParser Char st [Int]
ints = many (andSpace int)

intn :: Int -> GenParser Char st [Int]
intn n = count n (andSpace int)

int :: GenParser Char st Int
int = do
  tokens <- many digit
  return $ read tokens

------------------------
-- Floats
------------------------

floats :: GenParser Char st [Int]
floats = many (andSpace int)

floatn :: Int -> GenParser Char st [Float]
floatn n = count n (andSpace float)

float :: GenParser Char st Float
float = do
  predot <- many digit
  char '.'
  postdot <- many digit
  return $ read (predot ++ "." ++ postdot)

-- Helpers

eol :: GenParser Char st Char
eol = char '\n'

andSpace :: GenParser Char st a -> GenParser Char st a
andSpace ap = do
  a <- ap
  space
  return a