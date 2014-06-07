module Code.Competition (
  Solveable(..), Parseable(..), runCompetition, runSolution, runWriteSolution
) where

-- getArgs
import System.Environment (getArgs)
import System.FilePath (replaceExtension)

-- Parsing
import Text.ParserCombinators.Parsec (parse, ParseError, GenParser)
import Code.Competition.Parsers (probfile)

------------------------
-- Type Classes
------------------------

-- Solveable denotes any type that represents a solveable problem
class (Show a) => Solveable p a where
  solve :: p -> a

class Parseable p where
  problem :: GenParser Char st p

------------------------
-- parsers
------------------------

parseProblemFile :: Parseable p => String -> Either ParseError [p]
parseProblemFile input = parse (probfile problem) "(unknown)" input

------------------------
-- Main and IO
------------------------

runCompetition :: (Solveable p a, Parseable p, Show p) => IO [(p, a)]
runCompetition = do
  args <- getArgs
  inputFn <- return $ head args
  let outputFn = if length args >= 2 then (args !! 1) else (replaceExtension inputFn ".out")
  (output, probans) <- runWriteSolution inputFn outputFn
  putStr output
  return probans

runSolution :: (Solveable p a, Parseable p, Show p) => String -> IO (String, [(p, a)])
runSolution inputFn = do
  input <- readFile inputFn
  problems <- case parseProblemFile input of
    Left pe -> handleParseError pe
    Right xs -> return xs
  answers <- return $ map solve problems
  output <- return $ (unlines . caseify) answers
  return (output, zip problems answers)

runWriteSolution :: (Solveable p a, Parseable p, Show p) => String -> String -> IO (String, [(p, a)])
runWriteSolution inputFn outputFn = do
  (output, probans) <- runSolution inputFn
  writeFile outputFn output
  return (output, probans)

handleParseError :: Parseable p => ParseError -> IO [p]
handleParseError err = do
  putStrLn $ show err
  return []

------------------------
-- Helpers
------------------------

caseify :: Show a => [a] -> [String]
caseify as = map (\(i,a) -> "Case #" ++ (show i) ++ ": " ++ (show a)) (zip [1..] as)