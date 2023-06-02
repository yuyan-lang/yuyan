
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randoms, mkStdGen)
import Data.List (sort)

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  array <- generateArray n
  start <- getCPUTime
  let sortedArray = quicksort array
  _ <- mapM (\x -> return x) sortedArray
  end <- getCPUTime
  let timeDiff = fromIntegral (end - start) / (10^12)
  putStrLn $ show timeDiff

generateArray :: Int -> IO [Double]
generateArray n = do
  seed <- round <$> getPOSIXTime
  let gen = randoms (mkStdGen seed) :: [Double]
  return (take n gen)

quicksort :: [Double] -> [Double]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

forceEvaluation :: [a] -> ()
forceEvaluation [] = ()
forceEvaluation (x:xs) = forceEvaluation xs