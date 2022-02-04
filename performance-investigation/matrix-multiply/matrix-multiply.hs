
import Data.List


import System.Environment 

import Data.Time.Clock
import Control.DeepSeq


import  System.Random

import Text.Printf


tabulate :: Int -> (Int -> a) -> [a]
tabulate i f = take i (map f [0..])

nth :: [a] -> Int -> a
nth l i = head (drop i l)

main:: IO ()
main = do
    args <- getArgs
    if length args < 1
        then error "must have arg"
        else do
            let n = read (head args) :: Int
            ma <- sequence $ tabulate n (\ _ -> sequence $ tabulate n (\_ -> randomIO :: IO Double))
            mb <- sequence $ tabulate n (\ _ -> sequence $ tabulate n (\_ -> randomIO :: IO Double))
            sTime <- getCurrentTime

            let mc = tabulate n (\i ->
                        tabulate n (\j ->
                            foldr (+) 0.0 (tabulate n (\k ->
                                nth (nth ma i) k * 
                                nth (nth mb k) j  
                        ))
                    ))

            mc `deepseq` return ()
            fTime <- getCurrentTime
            let timeInDouble = (fromRational (toRational (diffUTCTime fTime sTime)) :: Double)
            printf "%.9f\n" timeInDouble




