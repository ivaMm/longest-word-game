{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( randomString
      , checkAttempt
      , calculateScore
    ) where


import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as B8L
import qualified Data.Char                  as C
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Time.Clock            as TC
import qualified Data.Time.Clock.POSIX      as TCP
import qualified GHC.Generics               as G
import qualified Network.HTTP.Simple        as HT
import qualified System.Random              as R



-- A standard function generating random strings.
randomString :: IO String
randomString = do
    liftM (take 10 . R.randomRs ('A','Z')) R.newStdGen

checkAttempt :: MonadIO m => [Char] -> [Char] -> m Bool
checkAttempt attempt randomStr = do
    lenAtLeast   <- atLeastNLength attempt 2
    inRandomStr  <- attemptInRandomStr attempt randomStr
    inDictionary <- attemptInDictionary attempt
    return $ lenAtLeast && inRandomStr && inDictionary
    
-- check if attempt is in random string
toUpperCase :: [Char] -> [Char]
toUpperCase attempt = map (\a -> C.toUpper a) attempt

atLeastNLength :: (Monad m, Foldable t) => t a -> Int -> m Bool
atLeastNLength attempt n = return $ L.length attempt >= n

attemptInRandomStr :: Monad m => [Char] -> [Char] -> m Bool
attemptInRandomStr attempt ys = do
    let att = toUpperCase attempt
    let fxs = frequency att
    return $ foldl(\acc (x, i) -> if x `elem` ys then (atLeastNtimes i ys x) && acc else False) True fxs

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

atLeastNtimes :: Eq p => Int -> [p] -> p -> Bool
atLeastNtimes n arr el = do
    numTimesFound el arr >= n

numTimesFound :: Eq a => a -> [a] -> Int
numTimesFound x xs = (length . filter (== x)) xs


-- api calls part - check if attempt is valid word
buildURL :: String -> HT.Request
buildURL attempt = do
    HT.parseRequestThrow_ ("https://wagon-dictionary.herokuapp.com/" <> attempt)

data Attempt = Attempt
  { found :: Bool
  , word :: String
  }
  deriving (Show, Eq, G.Generic, A.ToJSON, A.FromJSON)

maybeWord :: B8L.ByteString -> Maybe Attempt
maybeWord jsonString = A.decode jsonString

attemptInDictionary :: MonadIO m => [Char] -> m Bool
attemptInDictionary attempt = do
    let endpoint = buildURL attempt
    response <- HT.httpLbs endpoint
    let 
      body = HT.getResponseBody response 
      result = maybeWord body
    case result of
      Just a  -> return $ found a
      Nothing -> return False

calculateScore :: Foldable t => TC.UTCTime -> TC.UTCTime -> t a -> Int
calculateScore startTime endTime attempt = 
  (10 * length attempt) - round (((TCP.utcTimeToPOSIXSeconds endTime) - (TCP.utcTimeToPOSIXSeconds startTime))) 
