{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import System.Microtimer (time)
import Control.Exception (evaluate)
import Data.ByteString.Base16 (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

stressTest :: ByteString -> Integer -> IO ()
stressTest block difficultyBits
  | difficultyBits > 32 = pure ()
  | otherwise = do
    let difficulty = 2^difficultyBits
    putStrLn $ "Difficulty: " ++ show difficulty ++ " (" ++ show difficultyBits ++ " bits)"
    putStrLn "Starting search.."
    (elapsedTime, result) <- time $ evaluate $ proofOfWork block difficultyBits
    case result of
        Nothing -> putStrLn $ "Failed after " ++ show maxNonce ++ "(max_nonce) tries"
        Just (nonce, hashResult) -> do
            let hashPower = fromIntegral nonce / elapsedTime
            putStrLn $ "Success with nonce " ++ show nonce
            putStrLn $ "Hash is " ++ show (encode hashResult)
            putStrLn $ "Hashing Power is " ++ show hashPower ++ " hashes per second"
            putStrLn $ "Elapsed Time is " ++ show elapsedTime ++ " seconds"
            putStrLn ""
            stressTest (block `B.append` hashResult) (difficultyBits+1)

main :: IO ()
main = do
    [genesisString] <- getArgs
    let genesisBlock = encodeUtf8 $ pack genesisString
    stressTest genesisBlock 1