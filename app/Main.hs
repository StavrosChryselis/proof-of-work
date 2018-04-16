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

main :: IO ()
main = do
    [genesisString] <- getArgs
    let genesisBlock = encodeUtf8 $ pack genesisString
    forM_ [1..32] $ \ difficultyBits -> do
        let difficulty = 2^difficultyBits
        putStrLn $ "Difficulty: " ++ show difficulty ++ " (" ++ show difficultyBits ++ " bits)"
        putStrLn "Starting search.."
        (elapsedTime, result) <- time $ evaluate $ proofOfWork genesisBlock difficultyBits
        case result of
            Nothing -> putStrLn $ "Failed after " ++ show maxNonce ++ "(max_nonce) tries"
            Just (nonce, hashResult) -> do
                let hashPower = fromIntegral nonce / elapsedTime
                putStrLn $ "Success with nonce " ++ show nonce
                putStrLn $ "Hash is " ++ show (encode hashResult)
                putStrLn $ "Hashing Power is " ++ show hashPower ++ " hashes per second"
        putStrLn $ "Elapsed Time is " ++ show elapsedTime ++ " seconds"
        putStrLn ""