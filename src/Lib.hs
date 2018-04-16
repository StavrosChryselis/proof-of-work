module Lib where

import           Crypto.Hash            (Digest, SHA256, hash)
import           Crypto.Util            (i2bs_unsized, bs2i)
import qualified Data.ByteArray         as A
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B

sha256 :: ByteString -> ByteString
sha256 bs = let digest = hash bs :: Digest SHA256 in B.pack $ A.unpack digest

maxNonce :: Integer
maxNonce = 2^32

proofOfWork :: ByteString -> Integer -> Maybe (Integer, ByteString)
proofOfWork block difficultyBits = findNonce block 0 target
  where
    target = 2^(256-difficultyBits)

findNonce :: ByteString -> Integer -> Integer -> Maybe (Integer, ByteString)
findNonce block nonce target 
    | nonce >= maxNonce        = Nothing
    | bs2i hashResult < target = Just (nonce, hashResult)
    | otherwise                = findNonce block (nonce+1) target
      where
        hashResult = sha256 $ B.append block $ i2bs_unsized nonce
