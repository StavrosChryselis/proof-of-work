module Lib where

import           Control.Monad          (guard)
import           Crypto.Hash            (Digest, SHA256, hash)
import           Crypto.Util            (i2bs_unsized, bs2i)
import qualified Data.ByteArray         as A
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Char8  as C
import           Data.Maybe             (fromMaybe)
import           Data.Word              (Word8)

-- |Computes the SHA256 hash of the given @'ByteString'@.
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
    | nonce >= maxNonce          = Nothing
    | (bs2i hashResult) < target = Just (nonce, hashResult)
    | otherwise                  = findNonce block (nonce+1) target
      where
        hashResult = sha256 $ B.append block $ i2bs_unsized nonce
