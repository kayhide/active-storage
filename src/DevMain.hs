module DevMain where

import ClassyPrelude

import Crypto.Hash (Digest, SHA1 (..))
import qualified Crypto.Hash as Crypto
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA1)
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Aeson (Object, Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.ByteArray (convert)
import qualified Data.ByteString.Base64 as Base64
import Data.Char (chr)
import System.Environment (lookupEnv)
import Text.Printf (printf)

run :: IO ()
run = do
  Just secret_key_base <- lookupEnv "SECRET_KEY_BASE"
  say $ pack $ secret_key_base

  let secret = generateSecret (encodeUtf8 $ pack secret_key_base) "ActiveStrage"
  let print x = do
        say ""
        sayShow x
        say . pack . concatMap (printf "%02x ") . dump $ x
        sayShow $ Aeson.encode $ wrap x "blob_id"
        say $ signedId secret x

  print 0
  print 1
  print 123
  print 256
  print (2^30 - 1)
  print (-1)
  print (-124)
  print (-257)
  print (-2^30)

generateSecret :: ByteString -> ByteString -> ByteString
generateSecret password salt =
  fastPBKDF2_SHA1 (PBKDF2.Parameters 1000 64) password salt

digest :: ByteString -> ByteString -> Text
digest secret xs = tshow (hmacGetDigest $ hmac secret xs :: Digest SHA1)

signedId :: ByteString -> Int -> Text
signedId secret x = decodeUtf8 body <> "--" <> digest secret body
  where
    body :: ByteString
    body = Base64.encode $ toStrict $ Aeson.encode $ wrap x "blob_id"

wrap :: Int -> Text -> Value
wrap x purpose =
  object [ "_rails" .= object [ "message" .= (decodeUtf8 $ Base64.encode $ dump x), "exp" .= Null, "pur" .= purpose ] ]

-- * Ruby Marshal dump
-- The following implementation is based on marshal format version 4.8.
-- https://docs.ruby-lang.org/ja/2.6.0/doc/marshal_format.html
-- This format only covers the range of Fixnum (-2^30 .. (2^30 - 1)).
--
-- ruby
-- > [0, 1, 123, 256].map { |x| Marshal.dump(x).unpack("H*") }
-- => [["04086900"], ["04086906"], ["040869017b"], ["040869020001"]]
-- > [-1, -124, -257, -2**30].map { |x| Marshal.dump(x).unpack("H*") }
-- => [["040869fa"], ["040869ff84"], ["040869fefffe"], ["040869fc000000c0"]]
dump :: Int -> ByteString
dump n
  | (-2^30) <= n && n < -123 = header <> len <> reverse xs
  | n < 0 = header <> (drop 7 $ toStrict $ Binary.encode $ n - 5)
  | n == 0 = header <> pack [0]
  | n < 123 = header <> (drop 7 $ toStrict $ Binary.encode $ n + 5)
  | n < 2^30 = header <> len <> reverse xs
  where
    header :: ByteString
    header = pack [4, 8] <> "i"

    xs :: ByteString
    xs = trim $ toStrict $ Binary.encode n

    len :: ByteString
    len = trim $ toStrict $ Binary.encode $ (length xs * bool (-1) 1 (0 < n))

    trim :: ByteString -> ByteString
    trim = dropWhile (\x -> x == 0 || x == 0xff)
