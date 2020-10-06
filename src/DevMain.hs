module DevMain where

import ClassyPrelude

import qualified Data.Binary as Binary
import Data.Char (chr)
import Text.Printf (printf)

run :: IO ()
run = do
  let print x = do
        sayShow x
        say . pack . concatMap (printf "%02x ") . dump $ x

  -- > [0, 1, 123, 256].map { |x| Marshal.dump(x).unpack("H*") }
  -- => [["04086900"], ["04086906"], ["040869017b"], ["040869020001"]]
  -- > [-1, -124, -257, -2**30].map { |x| Marshal.dump(x).unpack("H*") }
  -- => [["040869fa"], ["040869ff84"], ["040869fefffe"], ["040869fc000000c0"]]
  print 0
  print 1
  print 123
  print 256
  print (2^30 - 1)
  print (-1)
  print (-124)
  print (-257)
  print (-2^30)


-- * Ruby Marshal dump
-- The following implementation is based on marshal format version 4.8.
-- https://docs.ruby-lang.org/ja/2.6.0/doc/marshal_format.html
-- This format only covers the range of Fixnum (-2^30 .. (2^30 - 1)).
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
