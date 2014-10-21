module Codec.Archive.Ar where

import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (isSpace)
import Control.Monad (when, liftM, unless)
import Control.Applicative ((<$>), (<*>))
-- import Control.Exception (assert)

type Entry = (String, Int, Int, Int, Int, B.ByteString)

readAr :: L.ByteString -> [Entry]
readAr = runGet $ do
    magicAsciiStr "!<arch>\LF"
    getEntries


getEntries :: Get [Entry]
getEntries = do
    empty <- isEmpty
    if empty
    then return []
    else (:) <$> getEntry <*> getEntries


-- | Read an int as a string of the given length
getAsciiInt :: Int -> Get Int
getAsciiInt l = liftM (read . unpack) (getByteString l)

getEntry :: Get Entry
getEntry = do
    -- entry are aligned on even byte boundary
    pos <- bytesRead
    when (odd pos) (skip 1)
    name    <- getArName
    time    <- getAsciiInt 12
    owner   <- getAsciiInt 6
    group   <- getAsciiInt 6
    mode    <- getAsciiInt 8
    content <- getContent
    return (name, time, owner, group, mode, content)

getArName :: Get String
getArName = liftM (strip . unpack) (getByteString 16)
  where
    strip = dropWhile isSpace . reverse . dropSlash . dropWhile isSpace . reverse
    dropSlash ('/':s) = s
    dropSlash s = s

getContent :: Get B.ByteString
getContent = do
    len <- getAsciiInt 10
    fm <- getByteString 2
    unless (fm == fileMagic) (fail ("Invalid file magic " ++ unpack fm))
    getByteString len
  where
    fileMagic = B.pack [0x60, 0x0A]


-- | Expect to be able to read the given ASCII string.
-- If it fails, no input is consumed.
magicAsciiStr :: String -> Get ()
magicAsciiStr s = do
    bs <- lookAhead $ getByteString n
    when (bs /= pack s) $ fail "Invalid magic ASCII string"
    skip n
  where
    n = length s
