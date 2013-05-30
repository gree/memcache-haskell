
{-# LANGUAGE OverloadedStrings #-}

module Network.Memcache.Parser (memcacheConduit) where

import Prelude hiding (takeWhile, getContents)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Binary
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlpha, ord)
import Data.Word
import Data.Conduit
import Data.Conduit.Attoparsec

import Network.Memcache.Op
import Network.Memcache.Message

import Debug.Trace

memcacheConduit :: Message msg => Conduit C.ByteString IO (PositionRange, (ProtocolType, msg))
memcacheConduit = conduitParser command

command :: Message msg => Parser (ProtocolType, msg)
command = do
  magic <- AL.peekWord8
  case magic of
    Just 0x80 -> do
      op <- binaryRequest
      return (BinaryPT, op)
    else do
    magic == Just 0x81
    op <- textRequest
    return (TextPT, op)
  
binaryCommand :: Parser Operation
binaryCommand = do
  _ <- AL.word8 0xa5
  ver   <- AL.anyWord8
  if ver == 1 then binaryCommandV1 else ErrorOp <$> "invalid version"

binaryCommandV1 :: Parser Operation
binaryCommandV1 = do
  cmd   <- anyWord16be
  case cmd of
    0 -> return QuitOp
    1 -> do
      keySize   <- fmap fromIntegral anyWord32be
      GetOp <$> AL.take keySize
    2 -> do
      keySize   <- fmap fromIntegral anyWord32be
      valueSize <- fmap fromIntegral anyWord32be
      SetOp <$> AL.take keySize <*> AL.take valueSize
    _ -> ErrorOp <$> "invalid command"

textCommand :: Parser Operation
textCommand = do
  cmd <- skipSpace' *> AL.takeTill isSpace_w8
  case {-trace ("command: " ++ C.unpack cmd)-} cmd of
    "quit" -> do
      _ <- endline
      return QuitOp
    "get" -> do
      operands <- skipSpace' *> AL.takeTill isEndOfLine <* endline
      return $ case C.words operands of
        [] -> ErrorOp "no key"
        k:_ -> GetOp k
    "set" -> do
      key <- skipSpace' *> AL.takeTill isSpace_w8
      flag <- skipSpace' *> decimal
      exptime <- skipSpace' *> decimal
      valueSize <- skipSpace' *> decimal <* endline
      value <- AL.take valueSize <* endline
      return $ SetOp key value
    _ -> do
      line <- AL.takeTill isEndOfLine <* endline
      return $ ErrorOp $ C.concat ["parse error <", cmd, trace ("line: " ++ C.unpack line) $ line, ">"]

isEndLine :: Parser Char
isEndLine = satisfy (\c -> c == '\n' || c == '\n')

endline :: Parser C.ByteString
endline = try (string "\r\n") <|> string "\n" <|> string "\r"

skipSpace' :: Parser ()
skipSpace' = skipWhile (== ' ')

