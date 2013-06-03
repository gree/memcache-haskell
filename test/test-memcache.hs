{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Monoid
import Data.Char
import Data.Maybe
import Data.List
import Data.Word
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Network.Memcache.Op

import Debug.Trace

type ValueT = BS.ByteString
type BytesT = Word64

newtype Key = Key { getKey :: BS.ByteString } deriving Show
newtype PrintableString = PrintableString { getPrintableString :: String } deriving Show

instance Arbitrary BS.ByteString where
  arbitrary = fmap BS.pack $ arbitrary `suchThat` printableNonSpace

instance Arbitrary Key where
  arbitrary = do
    bs <- fmap BS.pack $ arbitrary `suchThat` printableNonSpace
    return (Key bs)

instance Arbitrary PrintableString where
  arbitrary = do
    bs <- arbitrary `suchThat` printableNonSpace
    return (PrintableString bs)

instance Arbitrary Option where
  arbitrary = oneof [ return Noreply ]

printableNonSpace xs = not (null xs) && (and $ map (\c -> isPrint c && c /= ' ') xs)

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

prop_toChunks_SetOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_toChunks_SetOp key flags exptime options value = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ SetOp key' flags exptime (fromIntegral $ BS.length value) value options)
   e = "set " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ (show $ BS.length value) ++ showOptions options ++ ln ++ BS.unpack value ++ ln

prop_toChunks_CasOp :: Key -> Word32 -> Word64 -> Word64 -> [Option] -> ValueT -> Bool
prop_toChunks_CasOp key flags exptime version options value = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ CasOp key' flags exptime (fromIntegral $ BS.length value) version value options)
   e = "cas " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ (show $ BS.length value) ++ " " ++ show version ++ showOptions options ++ ln ++ BS.unpack value ++ ln

prop_toChunks_AddOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_toChunks_AddOp key flags exptime options value = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ AddOp key' flags exptime (fromIntegral $ BS.length value) value options)
   e = "add " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ (show $ BS.length value) ++ showOptions options ++ ln ++ BS.unpack value ++ ln

prop_toChunks_ReplaceOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_toChunks_ReplaceOp key flags exptime options value = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ ReplaceOp key' flags exptime (fromIntegral $ BS.length value) value options)
   e = "replace " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ (show $ BS.length value) ++ showOptions options ++ ln ++ BS.unpack value ++ ln

prop_toChunks_AppendOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_toChunks_AppendOp key flags exptime options value = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ AppendOp key' flags exptime (fromIntegral $ BS.length value) value options)
   e = "append " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ (show $ BS.length value) ++ showOptions options ++ ln ++ BS.unpack value ++ ln

prop_toChunks_PrependOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_toChunks_PrependOp key flags exptime options value = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ PrependOp key' flags exptime (fromIntegral $ BS.length value) value options)
   e = "prepend " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ (show $ BS.length value) ++ showOptions options ++ ln ++ BS.unpack value ++ ln

prop_toChunks_GetOp :: [Key] -> Bool
prop_toChunks_GetOp keys = r == e
 where
   keys' = map getKey keys
   r = chunk2string (toChunks $ GetOp keys')
   e = "get " ++ BS.unpack (BS.intercalate " " keys') ++ ln

prop_toChunks_GetsOp :: [Key] -> Bool
prop_toChunks_GetsOp keys = r == e
 where
   keys' = map getKey keys
   r = chunk2string (toChunks $ GetsOp keys')
   e = "gets " ++ BS.unpack (BS.intercalate " " keys') ++ ln

prop_toChunks_DeleteOp :: Key -> [Option] -> Bool
prop_toChunks_DeleteOp key options = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ DeleteOp key' options)
   e = "delete " ++ BS.unpack key' ++ showOptions options ++ ln

prop_toChunks_IncrOp :: Key -> Word64 -> [Option] -> Bool
prop_toChunks_IncrOp key diff options = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ IncrOp key' diff options)
   e = "incr " ++ BS.unpack key' ++ " " ++ show diff ++ showOptions options ++ ln

prop_toChunks_DecrOp :: Key -> Word64 -> [Option] -> Bool
prop_toChunks_DecrOp key diff options = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ DecrOp key' diff options)
   e = "decr " ++ BS.unpack key' ++ " " ++ show diff ++ showOptions options ++ ln

prop_toChunks_TouchOp :: Key -> Word64 -> [Option] -> Bool
prop_toChunks_TouchOp key exptime options = r == e
 where
   key' = getKey key
   r = chunk2string (toChunks $ TouchOp key' exptime options)
   e = "touch " ++ BS.unpack key' ++ " " ++ show exptime ++ showOptions options ++ ln

prop_toChunks_PingOp :: Bool
prop_toChunks_PingOp = chunk2string (toChunks PingOp) == "ping" ++ ln

prop_toChunks_FlushAllOp :: Bool
prop_toChunks_FlushAllOp = chunk2string (toChunks FlushAllOp) == "flush_all" ++ ln

prop_toChunks_QuitOp :: Bool
prop_toChunks_QuitOp = chunk2string (toChunks QuitOp) == "quit" ++ ln

prop_toChunks_StatsOp :: [PrintableString] -> Bool
prop_toChunks_StatsOp args = chunk2string (toChunks $ StatsOp (map getPrintableString args)) == "stats" ++ concat (map (\s -> " " ++ getPrintableString s) args) ++ ln

--------------------------------

prop_parseOpHeader_SetOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_parseOpHeader_SetOp key flags exptime options value = parseOpHeader_SetOp "set" SetOp key flags exptime options value

prop_parseOpHeader_CasOp :: Key -> Word32 -> Word64 -> Word64 -> [Option] -> ValueT -> Bool
prop_parseOpHeader_CasOp key flags exptime version options value = op == Just (CasOp key' flags exptime (fromIntegral $ BS.length value) version "" options)
 where
   key' = getKey key
   op = parseOpHeader (BS.pack ("cas " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ show (BS.length value) ++ " " ++ show version ++ showOptions options))

prop_parseOpHeader_AddOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_parseOpHeader_AddOp key flags exptime options value = parseOpHeader_SetOp "add" AddOp key flags exptime options value

prop_parseOpHeader_ReplaceOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_parseOpHeader_ReplaceOp key flags exptime options value = parseOpHeader_SetOp "replace" ReplaceOp key flags exptime options value

prop_parseOpHeader_AppendOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_parseOpHeader_AppendOp key flags exptime options value = parseOpHeader_SetOp "append" AppendOp key flags exptime options value

prop_parseOpHeader_PrependOp :: Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
prop_parseOpHeader_PrependOp key flags exptime options value = parseOpHeader_SetOp "prepend" PrependOp key flags exptime options value

prop_parseOpHeader_GetOp :: [Key] -> Bool
prop_parseOpHeader_GetOp keys = op == if null keys then Nothing else Just (GetOp keys')
  where
   keys' = map getKey keys
   op = parseOpHeader (BS.pack ("get " ++ BS.unpack (BS.intercalate " " keys')))

prop_parseOpHeader_GetsOp :: [Key] -> Bool
prop_parseOpHeader_GetsOp keys = op == if null keys then Nothing else Just (GetsOp keys')
  where
   keys' = map getKey keys
   op = parseOpHeader (BS.pack ("gets " ++ BS.unpack (BS.intercalate " " keys')))

prop_parseOpHeader_DeleteOp :: Key -> [Option] -> Bool
prop_parseOpHeader_DeleteOp key options = op == Just (DeleteOp key' options)
 where
   key' = getKey key
   op = parseOpHeader (BS.pack ("delete " ++ BS.unpack key' ++ showOptions options))

prop_parseOpHeader_IncrOp :: Key -> Word64 -> [Option] -> Bool
prop_parseOpHeader_IncrOp key value options = op == Just (IncrOp key' value options)
 where
   key' = getKey key
   op = parseOpHeader (BS.pack ("incr " ++ BS.unpack key' ++ " " ++ show value ++ showOptions options))

prop_parseOpHeader_DecrOp :: Key -> Word64 -> [Option] -> Bool
prop_parseOpHeader_DecrOp key value options = op == Just (DecrOp key' value options)
 where
   key' = getKey key
   op = parseOpHeader (BS.pack ("decr " ++ BS.unpack key' ++ " " ++ show value ++ showOptions options))

prop_parseOpHeader_TouchOp :: Key -> Word64 -> [Option] -> Bool
prop_parseOpHeader_TouchOp key exptime options = op == Just (TouchOp key' exptime options)
 where
   key' = getKey key
   op = parseOpHeader (BS.pack ("touch " ++ BS.unpack key' ++ " " ++ show exptime ++ showOptions options))

prop_parseOpHeader_PingOp :: Bool
prop_parseOpHeader_PingOp = parseOpHeader "ping" == Just (PingOp)

prop_parseOpHeader_FlushAllOp :: Bool
prop_parseOpHeader_FlushAllOp = parseOpHeader "flush_all" == Just (FlushAllOp)

prop_parseOpHeader_VersionOp :: Bool
prop_parseOpHeader_VersionOp = parseOpHeader "version" == Just (VersionOp)

prop_parseOpHeader_QuitOp :: Bool
prop_parseOpHeader_QuitOp = parseOpHeader "quit" == Just (QuitOp)

prop_parseOpHeader_StatsOp :: [PrintableString] -> Bool
prop_parseOpHeader_StatsOp stats = parseOpHeader (BS.pack $ "stats" ++ concat (map (\s -> " " ++ getPrintableString s) stats)) == Just (StatsOp (map getPrintableString stats))

----------------------------------------------------------------

chunk2string = BS.unpack . BS.concat

ln = "\r\n"

showOptions options = (concat $ map (\o -> " " ++ show o) options)

parseOpHeader_SetOp :: String -> (BS.ByteString -> Word32 -> Word64 -> BytesT -> ValueT -> [Option] -> Op) -> Key -> Word32 -> Word64 -> [Option] -> ValueT -> Bool
parseOpHeader_SetOp cmd opType key flags exptime options value = op == Just (opType key' flags exptime (fromIntegral $ BS.length value) "" options)
 where
   key' = getKey key
   op = parseOpHeader (BS.pack (cmd ++ " " ++ BS.unpack key' ++ " " ++ show flags ++ " " ++ show exptime ++ " " ++ show (BS.length value) ++ showOptions options))



