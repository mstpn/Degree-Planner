
{-# LANGUAGE OverloadedStrings #-} 

-- This is a very simple example of parsing and generating json with Aeson,
-- by a haskell newbie and intended for similar newbies who are 
-- eager to interact with real-world stuff via json.
--
---- I couldn't find a stand-alone example of Aeson and found it tricky to get
-- started.  For example, if you don't understand to enable OverloadedStrings 
-- extension, even copy-pasting partial examples from the docs won't work!
--
-- It seems an easy enough library once you're past the first stage, though.
--
-- This almost certainly contains a number of bad practices.  It works for 
-- me and hopefully will get you started, at least.

-- The above OverloadedStrings extension allows a literal String, e.g. "name"
-- to be automatically converted to Data.Text 
-- This is useful when using Aeson's methods such as (.:) which expect Text

module Main where

import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec hiding (take, takeWhile)
import Data.Text (Text, pack)
import Control.Applicative ((<$>))
import Control.Monad (mzero)
-- 
import qualified Data.ByteString.Char8 as BS
-- Aeson's "encode" to json generates lazy bytestrings
import qualified Data.ByteString.Lazy.Char8 as BSL

-- this is the type we'll construct from the contents of the json
data Msg = Msg Text deriving (Show)

-- here's how we should parse json and construct a Msg
instance FromJSON Msg where
  parseJSON (Object v) = Msg <$> v .: "message"
  parseJSON _ = mzero

instance ToJSON Msg where
  toJSON (Msg s) = object [ "message" .= s]

-- here's one way to actually run the parsers, to go from a string
-- to our result type (in this case Msg).
--
-- Note that we do two parses: 
-- once into JSON then one more into our final type.
-- There are a number of choices when dealing with parse failures. 
-- I've chosen to parse to Maybe Msg, so that a Nothing will be returned 
-- if parseJSON fails.  (More informative options are available.)
--
-- This should take us (depending on success/failure)
-- from {"message": "hello world"} to Just (Msg "hello world")
--                              or to Nothing
--
-- Note also that I have not checked here that the input has been completed 
-- consumed, so:
-- {"message": "hello world"} foo BIG mistake
-- would yield the same successfully translated message!
-- We could look in "rest" for the remainder.
parseMsgFromString :: String -> Maybe Msg
parseMsgFromString s = let bs = BS.pack s
                       in case parse json bs of
                               (Done rest r) -> T.parseMaybe parseJSON r :: Maybe Msg
                               _             -> Nothing

-- here's the example json message we're going to try to parse: 
-- {"message": "hello world"} 
-- It's a json object with a single pair, having key 'message', and a string value.
-- It could have more fields and structure, but that's all we're going to parse out of it.
exampleJSONMessage :: String
exampleJSONMessage = "{\"message\":\"hello world\"}"

-- in main we'll parse a json message into a Msg and display that,
-- then we'll take another Msg, encode it as json, and display that.
main ::IO ()
main = do 
  print $ parseMsgFromString exampleJSONMessage
  let reply = Msg "hello Aeson!"
  putStrLn $ "Encoded reply: " ++ (BSL.unpack (encode reply))
