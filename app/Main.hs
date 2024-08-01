{-# LANGUAGE GADTs #-}

module Main where

-- imported per recommendation
-- https://markkarpov.com/tutorial/megaparsec.html#working-with-alternatives
import Control.Applicative
import Data.Scientific (toRealFloat)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data JsonValue where
  JsonNull :: JsonValue
  JsonBool :: Bool -> JsonValue
  JsonNumber :: Double -> JsonValue
  JsonString :: String -> JsonValue
  JsonArray :: [JsonValue] -> JsonValue
  JsonObject :: [(String, JsonValue)] -> JsonValue
  deriving (Show, Eq)

type Parser = Parsec Void String

-- this would probably suck if we didn't bother to parse white space first.
--
-- note json doesn't support comments of any kind, so we resort to empty
-- empty to skip that part of the white space parser.
--
-- note that we use space1 instead of space bc space accepts the empty
-- input, space could therefore start an infinate loop.

ws :: Parser ()
ws = L.space space1 empty empty

-- choice functions identical to <|> note that order matters
-- here and determines which json parser gets ran first. Here
-- are all the various smaller parsers that make up our big one.

pJson :: Parser JsonValue
pJson =
  choice
    [ jsonNull
    , jsonBool
    , jsonNumber
    , jsonString
    , jsonArray
    , jsonObject
    ]

-- string "s" parses a sequence of charecters
-- given by "s", returns the parsed string.

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

-- the <$ operator just puts the value on its left-hand side
-- into a functional context replacing whatever is there at the
-- moment.

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
 where
  jsonTrue = JsonBool True <$ string "true"
  jsonFalse = JsonBool False <$ string "false"

-- to parse a JsonNumber, we must accept a Double, so we parse
-- scientific values with L.scientific, we also check for a
-- sign (+,-) using L.signed.
jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . toRealFloat <$> L.signed space L.scientific

-- used to parse what is considered a string, and strings are
-- usually surrounded by quotes, so we bake that assumption in
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray =
  JsonArray
    <$> between
      (char '[' *> ws) -- note the use of our white space parser here
      (ws *> char ']') -- and again here
      (sepBy pJson $ char ',' *> ws) -- here too

-- remember, objects are key value pairs
jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> between
      (char '{' *> ws)
      (ws *> char '}')
      (sepBy pair $ char ',' *> ws)
 where
  pair = do
    key <- jsonString
    _ <- char ':' *> ws -- ignore the colan
    value <- pJson
    case key of
      -- object keys are always strings no?
      JsonString k -> return (k, value)
      -- therefore,
      _ -> fail "This should never happen"

-- allows for testing
parseFile :: (Show a) => FilePath -> Parser a -> IO ()
parseFile file parser = do
  input <- readFile file
  parseTest parser input

main :: IO ()
main = hspec $
  describe "running json parser tests:" $ do
    it "should parse null values" $
      parse pJson "" "null" `shouldParse` JsonNull

    it "should parse true values" $
      parse pJson "" "true" `shouldParse` JsonBool True

    it "should parse false values" $
      parse pJson "" "false" `shouldParse` JsonBool False

    it "should parse integers" $
      parse pJson "" "11" `shouldParse` JsonNumber 11

    it "should parse doubles" $
      parse pJson "" "3.14159" `shouldParse` JsonNumber 3.14159

    it "should parse negative numbers" $
      parse pJson "" "-1" `shouldParse` JsonNumber (-1)

    it "should parse simple strings" $
      let input = "\"a simple string\""
          output = JsonString "a simple string"
       in parse pJson "" input `shouldParse` output

    it "should parse numbers and letters" $
      let input = "\"numbers11and22letters\""
          output = JsonString "numbers11and22letters"
       in parse pJson "" input `shouldParse` output

    it "should parse symbols and letters" $
      let input = "\"sergiupod@gmail.com\""
          output = JsonString "sergiupod@gmail.com"
       in parse pJson "" input `shouldParse` output

    it "should parse arrays of numbers" $
      let input = "[1,2,3]"
          output = JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3]
       in parse pJson "" input `shouldParse` output

    it "should parse arrays of different values" $
      let input = "[1, true, \"foo\"]"
          output = JsonArray [JsonNumber 1, JsonBool True, JsonString "foo"]
       in parse pJson "" input `shouldParse` output

    it "should parse nested arrays" $
      let input = "[1, [2, 3]]"
          output = JsonArray [JsonNumber 1, JsonArray [JsonNumber 2, JsonNumber 3]]
       in parse pJson "" input `shouldParse` output

    it "should parse objects" $
      let input = "{ \"hello\": \"world\" }"
          output = JsonObject [("hello", JsonString "world")]
       in parse pJson "" input `shouldParse` output

    it "should parse nested objects" $
      let input = "{ \"top\": { \"bottom\": 50 }}"
          output = JsonObject [("top", JsonObject [("bottom", JsonNumber 50)])]
       in parse pJson "" input `shouldParse` output
