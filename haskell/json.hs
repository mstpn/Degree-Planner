import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL

data Person = Person
    { name :: Text
    , age :: Int
    } deriving (Show)

instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        name <- o .: "name"
        age <- o .: "age"
        return $ Person name age

main :: IO ()
main = do
    let jsonString = "{\"name\":\"Alice\",\"age\":30}"
    case eitherDecode jsonString of
        Left err -> putStrLn $ "Error: " ++ err
        Right person -> print (person :: Person)