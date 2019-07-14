module Data.Languages
  ( languageForPath
  
  , languages
  , languagesByExtension
  , languagesByFileName
  , Language(..)
  ) where

import           Control.Applicative
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Gen_Languages
import           System.FilePath.Posix

languageForPath :: FilePath -> Maybe Language
languageForPath path = languageForFileName <|> languageForExtension
  where
    languageForFileName = languageFor (takeFileName path) languagesByFileName
    languageForExtension = languageFor (takeExtension path) languagesByExtension
    languageFor k f = listToMaybe . fromMaybe mempty $ Map.lookup (Text.pack k) f

languagesByExtension :: Map.Map Text [Language]
languagesByExtension = Map.foldrWithKey (buildMap languageExtensions) mempty languages

languagesByFileName :: Map.Map Text [Language]
languagesByFileName = Map.foldrWithKey (buildMap languageFileNames) mempty languages

buildMap :: (Language -> [Text]) -> Text -> Language -> Map.Map Text [Language] -> Map.Map Text [Language]
buildMap f k lang m = foldr (\ext b -> Map.insertWith (<>) ext (pure lang) b) m (f lang)
