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
    languageFor k f = maybe Nothing lookupPrimary $ Map.lookup (Text.pack k) f

    -- Some extensions and filenames associate with multiple languages, this is
    -- a dumb way to specify which is the primary language.
    lookupPrimary xs
      | null xs = Nothing
      | "Markdown" `elem` xs = lookup "Markdown"
      | otherwise = lookup (head xs)
      where lookup k = Map.lookup k languages
