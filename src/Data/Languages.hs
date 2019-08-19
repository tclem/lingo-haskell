-- | Primary interface for looking up the programming language of a file or
-- interacting with languages known to linguist
-- (https://github.com/github/linguist).
module Data.Languages
  ( languageForPath
  , languages
  , languagesByExtension
  , languagesByFileName
  , LanguageKey
  , Language(..)
  ) where

import           Control.Applicative
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Gen_Languages
import           System.FilePath.Posix

-- | Find the Language (if any) for a FilePath.
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
