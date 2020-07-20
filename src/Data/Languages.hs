{-# LANGUAGE TemplateHaskell #-}

-- | Primary interface for looking up the programming language of a file or
-- interacting with languages known to linguist
-- (https://github.com/github/linguist).
module Data.Languages
  ( languagesForPath,
    languages,
    languagesByExtension,
    languagesByFileName,
    LanguageKey,
    Language (..),
    languageName
  )
where

import Control.Applicative
import Data.Languages.Templates
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath.Posix

$(generateLanguageMap)

-- | Find the set of possible languages for a given file path.
--
-- Multiple results will be returned for ambiguous files; for example, @.md@ files can be Markdown or GCC machine descriptions, and @.php@ files can be PHP or Hack source files.
languagesForPath :: FilePath -> [Language]
languagesForPath path = languageForFileName <|> languageForExtension
  where
    languageForFileName = languageFor (takeFileName path) languagesByFileName
    languageForExtension = languageFor (takeExtension path) languagesByExtension
    languageFor :: String -> Map.Map String [LanguageKey] -> [Language]
    languageFor k =
      foldMap (maybeToList . flip Map.lookup languages)
        . fromMaybe []
        . Map.lookup k
