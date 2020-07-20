{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Data.Languages.Templates
  ( Language (..),
    LanguageKey,
    generateLanguageMap,
    languageName,
  )
where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
#ifndef LANGUAGES_YAML_PATH
import Paths_lingo
#endif

-- | Type synonym for linguist's language name key.
type LanguageKey = String

-- | Identifies a programming language.
data Language = Language
  { languageId :: Integer,
    languageKey :: LanguageKey,
    languageExtensions :: [String],
    languageFileNames :: [String]
  }
  deriving (Eq, Show, Lift)

languageName :: Language -> Text
languageName = Text.pack . languageKey

instance FromJSON Language where
  parseJSON = withObject "Language" $ \l ->
    Language
      <$> l .: "language_id"
      <*> pure "unspecified" -- this is set later in map iteration
      <*> l .:? "extensions" .!= []
      <*> l .:? "filenames" .!= []

languagesYamlPath :: IO String
#ifdef LANGUAGES_YAML_PATH
languagesYamlPath = pure LANGUAGES_YAML_PATH
#else
languagesYamlPath = getDataFileName "languages.yml"
#endif

generateLanguageMap :: DecsQ
generateLanguageMap = do
  langYaml <- runIO languagesYamlPath
  langs <- runIO (decodeFileThrow @IO @(Map.Map LanguageKey Language) langYaml)
  let normalizedLangs = Map.mapWithKey (\name lang -> lang {languageKey = name}) langs

      byExtension = Map.foldr (process languageExtensions) mempty normalizedLangs
      byFileName = Map.foldr (process languageFileNames) mempty normalizedLangs

      process :: (Language -> [String]) -> Language -> Map.Map String [LanguageKey] -> Map.Map String [LanguageKey]
      process selector lang acc = foldr (\ext -> Map.insertWith mappend ext [languageKey lang]) acc (selector lang)

  [d|
    languages :: Map.Map LanguageKey Language
    languages = Map.fromList $(lift (Map.toList normalizedLangs))

    languagesByExtension :: Map.Map String [LanguageKey]
    languagesByExtension = Map.fromList $(lift (Map.toList byExtension))

    languagesByFileName :: Map.Map String [LanguageKey]
    languagesByFileName = Map.fromList $(lift (Map.toList byFileName))
    |]
