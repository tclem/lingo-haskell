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
  )
where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Yaml
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
#ifndef LANGUAGES_YAML_PATH
import Paths_lingo
#endif

-- | Type synonym for linguist's language name key.
type LanguageKey = Text

-- | Identifies a programming language.
data Language = Language
  { languageId :: Integer,
    languageName :: LanguageKey,
    languageExtensions :: [Text],
    languageFileNames :: [Text]
  }
  deriving (Eq, Show, Lift)

instance FromJSON Language where
  parseJSON = withObject "Language" $ \l ->
    Language
      <$> l .: "language_id"
      <*> "" -- this is set later in map iteration
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
  let normalizedLangs = Map.mapWithKey (\name lang -> lang {languageName = name}) langs

      byExtension = Map.foldr (process languageExtensions) mempty normalizedLangs
      byFileName = Map.foldr (process languageFileNames) mempty normalizedLangs

      process :: (Language -> [Text]) -> Language -> Map.Map Text [LanguageKey] -> Map.Map Text [LanguageKey]
      process selector lang acc = foldr (\ext -> Map.insertWith mappend ext [languageName lang]) acc (selector lang)

  [d|
    languages :: Map.Map LanguageKey Language
    languages = Map.fromList $(lift (Map.toList normalizedLangs))

    languagesByExtension :: Map.Map Text [LanguageKey]
    languagesByExtension = Map.fromList $(lift (Map.toList byExtension))

    languagesByFileName :: Map.Map Text [LanguageKey]
    languagesByFileName = Map.fromList $(lift (Map.toList byFileName))
    |]
