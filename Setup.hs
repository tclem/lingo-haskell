{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Debug.Trace
import           Distribution.PackageDescription
import           Distribution.Simple hiding (Language)
import           Distribution.Simple.BuildPaths
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           System.Directory
import           System.FilePath.Posix ((</>))
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Y
import           Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))

main = defaultMainWithHooks simpleUserHooks { postConf = conf }

conf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
conf _ _ _ info = do
  let autogenDir = autogenPackageModulesDir info
  createDirectoryIfMissing True autogenDir
  traceM $ "Writing Gen_Languages.hs to autogenDir: " <> autogenDir
  dir <- getCurrentDirectory
  let yamlFile = dir </> "languages.yml"
  yaml <- B.readFile yamlFile
  B.writeFile (autogenDir </> "Gen_Languages.hs") (contents yaml)
  pure ()

contents :: B.ByteString -> B.ByteString
contents yaml =
  "module Gen_Languages where\n\
  \\n\
  \import Data.ByteString (ByteString)\n\
  \import Data.Text (Text)\n\
  \import qualified Data.Map.Strict as Map\n\
  \\n\
  \data Language\n\
  \  = Language\n\
  \  { languageID         :: Integer\n\
  \  , languageName       :: Text\n\
  \  , languageExtensions :: [Text]\n\
  \  , languageFileNames  :: [Text]\n\
  \  } deriving (Eq, Show)\n\
  \\n\
  \languages :: Map.Map Text Language\n\
  \languages = Map.fromList\
  \  [\n    "
  <>
  either (BC.pack . show) (B.intercalate ",\n    ") langs
  <>
  "  ]\n"

  where
    langs = Map.foldrWithKey (\k v xs -> "(\"" <> encodeUtf8 k <> "\", " <> BC.pack (show v { languageName = k }) <> ")" : xs) mempty <$> languages

    languages :: Either Y.ParseException (Map.Map Text Language)
    languages = Y.decodeEither' yaml

    languagesByExtension :: Either Y.ParseException (Map.Map Text Language)
    languagesByExtension = Map.foldrWithKey insertExts mempty <$> languages
      where
        insertExts :: Text -> Language -> Map.Map Text Language -> Map.Map Text Language
        insertExts k lang m = foldr (\ext m -> Map.insert ext lang { languageName = k } m) m (languageExtensions lang)

data Language
  = Language
  { languageID         :: Integer
  , languageName       :: Text
  , languageExtensions :: [Text]
  , languageFileNames  :: [Text]
  } deriving (Eq, Show)

instance FromJSON Language where
  parseJSON (Y.Object v) =
    Language <$>
    v .: "language_id" <*>
    v .:? "language_name" .!= mempty <*>
    v .:? "extensions" .!= mempty <*>
    v .:? "filenames" .!= mempty
  parseJSON _ = fail "Expected Object for Language value"
