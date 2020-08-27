{-# LANGUAGE TemplateHaskell #-}

module Data.Languages.EmbeddedYaml
    ( languageYamlByteString
    ) where

import Data.ByteString (ByteString)
import Data.FileEmbed

languageYamlByteString :: ByteString
languageYamlByteString = $(makeRelativeToProject "languages.yml" >>= embedFile)
