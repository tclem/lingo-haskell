module Main where

import Test.Hspec
import Data.Languages

main :: IO ()
main = hspec $ do
  describe "detect language for path" $ do
    it "can detect Ruby by file extension" $
      languageName <$> languageForPath "test.rb" `shouldBe` Just "Ruby"

    it "can detect Ruby by filename" $
      languageName <$> languageForPath "Rakefile" `shouldBe` Just "Ruby"

    it "returns Nothing for unknown files" $
      languageName <$> languageForPath "noideawhatthisis" `shouldBe` Nothing

    it "returns Nothing for unknown extensions" $
      languageName <$> languageForPath ".noideawhatthisis" `shouldBe` Nothing

  describe "languages" $
    it "parsed languages.yml" $ do
      length languages `shouldBe` 519
      length languagesByExtension `shouldBe` 1117
      length languagesByFileName `shouldBe` 235
