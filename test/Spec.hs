module Main where

import Test.Hspec
import Data.Languages

main :: IO ()
main = hspec $ do
  describe "detect language for path" $ do
    it "can detect Ruby by file extension" $
      languageName <$> languagesForPath "test.rb" `shouldBe` ["Ruby"]

    it "can detect Ruby by filename" $
      languageName <$> languagesForPath "Rakefile" `shouldBe` ["Ruby"]

    it "Gemfile.lock is not Ruby" $
      languageName <$> languagesForPath "Gemfile.lock" `shouldBe` []

    it "returns Nothing for unknown files" $
      languageName <$> languagesForPath "noideawhatthisis" `shouldBe` []

    it "returns Nothing for unknown extensions" $
      languageName <$> languagesForPath ".noideawhatthisis" `shouldBe` []

  describe "languages" $
    it "parsed languages.yml" $ do
      length languages `shouldBe` 519
      length languagesByExtension `shouldBe` 1117
      length languagesByFileName `shouldBe` 234
