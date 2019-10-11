module Main where

import Data.Languages
import Data.List (sort)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "detect language for path" $ do
    it "can detect Ruby by file extension" $
      languageName <$> languagesForPath "test.rb" `shouldBe` ["Ruby"]

    it "can detect Ruby by filename" $
      languageName <$> languagesForPath "Rakefile" `shouldBe` ["Ruby"]

    it "returns all languages that a PHP file could be" $
      sort (languageName <$> languagesForPath "test.php") `shouldBe` ["Hack", "PHP"]

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
