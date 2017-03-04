{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
import Test.Hspec
import Data.Text
import Yesod hiding (get)
import Yesod.Articles
import Yesod.Test
import Handlers


spec :: Spec
spec = yesodSpec (ArticlesTestApp) $ do
         ydescribe "Basic test" $ do 
           yit "Get homepage with previews and contents" $ do
             get ("/" :: Text)
             statusIs 200
             -- printBody
             bodyContains "Test paragraph 1.1"
             bodyNotContains "Test paragraph 1.5"
             bodyContains "<a href=/articles/test1>Read More...</a>"
             bodyContains "<li><a href=\"#test1\">Test Article 1</a>"

             get ("/articles/test1" :: Text)
             statusIs 200
             -- printBody
             bodyContains "Test paragraph 1.5"
             bodyContains "<title>Test: Test Article 1</title>"

main :: IO ()
main = hspec spec
