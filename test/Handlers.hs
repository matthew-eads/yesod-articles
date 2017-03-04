{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}

module Handlers where

import Yesod.Articles
import Data.Text
import Yesod hiding (get)

data ArticlesTestApp = ArticlesTestApp 

$(makeGetsWithOptions "Test:" "test/articles" [e|\x -> do defaultLayout $ (x :: WidgetT ArticlesTestApp  IO ())|])

mkYesod "ArticlesTestApp" [parseRoutes|
/ HomeR GET
!/articles/#Text ArticleR GET
|]

getHomeR :: HandlerT ArticlesTestApp IO Html
getHomeR = defaultLayout $ do
             setTitle "Test"
             $(makeContentsWithOptions "test/articles")
             $(makePreviewsWithOptions "test/articles/")



instance Yesod ArticlesTestApp where
    defaultLayout widget = do
      pc <- widgetToPageContent widget
      withUrlRenderer [hamlet| $doctype 5
                               <html>
                                    <head>
                                         <title>#{pageTitle pc}
                                         ^{pageHead pc}
                                    <body>
                                         ^{pageBody pc}
                       |]

