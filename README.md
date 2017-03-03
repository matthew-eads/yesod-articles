# Yesod.Articles

This library provides three core funtions for automatically generating
a homepage with articles, with seperate pages for each article.

`makeGets` and `makeGetsWithOptions` should be spliced in at/near the top 
of the file where you will put the handler for your homepage. 
`makeGets` will call `makeGetsWithOptions` with the following args:
  * `prefix = ""` This is the prefix for the `<title>` tag of each article page,
    for example in my website, the prefix is "Matt Eads:", so each page title
    will be "Matt Eads: <article_title>"
  * `path = "templates/articles"` This is the directory where all your 
    `<article>.hamlet` files live.
  * `exp = return (VarE (mkName "defaultLayout"))` This one makes me uneasy, and
    I will probably change it soon. The purpose of this argument is to wrap 
    `do {$(whamletFile <article>.hamlet)}`. In my website, I pass in 
    `[e|\x -> do defaultLayout $ x|]`, which is probably a good idea for you too.

Because of the wonderful type-safety-ness of all of Yesod's links, it is impossible,
or at least infeasable, to generate truly seperate pages for each article. 
Instead the workaround that I use is to generate a `getArticleR` handler, which
takes in an argument from the url, this is used to then defer to the appropriate
handler function (ex: `getArticleR "foo"` will return the `getFooR` function).
However, you still need to make sure you put this in your routes file, 
specifically in addition to any other routes you have, you need to add
`!/articles/#Text ArticleR GET`.
    
Inside your handler for your homepage (ex: `getHomeR`), you should
splice in `makePreviews` or `makePreviewsWithOptions`. These will generate all 
the previews for your articles, as well as a 'Read More...' link to the full article.
This _will_ fail if you haven't first spliced in `makeGets` or its sibling.
Currently the only option for `makePreviewsWithOptions` is the path, which is used
in the same way as in `makeGetsWithOptions`, and is defaulted to `templates/articles`.
The path is also the only option for `makeContentsWithOptions`. `makeContents` is optional,
and if spliced in (ideally before `makePreviews`) will generate a small table of contents.

Here my `Home.hs` file for reference:

```haskell
module Handler.Home where

import Import
import Yesod.Articles

  
-- $(makeGetsWithOptions "Matt Eads:" "templates/articles" [e|(\x -> do defaultLayout $ x)|])
$(makeGets)

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Matt Eads"
        $(makeContents)
        $(makePreviewsWithOptions "templates/articles/")
        

getAboutR :: Handler Html
getAboutR = defaultLayout $
              do {
                setTitle "Matt Eads: About";
                $(widgetFile "about");
              }
```

