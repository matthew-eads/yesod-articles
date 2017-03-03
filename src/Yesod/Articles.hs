{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Articles where
--import Import
import Language.Haskell.TH
import System.Directory
import Text.Hamlet as NP
--import qualified Data.Text.Lazy as TL
import qualified Data.Text as T --(splitOn, length, intercalate, isSuffixOf, pack, append) --as DT (splitOn, length, intercalate) 
import Data.Text (Text, pack, splitOn, intercalate, isSuffixOf, unpack)
import qualified Data.Text.IO as TI
import Data.Dates (DateTime, parseDate, getCurrentDateTime)
import Prelude -- (last, head, lines) 
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Regex
import Data.List (sortBy)
import Yesod.Core.Widget
import Instances.TH.Lift
import Text.Blaze.Html (toHtml)
import Yesod.Core.Handler (getMessageRender, getUrlRenderParams)

{-
 - A word on naming conventions and directories:
 - All articles should live in templates/articles
 - The name of the file will be the id for the post
 - as well as the route name. If no title is given 
 - in the file, the filename serves for the page title
 - as well. Please don't put spaces in your filename.
 -}

-- | Snatched from Yesod.Core.Widget, used in other functions
rules :: Q NP.HamletRules
rules = do
    ah <- [|asWidgetT . toWidget|]
    let helper qg f = do
            x <- newName "urender"
            e <- f $ VarE x
            let e' = LamE [VarP x] e
            g <- qg
            bind <- [|(>>=)|]
            return $ InfixE (Just g) bind (Just e')
    let ur f = do
            let env = NP.Env
                    (Just $ helper [|getUrlRenderParams|])
                    (Just $ helper [|fmap (toHtml .) getMessageRender|])
            f env
    return $ NP.HamletRules ah ur $ \_ b -> return $ ah `AppE` b

-- | Takes a file path/name (ex. templates/articles/foo.hamlet)
-- Looks at the first line, which should contain the date in the form <!--YYYY-MM-DD-->
-- If date is malformed or nonexistent, it returns todays date.
getDate :: Text -> IO DateTime
getDate file = 
    do
      contents <- TI.readFile $ unpack file
      today <- getCurrentDateTime
      return $ 
             let header = getHeader contents
                 rgx = mkRegexWithOpts "<!--([[:digit:]]{4}.[[:digit:]]{2}.[[:digit:]]{2})-->" False True 
                 date_str = case matchRegex rgx $ unpack header of
                              Just matches -> head matches
                              Nothing -> "1970-01-01"
                 date_str' = map (\x -> if x == '-' then '/' else x) date_str -- parseDate wants dates seperated by / not -
                 date = case Data.Dates.parseDate today date_str' of
                          Right d -> d
                          _ -> today
             in date

-- | Recursively fetches dates for a given list of file paths
getDates :: [Text] -> IO [DateTime]
getDates [] = return []
getDates (x:xs) = 
    do 
      date <- getDate x
      dates <- getDates xs
      return (date:dates)
      
-- | Fetch the paths for all the .hamlet files in the templates/articles/ dir.
-- Returns the articles as a list of strings, sorted by date, newest->oldest,
-- based on the dates from getDate(s). 
-- File names returned are of the form "templates/articles/foo.hamlet"
getArticleNames :: IO [Text]
getArticleNames = 
    do {
      files <- getDirectoryContents "templates/articles/";
      let hamlet_files = filter (isSuffixOf ".hamlet") (map pack files)
          file_names_2 = map (\fname -> T.append "templates/articles/" fname) hamlet_files
      in do {
           dates <- getDates file_names_2;
           return (map fst $ sortBy (\(x,y) -> \(x', y') -> compare y' y) $ zip file_names_2 dates);
         }
      }

-- | Takes a file path (templates/articles/foo.hamlet), and returns
-- a string of just the file name without the extension (foo).
stripName :: Text -> Text
stripName file_name =
    let n = T.length $ pack "templates/articles/"
        m = T.length $ pack ".hamlet"
        article' = T.drop n file_name
        article = T.take (T.length article' - m) article'
    in article

-- | Takes a file path (templates/articles/foo.hamlet), and returns
-- a route name for it (fooR). Undefined behaviour if file names not fetched from
-- getArticleNames (or if not prefixed by templates/articles/ and suffixed by .hamlet)
makeRouteName :: Text -> Text
makeRouteName file_name = T.concat [stripName file_name, "R"]

-- | Generates the Read More... link for an article. 
readMore :: Text -> Text
readMore fp = T.concat ["<p><a href=@{ArticleR \"", stripName fp, "\"}>Read More...</a>\n"]
                  --read_more = concat ["<p><a href=@{", makeRouteName fp, "}>Read More...</a>\n"]

contentsLink :: Text -> Text
contentsLink fp = T.concat ["<div syle=\"display:none\"  #", stripName fp, ">\n"]

-- | See https://hackage.haskell.org/package/shakespeare-2.0.12.1/docs/src/Text-Hamlet.html#hamletFileWithSettings
-- This replaces hamletFileWithSettings, instead of just passing the full contents to hamletFromString,
-- We truncate the contents (currently just up to not including the third <p> tag
-- and append the 'Read More...' link.
getPreview :: Q HamletRules -> HamletSettings -> FilePath -> Q Exp
getPreview qhr set fp = do
-- #ifdef GHC_7_4
--    qAddDependentFile fp
-- #endif
    contents <- fmap T.unpack $ runIO $ TI.readFile fp
    -- hear we can truncate the contents
    let split_contents = splitOn "<p>" $ pack contents
    let take_n = if Prelude.length split_contents > 3 then 3 else Prelude.length split_contents
    let preview = intercalate "<p>" $ take take_n split_contents
    let read_more = readMore $ pack fp
    let contents_link = contentsLink $ pack fp
    hamletFromString qhr set $ unpack $ intercalate "\n" [contents_link, preview, read_more]


-- | This can be used to make the previews for all the articles
-- It fetches the articles to display, and for each one calls our modified
-- hamletFileWithSettings (getPreview) 
makePreviews :: Q Exp
makePreviews = 
    do 
      articles <- runIO getArticleNames
      xs <- mapM (\x -> getPreview rules NP.defaultHamletSettings $ unpack x) articles
      return $ DoE (map NoBindS xs)
          
      
-- | Given a stripped article names (eg 'civ6', 'foo'), makeGet
-- will create the functions which 'gets' that article. It names the function
-- by prefixing the article name with 'get' and suffixing with 'R' (eg 'foo' -> 'getfooR')
-- The function returned calls defaultLayout, sets the title based on the 
-- given prefix and the article name, and calls widgetFile on the article.
-- The function returned effectively does:
-- defaultLayout $ do {
--   $(setTitle prefix ++ article_name)
--   $(widgetFile articles/article_name)}
makeGet :: Text -> Text -> Q Dec
--makeGet [] _ = return []
makeGet prefix article_name = 
    let article_name' = unpack article_name
        prefix' = unpack prefix
        fname = mkName $ concat ["get", article_name', "R"]
    in 
      do {
        widget <- whamletFile $ concat ["templates/articles/", article_name', ".hamlet"];
        --dl <- [e|defaultLayout|];
        title <- runIO $ getTitle (concat ["templates/articles/", article_name', ".hamlet"]);
        set_title <- [e| setTitle $ toHtml (prefix' ++ " " ++ (unpack title)) |];
        --decs <- makeGet rest;
        return (ValD (VarP fname) 
            (NormalB (AppE (VarE (mkName "defaultLayout"))  (ParensE
                      (DoE [NoBindS set_title,
                            NoBindS widget])))) []);
      }

-- | Gets the list of articles in templates/articles, makes all
-- the 'getter' functions for the articles (see makeGet), and creates
-- the 'getArticleR' function (see makeGetArticle).
makeGets :: Text -> Q [Dec]
makeGets prefix =
    do
      articles <- runIO getArticleNames
      let articles' = map stripName articles
      decs <- mapM (makeGet prefix) articles'
      getArticle <- makeGetArticle articles'
      return (decs ++ getArticle)

-- | Given a list of articles, returns the 'getArticleR' function.
-- The function returned will take the article_name as an arg, and
-- defer to the appropriate get<article_name>R function. This relies
-- on the makeGet function being already run on the same articles given to 
-- it. This is the handler for the /article/#Text route. 
makeGetArticle :: [Text] -> Q [Dec]
makeGetArticle articles  = 
    let fname = mkName "getArticleR"
        arg = mkName "article_name"
        article_cases = map (\article -> Match (LitP (StringL $ unpack article)) 
                                          (NormalB (VarE $ mkName $ concat ["get", unpack article, "R"]))[]) articles
    in
    do {
      --notfound <- [e|notFound|];
      return [FunD fname [Clause [VarP arg]
               (NormalB (CaseE (VarE arg)
               (article_cases ++ [Match WildP (NormalB (VarE $ mkName "notFound")) []])))[]]] 
                        
    }

-- | Given the contents of an article, returns the 'article header'
-- that is the contents of the file up to the first html tag (<p>, <h3>, etc.)
getHeader :: Text -> Text
getHeader contents = 
    let rgx = mkRegexWithOpts "(.*)<[a-zA-Z]" False True 
    in case matchRegex rgx $ unpack contents of
         (Just matches) -> pack $ head matches
         Nothing -> ""

-- | Given a file path, returns the page title for the article.
-- The title should be a html comment (<!--title-->) and occur before
-- any other text, other than the date comment. The date and title can
-- be put in either order. If no title can be found, the stripped file
-- path is used.
getTitle :: String -> IO Text
getTitle fp = 
    do
      contents <- TI.readFile fp
      return $ 
             let header = getHeader contents
                 rgx = mkRegexWithOpts "<!--[[:digit:]]{4}.[[:digit:]]{2}.[[:digit:]]{2}-->" False True 
                 non_match = case matchRegexAll rgx $ unpack header of 
                               Nothing -> ""
                               Just (before, _, after, _) -> concat [before, after]
                 rgx2 = mkRegexWithOpts "<!--(.*)-->" False True 
             in case matchRegex rgx2 non_match of
                  (Just matches) -> pack $ head matches
                  Nothing -> stripName $ pack fp


makeContentsEntry :: (Text, Text) -> Text
makeContentsEntry (id, title) = T.concat ["<li><a href=\"#", id, "\">", title, "</a>"]

-- | Makes the contents for the page of previews. 
makeContents :: Q Exp
makeContents = 
    do 
      articles <- runIO getArticleNames
      titles <- runIO $ mapM (\x -> getTitle $ unpack x) articles
      let articles' = zip (map stripName articles) titles
      let entries = map makeContentsEntry articles'
      ham <- hamletFromString rules NP.defaultHamletSettings $ 
               unpack $ intercalate "\n    " (["<div #contents>\n  <ul>"] ++ entries)
      return $ DoE [NoBindS ham]

