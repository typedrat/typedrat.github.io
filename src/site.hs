--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (filterM)
import           Data.List (isSuffixOf)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mappend, (<>))
import           Hakyll
import qualified Text.Pandoc as P
import           Text.Read (readMaybe)
import           Typedrat.Hakyll.MinifyCss
import           Typedrat.Hakyll.MinifyJs
import           System.FilePath (splitPath)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
        match "static/css/*" $ compile getResourceBody
        match "static/fonts/**" $ do
            route idRoute
            compile copyFileCompiler
        match "static/img/*" $ do
            route idRoute
            compile copyFileCompiler
        match "static/js/*" $ compile getResourceBody
        match "static/highlight.pack.js" $ do
            route $ constRoute "static/highlight.js"
            compile copyFileCompiler

        create ["static/all.js"] $ do
            route idRoute
            compile $ minifyJsCompiler "static/js/*"
        
        create ["static/all.css"] $ do
            route idRoute
            compile $ minifyCssCompiler "static/css/*"

        match "posts/*" $ do
            let 
                markdownOpts = P.def
                    { P.readerExtensions = P.enableExtension P.Ext_literate_haskell P.pandocExtensions
                    }
                htmlOptions = P.def
                    { P.writerHTMLMathMethod = P.MathJax ""
                    , P.writerHighlightStyle = Nothing
                    }

            route . metadataRoute $ \meta ->
                case lookupString "slug" meta of
                    Just slug -> constRoute ("posts/" ++ slug ++ "/index.html")
                    Nothing -> error "Can't render post without slug."
            compile $ pandocCompilerWith markdownOpts htmlOptions
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

        create ["posts/index.html"] $ do
            route idRoute
            compile $ do
                posts <- filterM hideHidden =<< recentFirst =<< loadAll "posts/*.md"
                let archiveCtx =
                        listField "posts" postCtx (return posts)  <>
                        constField "path" "~/posts"               <>
                        constField "cmd" "ls"                     <>
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls
                    >>= cleanIndexUrls

        create ["index.html"] $ do
            route idRoute
            compile $ do
                posts <- filterM hideHidden =<< recentFirst =<< loadAllSnapshots "posts/*.md" "content"
                let indexCtx =
                        listField "posts" postCtx (return (take 3 posts))  <>
                        constField "path" "~"                              <>
                        constField "cmd" "ls -1t posts | head -3"          <>
                        defaultContext

                makeItem ""
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/index.html" indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls
                    >>= cleanIndexUrls

        match "templates/*" $ compile templateBodyCompiler
    where
        hideHidden (Item id' _) = not . (== Just "true") <$> getMetadataField id' "hidden"

        

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    constField "path" "~/posts"  <>
    cmdField                     <>
    defaultContext

    where
        cmdField = field "cmd" $ \(Item id' _) -> do
            meta <- getMetadata id'
            case lookupString "slug" meta of
                Just slug -> return ("cat " ++ slug ++ ".md")
                Nothing -> fail "Can't render post without slug."

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const ""

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "/index.html"
