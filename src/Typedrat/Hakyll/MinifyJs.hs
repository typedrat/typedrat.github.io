module Typedrat.Hakyll.MinifyJs (minifyJsCompiler) where

import Language.JavaScript.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.Minify (minifyJS)

import Hakyll.Core.Compiler ( Compiler, loadAll, makeItem )
import Hakyll.Core.Identifier ( toFilePath )
import Hakyll.Core.Identifier.Pattern ( Pattern )
import Hakyll.Core.Item ( Item(..) )

minifyJsCompiler :: Pattern -> Compiler (Item String)
minifyJsCompiler pat = do
        jsFiles <- loadAll pat
        minifiedFiles <- mapM minify jsFiles
        makeItem $ foldr (\(Item _ a) b -> b ++ a) "" minifiedFiles
    where
        minify (Item id' body) = either fail (return . Item id' . renderToString . minifyJS) $ parse body (toFilePath id')
