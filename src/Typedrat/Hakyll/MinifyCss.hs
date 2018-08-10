module Typedrat.Hakyll.MinifyCss (minifyCssCompiler) where

-- import Hasmin ( minifyCSS )
import Hakyll.Web.CompressCss ( compressCss )

import Hakyll.Core.Compiler ( Compiler, loadAll, makeItem )
import Hakyll.Core.Identifier.Pattern ( Pattern )
import Hakyll.Core.Item ( Item(..) )

import qualified Data.Text as T

minifyCssCompiler :: Pattern -> Compiler (Item String)
minifyCssCompiler pat = do
        cssFiles <- loadAll pat
        minifiedFiles <- mapM minify cssFiles
        makeItem $ foldr (\(Item _ a) b -> b ++ a) "" minifiedFiles
    where
        -- minify = traverse (either fail (return . T.unpack) . minifyCSS . T.pack)
        minify = traverse (return . compressCss)