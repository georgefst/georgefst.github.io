#!/usr/bin/env cabal

{- cabal:
build-depends:
    base,
    blaze-html,
    blaze-markup,
    directory,
    extra,
    filepath,
    mtl,
    pandoc-types,
    pandoc,
    pretty-simple,
    shake,
    text,
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Writer
import Data.Function
import Data.Monoid.Extra
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.Tuple.Extra
import Development.Shake
import System.FilePath
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Text.Pandoc
import Text.Pandoc.Walk

main :: IO ()
main = shakeArgs shakeOpts do
    want [outDir </> "index.html"]

    (outDir <//> stylesheet) %> \p -> do
        copyFileChanged stylesheet p

    (outDir <//> "*" <.> "html") %> \p -> do
        let inFile = inDir </> htmlOutToIn p
        need [inFile, outDir </> stylesheet]
        (contents, localLinks) <- liftIO $ runIOorExplode do
            doc <- readMarkdown def =<< liftIO (T.readFile inFile)
            firstM (writeHtml5 def) . runWriter $
                doc & walkM \case
                    Link attrs inlines (url, target) ->
                        Link attrs inlines . (,target)
                            <$> if takeExtension (T.unpack url) == ".md"
                                then do
                                    let url' = htmlInToOut $ T.unpack url
                                    tell [outDir </> url']
                                    pure $ T.pack url'
                                else
                                    pure url
                    x -> pure x
        need localLinks
        liftIO $ TL.writeFile p . renderHtml $ addDocHead "" contents

shakeOpts :: ShakeOptions
shakeOpts =
    shakeOptions
        { shakeColor = True
        , shakeVerbosity = Verbose
        , shakeThreads = 4
        , shakeLint = Just LintBasic
        }

inDir :: FilePath
inDir = "./content"
outDir :: FilePath
outDir = "./dist"
stylesheet :: FilePath
stylesheet = "style.css"

htmlOutToIn :: FilePath -> FilePath
htmlOutToIn p = takeFileName p -<.> "md"
htmlInToOut :: FilePath -> FilePath
htmlInToOut p = takeFileName p -<.> "html"

addDocHead :: Text -> Html -> Html
addDocHead title body = H.docTypeHtml do
    H.head do
        H.title . H.text $ "George Thomas" <> mwhen (not $ T.null title) " - " <> title
        H.link ! HA.rel "stylesheet" ! HA.href "style.css"
    H.body body
