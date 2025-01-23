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
    tagsoup,
    text,
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Writer
import Data.Foldable
import Data.Function
import Data.Maybe
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
import Text.HTML.TagSoup
import Text.Pandoc
import Text.Pandoc.Walk

main :: IO ()
main = shakeArgs shakeOpts do
    want [outDir </> "index.html"]

    (outDir <//> stylesheet) %> \p -> do
        copyFileChanged stylesheet p

    (outDir </> "monpad.html") %> \_ -> do
        -- TODO this needs to be re-run when the submodule hash changes
        -- for now we must manually run `./Build.hs dist/monpad.html -B`
        command_
            [Cwd "monpad"]
            "./build.sh"
            []
        command_
            []
            "./monpad/dist/monpad"
            [ "dump-html"
            , "--no-ws"
            , "--login"
            , "/dev/null"
            , "--main"
            , outDir </> "monpad.html"
            , "--json"
            , "--layout"
            , "((./monpad/dhall/lib/map-layout.dhall).void ./monpad/dhall/default.dhall)"
                <> " // "
                -- TODO DRY with stylesheet - this is `--blue-light`
                <> "{backgroundColour={red=0.552350,green=0.714858,blue=0.899937,alpha=1.0}}"
            ]

    (outDir <//> "*" <.> "html") %> \p -> do
        let inFile = inDir </> htmlOutToIn p
        need [inFile, outDir </> stylesheet]
        (contents, localLinks) <- liftIO $ runIOorExplode do
            doc <- readMarkdown pandocReaderOpts =<< liftIO (T.readFile inFile)
            firstM (writeHtml5 def) . runWriter $
                doc & walkM \case
                    RawBlock format t -> do
                        tell . map ((outDir </>) . T.unpack) $
                            parseTags t & mapMaybe \case
                                TagOpen _ as -> T.takeWhile (/= '?') . snd <$> find ((== "src") . fst) as
                                _ -> Nothing
                        pure $ RawBlock format t
                    block ->
                        block & walkM \case
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

pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
    def
        { readerExtensions = enableExtension Ext_raw_html $ readerExtensions def
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
