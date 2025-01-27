#!/usr/bin/env cabal

{- cabal:
build-depends:
    base,
    blaze-html,
    blaze-markup,
    containers,
    directory,
    extra,
    filepath,
    mtl,
    neat-interpolation,
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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid.Extra
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.Tuple.Extra
import Development.Shake
import NeatInterpolation
import System.Directory
import System.Exit
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
    want [rootHtml]

    "release" ~> do
        need [rootHtml]
        Stdout (originalBranch :: String) <- cmd ("git branch --show-current" :: String)
        liftIO $ putStrLn originalBranch
        either (\e -> liftIO $ putStrLn $ "git command failed: " <> show e) pure =<< runExceptT do
            let gitCmd c =
                    lift (cmd @(String -> String -> Action ExitCode) "git" c) >>= \case
                        ExitSuccess -> pure ()
                        ExitFailure e' -> throwError (c, e')
            gitCmd "switch release"
            gitCmd "merge master --no-edit"
            liftIO do
                removeDirectoryRecursive "docs"
                cmd_ ("cp -r dist docs" :: String) -- TODO find a Haskell implementation of this
                writeFile "docs/CNAME" "georgefst.com"
            gitCmd "add docs"
            gitCmd "commit -m Release"
            gitCmd "push github.io HEAD:georgefst.com"
        cmd_ $ "git switch " <> originalBranch

    (outDir </> stylesheet) %> \p -> do
        copyFileChanged stylesheet p

    (outDir </> "monpad.html") %> \_ -> do
        -- TODO this needs to be re-run when the submodule hash changes
        -- for now we must manually run `./Build.hs dist/monpad.html -B`
        command_
            [Cwd "monpad"]
            "./build.sh"
            []
        need $ Map.keys monpadLayouts <&> \layout -> monpadLayoutDir </> layout <.> "dhall"
        command_ [] "./monpad/dist/monpad" $
            [ "dump-html"
            , "--no-ws"
            , "--login"
            , "/dev/null"
            , "--main"
            , outDir </> "monpad.html"
            , "--json"
            ]
                <> concatMap (\layout -> ["--layout", layout.dhall]) monpadLayouts

    (monpadLayoutDir </> "*") %> \p ->
        let layout = fromMaybe (error $ "unknown Monpad layout: " <> p) $ Map.lookup (takeBaseName p) monpadLayouts
         in copyFileChanged ("./monpad/dhall/" <> layout.path <> ".dhall") p

    (outDir <//> "index.html") %> \p -> do
        let inFile = inDir </> htmlOutToIn (joinPath . drop 1 $ splitPath p)
        need [inFile, outDir </> stylesheet]
        (contents, localLinks) <- liftIO $ runIOorExplode do
            doc <- readMarkdown pandocReaderOpts =<< liftIO (T.readFile inFile)
            firstM (writeHtml5 def) . runWriter $
                doc & walkM \case
                    RawBlock format t -> do
                        tell $
                            parseTags t & mapMaybe \case
                                TagOpen _ as ->
                                    find ((== "src") . fst) as <&> \(_, src) ->
                                        outDir </> T.unpack (T.dropWhile (== '/') $ T.takeWhile (/= '?') src)
                                _ -> Nothing
                        pure $ RawBlock format t
                    block ->
                        block & walkM \case
                            Link attrs inlines (url@(T.unpack -> url'), target) ->
                                Link attrs inlines . (,target)
                                    <$> if takeExtension (T.unpack url) == ".md"
                                        then do
                                            tell [outDir </> htmlInToOut url']
                                            pure $ T.pack $ htmlInToOut' url'
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

data MonpadLayout = MonpadLayout
    { path :: String
    , dhall :: String
    }
monpadLayouts :: Map String MonpadLayout
monpadLayouts =
    Map.fromList $
        [ dupe "default"
        , dupe "full"
        , ("mouse", "mouse-landscape")
        ]
            <&> \(name, path) ->
                ( name
                , MonpadLayout
                    { path
                    , dhall =
                        T.unpack
                            let p = T.pack path
                                n = T.pack name
                             in [text|
                                    ((./monpad/dhall/lib/map-layout.dhall).void ./monpad/dhall/$p.dhall)
                                    -- TODO DRY with stylesheet - this is `--blue-light`
                                    // {backgroundColour={red=0.552350,green=0.714858,blue=0.899937,alpha=1.0}}
                                    // {name="$n"}
                                |]
                    }
                )

inDir :: FilePath
inDir = "./content"
outDir :: FilePath
outDir = "./dist"
rootHtml :: FilePath
rootHtml = outDir </> "index.html"
stylesheet :: FilePath
stylesheet = "style.css"
monpadLayoutDir :: FilePath
monpadLayoutDir = outDir </> "portfolio/monpad/layouts"

htmlOutToIn :: FilePath -> FilePath
htmlOutToIn p = case reverse $ splitDirectories p of
    "index.html" : rest0 -> joinPath (reverse rest) </> dir <.> "md"
      where
        (dir, rest) = case rest0 of
            [] -> root
            ["."] -> root
            dir' : rest' -> (dir', rest')
          where
            root = ("index", rest0)
    _ -> error $ "bad HTML output path: " <> p
htmlInToOut :: FilePath -> FilePath
htmlInToOut p = htmlInToOut' p </> "index.html"
htmlInToOut' :: FilePath -> FilePath
htmlInToOut' p = case splitFileName p of
    ("./", "index.md") -> "./"
    (dir, file) -> dir </> dropExtension file

addDocHead :: Text -> Html -> Html
addDocHead title body = H.docTypeHtml do
    H.head do
        H.title . H.text $ "George Thomas" <> mwhen (not $ T.null title) " - " <> title
        H.link ! HA.rel "stylesheet" ! HA.href "/style.css"
    H.body body
