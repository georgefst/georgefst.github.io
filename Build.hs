#!/usr/bin/env cabal

{- cabal:
build-depends:
    base,
    binary,
    blaze-html,
    blaze-markup,
    clay,
    colour,
    containers,
    deepseq,
    directory,
    extra,
    filepath,
    hashable,
    mtl,
    neat-interpolation,
    pandoc-types,
    pandoc,
    pretty-simple,
    shake,
    tagsoup,
    text,
    time,
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Clay qualified
import Clay.Pseudo qualified
import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Binary
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid.Extra
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.Time
import Data.Traversable
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
import Text.Pandoc hiding (getCurrentTime, getCurrentTimeZone)
import Text.Pandoc.Walk

main :: IO ()
main = shakeArgs shakeOpts do
    mediaFiles <- liftIO $ map (mediaDir </>) <$> listDirectory mediaDir
    (outDir </> mediaDir </> "*") *%> \dest (f :! EmptyList) -> do
        liftIO $ createDirectoryIfMissing True $ outDir </> mediaDir
        copyFileChanged (mediaDir </> f) dest

    let wanted = rootHtml : map (outDir </>) mediaFiles
    want wanted

    getSubmoduleState <- addSubmoduleOracle

    -- TODO requires one-time setup on each dev machine:
    -- git remote add github.io git@github.com:georgefst/georgefst.github.io.git
    -- git branch --no-track release github.io/georgefst.com
    "release" ~> do
        alwaysRerun
        liftIO $ removeDirectoryRecursive outDir
        need wanted
        Stdout originalBranch <- cmd ("git branch --show-current" :: String)
        liftIO $ putStrLn originalBranch
        either (\e -> liftIO $ putStrLn $ "git command failed: " <> show e) pure =<< runExceptT do
            let gitCmd c =
                    lift (cmd @(String -> String -> _) "git" c) >>= \case
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

    (outDir </> stylesheet) %> copyFileChanged stylesheet

    (outDir </> stylesheetClay) %> \p -> liftIO $ TL.writeFile p $ Clay.render do
        Clay.star Clay.# Clay.Pseudo.root Clay.? do
            -- TODO add support for CSS custom properties (a.k.a. variables) to Clay
            "--blue-dark" Clay.-: T.pack (sRGB24show blueDark)
            "--blue-medium" Clay.-: T.pack (sRGB24show blueMedium)
            "--blue-light" Clay.-: T.pack (sRGB24show blueLight)

    (outDir </> profilePic) %> \p -> do
        need [profilePic]
        magick
            ( mconcat
                [ ["-crop", "1024x1024+320+56"]
                , ["-resize", "512x512"]
                , ["-quality", "90"]
                , ["(", "+clone", "-alpha", "extract", "-draw", "circle 255,255 0,256", "-negate", ")"]
                , ["-alpha", "off"]
                , ["-compose", "CopyOpacity"]
                , ["-composite"]
                ]
            )
            profilePic
            p

    (outDir </> favicon) %> \p -> do
        let source = outDir </> profilePic
        need [source]
        magick ["-resize", "16x16"] source p

    (outDir </> "monpad.html") %> \_ -> do
        _ <- getSubmoduleState $ Submodule "monpad"
        -- TODO we may wish to return to calling `./build.sh` once that works on NixOS
        command_
            [Cwd "monpad"]
            "nix"
            ["build", ".#monpad:exe:monpad"]
        need $ Map.keys monpadLayouts <&> \layout -> monpadLayoutDir </> layout <.> "dhall"
        command_ [] "./monpad/result/bin/monpad" $
            [ "dump-html"
            , "--no-ws"
            , "--login"
            , "/dev/null"
            , "--main"
            , outDir </> "monpad.html"
            , "--json"
            ]
                <> concatMap (\layout -> ["--layout", layout.dhall]) monpadLayouts

    (monpadLayoutDir </> "*" <.> "dhall") %> \p ->
        let layout = fromMaybe (error $ "unknown Monpad layout: " <> p) $ Map.lookup (takeBaseName p) monpadLayouts
         in copyFileChanged ("./monpad/dhall/" <> layout.path <.> "dhall") p

    pandocStuff <- newCache \inFile -> liftIO $ runIOorExplode do
        doc <- readMarkdown pandocReaderOpts =<< liftIO (T.readFile inFile)
        let title = case doc of
                Pandoc _ (Header 1 _ h : _) ->
                    mconcat $
                        h & mapMaybe \case
                            Str s -> Just s
                            Space -> Just " "
                            _ -> Nothing
                _ -> error $ "no top-level header in input: " <> inFile
        let (content, localLinks) =
                runWriter $
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
                                                -- TODO prepending slash won't be enough for nested paths,
                                                -- but it fixes things for bits-and-bobs, which is the only use for now
                                                pure $ T.pack $ '/' : htmlInToOut' url'
                                            else
                                                pure url
                                x -> pure x
        contentHtml <- writeHtml5 def content
        pure (title, contentHtml, localLinks)

    (outDir <//> "index.html") *%> \p (pc :! EmptyList) -> do
        need [outDir </> favicon]
        (title, contents) <- case pc of
            "" -> pure ("", Nothing)
            "posts/" -> do
                posts <- getDirectoryFiles inDir ["posts" </> "*" <.> "md"]
                draftPosts <- getDirectoryFiles inDir ["posts" </> "drafts" </> "*" <.> "md"]
                need $ posts <&> \w -> outDir </> htmlInToOut w
                need $ draftPosts <&> \w -> outDir </> htmlInToOut w
                posts' <- for posts \post ->
                    (post,,)
                        <$> pandocStuff (inDir </> post)
                        <*> ( maybe "unknown date" (formatTime defaultTimeLocale "%d/%m/%Y" . localDay)
                                . runReadS
                                . readSTime True defaultTimeLocale "%a %b %e %H:%M:%S %Y %z"
                                . fromStdout
                                <$> command [] "git" ["log", "--diff-filter=A", "--format=%ad", "--", inDir </> post]
                            )
                draftPosts' <- for draftPosts \post -> (post,) <$> pandocStuff (inDir </> post)
                pure . ("Blog",) $ Just do
                    H.h1 "Blog"
                    (H.ul ! HA.id "blog-links") do
                        for_ posts' \(post, (title, _, _), date) -> H.li do
                            H.a (H.text title) ! HA.href (H.stringValue $ "/" </> htmlInToOut' post)
                            H.span $ H.string date
                    when (not $ null draftPosts') $ H.h2 "Drafts"
                    (H.ul ! HA.id "blog-links-draft") do
                        for_ draftPosts' \(post, (title, _, _)) ->
                            H.li $ H.a (H.text title) ! HA.href (H.stringValue $ "/" </> htmlInToOut' post)
            _ -> do
                let inFile = inDir </> htmlOutToIn (pc </> "index.html")
                need [inFile]
                (title, content, localLinks) <- pandocStuff inFile
                need localLinks
                pure (title, Just content)
        let noDep p' =
                -- TODO this is a bit of a hack
                -- we can't make every page that uses the sidebar depend on all of its link targets
                -- as that would cause dependency cycles
                -- so instead we register the dependency only for the root
                -- this means that we _can_ end up with pages that are reachable but outdated
                -- but only in the rare event that we specifically request just a single non-root page from the command line
                -- we'll likely revisit this at some point if we end up with mutually-linked markdown sources
                -- which should be fine in principle, but will hit the same problem
                -- the answer _might_ just be to stop being clever and always compile all HTML files
                not (null pc)
                    || null p' -- avoids trivial recursion
        liftIO . TL.writeFile p . renderHtml =<< addDocHead title =<< addCommonHtml noDep ((,T.pack pc) <$> contents)

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
        { readerExtensions =
            foldr
                enableExtension
                (readerExtensions def)
                [ Ext_raw_html
                , Ext_footnotes
                , Ext_inline_notes
                ]
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
                                c = toSRGB blueLight
                                r = T.pack $ show $ channelRed c
                                g = T.pack $ show $ channelGreen c
                                b = T.pack $ show $ channelBlue c
                             in [text|
                                    ((./monpad/dhall/lib/map-layout.dhall).void ./monpad/dhall/$p.dhall)
                                    // {backgroundColour={red=$r,green=$g,blue=$b,alpha=1.0}}
                                    // {name="$n"}
                                |]
                    }
                )

inDir :: FilePath
inDir = "./content"
outDir :: FilePath
outDir = "./dist"
mediaDir :: FilePath
mediaDir = "media"
rootHtml :: FilePath
rootHtml = outDir </> "index.html"
stylesheet :: FilePath
stylesheet = "style.css"
stylesheetClay :: FilePath
stylesheetClay = "generated.css"
profilePic :: FilePath
profilePic = "me.avif"
favicon :: FilePath
favicon = "favicon.ico"
monpadLayoutDir :: FilePath
monpadLayoutDir = outDir </> "portfolio/monpad/layouts"

blueDark, blueMedium, blueLight :: Colour Double
blueDark = sRGB24read "#162745"
(blueMedium, blueLight) =
    both
        (flip lighten blueDark)
        -- N.B. `0 / 3` would be `blueDark`, and `3 / 3` pure white
        ( 1 / 3
        , 2 / 3
        )

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

addDocHead :: Text -> Html -> Action Html
addDocHead title body = do
    need $ map (outDir </>) stylesheets
    pure $ H.docTypeHtml do
        H.head do
            H.meta ! HA.charset "UTF-8"
            H.title . H.text $ title <> mwhen (not $ T.null title) " | " <> "George Thomas"
            for_ stylesheets \s -> H.link ! HA.rel "stylesheet" ! HA.href (H.stringValue $ "/" </> s)
        H.body body
  where
    stylesheets = [stylesheet, stylesheetClay]

addCommonHtml :: (FilePath -> Bool) -> Maybe (Html, Text) -> Action Html
addCommonHtml noDep body = do
    need [outDir </> profilePic]
    need $ links & mapMaybe \(p, _) -> guard (not $ noDep p) $> (outDir </> p </> "index.html")
    pure do
        (H.div ! HA.id "under-construction") $ H.text "Under construction!"
        (H.div ! HA.id "sidebar") do
            H.a (H.img ! HA.src (H.stringValue $ "/" </> profilePic)) ! HA.href "/" ! HA.id "home-image"
            for_ links \(p, t) ->
                H.a (H.string t) ! HA.href (H.stringValue ("/" <> p)) ! HA.class_ "button-link"
        body & foldMap \(b, t) -> H.div (H.div b) ! HA.id "content" ! HA.class_ (H.textValue $ T.dropWhileEnd (== '/') t)
  where
    links =
        [ ("posts", "Blog")
        , ("portfolio", "Portfolio")
        , ("misc", "Misc")
        ]

-- TODO do this in Haskell, e.g. with `JuicyPixels-extra`?
magick :: [String] -> FilePath -> FilePath -> Action ()
magick c i o = command_ [] "magick" ([i] <> c <> [o])

-- allows for ad-hoc partial list patterns without triggering a warning
-- TODO replace this whenever GHC allows per-line disabling of warnings
{-# COMPLETE (:!) #-}
infixr 5 :!
pattern (:!) :: a -> [a] -> [a]
pattern (:!) x y = x : y
{-# COMPLETE EmptyList #-}
pattern EmptyList :: [a]
pattern EmptyList = []

-- TODO upstream to Shake
-- this is essentially what was requested by OP in https://github.com/ndmitchell/shake/issues/499
(*%>) :: FilePattern -> (FilePath -> [String] -> Action ()) -> Rules ()
p *%> f =
    p %> \x ->
        f x
            . fromMaybe (error "internal: failed to re-match pattern")
            $ filePattern p x

-- TODO upstream these, or use as the basis of a library, maybe with optics
adjustHSL ::
    (Double -> Double) ->
    (Double -> Double) ->
    (Double -> Double) ->
    Colour Double ->
    Colour Double
adjustHSL fh fs fl c = let (h, s, l) = hslView $ toSRGB c in uncurryRGB sRGB $ hsl (fh h) (fs s) (fl l)
adjustLightness :: (Double -> Double) -> Colour Double -> Colour Double
adjustLightness = adjustHSL id id
lighten :: Double -> Colour Double -> Colour Double
lighten x = adjustLightness (\l -> l + (1 - l) * x)

-- TODO turn this in to a library?
newtype Submodule = Submodule FilePath deriving newtype (Eq, Ord, Show, NFData, Hashable, Binary)
type instance RuleResult Submodule = (String, String)
addSubmoduleOracle :: Rules (Submodule -> Action (String, String))
addSubmoduleOracle = addOracle $ \(Submodule p) ->
    quietly $
        curry (both fromStdout)
            <$> command [Cwd p] "git" ["rev-parse", "HEAD"]
            <*> command [Cwd p] "git" ["diff"]

runReadS :: [(a, b)] -> Maybe a
runReadS = \case
    [(r, _)] -> Just r
    _ -> Nothing
