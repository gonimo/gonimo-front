{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8  as B
import qualified Data.ByteString.Char8  as B'
{-import           Data.ByteString.Lazy.Char8  (ByteString)-}
import           Data.ByteString.Lazy.Search
import           Data.ByteString.Search.Substitution
import           Data.Digest.Pure.MD5        as MD5
import           Data.Foldable
import qualified Data.Map                    as M
import           Data.Monoid                 ((<>))
import           Development.Shake
import           System.Directory            (createDirectory)
import           System.FilePath

data Located a = Located FilePath a
  deriving (Eq, Ord, Show)

main :: IO ()
main = shakeArgs shakeOptions build

srcDir, outputDir :: FilePath
srcDir = "support"
outputDir = "dist"
templateName, templateName' :: FilePath
templateName = "prod.index.html"
templateName' = srcDir </> templateName
outputName, outputName' :: FilePath
outputName = "index.html"
outputName' = outputDir </> outputName
appName, appName' :: FilePath
appName = "app.js"
appName' = srcDir </> appName

build :: Rules ()
build = do
  want [srcDir</> "app.js"]
  action $ do psFiles <- getDirectoryFiles "src" ["//*"]
              need psFiles
              unit $ cmd browserify appName'
  want $ map ("static" </>) [outputName']
  action $ do
    support <- getDirectoryFiles "support" ["//*"]
    supportMD5 <-
      fmap M.fromList $
      forM support $ \p -> do
           p' <- liftIO $ B.readFile $ "support" </> p
           return ( B.toStrict $ B.pack p
                  , B.pack $ "/" </> outputDir </> p <> "?" <> show (md5 p'))
    b <- doesDirectoryExist ("static" </> outputDir)
    unless b $ liftIO $ createDirectory ("static" </> outputDir)
    need [templateName']
    template <- liftIO $ B.readFile templateName'
    let (indexHTML,filesToCopy) = M.foldrWithKey replaceCollect (template,[]) supportMD5

    for_ filesToCopy $ \f -> copyFile' (srcDir </> f) ("static" </> outputDir </> f)
    liftIO $ print supportMD5
    liftIO $ B.writeFile ("static"</>outputName') indexHTML

 where replaceCollect str rep tl@(tmplt,l) =
         if null $ indices str tmplt
            then tl
            else (replace ("/"<>str) rep tmplt, B'.unpack str:l)
       browserify :: String
       browserify = "pulp browserify --to"

removeTLD :: FilePath -> FilePath
removeTLD = joinPath . drop 1 . splitPath
