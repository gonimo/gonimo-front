{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import           Control.Monad
import           System.Console.GetOpt
import           Control.Category ((>>>))
import qualified Data.ByteString.Lazy.Char8  as B
import           Data.ByteString.Lazy.Char8  (ByteString)
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

data Flags = Prod
  deriving (Eq)

flags :: forall a. [OptDescr (Either a Flags)]
flags = [Option "" ["prod"] (NoArg $ Right Prod) "build for production"]

main :: IO ()
main = shakeArgsWith shakeOptions flags $ \flagVals _ -> build (Prod `elem` flagVals)

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

build :: Bool -> IO (Maybe (Rules ()))
build isProd = return $ Just $ do
  want [appName']
  action $ do
    psFiles <- getDirectoryFiles "" ["src//*"]
    need (psFiles ++ ["bower.json", "package.json"])
    unit (bowerInstall >> bowerPrune)
    unit npmInstall
    fmap sed browserify >>= (if isProd then uglify else return)
                        >>= (liftIO . B.writeFile appName')

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
    liftIO $ B.writeFile ("static" </> outputName') indexHTML

 where replaceCollect str rep tl@(tmplt,l) =
         if null $ indices str tmplt
            then tl
            else (replace (B'.pack "/"<>str) rep tmplt, B'.unpack str:l)

       browserify :: Action ByteString
       browserify = fromStdout <$> cmd "pulp" ("browserify" : ["-O" | isProd])
       sed :: ByteString -> ByteString
       sed = replace (B'.pack "://localhost:8081/") (B.pack "s://b00.gonimo.com/")
       uglify :: ByteString -> Action ByteString
       uglify s = fromStdout <$> cmd (AddPath ["./node_modules/uglify-js/bin/"] []) "uglifyjs" ["-c","--screw-ie8","-m"] (StdinBS s)
       npmInstall, bowerInstall, bowerPrune :: Action ()
       npmInstall = cmd "npm" ["install"]
       bowerInstall = cmd "bower" ["install"]
       bowerPrune = cmd "bower" ["prune"]

removeTLD :: FilePath -> FilePath
removeTLD = joinPath . drop 1 . splitPath
