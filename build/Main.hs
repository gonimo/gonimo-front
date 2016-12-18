{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import qualified Data.ByteString.Char8               as B'
import qualified Data.ByteString.Lazy.Char8          as B
import           System.Console.GetOpt
{-import           Data.ByteString.Lazy.Char8  (ByteString)-}
import qualified Data.ByteString.Lazy.Search         as B
import           Data.Conduit                        ((=$=))
import qualified Data.Conduit                        as C
import qualified Data.Conduit.Binary                 as C
import qualified Data.Conduit.List                   as C
import           Data.Digest.Pure.MD5                as MD5
import qualified Data.Map                            as M
import           Data.Monoid                         ((<>))
import           Development.Shake
import           System.Directory                    (createDirectoryIfMissing)
import           System.FilePath

data Located a = Located FilePath a
  deriving (Eq, Ord, Show)

data Flags = Prod
  deriving (Eq)

flags :: forall a. [OptDescr (Either a Flags)]
flags = [Option "" ["prod"] (NoArg $ Right Prod) "build for production"]

main :: IO ()
main = shakeArgsWith shakeOptions flags $ \flagVals _ -> build (Prod `elem` flagVals)

srcDir, outputDir, outputDir':: FilePath
srcDir = "support"
outputDir = "dist"
outputDir' = "static" </> outputDir

templateName, templateName' :: FilePath
templateName = "prod.index.html"
templateName' = srcDir </> templateName

outputName, outputName' :: FilePath
outputName = "index.html"
outputName' = outputDir' </> outputName

appName, appName' :: FilePath
appName = "app.js"
appName' = outputDir' </> appName

blacklist :: [String]
-- | list of extensions
blacklist = [ ".jpg", ".jpeg", ".png", ".gif"
            , ".mp3", ".wav", ".ogg"
            , ".otf", ".woff", ".woff2", ".ttf", ".eot"
            ]

build :: Bool -> IO (Maybe (Rules ()))
build isProd = return $ Just $ do
  want [appName', outputName']                                                                -- step 1 - generate app.js to static/dist/
  action $ do
    liftIO $ createDirectoryIfMissing True outputDir'
    psFiles <- getDirectoryFiles "" ["src//*"]
    need (psFiles ++ ["bower.json", "package.json"])
    unit (bowerInstall >> bowerPrune)
    unit npmInstall
    fmap sed browserify >>= uglify >>= (liftIO . B.writeFile appName')

    support <- getDirectoryFiles "" ["support//*"]
    mapM_ (\f -> safeCopyFile' f (outputDir' </> removeTLD f)) support
    distFiles <- getDirectoryFiles "" ["static/dist//*"]
    need distFiles
                                                                                 -- step 3 - generate md5sums for all files

    md5Sums :: M.Map B'.ByteString B'.ByteString <-
      fmap M.fromList $
        forM distFiles $ \filePath -> do
           fileContent <- liftIO $ B.readFile filePath
           return ( B'.pack $ takeFileName filePath
                  , B'.pack $ removeTLD $ filePath <> "?" <> show (md5 fileContent))
    let md5Files = filter ((`notElem` blacklist) . takeExtensions) distFiles     -- step 4 - replace links with link?md5um
    forM_ md5Files $ \fileName ->
      let replaceContents content = M.foldrWithKey B.replace (B.fromStrict content) md5Sums
       in liftIO $  C.runConduitRes
                 $  C.sourceFile fileName
                =$= C.map (B.toStrict . replaceContents)
                =$= C.sinkFileCautious fileName

 where safeCopyFile' :: FilePath -> FilePath -> Action ()
       safeCopyFile' from to = do liftIO $ createDirectoryIfMissing True (takeDirectory to)
                                  copyFile' from to

       browserify :: Action B.ByteString
       browserify = fromStdout <$> cmd "pulp" ("browserify" : ["-O" | isProd])
       sed :: B.ByteString -> B.ByteString
       sed = if isProd
                then B.replace (B'.pack "://localhost:8081/") (B.pack "s://b00.gonimo.com/")
                else id
       uglify :: B.ByteString -> Action B.ByteString
       uglify s = if isProd
                     then fromStdout <$> cmd (AddPath ["./node_modules/uglify-js/bin/"] []) "uglifyjs" ["-c","--screw-ie8","-m"] (StdinBS s)
                     else return s
       npmInstall, bowerInstall, bowerPrune :: Action ()
       npmInstall = cmd "npm" ["install"]
       bowerInstall = cmd "bower" ["install"]
       bowerPrune = cmd "bower" ["prune"]

removeTLD :: FilePath -> FilePath
removeTLD = joinPath . drop 1 . splitPath
