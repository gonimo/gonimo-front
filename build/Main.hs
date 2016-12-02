{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Development.Shake
import Control.Monad
import System.Directory (createDirectory)
--import Development.Shake.FilePath
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.MD5 as MD5
import System.FilePath (takeFileName, (</>))

data Located a = Located FilePath a
  deriving (Eq, Ord, Show)

main :: IO ()
main = shakeArgs shakeOptions build


build :: Rules ()
build = do
  {-liftIO $ removeFiles "dist" ["//*"]-}
  want $ map ("dist" </>) ["index.txt"]
  action $ do
    need ["support" </> "prod.index.html"]
    statics <- getDirectoryFiles "static" ["//*"]
    staticMD5 <- forM statics $ \p -> do
       p' <- liftIO $ B.readFile $ "static" </> p
       return (takeFileName p,"static" </> p, md5 p')
    b <- doesDirectoryExist "dist"
    unless b $ liftIO $ createDirectory "dist"
    writeFile' "dist/index.txt" (unlines $ map show staticMD5)


