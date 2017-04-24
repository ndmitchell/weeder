{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Stack(Stack(..), parseStack) where

import Data.Yaml
import Data.List.Extra
import Control.Exception
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import System.Process


data Stack = Stack
    {stackPackages :: [FilePath]
    ,stackDistDir :: FilePath
    }

-- | Note that in addition to parsing the stack.yaml file it also runs @stack@ to
--   compute the dist-dir.
parseStack :: FilePath -> IO Stack
parseStack file = do
    stackDistDir <- fst . line1 <$> readCreateProcess (proc "stack" ["path","--dist-dir","--stack-yaml=" ++ file]) ""
    stackPackages <- either throwIO (return . f) =<< decodeFileEither file
    return Stack{..}
    where
        fromObject (Object x) = x
        fromArray (Array xs) = V.toList xs
        fromString (String s) = T.unpack s

        f x = case Map.lookup "packages" $ fromObject x of
            Nothing -> ["."]
            Just xs -> map fromString $ fromArray xs
