{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Stack(Stack(..), parseStack, buildStack) where

import Data.Yaml
import Data.List.Extra
import Control.Exception
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import System.Process
import Data.Functor
import Prelude


data Stack = Stack
    {stackPackages :: [FilePath]
    ,stackDistDir :: FilePath
    }

buildStack :: FilePath -> IO ()
buildStack file = callProcess "stack" ["build","--stack-yaml=" ++ file,"--test","--bench","--no-run-tests","--no-run-benchmarks"]

-- | Note that in addition to parsing the stack.yaml file it also runs @stack@ to
--   compute the dist-dir.
parseStack :: FilePath -> IO Stack
parseStack file = do
    stackDistDir <- fst . line1 <$> readCreateProcess (proc "stack" ["path","--dist-dir","--stack-yaml=" ++ file]) ""
    stackPackages <- f . decodeYaml <$> readCreateProcess (proc "stack" ["query","locals","--stack-yaml=" ++ file]) ""
    return Stack{..}
    where
        decodeYaml = either throw id . decodeEither' . BS.pack
        fromObject (Object x) = x
        fromString (String s) = T.unpack s
        f = map (fromString . (Map.! "path") . fromObject) . Map.elems . fromObject
