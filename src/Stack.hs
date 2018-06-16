{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Stack(Stack(..), findStack, parseStack, buildStack) where

import Data.Yaml
import Data.List.Extra
import Control.Exception
import Control.Monad.Extra
import System.Directory.Extra
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import Util
import Data.Functor
import Prelude


data Stack = Stack
    {stackPackages :: [FilePath]
    ,stackDistDir :: FilePath
    }

findStack :: FilePath -> IO FilePath
findStack dir = withCurrentDirectory dir $ do
    let args = ["path","--config-location","--color=never","--no-install-ghc"]
    let compute = cmdStdout "stack" args

    let parse = trim . fst . line1

    res <- compute
    -- rerun if dodgy, since the first time it might be secretly doing a stack setup
    -- which leaves lots of garbage on the stdout (so just have another go)
    -- res <- if length (lines res) == 1 then return res else compute
    when (parse res == "") $
        fail $ "Failed to find stack.yaml file\nCommand: " ++ unwords ("stack":args) ++ "\nOutput: " ++ res
    return $ parse res

buildStack :: FilePath -> IO ()
buildStack file = cmd "stack" ["build","--stack-yaml=" ++ file,"--test","--bench","--no-run-tests","--no-run-benchmarks","--color=never"]

-- | Note that in addition to parsing the stack.yaml file it also runs @stack@ to
--   compute the dist-dir.
parseStack :: Maybe FilePath -> FilePath -> IO Stack
parseStack distDir file = do
    stackDistDir <- case distDir of
        Nothing -> fst . line1 <$> cmdStdout "stack" ["path","--dist-dir","--stack-yaml=" ++ file,"--color=never"]
        Just x -> return x
    stackPackages <- f . decodeYaml <$> cmdStdout "stack" ["query","locals","--stack-yaml=" ++ file,"--color=never"]
    return Stack{..}
    where
        decodeYaml = either throw id . decodeEither' . BS.pack
        fromObject (Object x) = x
        fromString (String s) = T.unpack s
        f = map (fromString . (Map.! "path") . fromObject) . Map.elems . fromObject
