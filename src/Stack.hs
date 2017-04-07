{-# LANGUAGE OverloadedStrings #-}

module Stack(Stack(..), parseStack) where

import Data.Yaml
import Control.Exception
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

data Stack = Stack {stackPackages :: [FilePath]}

parseStack :: FilePath -> IO Stack
parseStack file = either throwIO (return . f) =<< decodeFileEither file
    where
        f (Object x)
            | Just (Array xs) <- Map.lookup "packages" x
            = Stack [T.unpack s | String s <- V.toList xs]
        f _ = error "Failed to parse stack file"
