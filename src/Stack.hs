{-# LANGUAGE OverloadedStrings #-}

module Stack(Stack(..), parseStack) where

import Data.Yaml
import Control.Exception
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

newtype Stack = Stack {stackPackages :: [FilePath]}

parseStack :: FilePath -> IO Stack
parseStack file = either throwIO (return . f) =<< decodeFileEither file
    where
        fromObject (Object x) = x
        fromArray (Array xs) = V.toList xs
        fromString (String s) = T.unpack s

        f x = case Map.lookup "packages" $ fromObject x of
            Nothing -> Stack ["."]
            Just xs -> Stack $ map fromString $ fromArray xs
