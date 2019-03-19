{-# LANGUAGE OverloadedStrings #-}

module Impl (helloMessage) where

import Data.Semigroup ((<>))
import Data.Text (Text)

helloMessage :: Text -> Text
helloMessage w = "Hello, " <> w <> "!"
