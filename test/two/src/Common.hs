{-# LANGUAGE DeriveDataTypeable #-}

module Common (Sample(..), helloMessage) where

import Data.Data (Data, Typeable)
import Data.Text (Text)
import Impl

data Sample = Sample {hello :: Text} deriving (Show, Data, Typeable)
