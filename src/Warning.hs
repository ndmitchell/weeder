{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Warning(
    Warning(..),
    showWarningsPretty,
    showWarningsYaml,
    showWarningsJson,
--    readConfigFle,
--    ignoreWarnings
    ) where

import Cabal
import Util
import Data.Maybe
import Data.List.Extra
import Data.Aeson as JSON
import Data.Yaml as Yaml
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


data Warning = Warning
    {warningPackage :: String
    ,warningSections :: [CabalSectionType]
    ,warningMessage :: String
    ,warningDepends :: Maybe PackageName
    ,warningModule :: Maybe ModuleName
    ,warningIdentifier :: Maybe IdentName
    } deriving Show

warningPath :: Warning -> [Maybe String]
warningPath Warning{..} =
    [Just warningPackage
    ,Just $ unwords $ map show warningSections
    ,Just warningMessage
    ,warningDepends
    ,warningModule
    ,warningIdentifier]

showWarningsPretty :: [Warning] -> [String]
showWarningsPretty [] = ["No warnings"]
showWarningsPretty warn = warningTree
    ([\x -> "= Package " ++ x ++ " =",\x -> "\n== Section " ++ x ++ " ==",id,("* "++),("  - "++)] ++ repeat id) $
    map (catMaybes . warningPath) warn

warningTree :: Ord a => [a -> a] -> [[a]] -> [a]
warningTree (f:fs) xs = concat
    [ f title : warningTree fs inner
    | (title,inner) <- groupSort $ mapMaybe uncons xs]

-- (section, name, children)
data Val = Val String String [Val]
         | End String [String]

val :: [Val] -> Value
val = Array . V.fromList . map f
    where
        f (Val sect name xs) = Object $ Map.singleton (T.pack sect) $ Array $ V.fromList $
            Object (Map.singleton (T.pack "name") (String $ T.pack name)) : map f xs
        f (End sect [x]) = Object $ Map.singleton (T.pack sect) $ String $ T.pack x
        f (End sect xs) = Object $ Map.singleton (T.pack sect) $ Array $ V.fromList $ map (String . T.pack) xs

showWarningsValue :: [Warning] -> Value
showWarningsValue = val . f ["package","section","message","depends","module","identifier",""] .
    map (dropWhileEnd isNothing . warningPath)
    where
        f (name:names) xs
            | all (\x -> length x <= 1) xs = [End name $ sort [x | [Just x] <- xs]]
            | otherwise = concat
                [ case a of
                    Nothing -> f names b
                    Just a -> [Val name a $ f names b]
                | (a,b) <- groupSort $ mapMaybe uncons xs]

showWarningsJson :: [Warning] -> String
showWarningsJson = LBS.unpack . JSON.encode . showWarningsValue

showWarningsYaml :: [Warning] -> String
showWarningsYaml = BS.unpack . Yaml.encode . showWarningsValue
