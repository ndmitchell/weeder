{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Warning(
    Warning(..),
    showWarningsPretty,
    showWarningsYaml,
    showWarningsJson,
    readWarningsFile,
    ignoreWarnings
    ) where

import Cabal
import Util
import Data.Maybe
import Data.List.Extra
import Control.Exception
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
    } deriving (Show,Eq,Ord)

warningLabels = ["package","section","message","depends","module","identifier"]

warningPath :: Warning -> [Maybe String]
warningPath Warning{..} =
    [Just warningPackage
    ,Just $ unwords $ map show warningSections
    ,Just warningMessage
    ,warningDepends
    ,warningModule
    ,warningIdentifier]

warningUnpath :: [String] -> Warning
warningUnpath [pkg,sect,msg,deps,mod,ident] = Warning
    pkg (map read $ words sect) msg
    (f deps) (f mod) (f ident)
    where f s = if null s then Nothing else Just s

showWarningsPretty :: PackageName -> [Warning] -> [String]
showWarningsPretty pkg [] = ["= Package " ++ pkg ++ " =","No warnings"]
showWarningsPretty _ warn = warningTree
    ([\x -> "= Package " ++ x ++ " =",\x -> "\n== Section " ++ x ++ " ==",id,("* "++),("  - "++)] ++ repeat id) $
    map (catMaybes . warningPath) warn

warningTree :: Ord a => [a -> a] -> [[a]] -> [a]
warningTree (f:fs) xs = concat
    [ f title : warningTree fs inner
    | (title,inner) <- groupSort $ mapMaybe uncons xs]


-- (section, name, children)
data Val = Val String String [Val]
         | End String [String]

valToValue :: [Val] -> Value
valToValue = Array . V.fromList . map f
    where
        f (Val sect name xs) = Object $ Map.singleton (T.pack sect) $ Array $ V.fromList $
            Object (Map.singleton (T.pack "name") (String $ T.pack name)) : map f xs
        f (End sect [x]) = Object $ Map.singleton (T.pack sect) $ String $ T.pack x
        f (End sect xs) = Object $ Map.singleton (T.pack sect) $ Array $ V.fromList $ map (String . T.pack) xs

valueToVal :: Value -> [Val]
valueToVal = f
    where
        f (Array xs) = concatMap f $ V.toList xs
        f (Object mp) | [(k,v)] <- Map.toList mp = return $ case v of
            v | Just (n, rest) <- findName v -> Val (T.unpack k) (T.unpack n) $ f rest
            Array xs -> End (T.unpack k) $ map (T.unpack . fromString) $ V.toList xs
            String x -> End (T.unpack k) [T.unpack x]
        fromString (String x) = x

        findName (Array xs)
            | ([name], rest) <- partition (isJust . fromName) $ V.toList xs
            = Just (fromJust $ fromName name, Array $ V.fromList rest)
        findName _ = Nothing

        fromName (Object mp) | [(k,String v)] <- Map.toList mp, T.unpack k == "name" = Just v
        fromName _ = Nothing

showWarningsValue :: [Warning] -> Value
showWarningsValue = valToValue . f warningLabels . map (dropWhileEnd isNothing . warningPath)
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


readWarningsFile :: FilePath -> IO [Warning]
readWarningsFile file = do
    x <- either throwIO return =<< Yaml.decodeFileEither file
    return $ map warningUnpath $ concatMap (f warningLabels) $ valueToVal x
    where
        f :: [String] -> Val -> [[String]]
        f names (End sect ns) = concatMap (f names) $ map (\n -> Val sect n []) ns
        f (name:names) val@(Val sect n xs)
            | sect == name = if null xs
                then [n : replicate (length names) ""]
                else map (n:) $ concatMap (f names) xs
            | otherwise = map ("":) $ f names val


-- | Ignore all found warnings that are covered by a template
ignoreWarnings :: [Warning] -> [Warning] -> [Warning]
ignoreWarnings template = filter (\x -> not $ any (`match` x) template)
    where
        unpack = map (fromMaybe "") . warningPath
        match template found = and $ zipWith (\t f -> t == "" || t == f) (unpack template) (unpack found)
