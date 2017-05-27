{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

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
import Control.Monad.Extra
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
           deriving Show

valToValue :: [Val] -> Value
valToValue = Array . V.fromList . map f
    where
        pair k v = Object $ Map.singleton (T.pack k) v
        f (Val sect name xs) = pair sect $ Array $ V.fromList $
            pair "name" (String $ T.pack name) : map f xs
        f (End sect [x]) = pair sect $ String $ T.pack x
        f (End sect xs) = pair sect $ Array $ V.fromList $ map (String . T.pack) xs

valueToVal :: Value -> [Val]
valueToVal = f
    where
        badYaml want x = error $ "Failed to understand Yaml fragment, expected " ++ want ++ ", got:\n" ++ BS.unpack (Yaml.encode x)

        f Null = []
        f (Object mp) | Map.null mp = []
        f (Array xs) = concatMap f $ V.toList xs
        f (Object mp) | [(k,v)] <- Map.toList mp = return $ case v of
            v | Just (n, rest) <- findName v -> Val (T.unpack k) (T.unpack n) $ f rest
            v | Just xs <- fromStrings v -> End (T.unpack k) xs
            String x -> End (T.unpack k) [T.unpack x]
            _ -> badYaml "either a dict with 'name' or a list/single string" $ Object mp
        f x = badYaml "either a singleton dict or an array" x

        fromStrings (Array xs) = concatMapM fromStrings $ V.toList xs
        fromStrings (String x) = Just [T.unpack x]
        fromStrings x = Nothing

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
            | all (\x -> length x <= 1) xs = [End name $ sort [x | [Just x] <- xs] | xs /= []]
            | otherwise = concat
                [ case a of
                    Nothing -> f names b
                    Just a -> [Val name a $ f names b]
                | (a,b) <- groupSort $ mapMaybe uncons xs]

showWarningsJson :: [Warning] -> String
showWarningsJson = LBS.unpack . JSON.encode . showWarningsValue

showWarningsYaml :: [Warning] -> String
showWarningsYaml [] = "" -- no need to write anything in the file
showWarningsYaml xs = BS.unpack $ Yaml.encode $ showWarningsValue xs


readWarningsFile :: FilePath -> IO [Warning]
readWarningsFile file = do
    x <- either throwIO return =<< Yaml.decodeFileEither file
    let res = map warningUnpath $ concatMap (f warningLabels) $ valueToVal x
    mapM_ evaluate res -- ensure exceptions happen immediately
    return res
    where
        f :: [String] -> Val -> [[String]]
        f names (End sect ns) = concatMap (\n -> f names $ Val sect n []) ns
        f (name:names) val@(Val sect n xs)
            | sect == name = if null xs
                then [n : replicate (length names) ""]
                else map (n:) $ concatMap (f names) xs
            | sect `notElem` names = error $
                "Warnings file " ++ file ++ ", invalid section name:\n" ++
                "Wanted one of: " ++ show (name:names) ++ "\n" ++
                "Got: " ++ show sect
            | otherwise = map ("":) $ f names val


-- | Ignore all found warnings that are covered by a template
ignoreWarnings :: [Warning] -> [Warning] -> [Warning]
ignoreWarnings template = filter (\x -> not $ any (`match` x) template)
    where
        unpack = map (fromMaybe "") . warningPath
        match template found = and $ zipWith (\t f -> t == "" || t == f) (unpack template) (unpack found)
