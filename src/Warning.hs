{-# LANGUAGE TupleSections, RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}

module Warning(
    Warning(..),
    showWarningsPretty,
--    showWarningsYaml,
--    showWarningsJSON,
--    readConfigFle,
--    ignoreWarnings
    ) where

import Cabal
import Util
import Data.Maybe
import Data.List.Extra


data Warning = Warning
    {warningMessage :: String
    ,warningSections :: [CabalSectionType]
    ,warningPackage :: Maybe PackageName
    ,warningModule :: Maybe ModuleName
    ,warningIdentifier :: Maybe IdentName
    } deriving Show

warningPath :: Warning -> [String]
warningPath Warning{..} =
    [unwords $ map show warningSections
    ,warningMessage] ++
    catMaybes [warningPackage, warningModule, warningIdentifier]

showWarningsPretty :: [Warning] -> [String]
showWarningsPretty [] = ["No warnings"]
showWarningsPretty warn = (\(x:xs) -> tail x:xs) $ warningTree
    ([\x -> "\n== Section " ++ x ++ " ==",id,("* "++),("  - "++)] ++ repeat id) $
    map warningPath warn

warningTree :: Ord a => [a -> a] -> [[a]] -> [a]
warningTree (f:fs) xs = concat
    [ f title : warningTree fs inner
    | (title,inner) <- groupSort $ mapMaybe uncons xs]
