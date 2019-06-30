{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Check(check) where

import Hi
import Cabal
import Util
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import System.Info.Extra
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Warning


data S = S
    {pkg :: PackageName
    ,hi :: HiKey -> Hi
    ,sections :: [(CabalSection, ([HiKey], [HiKey], [ModuleName]))]
    }

check :: (HiKey -> Hi) -> PackageName -> [(CabalSection, ([HiKey], [HiKey], [ModuleName]))] -> [Warning]
check hi pkg sections2 = map (\x -> x{warningSections = sort $ warningSections x}) $
    warnReusedModuleBetweenSections s ++
    warnRedundantPackageDependency s ++
    warnIncorrectOtherModules s ++
    warnUnusedExport s ++
    warnNotCompiled s ++
    warnUnusedImport s
    where
        s = S{..}
        sections = map (second $ \(a,b,c) -> let aa = nubOrd a in (aa,nubOrd b \\ aa,c)) sections2


warnNotCompiled :: S -> [Warning]
warnNotCompiled S{..} =
    [ Warning pkg [cabalSectionType s] "Module not compiled" Nothing (Just m) Nothing
    | (s, (_, _, missing)) <- sections, m <- missing]


warnReusedModuleBetweenSections :: S -> [Warning]
warnReusedModuleBetweenSections S{..} =
    [ Warning pkg ss "Module reused between components" Nothing (Just $ hiModuleName $ hi m) Nothing
    | (m, ss) <- groupSort [(x, cabalSectionType c) | (c, (x1,x2,_)) <- sections, x <- x1++x2]
    , length ss > 1]


warnRedundantPackageDependency :: S -> [Warning]
warnRedundantPackageDependency S{..} =
    [ Warning pkg [cabalSectionType] "Redundant build-depends entry" (Just p) Nothing Nothing
    | (CabalSection{..}, (x1,x2,_)) <- sections
    , let usedPackages = Set.unions $ map (Set.map fst . hiImportPackageModule . hi) $ x1 ++ x2
    , not $ "" `Set.member` usedPackages -- Sometimes we don't get the package name at all, e.g. https://gitlab.haskell.org/ghc/ghc/issues/16886
    , p <- Set.toList $ Set.fromList cabalPackages `Set.difference` usedPackages
    , p /= if isWindows then "unix" else "Win32" -- ignore packages that must be conditional on the other platform
    , p /= "semigroups" -- ignore packages that are often conditional
    , p /= "base" -- used by Paths_ modules which we have thrown away by this point
    ]


warnIncorrectOtherModules :: S -> [Warning]
warnIncorrectOtherModules S{..} = concat
    [ [Warning pkg [cabalSectionType] "Missing other-modules entry" Nothing (Just m) Nothing | m <- Set.toList missing] ++
      [Warning pkg [cabalSectionType] "Excessive other-modules entry" Nothing (Just m) Nothing | m <- Set.toList excessive]
    | (CabalSection{..}, (external, internal,_)) <- sections
    , let imports = Map.fromList [(hiModuleName, hiImportModule) | Hi{..} <- map hi $ external ++ internal]
    , let missing =  Set.filter (not . isPathsModule) $
                     Set.unions (Map.elems imports) `Set.difference`
                     Set.fromList (Map.keys imports)
    , let excessive = Set.fromList (map (hiModuleName . hi) internal) `Set.difference`
                      reachable (\k -> maybe [] Set.toList $ Map.lookup k imports) (map (hiModuleName . hi) external)
    ]


-- Primarily looking for import Foo() where Foo is not an orphan
warnUnusedImport :: S -> [Warning]
warnUnusedImport S{..} =
    [ Warning pkg [cabalSectionType] "Unused import" Nothing (Just $ hiModuleName mod) (Just $ hiModuleName imp)
    | (CabalSection{..}, (external, internal,_)) <- sections
    , let mods = Map.fromList $ map ((hiModuleName &&& id) . hi) $ external ++ internal
    , mod <- Map.elems mods
    , imp <- mapMaybe (`Map.lookup` mods) $ Set.toList $
        hiImportModule mod `Set.difference`
        (Set.map identModule (hiImportIdent mod) `Set.union` hiImportOrphan mod)
    , Set.null $ hiImportIdent mod `Set.intersection` hiExportIdent imp -- reexporting for someone else
    , Set.null $ Set.map snd (hiImportPackageModule mod) `Set.intersection` Set.map identModule (hiExportIdent imp) -- reexporting for another package
    , Set.null $ Set.map identModule (Set.filter (isHaskellCtor . identName) $ hiExportIdent imp) `Set.difference` Set.insert (hiModuleName imp) (hiImportModule imp) -- reexport a type for another package, see #15
    ]

warnUnusedExport :: S -> [Warning]
warnUnusedExport S{..} =
    [ Warning pkg ss "Weeds exported" Nothing (Just $ hiModuleName $ hi m) (Just i)
    | (m,(ss,is)) <- Map.toList unused, i <- Set.toList is]
    where
        unionsWith f = foldr (Map.unionWith f) Map.empty
        -- important: for an identifer to be unused, it must be unused in all sections that use that key
        unused = unionsWith (\(s1,i1) (s2,i2) -> (s1++s2, i1 `Set.intersection` i2))
                 [ Map.fromList [(k, ([cabalSectionType], Set.fromList $ Map.lookupDefault [] (hiModuleName $ hi k) bad)) | k <- internal ++ external]
                 | (CabalSection{..}, (external, internal,_)) <- sections
                 , let bad = Map.fromListWith (++) $ map (identModule &&& return . identName) $ notUsedOrExposed (map hi external) (map hi internal)]

notUsedOrExposed :: [Hi] -> [Hi] -> [Ident]
notUsedOrExposed external internal = Set.toList $
        privateAPI `Set.difference` Set.unions [publicAPI,supported,usedAnywhere]
    where
        modules = Map.fromList [(hiModuleName x, x) | x <- external ++ internal]

        -- things exported from this package
        publicAPI = Set.unions $ map hiExportIdent external

        -- Types that are required to define things that are public
        supported = Set.unions
            [ Map.lookupDefault Set.empty x hiSignatures
            | (m, xs) <- groupSort $ map (identModule &&& identName) $ Set.toList $ Set.union publicAPI usedAnywhere
            , Just Hi{..} <- [Map.lookup m modules], x <- xs]

        -- things that are defined in other modules and exported
        -- (ignoring field name since they provide handy documentation)
        privateAPI = Set.unions
            [ Set.filter ((==) hiModuleName . identModule) $ hiExportIdent `Set.difference` hiFieldName
            | Hi{..} <- internal]

        -- things that are used anywhere, if someone imports and exports something
        -- assume that isn't also a use (find some redundant warnings)
        usedAnywhere = Set.unions
            [ hiImportIdent `Set.difference` hiExportIdent
            | Hi{..} <- external ++ internal]
