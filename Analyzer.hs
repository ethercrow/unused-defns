{-# LANGUAGE OverloadedStrings #-}

module Analyzer where

import Control.Comonad.Cofree
import Control.Lens hiding (children)
import Data.Data.Lens
import Data.List (sort, group, nub)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T

import Types

extractNamespaceAndRequires :: AST -> Maybe (Namespace, [(Namespace, Namespace)])
extractNamespaceAndRequires (_ :< List (nsform : _)) =
    case nsform of
        (_ :< List ( (_ :< Atom "ns")
                   : (_ :< Atom ns)
                   : rest)) -> Just (ns, extractRequires rest)
        _ -> Nothing
extractNamespaceAndRequires _ = Nothing


extractRequires :: [AST] -> [(Namespace, Namespace)]
extractRequires = concatMap extract
    where extract (_ :< List ((_ :< Atom ":require") : rs)) = go [] rs
          extract _ = []
          go accum [] = accum
          go accum (r:rest) = case r of
            _ :< Atom ns -> go ((ns, ns) : accum) rest
            _ :< Vec [_ :< Atom ns] -> go ((ns, ns) : accum) rest
            _ :< Vec atoms -> case atoms ^.. biplate of
                    [ns, ":as", alias] -> go ((alias, ns) : accum) rest
                    (parent : children) ->
                        go ([ (parent <> "." <> child, parent <> "." <> child)
                            | child <- children]
                              <> accum)
                           rest
            _ -> go accum rest

extractDefns :: AST -> [Var]
extractDefns (_ :< List sexps) = mapMaybe go sexps
    where go (_ :< List ( (_ :< Atom "defn")
                        : atoms)) = atoms ^.. biplate
                                  & filter ((/= '^') . T.head)
                                  & listToMaybe
          go _ = Nothing
extractDefns _ = []

extractExternalUsages :: [(Namespace, Namespace)] -> AST -> [(Namespace, Var)]
extractExternalUsages requires sexps
    = sexps ^.. biplate
    & mapMaybe parseQualifiedName
    & over (mapped . _1) expandNs
    & filter (isJust . fst)
    & over (mapped . _1) fromJust
    & nub
    where expandNs = flip lookup requires

extractInternalUsages :: [Var] -> AST -> [Var]
extractInternalUsages defns sexps
    = sexps ^.. biplate
    & filter (`elem` defns)
    & sort
    & group
    & filter ((> 1) . length)
    & fmap head
    & nub

parseQualifiedName :: T.Text -> Maybe (Namespace, Var)
parseQualifiedName qname =
    let (ns, slashVar) = T.break (== '/') qname
    in if T.null slashVar
       then Nothing
       else Just (ns, T.drop 1 slashVar)

extractRedundantRequires :: SourceFile -> [Namespace]
extractRedundantRequires sf =
    [ns | ns <- fmap snd (sfRequires sf)
        , ns `notElem` fmap fst (sfExternalUsages sf)]
