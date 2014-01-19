{-# LANGUAGE DeriveFunctor,
             FlexibleInstances,
             DeriveDataTypeable #-}

module Types where

import Control.Comonad.Cofree
import Data.Data
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Typeable

type Namespace = T.Text 
type Var = T.Text 

data SourceFile = SourceFile {
    sfNs :: !Namespace
  , sfRequires :: ![(Namespace, Namespace)]
  , sfDefns :: ![Var]
  , sfInternalUsages :: ![Var]
  , sfExternalUsages :: ![(Namespace, Var)]
  }

newtype CljString = CljString { unCljString :: T.Text }
  deriving (Data, Typeable, Eq, Show)

newtype CljRegex = CljRegex { unCljRegex :: T.Text }
  deriving (Data, Typeable, Eq, Show)

data ExprF a
    = List [a]
    | AnonFn [a]
    | Vec [a]
    | Map [(a, a)]
    | Set [a]
    | Quote a
    | CljStringLiteral CljString
    | CljRegexLiteral CljRegex
    | Atom T.Text
    deriving (Eq, Functor, Data, Typeable, Show)

type AST = Cofree ExprF ()

-- instance Show a => Show (ExprF a) where
--     show (Atom t1) = T.unpack t1
--     show (List xs) = surround "(" ")" xs
--     show (AnonFn xs) = surround "#(" ")" xs
--     show (Vec xs) = surround "[" "]" xs
--     show (Map xys) = surround "{" "}" xys
--     show (CljStringLiteral (CljString s)) = "\"" ++ show s ++ "\""
--     show (CljRegexLiteral (CljRegex s)) = "#\"" ++ show s ++ "\""
--     show (Set xs) = surround "#{" "}" xs
--     show (Quote x) = '\'' : show x

surround :: Show a => String -> String -> [a] -> String
surround l r xs = l ++ unwords (fmap show xs) ++ r

instance Show SourceFile where
    show (SourceFile n rs ds ius eus) =
        intercalate "\n" [ show n
                         , "Requires:"
                         , unlines (map show rs)
                         , "Defns:"
                         , unlines (map show ds)
                         , "External usages:"
                         , unlines (map show eus)
                         , "Internal usages:"
                         , unlines (map show ius)
                         , "\n"
                         ]
