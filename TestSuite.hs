{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Comonad.Cofree

import Types
import Parser

import Test.Tasty.TH
import Test.Tasty.HUnit

main :: IO ()
main = $(defaultMainGenerator)

case_foo :: Assertion
case_foo = do
    let east = parseText "(ns foo (:require bar baz)) (defn f1 [x y z] (bar/bar bar bar 42))"
    case east of
        Right ast ->
            () :< List [ () :< List [ () :< Atom "ns"
                                    , () :< Atom "foo"
                                    , () :< List [ () :< Atom ":require"
                                                 , () :< Atom "bar"
                                                 , () :< Atom "baz"
                                                 ]
                                    ]
                       , () :< List [ () :< Atom "defn"
                                    , () :< Atom "f1"
                                    , () :< Vec [ () :< Atom "x"
                                                , () :< Atom "y"
                                                , () :< Atom "z"
                                                ]
                                    , () :< List [ () :< Atom "bar/bar"
                                                 , () :< Atom "bar"
                                                 , () :< Atom "bar"
                                                 , () :< Atom "42"
                                                 ]
                                    ]
                       ]
            @=? ast
        Left _ -> assertFailure "no parse"
