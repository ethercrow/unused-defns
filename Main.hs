{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.List (isSuffixOf)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath

import Analyzer
import Parser
import Types

main :: IO ()
main = do
    srcDir <- (fromMaybe "." . listToMaybe) <$> getArgs
    filenames <- filter (\f -> ".clj" `isSuffixOf` f
                               && not ("project.clj" `isSuffixOf` f))
                 <$> getRecursiveFiles srcDir

    sourceFiles <- catMaybes <$> forM filenames (\f -> do
        text <- T.readFile f
        case mkSourceFile text of
            Right sf -> return $ Just sf
            Left diag -> do
                putStrLn $ "Error parsing " ++ f
                T.putStrLn diag
                return Nothing)

    putStrLn $ "Parsed " ++ show (length sourceFiles) ++ " out of " ++ show (length filenames)

    let allExternalUsages = concatMap sfExternalUsages sourceFiles
        publiclyUnusedDefns = [(sfNs file, defn) | file <- sourceFiles
                                                 , defn <- sfDefns file
                                                 , (sfNs file, defn) `notElem` allExternalUsages
                                                 , defn `elem` sfInternalUsages file]
        unusedDefns = [(sfNs file, defn) | file <- sourceFiles
                                         , defn <- sfDefns file
                                         , (sfNs file, defn) `notElem` allExternalUsages
                                         , defn `notElem` sfInternalUsages file]
        redundantRequires = [(sfNs file, reqs) | file <- sourceFiles
                                               , let reqs = extractRedundantRequires file
                                               , not (null reqs)]
    print sourceFiles
    putStrLn "\nPublicly unused defns:"
    putStr $ concatMap (\(ns, var) -> T.unpack ns ++ "/" ++ T.unpack var ++ "\n")

                       publiclyUnusedDefns
    putStrLn "\nUnused defns:"
    putStr $ concatMap (\(ns, var) -> T.unpack ns ++ "/" ++ T.unpack var ++ "\n")
                       unusedDefns
    putStrLn "\nRedundant requires:"
    putStr $ concatMap (\(ns, reqs) -> T.unpack ns ++ "\n"
                                    ++ show reqs
                                    ++ "\n")
                       redundantRequires

mkSourceFile :: T.Text -> Either T.Text SourceFile
mkSourceFile content =
    case parseText content of
        Right sexps ->
            let defns = extractDefns sexps
                msf = do (ns, requires) <- extractNamespaceAndRequires sexps
                         return $ SourceFile ns
                                             requires
                                             defns
                                             (extractInternalUsages defns sexps)
                                             (extractExternalUsages requires sexps)
            in case msf of
                Just sf -> Right sf
                Nothing -> Left "Parsing namespaces failed"
        Left diag -> Left diag

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".git", ".svn"]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveFiles path
            else return [path]
    return (concat paths)

