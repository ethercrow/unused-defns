{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens
import Control.Monad
import Data.Data.Lens
import Data.List (isSuffixOf)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath

import Types
import Parser

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

    let allExternalUsages = concatMap sfUsages sourceFiles
        unusedDefns = [(sfNs file, defn) | file <- sourceFiles
                                         , defn <- sfDefns file
                                         , (sfNs file, defn) `notElem` allExternalUsages]
    print sourceFiles
    -- putStrLn "All external usages:"
    -- print allExternalUsages
    putStrLn "All unused defns:"
    putStr $ concatMap (\(ns, var) -> T.unpack ns ++ "/" ++ T.unpack var ++ "\n")
                       unusedDefns

mkSourceFile :: T.Text -> Either T.Text SourceFile
mkSourceFile content =
    case parseText content of
        Right sexps -> let msf = do (ns, requires) <- extractNamespaceAndRequires sexps
                                    return $ SourceFile ns
                                                        requires
                                                        (extractDefns sexps)
                                                        (extractUsages requires sexps)
                       in case msf of
                           Just sf -> Right sf
                           Nothing -> Left "Parsing namespaces failed"
        Left diag -> Left diag

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
    where extract (_ :< List ((_ :< Atom ":require") : rs)) = go [] (rs ^.. biplate)
          extract _ = []
          go accum [] = accum
          go accum (ns : ":as" : alias : rest) = go ((alias, ns) : accum) rest
          go accum (ns : rest) = go ((ns, ns) : accum) rest

extractDefns :: AST -> [Var]
extractDefns (_ :< List sexps) = mapMaybe go sexps
    where go (_ :< List ( (_ :< Atom "defn")
                         : (_ :< Atom f)
                         : _)) = Just f
          go _ = Nothing
extractDefns _ = []

extractUsages :: [(Namespace, Namespace)] -> AST -> [(Namespace, Var)]
extractUsages requires sexps = over (mapped . _1) fromJust
                             $ filter (isJust . fst)
                             $ over (mapped . _1) expandNs
                             $ mapMaybe parseQualifiedName
                             $ sexps ^.. biplate
    where expandNs = flip lookup requires

parseQualifiedName :: T.Text -> Maybe (Namespace, Var)
parseQualifiedName qname =
    let (ns, slashVar) = T.break (== '/') qname
    in if T.null slashVar
       then Nothing
       else Just (ns, T.drop 1 slashVar)

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
