module Main (main) where

import Lib
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import Data.Char(isAlpha, toLower, isSpace)
import Data.Map(fromListWith, toList, unionsWith)
import Data.List(sortBy)
import Control.Parallel.Strategies(runEval, parBuffer, rdeepseq, parMap, rpar)

main :: IO ()
main = do args <- getArgs
          case args of
            [filename, k, version] -> do
              if version == "seq" 
                then do
                  contents <- readFile filename
                  let cWords = zip (getWords contents) $ repeat 1
                      counts = toList $ fromListWith (+) cWords
                      trie = initTrie counts
                  autocomplete trie k
              else if version == "par"
                then do
                  contents <- readFile filename
                  let pWords = getWords contents
                      pairs = runEval ((parBuffer 100 rdeepseq) (map (\x -> (x, 1)) pWords))
                      counts = toList $ unionsWith (+) (parMap rpar (fromListWith (+)) (chunk 200000 pairs))
                      trie = initTrie counts
                  parAutocomplete trie k
              else do
                pn <- getProgName
                die $ "Usage: " ++ (fst $ span (/= '.') pn) ++ " <filename> <k> <seq|par>"
            _ -> do pn <- getProgName
                    die $ "Usage: " ++ (fst $ span (/= '.') pn) ++ " <filename> <k> <seq|par>"

getWords :: [Char] -> [String]
getWords = words . map toLower . filter (\c -> isAlpha c || isSpace c)

autocomplete :: Trie -> String -> IO b
autocomplete trie k = do putStrLn "Please enter a query: "
                         line <- getLine
                         let node = traverseTrie line trie
                             counts = dfs node line
                             sorted = sortBy (\(_,a) (_,b) -> compare b a) counts
                         mapM_ (putStrLn . fst) $ take (read k :: Int) sorted  
                         autocomplete trie k

parAutocomplete :: Trie -> String -> IO b
parAutocomplete trie k = do putStrLn "Please enter a query: "
                            line <- getLine
                            let node = traverseTrie line trie
                                counts = parDFS node line
                                sorted = sortBy (\(_,a) (_,b) -> compare b a) counts
                            mapM_ (putStrLn . fst) $ take (read k :: Int) sorted  
                            parAutocomplete trie k

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs