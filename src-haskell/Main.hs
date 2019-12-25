{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Main
Description :  Creates a chart using unicode characters.
Copyright   :  (c) 2019 Griffin O'Neill
License     :  MIT license

This program parses input split by some delimeters and returns a
chart built with unicode characters. It can use standard input 
or a file.

This is the entry-point for this program's executable.

Usage: chart [-h] [-d delimiter]... [FILE]

-}

module Main (main) where
import Chart (chart)

import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt, usageInfo)
import System.Environment (getArgs)
import Data.Monoid ((<>))
import Data.List (transpose, foldl')
import Data.Text as T (Text, pack, splitOn, lines, unpack, null)
import Data.Text.IO as I (putStrLn, putStr, getContents, readFile)

version :: Text
version = "unicode chart generator v1.1"
usage :: Text
usage = pack $ usageInfo "Usage: chart [-h] [-d delimiter]+ [FILE]" options


-- Try new form of arg parsing
type Opts = (Bool,Bool,[Text])

def :: Opts
def = (False,False,[])

options :: [OptDescr (Opts -> Opts)]
options =
    [ Option "d" [] (ReqArg (\d (v,h,ds) -> (v, h, pack d:ds)) "delimiter") "specify delimiter"
    , Option "h" ["header"] (NoArg (\(v,_,ds) -> (v,True,ds))) "include a header"
    , Option "v" ["version"] (NoArg (\(_,h,ds) -> (True,h,ds))) "display version info"
    , Option "?" ["help"] (NoArg (\(v,h,ds) -> (v,h,"USAGE_MSG":ds))) "print this message"
    ]

-- |Entry-point
main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
      (_,_,err:_) -> fail $ unpack $ (pack err) <> usage
      (fs,as,[])  ->
        case foldr ($) def fs of
          (False,_,["USAGE_MSG"]) -> I.putStr usage
          (True,_,_)              -> I.putStr version
          (_,h,ds)                -> main' h tokenize
            where splitOnAny ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds
                  chartSplit s     = nonNullTokens $ (fmap $ fmap $ splitOnAny delims) (nonNullString $ fmap T.lines s) -- may need another nonNull here
                  delims           = if ds == [] then [" "] else ds
                  nonNullString    = fmap $ filter $ not . T.null
                  nonNullTokens    = fmap $ fmap $ filter $ not . T.null
                  tokenize         = if as == [] then chartSplit I.getContents else fmap concat $ sequence [tokenizeFile f | f <- as]
                  tokenizeFile f   = chartSplit $ I.readFile f

-- |Main after all argument parsing. Takes a bool representing whether or not
-- to include a header and a 2d list of token strings.
main' :: Bool -> IO [[Text]] -> IO ()
main' header tokens = fmap (chart header) tokens >>= I.putStrLn
