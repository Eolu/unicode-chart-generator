{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Chart.Internal
Description :  Creates a chart using unicode characters.
Copyright   :  (c) 2019 Griffin O'Neill
License     :  MIT license

This is the internal logic for the unicode chart generator.

-}

module Chart.Internal where

import Data.Monoid ((<>))
import Data.Text as T (Text, length, replace, replicate)

-- |Type for chart style
data Font = Font { blankspace :: Text, horizontal :: Text, vertical :: Text,
                   top_left :: Text, top_center :: Text, top_right :: Text,
                   mid_left :: Text, mid_center :: Text, mid_right :: Text,
                   low_left :: Text, low_center :: Text, low_right :: Text } deriving (Show)

-- |The base font
base_font = Font { blankspace = " ", horizontal = "─", vertical = "│",
                   top_left = "┌",   top_center = "┬", top_right = "┐",
                   mid_left = "├",   mid_center = "┼", mid_right = "┤",
                   low_left = "└",   low_center = "┴", low_right = "┘" }

-- |Same as base font but bold
bold_font = Font { blankspace = " ",  horizontal = "━",  vertical = "┃", 
                   top_left = "┏",    top_center = "┳",  top_right = "┓",
                   mid_left = "┡",    mid_center = "╇",  mid_right = "┩",
                   low_left = "┗",    low_center = "┻",  low_right = "┛" } 
    
-- |Determine the sizes of columns from lists containing each word in each column
getColumnSizes :: [[Text]] -> [Int]
getColumnSizes []     = []
getColumnSizes [x]    =  [2 + (maximum $ map T.length x)]
getColumnSizes (x:xs) = getColumnSizes [x] <> getColumnSizes xs

-- |Create the top of the data section (for when there's no header)
genTop :: Font -> [Int] -> Text
genTop t c = top_left t <> genTop' t c
genTop' :: Font -> [Int] -> Text
genTop' t []     = ""
genTop' t [c]    = (T.replicate c $ horizontal t) <> top_right t
genTop' t (c:cs) = (T.replicate c $ horizontal t) <> top_center t <> (genTop' t cs)

-- |Create the bottom of the data section
genBottom :: Font -> [Int] -> Text
genBottom t c = low_left t <> genBottom' t c
genBottom' :: Font -> [Int] -> Text
genBottom' t []     = ""
genBottom' t [c]    = (T.replicate c $ horizontal t) <> low_right t
genBottom' t (c:cs) = (T.replicate c $ horizontal t) <> low_center t <> (genBottom' t cs)

-- |Create a separator row
genSeparator :: Font -> [Int] -> Text
genSeparator t c = mid_left t <> genSeparator' t c
genSeparator' :: Font -> [Int] -> Text
genSeparator' t []     = ""
genSeparator' t [c]    = (T.replicate c $ horizontal t) <> mid_right t
genSeparator' t (c:cs) = (T.replicate c $ horizontal t) <> mid_center t <> (genSeparator' t cs)

-- |Create each row of the chart
genRows :: Font -> [[Text]] -> [Int] -> Text
genRows t [] c     = ""
genRows t [x] c    = genRow t x c
genRows t (x:xs) c = genRow t x c <> "\n" <> genSeparator t c <> "\n" <> genRows t xs c

-- |Create a row of cells
genRow :: Font -> [Text] -> [Int] -> Text
genRow t [] []         = vertical t
genRow t [] c          = genRow t [""] c
genRow t x []          = "ERROR"
genRow t [x] [c]       = vertical t <> (genCell t x c) <> vertical t
genRow t (x:xs) (c:cs) = vertical t <> (genCell t x c) <> (genRow t xs cs)

-- |Create a cell for a word. If greater than the max size, will be a blank cell.
genCell :: Font -> Text -> Int -> Text
genCell t word size
    | T.length word > size = T.replicate size (blankspace t)
    | otherwise = left <> word <> right
    where pads      = size - (T.length word)
          halfPads  = pads `div` 2
          oddPads   = pads `mod` 2
          left      = T.replicate halfPads (blankspace t)
          right     = T.replicate (halfPads + oddPads) (blankspace t)
