{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Chart
Description :  Creates a chart using unicode characters.
Copyright   :  (c) 2019 Griffin O'Neill
License     :  MIT license

This program parses input split by some delimeters and returns a
chart built with unicode characters. It can use standard input 
or a file.

-}

module Chart (chart) where
import Chart.Internal
import Data.List (transpose, reverse)
import Data.Monoid ((<>))
import Data.Text (Text)

-- |Generate a chart string
-- include header -> Chart content  -> chart string
chart :: Bool -> [[Text]] -> Text
chart includeHeader [] = ""
chart includeHeader (x:xs) = 
   if includeHeader
      then top bold_font <> "\n" <> rows bold_font [x] <> "\n" <> if xs == []
         then bottom bold_font
         else separator bold_font <> "\n" <> rows base_font xs <> "\n" <> bottom base_font
      else top base_font <> "\n" <> rows base_font (x:xs) <> "\n" <> bottom base_font
   where cols            = getColumnSizes $ (map reverse . transpose) (x:xs)
         top font        = genTop font cols
         rows font input = genRows font input cols
         bottom font     = genBottom font cols
         separator font  = genSeparator font cols
