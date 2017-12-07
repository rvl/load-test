module Data.Aeson.Extra
  ( prefixOptions
  ) where

import Data.Aeson
import qualified Data.Aeson.Types as JSON
import Data.Aeson.Types           (camelTo2)
import Data.Char

modifierCamel :: String -> String
modifierCamel = drop 1 . dropWhile (/= '_') . dropWhile (== '_') . camelTo2 '_'

modifier :: String -> String
modifier = firstLower . dropPrefix
  where
    dropPrefix :: String -> String
    dropPrefix = dropWhile isLower
    firstLower [] = []
    firstLower (x:xs) = (toLower x:xs)

prefixOptions :: JSON.Options
prefixOptions = JSON.defaultOptions { JSON.fieldLabelModifier = modifier }
