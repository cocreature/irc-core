{-# Language OverloadedStrings #-}

{-|
Module      : Client.Configuration
Description : Client configuration format and operations
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the top-level configuration information for the client.
-}

module Client.Configuration.Colors
  ( parseColor
  , parseAttr
  ) where

import           Config
import           Config.FromConfig
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Foldable
import           Data.Ratio
import           Data.Text (Text)
import           Graphics.Vty.Attributes

-- | Parse a text attribute. This value should be a sections with the @fg@ and/or
-- @bg@ attributes. Otherwise it should be a color entry that will be used
-- for the foreground color. An empty sections value will result in 'defAttr'
parseAttr :: Value -> ConfigParser Attr
parseAttr (Sections xs) = parseSectionsWith parseAttrEntry defAttr (Sections xs)
parseAttr v             = withForeColor defAttr <$> parseColor v

parseAttrEntry :: Attr -> Text -> Value -> ConfigParser Attr
parseAttrEntry acc k v =
    case k of
        "fg" -> parseColor' withForeColor
        "bg" -> parseColor' withBackColor
        "style" -> parseStyle'
        _    -> failure "Unknown attribute entry"
  where
    parseStyle' =
      do xs <- parseStyles v
         return $! foldl' withStyle acc xs

    parseColor' f =
      do c <- parseColor v
         return $! f acc c

parseStyles :: Value -> ConfigParser [Style]
parseStyles (List xs) = parseList parseStyle (List xs)
parseStyles v         = pure <$> parseStyle v

parseStyle :: Value -> ConfigParser Style
parseStyle v =
  case v of
    Atom "blink"         -> pure blink -- You're the boss...
    Atom "bold"          -> pure bold
    Atom "dim"           -> pure bold
    Atom "reverse-video" -> pure reverseVideo
    Atom "standout"      -> pure standout
    Atom "underline"     -> pure underline
    _ -> failure "expected blink, bold, dim, reverse-video, standout, underline"


-- | Parse a color. Support formats are:
--
-- * Number between 0-255
-- * Name of color
-- * RGB values of color as a list
parseColor :: Value -> ConfigParser Color
parseColor v =
  case v of
    _ | Just i <- parseInteger v -> parseColorNumber i
    Atom a | Just c <- HashMap.lookup (atomName a) namedColors -> return c
    List [r,g,b]
      | Just r' <- parseInteger r
      , Just g' <- parseInteger g
      , Just b' <- parseInteger b ->
         parseRgb r' g' b'
    _ -> failure "Expected a color number, name, or RBG list"

parseColorNumber :: Integer -> ConfigParser Color
parseColorNumber i
  | i < 0 = failure "Negative color not supported"
  | i < 16 = return (ISOColor (fromInteger i))
  | i < 256 = return (Color240 (fromInteger (i - 16)))
  | otherwise = failure "Color value too high"

parseInteger :: Value -> Maybe Integer
parseInteger v =
  case v of
    Number _ i -> Just i
    Floating c e
      | denominator r == 1 -> Just (numerator r)
      where r = fromInteger c * 10^^e
    _ -> Nothing

parseRgb :: Integer -> Integer -> Integer -> ConfigParser Color
parseRgb r g b
  | valid r, valid g, valid b = return (rgbColor r g b)
  | otherwise = failure "RGB values must be in range 0-255"
  where
    valid x = 0 <= x && x < 256

namedColors :: HashMap Text Color
namedColors = HashMap.fromList
  [ ("black"         , black        )
  , ("red"           , red          )
  , ("green"         , green        )
  , ("yellow"        , yellow       )
  , ("blue"          , blue         )
  , ("magenta"       , magenta      )
  , ("cyan"          , cyan         )
  , ("white"         , white        )
  , ("bright-black"  , brightBlack  )
  , ("bright-red"    , brightRed    )
  , ("bright-green"  , brightGreen  )
  , ("bright-yellow" , brightYellow )
  , ("bright-blue"   , brightBlue   )
  , ("bright-magenta", brightMagenta)
  , ("bright-cyan"   , brightCyan   )
  , ("bright-white"  , brightWhite  )
  ]
