{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2020  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

  gg is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  gg is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with gg.  If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module GG.UI.Theme
  ( ColorScheme(..)
  , builtinColorSchemes
  , attrMap
  ) where

import qualified Brick.AttrMap             as B
import           Data.Colour.RGBSpace      (uncurryRGB)
import           Data.Colour.RGBSpace.HSV  (hsv, hue, saturation, value)
import           Data.Colour.SRGB          (RGB (..), sRGBBounded, sRGB, toSRGB, toSRGB24)
import           Data.Either.Utils         (fromRight)
import           Data.Foldable             (find)
import           Data.List                 (intercalate)
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Word                 (Word8)
import qualified GG.UI.Attrs               as Attr
import           GG.UI.Common              (ColorScheme (..))
import           GG.Utils                  (uncurry3)
import qualified Graphics.Vty              as V

-- Generated:
import           GG.UI.BuiltinColorSchemes (builtinColorSchemes)

data Attr =
  Attr
    { attrStyle     :: Maybe V.Style
    , _attrForeColor :: Maybe ColorReference
    , _attrBackColor :: Maybe ColorReference
    }

data ColorReference
  = SchemeColor (ColorScheme -> (Word8, Word8, Word8))
  | DirectColor V.Color
  | ColorName String
  | ColorMix
      { _base :: ColorReference
      , _hue  :: ColorReference
      }

type ColorNames = [(String, ColorReference)]

defaultColorNames :: ColorNames
defaultColorNames =
  [ ("c00", SchemeColor base00)
  , ("c01", SchemeColor base01)
  , ("c02", SchemeColor base02)
  , ("c03", SchemeColor base03)
  , ("c04", SchemeColor base04)
  , ("c05", SchemeColor base05)
  , ("c06", SchemeColor base06)
  , ("c07", SchemeColor base07)
  , ("c08", SchemeColor base08)
  , ("c09", SchemeColor base09)
  , ("c0A", SchemeColor base0A)
  , ("c0B", SchemeColor base0B)
  , ("c0C", SchemeColor base0C)
  , ("c0D", SchemeColor base0D)
  , ("c0E", SchemeColor base0E)
  , ("c0F", SchemeColor base0F)
  , ("fg", ColorName "c05")
  , ("emphasis", ColorName "c04")
  , ("subtle", ColorName "c06")
  , ("add", ColorName "c0B")
  , ("delete", ColorName "c08")
  , ("modify", ColorName "c0C")
  , ("rename", ColorName "c0E")
  , ("copy", ColorName "c0F")
  , ("error", ColorName "c08")
  , ("bg", ColorName "c00")
  , ("bg-emphasis", ColorName "c01")
  , ("bg-add", ColorMix (ColorName "c00") (ColorName "add"))
  , ("bg-add-emphasis", ColorMix (ColorName "c03") (ColorName "add"))
  , ("bg-delete", ColorMix (ColorName "c00") (ColorName "delete"))
  , ("bg-delete-emphasis", ColorMix (ColorName "c03") (ColorName "delete"))
  ]

resolveColorRef :: [String] -> ColorScheme -> ColorNames -> ColorReference -> Either String V.Color
resolveColorRef stack colorScheme colorNames =
  \case
    SchemeColor colorCtor -> Right $ uncurry3 V.RGBColor $ colorCtor colorScheme
    DirectColor color -> Right color
    ColorName colorName
      | colorName `elem` stack ->
        Left $ "Cycle found in color references: " <> intercalate " -> " (stack <> [colorName])
    ColorName colorName ->
      case lookup colorName colorNames of
        Just colorRef -> resolveColorRef (stack <> [colorName]) colorScheme colorNames colorRef
        Nothing -> Left $ "Unknown color name " <> colorName
    ColorMix (resolveColorRef stack colorScheme colorNames -> Right baseColor)
              (resolveColorRef stack colorScheme colorNames -> Right hueColor) -> Right $ colorMix baseColor hueColor
    ColorMix (resolveColorRef stack colorScheme colorNames -> Right _)
              (resolveColorRef stack colorScheme colorNames -> Left errorMsg) -> Left errorMsg
    ColorMix colorRef _ -> resolveColorRef stack colorScheme colorNames colorRef

colorMix :: V.Color -> V.Color -> V.Color
colorMix _ (V.ISOColor cH) = V.ISOColor cH
colorMix cB cH@(V.Color240 _) =
  V.Color240 $ uncurry3 V.rgbColorToColor240 $ colorMix_ (vtyColorToRGB cB) (vtyColorToRGB cH)
colorMix cB cH = uncurry3 V.RGBColor $ colorMix_ (vtyColorToRGB cB) (vtyColorToRGB cH)

vtyColorToRGB :: V.Color -> (Word8, Word8, Word8)
vtyColorToRGB (V.RGBColor r g b) = (r, g, b)
vtyColorToRGB (V.Color240 c) =
  (\(r, b, g) -> (fromIntegral r, fromIntegral g, fromIntegral b)) $ fromJust $ V.color240CodeToRGB c
vtyColorToRGB (V.ISOColor _) = error "There is not standard RGB value for the ISO terminal colors"

colorMix_ :: (Word8, Word8, Word8) -> (Word8, Word8, Word8) -> (Word8, Word8, Word8)
colorMix_ (rB, gB, bB) (rH, gH, bH) = (rR, gR, bR)
  where
    cB = toSRGB $ sRGBBounded rB gB bB :: RGB Double
    cH = toSRGB $ sRGBBounded rH gH bH :: RGB Double
    h = hue cH
    s = saturation cB
    v = value cB
    RGB rR gR bR = toSRGB24 $ uncurryRGB sRGB $ hsv h s v

resolveAttr :: ColorScheme -> ColorNames -> Attr -> Either String V.Attr
resolveAttr colorScheme colorNames (Attr style c_fg c_bg) = do
  fg_ <- toVtyColor c_fg
  bg_ <- toVtyColor c_bg
  pure $ V.Attr (toVtyStyle style) fg_ bg_ V.KeepCurrent
  where
    toVtyColor :: Maybe ColorReference -> Either String (V.MaybeDefault V.Color)
    toVtyColor (Just cr) = V.SetTo <$> resolveColorRef [] colorScheme colorNames cr
    toVtyColor Nothing = pure V.KeepCurrent
    toVtyStyle :: Maybe V.Style -> V.MaybeDefault V.Style
    toVtyStyle (Just style_) = V.SetTo style_
    toVtyStyle Nothing      = V.KeepCurrent

type Theme = [(B.AttrName, Attr)]

on :: String -> String -> Attr
c_fg `on` c_bg = Attr Nothing (Just $ ColorName c_fg) (Just $ ColorName c_bg)

fg :: String -> Attr
fg c = Attr Nothing (Just $ ColorName c) Nothing

bg :: String -> Attr
bg c = Attr Nothing Nothing (Just $ ColorName c)

withStyle :: Attr -> V.Style -> Attr
a `withStyle` style = a {attrStyle = Just style}

defaultTheme :: Theme
defaultTheme =
  [ (Attr.defaultAttr, "fg" `on` "bg")
  , (Attr.list, "fg" `on` "bg")
  , (Attr.listSelected, "fg" `on` "bg" `withStyle` V.bold)
  , (Attr.oid, fg "emphasis")
  , (Attr.author, fg "c0E")
  , (Attr.date, fg "c0B")
  , (Attr.statusBar, "fg" `on` "bg-emphasis")
  , (Attr.statusBranch, fg "fg")
  , (Attr.commitSummary, "fg" `on` "bg-emphasis" `withStyle` V.bold)
  , (Attr.fullOid, fg "emphasis" `withStyle` V.bold)
  , (Attr.statsFilesModified, fg "modify")
  , (Attr.statsInsertions, fg "add")
  , (Attr.statsDeletions, fg "delete")
  , (Attr.fileDelta, "fg" `on` "bg")
  , (Attr.fileAdded, fg "add")
  , (Attr.fileDeleted, fg "delete")
  , (Attr.fileModified, fg "modify")
  , (Attr.fileRenamed, fg "rename")
  , (Attr.fileCopied, fg "copy")
  , (Attr.diff, "fg" `on` "bg")
  , (Attr.diffHeader, fg "subtle" `withStyle` V.italic)
  , (Attr.diffAddedLine, bg "bg-add")
  , (Attr.diffAddedText, bg "bg-add-emphasis")
  , (Attr.diffDeletedLine, bg "bg-delete")
  , (Attr.diffDeletedText, bg "bg-delete-emphasis")
  , (Attr.diffSpecialText, Attr (Just V.italic) Nothing Nothing)
  , (Attr.diffLineNumber, "emphasis" `on` "bg-emphasis")
  , (Attr.diffLineNumberSep, "subtle" `on` "bg-emphasis")
  ]

themeToAttrMap :: ColorScheme -> ColorNames -> Theme -> Either String B.AttrMap
themeToAttrMap colorScheme colorNames theme = do
  resolvedAttrMap <- sequence [(attrName, ) <$> resolveAttr colorScheme colorNames attr | (attrName, attr) <- theme]
  defaultAttr <- maybe (Left "Default attr not found") Right $ lookup Attr.defaultAttr resolvedAttrMap
  pure $ B.attrMap defaultAttr resolvedAttrMap

attrMap :: String -> B.AttrMap
attrMap name =
  fromRight $ themeToAttrMap scheme defaultColorNames defaultTheme
  where
    scheme = fromMaybe (error $ "Unknown theme: " ++ name) $ find (\x -> colorSchemeName x == name) builtinColorSchemes
