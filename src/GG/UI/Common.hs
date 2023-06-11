{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2023  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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
module GG.UI.Common
  ( ColorScheme(..)
  ) where

import           Data.Word (Word8)

data ColorScheme =
  ColorScheme
    { colorSchemeName        :: String
    , colorSchemeDescription :: String
    , colorSchemeAuthor      :: String
    , base00                 :: (Word8, Word8, Word8)
    , base01                 :: (Word8, Word8, Word8)
    , base02                 :: (Word8, Word8, Word8)
    , base03                 :: (Word8, Word8, Word8)
    , base04                 :: (Word8, Word8, Word8)
    , base05                 :: (Word8, Word8, Word8)
    , base06                 :: (Word8, Word8, Word8)
    , base07                 :: (Word8, Word8, Word8)
    , base08                 :: (Word8, Word8, Word8)
    , base09                 :: (Word8, Word8, Word8)
    , base0A                 :: (Word8, Word8, Word8)
    , base0B                 :: (Word8, Word8, Word8)
    , base0C                 :: (Word8, Word8, Word8)
    , base0D                 :: (Word8, Word8, Word8)
    , base0E                 :: (Word8, Word8, Word8)
    , base0F                 :: (Word8, Word8, Word8)
    }
