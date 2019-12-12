{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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
module Libgit2.Signature
  ( gitToLocalTime
  , signatureName
  , signatureEmail
  , signatureWhen
  , signatureDefault
  ) where

{#import Libgit2.Types#}

import Foreign (Ptr, alloca, withForeignPtr)
import Foreign.C (peekCString)
import Data.Time.LocalTime (ZonedTime)
import Libgit2.Errors (checkReturnCode)
import Libgit2.Utils (peekStruct)

#include "git2/signature.h"

{#context lib="git2" prefix="git_"#}

_peekSignature :: (Ptr Signature -> IO a) -> (a -> IO b) -> Signature -> IO b
_peekSignature getter outMarshaller (Signature fp) = peekStruct getter outMarshaller fp

signatureName :: Signature -> IO String
signatureName = _peekSignature ({#get signature->name#}) peekCString

signatureEmail :: Signature -> IO String
signatureEmail = _peekSignature ({#get signature->email#}) peekCString

signatureWhen :: Signature -> IO ZonedTime
signatureWhen (Signature fp) =
  withForeignPtr fp (\p -> do
    time <- fromIntegral <$> ({#get signature->when.time#} p)
    offset <- fromIntegral <$> ({#get signature->when.offset#} p)
    pure $ gitToLocalTime time offset
  )

{#fun signature_default as signatureDefault { alloca- `Signature' peekNewSignature*, `Repository' } -> `Int' checkReturnCode*-#}
