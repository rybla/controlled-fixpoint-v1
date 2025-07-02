module Spec.Engine.Library.Common where

import qualified Spec.Engine.Common as Common
import System.FilePath ((</>))

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "Library"
