module Spec.Engine.PruneAtRequiredGoalFailure (tests) where

import Control.Lens ((&))
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (runWriterT)
import qualified ControlledFixpoint.Engine as Engine
import Data.String (IsString (fromString))
import qualified Spec.Engine.Common as Common
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.PrettyPrint.HughesPJClass (prettyShow)

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "PruneAtRequiredGoalFailure"

tests :: TestTree
tests =
  testGroup
    "PruneAtRequiredGoalFailure"
    [ goldenVsString "ex1" (goldenDirpath </> "ex1" <.> "golden") do
        let cfg :: Engine.Config String String String
            cfg = Engine.Config {}
        (err_or_envs, msgs) <-
          Engine.run cfg
            & runExceptT
            & runWriterT
        return . fromString . prettyShow $ err_or_envs
    ]
