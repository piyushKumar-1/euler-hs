module EulerHS.Core.Types.Common
  (
    -- * Guid for a forked flow
    ForkGUID
    -- * Guid for a safe flow
  , SafeFlowGUID
    -- * Network manager selector
  , ManagerSelector
    -- * Description type
  , Description
    -- * A variable for await results from a forked flow
  , Awaitable (..)
  , Microseconds (..)
  ) where

import           EulerHS.Prelude
import qualified Data.Word as W

type ForkGUID = Text
type SafeFlowGUID = Text
type ManagerSelector = String
type Description = Text
data Awaitable s = Awaitable (MVar s)
data Microseconds = Microseconds W.Word16