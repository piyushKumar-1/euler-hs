{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.DefaultDate where

import EulerHS.Prelude
import Data.Time



defaultDate =  LocalTime
  { localDay = toEnum 1 --   :: Day,
  , localTimeOfDay = defaultTimeOfDay --  :: TimeOfDay
  }

defaultTimeOfDay = TimeOfDay
  { todHour = 1  -- :: Int,-  range 0 - 23
  , todMin = 1   -- :: Int, --  range 0 - 59
  -- Note that 0 <= 'todSec' < 61, accomodating leap seconds.
  -- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
  , todSec  = 1  -- :: Pico
  }