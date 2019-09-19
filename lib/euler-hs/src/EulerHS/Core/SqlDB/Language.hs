
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module EulerHS.Core.SqlDB.Language where

import           EulerHS.Prelude

data SqlDBAction next where
  RawQuery :: String -> (a -> next) -> SqlDBAction next

instance Functor SqlDBAction where
  fmap f (RawQuery q next) = RawQuery q (f . next)

type SqlDB = F SqlDBAction

rawQuery' :: String -> SqlDB a
rawQuery' q = liftFC $ RawQuery q id
