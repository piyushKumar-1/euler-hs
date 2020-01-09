module WebService.UUID
  ( getUUID32
  , getShortUUID
  , shortenUUID
  , enlargeUUID
  ) where

import EulerHS.Prelude

import WebService.AnyBase

import qualified Data.Text        as T
import qualified EulerHS.Language as L

filterHyphens :: Text -> Text
filterHyphens = T.filter (/= '-')

shortenUUID :: Text -> Maybe Text
shortenUUID = hex2Flickr . filterHyphens

enlargeUUID :: Text -> Maybe Text
enlargeUUID u =
  case flickr2Hex u of
    Just u' ->
      let
        zerosCount = 32 - (T.length u')
        zeros = T.replicate zerosCount "0"
        fullUuid = zeros <> u'
        idxs = [8,4,4,4,12]
      in
        Just $ T.intercalate "-" $ splitToUuidGroups idxs fullUuid
    Nothing -> Nothing

splitToUuidGroups :: [Int] -> Text -> [Text]
splitToUuidGroups [] _ = []
splitToUuidGroups (i:is) s =
  let
    (h,t) = T.splitAt i s
  in h : splitToUuidGroups is t

getUUID32 :: L.Flow Text
getUUID32 = filterHyphens <$> L.generateGUID

getShortUUID :: L.Flow Text
getShortUUID = fromJust . shortenUUID <$> L.generateGUID
