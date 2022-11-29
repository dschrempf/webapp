{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Weather.Zamg
-- Description :  Zamg Rest interface
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov 28 09:14:33 2022.
module Weather.Zamg
  ( Station (..),
    showStation,
    showStationDetailed,
    parseStation,
    zamgDownloadData,
  )
where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function
import Data.Functor
import qualified Data.Text as TS
import Data.Time
import Data.Time.Format.ISO8601
import Network.HTTP.Simple
import Numeric.Natural

-- Download form:
--
-- https://dataset.api.hub.zamg.ac.at/app/station-new/historical/klima-v1-1d?anonymous=true
--
-- Request Url:
--
-- Base: https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-1d
-- Parameters: ?parameters=bewmit&parameters=nied&parameters=t
-- Date: &start=2022-10-01T00%3A00%3A00&end=2022-10-14T23%3A59%3A59
-- Station: &station_ids=5904
-- Output: &output_format=csv
--
data Station = HoheWarte | LinzStadt

showStation :: Station -> TS.Text
showStation HoheWarte = "Vienna, Austria"
showStation LinzStadt = "Linz, Austria"

showStationDetailed :: Station -> TS.Text
showStationDetailed HoheWarte = "Hohe Warte (ID 5904), Vienna, Austria"
showStationDetailed LinzStadt = "Linz Stadt (ID 3202), Linz, Austria"

parseStation :: MonadFail m => String -> m Station
parseStation "hohe_warte" = pure HoheWarte
parseStation "linz_stadt" = pure LinzStadt
parseStation x = fail x

getId :: Station -> Natural
getId HoheWarte = 5904
getId LinzStadt = 3202

showDay :: Day -> BS.ByteString
showDay d = BS.pack $ iso8601Show $ LocalTime d midnight

zamgDay :: Day -> Day -> Station -> Request
zamgDay a b s =
  defaultRequest
    & setRequestSecure True
    & setRequestHost "dataset.api.hub.zamg.ac.at"
    & setRequestPath "/v1/station/historical/klima-v1-1d"
    & setRequestPort 443
    & setRequestHeader "Accept" ["text/csv"]
    & setRequestQueryString q
  where
    ps = [("parameters", Just x) | x <- ["bewmit", "nied", "t"]]
    st = ("station_ids", Just $ BS.toStrict $ BB.toLazyByteString $ BB.intDec $ fromIntegral $ getId s)
    dt = [("start", Just $ showDay a), ("end", Just $ showDay b)]
    op = ("output_format", Just "csv")
    q = ps ++ dt ++ [st, op]

zamgDownloadData :: Day -> Day -> Station -> IO BL.ByteString
zamgDownloadData a b s = httpLBS r <&> getResponseBody
  where
    r = zamgDay a b s
