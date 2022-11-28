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
  )
where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B
import Data.Function
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
-- Filename &filename=TAG+Datensatz_20221001_20221014

data Station = HoheWarte | LinzStadt

getId :: Station -> Natural
getId HoheWarte = 5904
getId LinzStadt = 3202

formatLocalTime :: LocalTime -> String
formatLocalTime = iso8601Show

zamgDay :: LocalTime -> LocalTime -> Station -> Request
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
    st = ("station_ids", Just $ B.toStrict $ B.toLazyByteString $ B.intDec $ fromIntegral $ getId s)
    dt = [("start", Just $ B.pack $ iso8601Show a), ("end", Just $ B.pack $ iso8601Show b)]
    op = ("output_format", Just "csv")
    q = ps ++ dt ++ [st, op]
