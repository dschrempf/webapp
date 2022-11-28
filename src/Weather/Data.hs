{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Weather.Data
-- Description :  Data types
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov 28 11:59:08 2022.
module Weather.Data
  ( Precipitation (..),
    precipitation,
    fromPrecipitation,
    DataPoint (..),
    dataPoint,
    WeatherData (..),
    parseData,
  )
where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Maybe
import qualified Data.Text as TS
import qualified Data.Vector as VS
import GHC.Generics
import Numeric.Natural

data Precipitation = NoPrecipitation | PrecipitationAmount Double
  deriving (Show)

precipitation :: Maybe Double -> Precipitation
precipitation Nothing = NoPrecipitation
precipitation (Just x)
  | x <= 0 = NoPrecipitation
  | otherwise = PrecipitationAmount x

fromPrecipitation :: Precipitation -> Double
fromPrecipitation NoPrecipitation = 0
fromPrecipitation (PrecipitationAmount x) = x

data DataPointRaw = DataPointRaw
  { station :: Natural,
    date :: TS.Text,
    cloudinessRaw :: Maybe Double,
    preicpitationRaw :: Maybe Double,
    temperatureRaw :: Double
  }
  deriving (Generic)

instance FromRecord DataPointRaw

data DataPoint = DataPoint
  { _date :: TS.Text,
    _cloudiness :: Double,
    _precipitation :: Precipitation,
    _temperature :: Double
  }
  deriving (Show)

dataPoint :: DataPointRaw -> DataPoint
dataPoint (DataPointRaw _ d cr pr tr) = DataPoint d (fromMaybe 0 cr) (precipitation pr) tr

newtype WeatherData = WeatherData {getWeatherData :: VS.Vector DataPoint}

parseData :: BL.ByteString -> (WeatherData, DataPoint)
parseData b =
  let d = either error id $ decode HasHeader b
      (xs, x) =
        fromMaybe (error "readSampleData: empty vector") $
          VS.unsnoc $
            VS.map dataPoint d
   in (WeatherData xs, x)
