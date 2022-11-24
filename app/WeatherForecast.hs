{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  WeatherForecast
-- Description :  Forecast Austrian weather
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov 24 11:53:58 2022.
module WeatherForecast
  (
  )
where

import qualified Data.Vector as V
import Mcmc
import Numeric.AD.Double

data Precipitation = NoPrecipitation | PrecipitationAmount Double

-- constrainPrecipitation :: RealFloat a => PriorFunctionG (Precipitation a) a
-- constrainPrecipitation NoPrecipitation = 1.0
-- constrainPrecipitation (PrecipitationAmount x) = positive x

data DataPoint = DataPoint
  { cloudiness :: Double,
    preciptiation :: Precipitation,
    temperature :: Double
  }

-- constrainDataPoint :: RealFloat a => PriorFunctionG (DataPoint a) a
-- constrainDataPoint (DataPoint c p t) =
--   product'
--     [ bounded 0 100 c,
--       constrainPrecipitation p
--     ]
-- {-# SPECIALIZE constrainDataPoint :: PriorFunctionG (DataPoint Double) Double #-}

newtype WeatherData = WeatherData (V.Vector DataPoint)

data I a = I
  { cMean :: a,
    cStdDev :: a,
    pJumpProb :: a,
    pMean :: a,
    pStdDev :: a,
    tMean :: a,
    tStdDev :: a
  }

pr :: RealFloat a => PriorFunctionG (I a) a
pr (I cm cs pj pm ps tm ts) =
  product'
    [ exponential 1.0 (abs cm),
      exponential 1.0 cs,
      uniform 0 1 pj,
      exponential 1.0 (abs pm),
      exponential 1.0 ps,
      exponential 1.0 (abs tm),
      exponential 1.0 ts
    ]

lhPrec :: (Scalar a ~ Double, RealFloat a, Mode a) => a -> a -> a -> Precipitation -> Precipitation -> Log a
lhPrec pj _ _ NoPrecipitation NoPrecipitation = Exp $ 1.0 - pj
lhPrec pj pm ps NoPrecipitation (PrecipitationAmount x') = Exp pj * normal pm ps (auto x')
lhPrec pj pm ps (PrecipitationAmount x) (PrecipitationAmount x') = Exp (1.0 - pj) * normal pm ps (auto $ x' - x)
lhPrec pj pm ps (PrecipitationAmount x) NoPrecipitation = Exp pj * normal pm ps (auto x)

lhStep :: (Scalar a ~ Double, RealFloat a, Mode a) => I (Mean a) -> DataPoint -> DataPoint -> Log a
lhStep (I cm cs pj pm ps tm ts) (DataPoint c p t) (DataPoint c' p' t') =
  product'
    [ normal cm cs (auto $ c' - c),
      lhPrec pj pm ps p p',
      normal tm ts (auto $ t' - t)
    ]

-- lh :: RealFloat a => WeatherData a -> LikelihoodFunctionG (I a) a
lh :: (Scalar a ~ Double, RealFloat a, Mode a) => WeatherData -> I (Mean a) -> Log a
lh (WeatherData xs) x = V.product $ V.zipWith (lhStep x) xs (V.tail xs)
