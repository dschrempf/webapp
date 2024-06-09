{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Weather.Forecast
-- Description :  Forecast weather
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov 24 11:53:58 2022.
module Weather.Forecast
  ( predictWeather,
  )
where

import Control.Lens
import Data.Aeson.TH
import qualified Data.Text as T
import qualified Data.Vector as V
import Mcmc
import Mcmc.Chain.Chain hiding (start)
import Mcmc.Chain.Link
import Mcmc.Chain.Trace
import Numeric.AD.Double
import System.Random (newStdGen)
import Weather.Data

-- | The state of the Markov chain is a set of parameters used to describe the
-- weather.
--
-- The type parameter will be instantiated to 'Double', but 'mcmc' can also find
-- proposals using automatic differentation which requires a more general type.
data IG a = IG
  { -- Mean, and standard deviation of cloudiness.
    _cMean :: !a,
    _cStdDev :: !a,
    -- Precipitation jump probability, mean and standard deviation.
    _pJumpProb :: !a,
    _pMean :: !a,
    _pStdDev :: !a,
    -- Temperature mean and standard deviation.
    _tMean :: !a,
    _tStdDev :: !a
  }
  deriving (Show)

-- We use JSON to store the trace of the Markov chain.
$(deriveJSON defaultOptions ''IG)

-- Proposals changing individual values use lenses to modify the state.
makeLenses ''IG

type I = IG Double

-- | Initial state.
i0 :: I
i0 = IG 0 1.0 0.5 0 1.0 0 1.0

-- | The prior function.
--
-- > type PriorFunctionG a b = a -> Log b
pr :: (RealFloat a) => PriorFunctionG (IG a) a
pr (IG cm cs pj pm ps tm ts) =
  product'
    [ normal 0.0 10.0 cm,
      exponential 10.0 cs,
      uniform 0 1 pj,
      normal 0.0 10.0 pm,
      exponential 10.0 ps,
      normal 0.0 10.0 tm,
      exponential 10.0 ts
    ]

-- | The likelihood of the precipitation is composed of parts with and without
-- precipitation.
lhPrec ::
  (Scalar a ~ Double, RealFloat a, Mode a) =>
  -- | Jump probability.
  a ->
  -- | Mean.
  a ->
  -- | Standard deviation.
  a ->
  Precipitation ->
  Precipitation ->
  Log a
lhPrec pj _ _ NoPrecipitation NoPrecipitation = Exp $ 1.0 - pj
lhPrec pj pm ps NoPrecipitation (PrecipitationAmount x') = Exp pj * normal pm ps (auto x')
lhPrec pj pm ps (PrecipitationAmount x) (PrecipitationAmount x') = Exp (1.0 - pj) * normal pm ps (auto $ x' - x)
lhPrec pj pm ps (PrecipitationAmount x) NoPrecipitation = Exp pj * normal pm ps (auto x)

-- | For a given set of parameters, calculate the likelihood of observing
-- weather data for two subsequent days.
lhStep ::
  (Scalar a ~ Double, RealFloat a, Mode a) =>
  IG a ->
  -- | Observed weather data at day X.
  DataPoint ->
  -- | Observed weather data at day (X+1).
  DataPoint ->
  Log a
lhStep (IG cm cs pj pm ps tm ts) (DataPoint _ c p t) (DataPoint _ c' p' t') =
  product'
    [ normal cm cs (auto $ c' - c),
      lhPrec pj pm ps p p',
      normal tm ts (auto $ t' - t)
    ]

-- | The likelihood function.
--
-- > type LikelihoodFunctionG a b = a -> Log b
lh :: (Scalar a ~ Double, RealFloat a, Mode a) => WeatherData -> LikelihoodFunctionG (IG a) a
lh (WeatherData xs) x = V.product $ V.zipWith (lhStep x) xs (V.tail xs)

cc :: Cycle I
cc =
  cycleFromList
    [ cMean @~ slideSymmetric 1.0 (PName "cMean") (pWeight 1) Tune,
      cStdDev @~ scaleUnbiased 1.0 (PName "cStdDev") (pWeight 1) Tune,
      pJumpProb @~ scaleUnbiased 1.0 (PName "pJump") (pWeight 1) Tune,
      pMean @~ slideSymmetric 1.0 (PName "pMean") (pWeight 1) Tune,
      pStdDev @~ scaleUnbiased 1.0 (PName "pStdDev") (pWeight 1) Tune,
      tMean @~ slideSymmetric 1.0 (PName "tMean") (pWeight 1) Tune,
      tStdDev @~ scaleUnbiased 1.0 (PName "tStdDev") (pWeight 1) Tune
    ]

-- | Monitor some values to standard output.
monStd :: MonitorStdOut I
monStd =
  monitorStdOut
    [ _cMean >$< monitorDouble "cMean",
      _pMean >$< monitorDouble "pMean",
      _tMean >$< monitorDouble "tMean"
    ]
    3

mon :: Monitor I
mon = Monitor monStd [] []

nIterations :: Int
nIterations = 2000

getMean :: V.Vector I -> I
getMean xs = normalizeI $ V.foldl1' addI xs
  where
    combine f (IG x0 x1 x2 x3 x4 x5 x6) (IG y0 y1 y2 y3 y4 y5 y6) =
      IG
        (f x0 y0)
        (f x1 y1)
        (f x2 y2)
        (f x3 y3)
        (f x4 y4)
        (f x5 y5)
        (f x6 y6)
    addI = combine (+)
    n1 = recip $ fromIntegral $ V.length xs
    nI = IG n1 n1 n1 n1 n1 n1 n1
    normalizeI = combine (*) nI

predictPrecipitation :: Precipitation -> Double -> Double -> Double -> Precipitation
predictPrecipitation NoPrecipitation pj pm ps
  | Exp pj * normal pm ps pm > Exp (1 - pj) = PrecipitationAmount pm
  | otherwise = NoPrecipitation
predictPrecipitation (PrecipitationAmount x) pj pm ps
  | Exp pj > normal pm ps x' * Exp (1 - pj) = NoPrecipitation
  | otherwise = PrecipitationAmount x'
  where
    x' = x + pm

predictWithDate :: T.Text -> DataPoint -> I -> DataPoint
predictWithDate dt (DataPoint _ c p t) (IG cm _ pj pm ps tm _) = DataPoint dt c'' p' t'
  where
    c' = c + cm
    c''
      | c' < 0 = 0
      | c' > 100 = 100
      | otherwise = c'
    p' = predictPrecipitation p pj pm ps
    t' = t + tm

predictWeather :: WeatherData -> IO DataPoint
predictWeather d = do
  g <- newStdGen
  -- Settings of the Metropolis-Hastings-Green (MHG) algorithm.
  let s =
        Settings
          (AnalysisName "WeatherForecast")
          (BurnInWithAutoTuning 1000 100)
          (Iterations nIterations)
          (TraceMinimum nIterations)
          Overwrite
          Sequential
          NoSave
          LogStdOutOnly
          Info
  -- Use the MHG algorithm.
  a <- mhg s pr (lh d) cc mon i0 g
  -- Run the MCMC sampler.
  c <- mcmc s a
  -- Post process.
  tr <- takeT nIterations $ trace $ fromMHG c
  let xs = V.map state tr
      m = getMean xs
      dl = V.last $ getWeatherData d
  print m
  let p = predictWithDate (date dl) dl m
  pure p
