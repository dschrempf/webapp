{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Weather.App
-- Description :  Render the forecast
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov 24 17:37:22 2022.
module Weather.App
  ( weatherApp,
  )
where

import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.RealFloat (FPFormat (..), formatRealFloat)
import qualified Data.Vector as V
import Graphics.Vega.VegaLite hiding (toHtml)
import Lucid
import Vega
import Weather.Forecast

renderPointWith :: T.Text -> DataPoint -> Html ()
renderPointWith rh (DataPoint _ c p t) = th <> td c <> td (fromPrecipitation p) <> td t
  where
    r = toHtml . toLazyText . formatRealFloat Fixed (Just 1)
    td x = td_ [style_ "text-align: right;"] (r x)
    th = th_ [style_ "text-align: left;"] (toHtml rh)

renderTable :: DataPoint -> DataPoint -> Html ()
renderTable p t =
  table_ $
    tr_ (th_ mempty <> th_ "Cloudiness [%]" <> th_ "Precipitation [mm]" <> th_ "Temperature [°C]")
      <> tr_ (renderPointWith "Predicted" p)
      <> tr_ (renderPointWith "Actual" t)

plotWeatherData :: WeatherData -> VegaLite
plotWeatherData (WeatherData xs) =
  toVegaLite
    [ dat [],
      vlConcat $ map asSpec specs
    ]
  where
    dat =
      dataFromColumns [Parse [("Date", FoDate "%Y-%m-%d")]]
        . dataColumn "Date" (Strings $ V.toList $ V.map _date xs)
        . dataColumn "Cloudiness" (Numbers $ V.toList $ V.map _cloudiness xs)
        . dataColumn "Precipitation" (Numbers $ V.toList $ V.map (fromPrecipitation . _precipitation) xs)
        . dataColumn "Temperature" (Numbers $ V.toList $ V.map _temperature xs)
    enc fld ttl =
      encoding
        . position X [PName "Date", PmType Temporal]
        . position Y [PName fld, PmType Quantitative, PTitle ttl]
    clouds = [mark Point [], enc "Cloudiness" "Cloudiness [%]" []]
    prec = [mark Point [], enc "Precipitation" "Precipitation [mm]" []]
    temp = [mark Point [], enc "Temperature" "Temperature [°C]" []]
    specs = [clouds, prec, temp]

renderForecast :: DataPoint -> DataPoint -> WeatherData -> Html ()
renderForecast p t x = embed "weather" (plotWeatherData x) <> p_ mempty <> renderTable p t

weatherApp :: IO (Html ())
weatherApp = do
  (p, t, xs) <- predictWeather
  pure $ renderForecast p t xs
