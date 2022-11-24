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
import Graphics.Vega.VegaLite
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Vega
import Weather.Forecast

renderPointWith :: T.Text -> DataPoint -> H.Html
renderPointWith rh (DataPoint _ c p t) = th <> td c <> td (fromPrecipitation p) <> td t
  where
    r = H.toMarkup . toLazyText . formatRealFloat Fixed (Just 1)
    td x = H.td (r x) H.! A.style "text-align: right;"
    th = H.th (H.text rh) H.! A.style "text-align: left;"

renderTable :: DataPoint -> DataPoint -> H.Html
renderTable p t =
  H.table $
    H.tr (H.th mempty <> H.th "Cloudiness [%]" <> H.th "Precipitation [mm]" <> H.th "Temperature [°C]")
      <> H.tr (renderPointWith "Predicted" p)
      <> H.tr (renderPointWith "Actual" t)

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

renderForecast :: DataPoint -> DataPoint -> WeatherData -> H.Html
renderForecast p t x = embed "weather" (plotWeatherData x) <> H.p mempty <> renderTable p t

weatherApp :: IO H.Html
weatherApp = do
  (p, t, xs) <- predictWeather
  pure $ renderForecast p t xs
