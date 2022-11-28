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
  ( WeatherApp (..),
    weatherApp,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.RealFloat (FPFormat (..), formatRealFloat)
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.Vector as V
import Graphics.Vega.VegaLite hiding (toHtml)
import Lucid
import Vega
import Weather.Data
import Weather.Forecast
import Weather.Zamg (Station (..), showStation, zamgDownloadData)
import Web.Scotty

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

form :: Day -> Html ()
form today =
  form_ [action_ "custom"] $ do
    p_ $ do
      label_ [for_ "start"] "Start date: "
      input_ [type_ "date", name_ "start", value_ "2022-10-01", max_ (T.pack $ iso8601Show maxStart)]
      br_ mempty
      label_ [for_ "end"] "End date: "
      input_ [type_ "date", name_ "end", value_ "2022-10-30", max_ (T.pack $ iso8601Show maxEnd)]
    p_ $ do
      "Weather station:"
      br_ mempty
      input_ [type_ "radio", id_ "hohe_warte", name_ "station", value_ "hohe_warte", checked_]
      label_ [for_ "hohe_warte"] "Hohe Warte (Vienna, Austria)"
      br_ mempty
      input_ [type_ "radio", id_ "linz_stadt", name_ "station", value_ "linz_stadt"]
      label_ [for_ "linz_stadt"] "Linz Stadt (Linz, Austria)"
    p_ $ do
      input_ [type_ "submit", name_ "submit"]
  where
    -- No Num instance, so we have to subtract days in a complicated way.
    maxEnd = iterate pred today !! 14
    maxStart = iterate pred maxEnd !! 7

renderForecast :: Day -> Station -> DataPoint -> DataPoint -> WeatherData -> Html ()
renderForecast today s p t x = do
  h1_ ("Weather forecast, " <> toHtml (showStation s))
  p_
    ( "This application downloads Austrian weather data (Zentralanstalt für Meteorologie und Geodynamik, "
        <> a_ [href_ "https://data.hub.zamg.ac.at/dataset/klima-v1-1d"] "ZAMG Data Hub"
        <> "), estimates some parameters using a Markov chain Monte Carlo sampler, and predicts the weather of the next day."
    )
  h2_ "Cloudiness, precipitation and temperature data"
  embed "weather" (plotWeatherData x)
  h2_ "Prediction"
  p_ ("The predicted and actual weather on the next day (" <> toHtml (_date t) <> "):")
  renderTable p t
  h2_ "Custom time period"
  p_ "The data (but not the prediction) of the default query is cached. Do you want to see a prediction for a different time period, or a different weather station?"
  form today

data WeatherApp
  = WAppDefault
  | WAppCustom {_startDate :: Day, _endDate :: Day, _station :: Station}

weatherApp :: WeatherApp -> ActionM (Html ())
weatherApp s = do
  d <- case s of
    WAppDefault -> liftIO $ BL.readFile "data/default-october2022-hohewarte.csv"
    (WAppCustom start end station)
      | end < start -> fail "End date is before start date."
      | end == start -> fail "Start date is end date."
      | otherwise -> liftIO $ zamgDownloadData start end station
  let st = case s of
        WAppDefault -> HoheWarte
        (WAppCustom _ _ station) -> station
  (p, t, xs) <- liftIO $ predictWeather d
  today <- utctDay <$> liftIO getCurrentTime
  pure $ renderForecast today st p t xs
