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
import qualified Data.Vector as VS
import Debug.Trace (traceShow)
import Graphics.Vega.VegaLite hiding (toHtml)
import Lucid
import Paths_webapp
import Vega
import Weather.Data
import Weather.Forecast
import Weather.Zamg (Station (..), showStation, showStationDetailed, zamgDownloadData)
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
  traceShow xs $
    toVegaLite
      [ dat [],
        vlConcat $ map asSpec specs
      ]
  where
    dat =
      dataFromColumns [Parse [("Date", FoDate "%Y-%m-%d")]]
        . dataColumn "Date" (Strings $ V.toList $ V.map (T.take 10 . date) xs)
        . dataColumn "Cloudiness" (Numbers $ V.toList $ V.map cloudiness xs)
        . dataColumn "Precipitation" (Numbers $ V.toList $ V.map (fromPrecipitation . precipitation) xs)
        . dataColumn "Temperature" (Numbers $ V.toList $ V.map temperature xs)
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
      input_ [type_ "submit", value_ "Predict weather"]
  where
    maxEnd = addDays (-14) today
    maxStart = addDays (-7) maxEnd

sIntro :: Station -> Html ()
sIntro s = do
  h1_ ("Weather forecast, " <> toHtml (showStation s))
  p_
    ( "This application downloads Austrian weather data (Zentralanstalt für Meteorologie und Geodynamik, "
        <> a_ [href_ "https://data.hub.zamg.ac.at/dataset/klima-v1-1d"] "ZAMG Data Hub"
        <> "), estimates some parameters using a Markov chain Monte Carlo sampler, and predicts the weather of the next day."
    )

ppDate :: (FormatTime t) => t -> Html ()
ppDate = toHtml . formatTime defaultTimeLocale "%B %e, %Y"

sData :: Day -> Day -> Station -> WeatherData -> Html ()
sData a b s x = do
  h2_ "Data"
  p_ $ do
    "We use cloudiness [%], precipitation [mm] and temperature [°C] data measured daily at the weather station "
    toHtml (showStationDetailed s)
    ". "
    "The time period of the training data ranges from "
    ppDate a
    ", to "
    ppDate b
    "."
  embed "weather" (plotWeatherData x)

sForecast :: Day -> DataPoint -> DataPoint -> Html ()
sForecast d p a = do
  h2_ "Forecast"
  p_ $ do
    "The data is used to estimate (or \"train\" as machine learning advocates would say) some parameters of a minimalist weather prediction model. A "
    a_ [href_ "https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo"] "Markov chain Monte Carlo"
    " sampler is used for this purpose. The estimation is fast and live, and happens when the page is loaded."
    " The weather model works well when the data exhibit linear trends, but fails when the data is periodic."
    " That is, I expect the prediction to be reasonably good when the time range is short (e.g., weeks or a few months), but I expect erroneous prediction when the time period spans a year or more."
  p_ $ do
    "The predicted and the actual weather on "
    ppDate d
    ", are:"
  renderTable p a

sCustom :: Day -> Html ()
sCustom today = do
  h2_ "Custom time period and weather station"
  p_ "The data of the default query is cached. The prediction is performed when the page is loaded. Do you want to see a prediction for a different time period, or a different weather station?"
  form today
  p_ "The ZAMG Data Hub is a bit slow to respond — expect a delay."

data WeatherApp
  = WAppDefault
  | WAppCustom {_startDate :: Day, _endDate :: Day, _station :: Station}

sAbout :: Html ()
sAbout = do
  h2_ "About"
  p_ $ do
    "This application was developed by "
    a_ [href_ "https://dschrempf.github.io/about/"] "Dominik Schrempf"
    ", and is a short proof of concept and stake."
  p_ $ do
    "The main elements of the Haskell tech stack are:"
    ul_ $ do
      li_ $ do
        a_ [href_ "https://hackage.haskell.org/package/scotty"] "Scotty"
        ", a web framework;"
      li_ $ do
        a_ [href_ "https://hackage.haskell.org/package/lucid"] "Lucid"
        ", a domain specific language for HTML;"
      li_ $ do
        a_ [href_ "https://hackage.haskell.org/package/mcmc"] "Mcmc"
        ", a Markov chain Monte Carlo sampler."
  p_ $ do
    "Other noteworthy components of this project:"
    ul_ $ do
      li_ $ do
        "the development environment is managed by the "
        a_ [href_ "https://github.com/NixOS/nix"] "Nix package manager"
        ";"
      li_ $ do
        "the application is deployed using a "
        a_ [href_ "https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html"] "Nix Flake"
        "."
  p_ $ do
    "For details, have a look at the "
    a_ [href_ "https://github.com/dschrempf/webapp"] "project source code"
    "."

parseZamgDate :: (MonadFail m) => String -> m Day
parseZamgDate = parseTimeM True defaultTimeLocale "%Y-%m-%dT%R%Ez"

weatherApp :: WeatherApp -> ActionM (Html ())
weatherApp s = do
  bs <- case s of
    WAppDefault -> do
      fn <- liftIO $ getDataFileName "data/default-october2022-hohewarte.csv"
      liftIO $ BL.readFile fn
    (WAppCustom start end station)
      | end < start -> fail "End date is before start date."
      | end == start -> fail "Start date is end date."
      | addDays 365 start < end -> fail "Please limit the time period to one year."
      | otherwise -> liftIO $ zamgDownloadData start end station
  liftIO $ BL.putStr bs
  let st = case s of
        WAppDefault -> HoheWarte
        (WAppCustom _ _ station) -> station
      (xs, x) = parseData bs
  a <- parseZamgDate $ T.unpack $ date $ VS.head $ getWeatherData xs
  b <- parseZamgDate $ T.unpack $ date $ VS.last $ getWeatherData xs
  d <- parseZamgDate $ T.unpack $ date x
  p <- liftIO $ predictWeather xs
  today <- utctDay <$> liftIO getCurrentTime
  pure $ do
    sIntro st
    sData a b st xs
    sForecast d p x
    sCustom today
    sAbout
