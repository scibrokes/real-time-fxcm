# Real Time FXCM

## 1. Data

You can get the tick data via [**FXCMTickData**](https://github.com/FXCMAPI/FXCMTickData).

  FXCM is proud to continue the drive for more openness in data.

  We now have an open repository of tick data for certain instruments starting from January 4th 2015. The tick data repository is free for unlimited individual usaage and will be updated once a week.

  The following instruments are available to download.

  - AUDCAD, AUDCHF, AUDJPY
  - AUDNZD, CADCHF, EURAUD
  - EURCHF, EURGBP, EURJPY
  - EURUSD, GBPCHF, GBPJPY
  - GBPNZD, GBPUSD, NZDCAD
  - NZDCHF, NZDJPY, NZDUSD
  - USDCAD, USDCHF, USDJPY

  To help with the downloading of data we have two python examples(Python 2.7 and Python 3.4).

  Files are stored in our public directory with the following path URL

`tickdata.fxcorporate.com/{instrument}/{year}/{int of week of year}.csv.gz`

  For example if we want to get the first week of 2015 for EURUSD the URL would be

<https://tickdata.fxcorporate.com/EURUSD/2015/1.csv.gz>

*All timestamps are in UTC.*

## 2. Web Application


