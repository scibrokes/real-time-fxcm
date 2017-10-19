library('shiny')
library('memoise')
library('magrittr')
library('stringr')
library('TFX')
library('quantmod')
library('QuantTools')
library('rugarch')
library('lubridate')
library('formattable')

symbols = c('EURUSD.FXCM', 'GBPUSD.FXCM', 'USDCHF.FXCM', 'USDJPY.FXCM')
from = round_date(now(), unit = '5 minutes') %m-% months(1)
to = round_date(now(), unit = '5 minutes')
path = paste(getwd(), 'data', 'finam', sep = '/' )
start_date = '2017-01-01'

settings = list(
  finam_storage = path,
  finam_storage_from = start_date,
  finam_symbols = symbols)
QuantTools_settings(settings)

store_finam_data()
get_iqfeed_data(symbol, from, to, period = '5min', local = TRUE)



