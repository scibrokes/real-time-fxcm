library('shiny')
library('R.utils')
library('memoise')
library('magrittr')
library('plyr')
library('dplyr')
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

llply(seq(1, week(now())), function(i) {
  if(!dir.exists('./data/2017')) dir.create('./data/2017')
  if(!file.exists(paste0('./data/2017/', i, '.csv.gz'))) {
    download.file(paste0('https://tickdata.fxcorporate.com/EURUSD/2017/', i, '.csv.gz'), 
                  destfile = paste0('./data/2017/', i, '.csv.gz'))
  }
  if(!file.exists(paste0('./data/2017/', i, '.csv'))) {
    gunzip(paste0('./data/2017/', i, '.csv.gz'), remove = FALSE)
  }
})


llply(seq(1, week(now())), function(i) {
  tryCatch(fread(paste0('./data/2017/', i, '.csv')), error = function(e) NA)
})
