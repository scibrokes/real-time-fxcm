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

## ====================================================================
## read tick-data
llply(seq(1, week(now(tzone = 'UTC'))), function(i) {
  if(!dir.exists('./data/EURUSD/2017')) dir.create('./data/EURUSD/2017')
  if(!file.exists(paste0('./data/EURUSD/2017/', i, '.csv.gz'))) {
    tryCatch(download.file(paste0('https://tickdata.fxcorporate.com/EURUSD/2017/', i, '.csv.gz'), 
                  destfile = paste0('./data/EURUSD/2017/', i, '.csv.gz')), 
             error = function(e) NULL)
    
  } else {
    if(!file.exists(paste0('./data/EURUSD/2017/', i, '.csv'))) {
      gunzip(paste0('./data/EURUSD/2017/', i, '.csv.gz'), remove = FALSE)
    }
  }
})


fx <- llply(seq(1, week(now(tzone = 'UTC'))), function(i) {
  if(file.exists(paste0('./data/EURUSD/2017/', i, '.csv'))) {
    file = tryCatch(
      read.csv(paste0('./data/EURUSD/2017/', i, '.csv'), fileEncoding = 'UTF-16LE'), 
      error = function(e) NA)
    cat(paste0('\nRead.. ./data/EURUSD/2017/', i, '.csv'))
    file %>% tbl_df
    
  } else {
    cat(paste0('\n./data/EURUSD/2017/', i, '.csv does not exist.'))
  }
})

fx <- bind_rows(fx)

fread(paste0('./data/HDD Basic/', dir('./data/HDD Basic/')))


## webdriver
lnk <- 'https://tradingstation.fxcm.com/'
user <- 'D173140088'
pwd <- '8876'
token <- 'a9c1d19d1dbce3e5a3bd94ed21cb6c79eaf47978'

## ==============================================================
## min1 data
## ------------- eval=FALSE ---------------
#'@ names(cr_code) <- c('USDAUD', 'USDEUR', 'USDGBP', 'USDCHF', 'USDCAD', 'USDCNY', 'USDJPY')
yr <- c(2015, 2016, 2017, 2018)
pr <- c('m1', 'H1', 'D1')
cr_code %<>% .[-c(1,6)]

#pr %<>% .[1]
pr %<>% .[2]

## get currency dataset online.
while(TRUE) {
  ## https://www.epochconverter.com/years
  llply(names(cr_code), function(x) {
    dr <- paste0('data/', x, '/')
    llply(yr, function(i) {
      
      if (pr != 'D1') {
        if(i == 2015|i == 2017) wk <- 1:53 else wk <- 1:52
        if(i == 2018) wk <- 1:27
        llply(wk, function(j) {
          lnk <- paste0(
            'https://candledata.fxcorporate.com/', pr,'/', x, '/', i, '/', j, '.csv.gz')
          if(!dir.exists(dr)) dir.create(dr)
          if(!file.exists(paste0(dr, 'Y', i, 'W', j, '_', tolower(pr), '.csv.gz'))) {
            tryCatch(download.file(lnk, destfile = paste0(
              dr, 'Y', i, 'W', j, '_', tolower(pr), '.csv.gz')), error = function(e) NULL)
            Sys.sleep(3)
          }
        })
        
      } else {
        lnk <- paste0(
          'https://candledata.fxcorporate.com/', pr,'/', x, '/', i, '.csv.gz')
        if(!dir.exists(dr)) dir.create(dr)
        if(!file.exists(paste0(dr, 'Y', i, '.csv.gz'))) {
          tryCatch(download.file(lnk, destfile = paste0(
            dr, 'Y', i, '.csv.gz')), error = function(e) NULL)
          Sys.sleep(3)
        }
      }
    })
  })
}

## extract data.
while(TRUE) {
  ## https://www.epochconverter.com/years
  llply(names(cr_code), function(x) {
    dr <- paste0('data/', x, '/')
    llply(yr, function(i) {
      
      if (pr != 'D1') {
        if(i == 2015|i == 2017) wk <- 1:53 else wk <- 1:52
        llply(wk, function(j) {
          if (file.exists(paste0(dr, 'Y', i, 'W', j, '_', tolower(pr), '.csv.gz'))) {
            if (!file.exists(paste0(dr, 'Y', i, 'W', j, '_', tolower(pr), '.csv'))) {
              R.utils::gunzip(paste0(dr, 'Y', i, 'W', j, '_', tolower(pr), 
                                     '.csv.gz'), remove = FALSE)
              if (file.exists(paste0(dr, 'Y', i, 'W', j, '_', tolower(pr), '.csv'))) {
                cat(paste0(dr, 'Y', i, 'W', j, '_', tolower(pr), '.csv.gz extracted!\n'))
                file.remove(paste0(dr, 'Y', i, 'W', j, '_', tolower(pr), '.csv.gz'))
              }
              Sys.sleep(3)
            }
          }
        })
        
      } else {
        if (file.exists(paste0(dr, 'Y', i, '.csv.gz'))) {
          if (!file.exists(paste0(dr, 'Y', i, '.csv'))) {
            R.utils::gunzip(paste0(dr, 'Y', i, '.csv.gz'), remove = FALSE)
            if (file.exists(paste0(dr, 'Y', i, '.csv'))) {
              cat(paste0(dr, 'Y', i, '.csv.gz extracted!\n'))
              file.remove(paste0(dr, 'Y', i, '.csv.gz'))
            }
            Sys.sleep(3)
          }
        }
      }
    })
  })
}

## saved *.csv file as *.rds files after tidy the dataset to ease the time for reading.
llply(names(cr_code), function(x) {
  dr <- paste0('data/', x, '/')
  
  if (pr != 'D1') {
    fls <- dir(dr, pattern = paste0('_', tolower(pr), '.csv$'))
    
    for(i in seq(length(fls))) {
      nm <- str_replace_all(fls, paste0('_', tolower(pr), '.csv'), '')
      
      if(!file.exists(paste0(dr, nm[i], '_', tolower(pr), '.rds'))) {
        assign(nm[i], read.csv(paste0(dr, fls[i]), skipNul = TRUE) %>% tbl_df)
        
        ## save dataset.
        eval(parse(text = paste0("saveRDS(", nm[i], ", '", dr, nm[i], 
                                 "_", tolower(pr), ".rds')")))
        eval(parse(text = paste0("rm(", nm[i], ")")))
        if(file.exists(paste0(dr, nm[i], '_', tolower(pr), '.rds'))) {
          cat(paste0(dr, nm[i], '_', tolower(pr), '.rds saved!\n'))
          file.remove(paste0(dr, nm[i], '_', tolower(pr), '.csv'))
        }
      }
    }; rm(i, fls, nm)
    
  } else {
    fls <- dir(dr, pattern = '^Y[0-9]{4}.csv$')
    
    for(i in seq(length(fls))) {
      nm <- str_replace_all(fls, '.csv', '')
      
      if (!file.exists(paste0(dr, nm[i], '.rds'))) {
        assign(nm[i], read.csv(paste0(dr, fls[i]), skipNul = TRUE) %>% tbl_df)
        
        ## save dataset.
        eval(parse(text = paste0("saveRDS(", nm[i], ", '", dr, nm[i], ".rds')")))
        eval(parse(text = paste0("rm(", nm[i], ")")))
        if (file.exists(paste0(dr, nm[i], '.rds'))) {
          cat(paste0(dr, nm[i], '.rds saved!\n'))
          file.remove(paste0(dr, nm[i], '.csv'))
        }
      }
    }; rm(i, fls, nm)
  }
})


