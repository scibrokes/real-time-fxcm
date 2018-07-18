library('data.table')
library('readr')

W25 <- fread('W25.csv')
W26 <- fread('W26.csv')
W27 <- fread('W27.csv')

W25 <- read.table('W25.txt', header = TRUE)
W26 <- read.table('W26.txt', header = TRUE)
W27 <- read.table('W27.txt', header = TRUE)

## ============================================================
library('plyr')
library('dplyr')
library('purrr')
library('magrittr')
library('lubridate')
library('data.table')
library('R.utils')

## get currency dataset online.
yr <- c(2015, 2016, 2017, 2018)
wk <- 1:53

## https://www.epochconverter.com/years
llply(yr, function(i) {
  if(i == 2015|i == 2017) wk <- 1:53 else wk <- 1:52
  llply(wk, function(j) {
    lnk <- paste0(
      'https://tickdata.fxcorporate.com/USDJPY/', i, '/', j, '.csv.gz')
    if(!dir.exists('data/USDJPY')) dir.create('data/USDJPY')
    if(!file.exists(paste0('data/USDJPY/Y', i, 'W', j, '.csv.gz'))) {
      download.file(lnk, destfile = paste0(
        'data/USDJPY/Y', i, 'W', j, '.csv.gz'))
      #cat(paste0('data/USDJPY/Y', i, 'W', j, '.csv.gz downloaded!\n'))
    }
  })
})

llply(yr, function(i) {
  if(i == 2015|i == 2017) wk <- 1:53 else wk <- 1:52
  llply(wk, function(j) {
    if(file.exists(paste0('data/USDJPY/Y', i, 'W', j, '.csv.gz')) & 
       !file.exists(paste0('data/USDJPY/Y', i, 'W', j, '.csv'))) {
      R.utils::gunzip(paste0('data/USDJPY/Y', i, 'W', j, '.csv.gz'), 
                      remove = FALSE)
      cat(paste0('data/USDJPY/Y', i, 'W', j, '.csv.gz extracted!\n'))
    }
  })
})

## remove all files size less than 1MB
if(any(file.exists(paste0(
  'data/USDJPY/', dir('data/USDJPY', pattern = '.csv')[file.size(paste0(
    'data/USDJPY/', dir('data/USDJPY', pattern = '.csv'))) <= 1000000])))) {
  file.remove(paste0(
    'data/USDJPY/', dir('data/USDJPY', pattern = '.csv')[file.size(paste0(
      'data/USDJPY/', dir('data/USDJPY', pattern = '.csv'))) <= 1000000]))
  }



