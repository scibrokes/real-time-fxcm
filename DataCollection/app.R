require('shiny')
#'@ require('rdrop2')
require('magrittr')
require('plyr')
require('dplyr')
require('stringr')
require('rvest')
require('quantmod')
require('lubridate')
require('ggplot2')
require('DT')

#'@ drop_auth()
## email : scibrokes_demo@gmail.com
## pass : trader888
#
# https://github.com/karthik/rdrop2
#
#'@ token <- drop_auth()
#'@ saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
#'@ token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#'@ drop_acc(dtoken = token)
#'@ token <<- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#'@ drop_acc(dtoken = token)

# === Data =====================================================
Sys.setenv(TZ = 'Asia/Tokyo')
zones <- attr(as.POSIXlt(now('Asia/Tokyo')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), zones[[1]])

# === UI =====================================================
ui <- shinyUI(fluidPage(
  
  titlePanel(
    tags$a(href='https://github.com/scibrokes', target='_blank', 
           tags$img(height = '120px', alt='HFT', #align='right', 
                    src='https://raw.githubusercontent.com/scibrokes/real-time-fxcm/master/www/HFT.jpg'))), 
  pageWithSidebar(
    mainPanel(
      tabsetPanel(
        tabPanel('Data Price', 
                 tabsetPanel(
                   tabPanel('Board', 
                            h3('Real Time Board'), 
                            p(strong(paste0('Current time (', zone, '):')),
                              textOutput('currentTime')),
                            br(), 
                            p(strong('Latest FX Quotes:'),
                              tableOutput('fxdata'))), 
                   tabPanel('Chart', 
                            h3('Real Time Chart'), 
                            p(strong(paste0('Current time (', zone, '):')),
                              textOutput('currentTime2')),
                            br(), 
                            plotOutput("plotEURUSD"), 
                            tags$hr(),
                            plotOutput("plotUSDJPY"), 
                            tags$hr(),
                            plotOutput("plotGBPUSD"), 
                            tags$hr(),
                            plotOutput("plotUSDCHF"), 
                            tags$hr(),
                            plotOutput("plotUSDCAD"), 
                            tags$hr(),
                            plotOutput("plotAUDUSD")), 
                   tabPanel('Data', 
                            h3('Data Download'), 
                            p(strong(paste0('Current time (', zone, '):')),
                              textOutput('currentTime3')),
                            p('The time zone of data in GMT.'), 
                            dataTableOutput('fxDataTable'), 
                            p('Please becareful, once you click on "Reset" button, ', 
                              'all data will be lost. Kindly download the dataset ', 
                              'as csv format prior to reset it.'), 
                            actionButton('refresh', 'Refresh', class = 'btn-primary'), 
                            actionButton('reset', 'Reset', class = 'btn-danger')))), 
        
        tabPanel('Appendix', 
                 tabsetPanel(
                   tabPanel('Reference', 
                            h3('Speech'), 
                            p('I try to refer to the idea from below reference to create this web ', 
                              'application for data collection.'), 
                            p('Kindly browse over', HTML("<a href='https://github.com/scibrokes/real-time-fxcm'>Real Time FXCM</a>"), 'for more information.'), 
                            br(), 
                            h3('Reference'), 
                            p('01. ', HTML("<a href='https://github.com/cran/TFX'>TFX r package</a>")), 
                            p('02. ', HTML("<a href='https://finance.yahoo.com/'>YAHOO! finance</a>")), 
                            p('03. ', HTML("<a href='https://github.com/scibrokes/real-time-fxcm'>Real Time FXCM</a>"))), 
                   
                   tabPanel('Author', 
                            h3('Author'), 
                            tags$iframe(src = 'https://beta.rstudioconnect.com/content/3091/ryo-eng.html', 
                                        height = 800, width = '100%', frameborder = 0)))))), 
    br(), 
    p('Powered by - Copyright® Intellectual Property Rights of ', 
      tags$a(href='http://www.scibrokes.com', target='_blank', 
             tags$img(height = '20px', alt='scibrokes', #align='right', 
                      src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
      HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>")))))

# === Server =====================================================
server <- shinyServer(function(input, output, session){
  
  output$currentTime <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(now('Asia/Tokyo'))
  })
  
  output$currentTime2 <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(now('Asia/Tokyo'))
  })
  
  output$currentTime3 <- renderText({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    as.character(now('Asia/Tokyo'))
  })
  
  output$fxdata <- renderTable({
    # Forces invalidation in 1000 milliseconds
    invalidateLater(1000, session)
    
    if(exists('realData')) {
      if(is.data.frame(realData) & (nrow(realData) > 0)) {
        tail(realData, 1) %>% tbl_df %>% mutate(
          `TimeStamp (GMT)` = format(`TimeStamp (GMT)`,'%Y-%m-%d %H:%M:%OS'))
      }
    }
  })
  
  # Function to get new observations
  get_new_data <- function(curr){
    data <- tryCatch(getSymbols('EURUSD=X', from = today('GMT'), 
                                auto.assign = FALSE), error=function(e){})
    if(!is.null(data)) {
      data %<>% Cl
      
      if(curr == 'EURUSD=X') {
        names(data) <- 'EURUSD'
        #'@ lnk <- 'https://finance.yahoo.com/quote/EURUSD=X?p=EURUSD=X'
      } else if(curr == 'JPY=X') {
        names(data) <- 'USDJPY'
        #'@ lnk <- 'https://finance.yahoo.com/quote/JPY=X?p=JPY=X'
      } else if(curr == 'GBPUSD=X') {
        names(data) <- 'GBPUSD'
        #'@ lnk <- 'https://finance.yahoo.com/quote/GBPUSD=X?p=GBPUSD=X'
      } else if(curr == 'CHF=X') {
        names(data) <- 'USDCHF'
        #'@ lnk <- 'https://finance.yahoo.com/quote/CHF=X?p=CHF=X'
      } else if(curr == 'CAD=X') {
        names(data) <- 'USDCAD'
        #'@ lnk <- 'https://finance.yahoo.com/quote/CAD=X?p=CAD=X'
      } else if(curr == 'AUDUSD=X') {
        names(data) <- 'AUDUSD'
        #'@ lnk <- 'https://finance.yahoo.com/quote/AUDUSD=X?p=AUDUSD=X'
      } else {
        stop('The selected currency is not in the range.')
      }
      
      #'@ msg <- lnk %>% read_html %>% html_nodes(xpath = '//*[@id="quote-market-notice"]/span') %>% html_text
      
      index(data) <- now('GMT')
      return(data)
    }
  }
  
  ## ----------------- Start EURUSD ---------------------------
  # Initialize EURUSD
  if(!file.exists('EURUSD.rds')) {
    EURUSD <<- get_new_data('EURUSD=X')
  } else {
    EURUSD <<- readRDS('EURUSD.rds')
  }
  
  # Function to update EURUSD, latest data will be showing upside.
  update_EURUSD <- function(){
    EURUSD <<- rbind(get_new_data('EURUSD=X'), EURUSD)#  %>% unique
    saveRDS(EURUSD, 'EURUSD.rds')
    return(EURUSD)
  }
  
  # Plot the 60 most recent values
  output$plotEURUSD <- renderPlot({
    invalidateLater(1000, session)
    update_EURUSD()
    
    ggplot() + geom_line(aes(x = index(EURUSD), y = coredata(EURUSD)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('EUR / USD')
  })
  ## ------------------ End EURUSD ----------------------------
  
  ## ----------------- Start USDJPY ---------------------------
  # Initialize USDJPY
  if(!file.exists('USDJPY.rds')) {
    USDJPY <<- get_new_data('JPY=X')
  } else {
    USDJPY <<- readRDS('USDJPY.rds')
  }
  
  # Function to update USDJPY, latest data will be showing upside.
  update_USDJPY <- function(){
    USDJPY <<- rbind(get_new_data('JPY=X'), USDJPY)#  %>% unique
    saveRDS(USDJPY, 'USDJPY.rds')
    return(USDJPY)
  }
  
  # Plot the 60 most recent values
  output$plotUSDJPY <- renderPlot({
    invalidateLater(1000, session)
    update_USDJPY()
    
    ggplot() + geom_line(aes(x = index(USDJPY), y = coredata(USDJPY)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('USD / JPY')
  })
  ## ------------------ End USDJPY ----------------------------
  
  ## ----------------- Start GBPUSD ---------------------------
  # Initialize GBPUSD
  if(!file.exists('GBPUSD.rds')) {
    GBPUSD <<- get_new_data('GBPUSD=X')
  } else {
    GBPUSD <<- readRDS('GBPUSD.rds')
  }
  
  # Function to update GBPUSD, latest data will be showing upside.
  update_GBPUSD <- function(){
    GBPUSD <<- rbind(get_new_data('GBPUSD=X'), GBPUSD)#  %>% unique
    saveRDS(GBPUSD, 'GBPUSD.rds')
    return(GBPUSD)
  }
  
  # Plot the 60 most recent values
  output$plotGBPUSD <- renderPlot({
    invalidateLater(1000, session)
    update_GBPUSD()
    
    ggplot() + geom_line(aes(x = index(GBPUSD), y = coredata(GBPUSD)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('GBP / USD')
  })
  ## ------------------ End GBPUSD ----------------------------
  
  ## ----------------- Start USDCHF ---------------------------
  # Initialize USDCHF
  if(!file.exists('USDCHF.rds')) {
    USDCHF <<- get_new_data('CHF=X')
  } else {
    USDCHF <<- readRDS('USDCHF.rds')
  }
  
  # Function to update USDCHF, latest data will be showing upside.
  update_USDCHF <- function(){
    USDCHF <<- rbind(get_new_data('CHF=X'), USDCHF)#  %>% unique
    saveRDS(USDCHF, 'USDCHF.rds')
    return(USDCHF)
  }
  
  # Plot the 60 most recent values
  output$plotUSDCHF <- renderPlot({
    invalidateLater(1000, session)
    update_USDCHF()
    
    ggplot() + geom_line(aes(x = index(USDCHF), y = coredata(USDCHF)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('USD / CHF')
  })
  ## ------------------ End USDCHF ----------------------------
  
  ## ----------------- Start USDCAD ---------------------------
  # Initialize USDCAD
  if(!file.exists('USDCAD.rds')) {
    USDCAD <<- get_new_data('CAD=X')
  } else {
    USDCAD <<- readRDS('USDCAD.rds')
  }
  
  # Function to update USDCAD, latest data will be showing upside.
  update_USDCAD <- function(){
    USDCAD <<- rbind(get_new_data('CAD=X'), USDCAD)#  %>% unique
    saveRDS(USDCAD, 'USDCAD.rds')
    return(USDCAD)
  }
  
  # Plot the 60 most recent values
  output$plotUSDCAD <- renderPlot({
    invalidateLater(1000, session)
    update_USDCAD()
    
    ggplot() + geom_line(aes(x = index(USDCAD), y = coredata(USDCAD)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('USD / CAD')
  })
  ## ------------------ End USDCAD ----------------------------
  
  ## ----------------- Start AUDUSD ---------------------------
  # Initialize AUDUSD
  if(!file.exists('AUDUSD.rds')) {
    AUDUSD <<- get_new_data('AUDUSD=X')
  } else {
    AUDUSD <<- readRDS('AUDUSD.rds')
  }
  
  # Function to update AUDUSD, latest data will be showing upside.
  update_AUDUSD <- function(){
    AUDUSD <<- rbind(get_new_data('AUDUSD=X'), AUDUSD)#  %>% unique
    saveRDS(AUDUSD, 'AUDUSD.rds')
    return(AUDUSD)
  }
  
  # Plot the 60 most recent values
  output$plotAUDUSD <- renderPlot({
    invalidateLater(1000, session)
    update_AUDUSD()
    
    ggplot() + geom_line(aes(x = index(AUDUSD), y = coredata(AUDUSD)), 
                         colour = 'blue') + xlab('Time [Seconds]') + ylab('AUD / USD')
  })
  ## ------------------ End AUDUSD ----------------------------
  
  terms <- reactive({
    input$refresh
    
    if(all(file.exists('EURUSD.rds'), 
           file.exists('USDJPY.rds'), 
           file.exists('GBPUSD.rds'), 
           file.exists('USDCHF.rds'), 
           file.exists('USDCAD.rds'), 
           file.exists('AUDUSD.rds'))) {
      realData <<- llply(dir(pattern = '.rds'), readRDS)
      realData <<- do.call(cbind, realData)
      if(!is.null(realData)) realData %<>% na.locf #%>% na.omit
      realData <<- realData %>% tbl_df %>% mutate(`TimeStamp (GMT)` = index(realData))
      realData <<- cbind(realData[ncol(realData)], realData[-ncol(realData)])
    } else {
      NULL
    }
  })
  
  observe({
    if(input$reset){
      do.call(file.remove, list(dir(pattern = '.rds')))
      rm(list = ls())
      stopApp('Delete all downloaded dataset!')
    }
  })
  
  output$fxDataTable <- renderDataTable({
    terms() %>% datatable(
      caption = "Table : Forex", 
      escape = FALSE, filter = "top", rownames = FALSE, 
      extensions = list("ColReorder" = NULL, "RowReorder" = NULL, 
                        "Buttons" = NULL, "Responsive" = NULL), 
      options = list(dom = 'BRrltpi', autoWidth = TRUE, scrollX = TRUE, 
                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                     ColReorder = TRUE, rowReorder = TRUE, 
                     buttons = list('copy', 'print', 
                                    list(extend = 'collection', 
                                         buttons = c('csv', 'excel', 'pdf'), 
                                         text = 'Download'), I('colvis'))))
  })
})

shinyApp(ui, server)