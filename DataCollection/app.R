require('shiny')
require('shinyTime')
#'@ require('rdrop2')
require('magrittr')
require('plyr')
require('dplyr')
require('stringr')
require('data.table')
#'@ require('rvest')
require('quantmod')
require('TFX')
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
                              tableOutput('fxdata'), 
                              checkboxInput('pause', 'Pause updates', FALSE))), 
                   tabPanel('Chart', 
                            h3('Real Time Chart'), 
                            p(strong(paste0('Current time (', zone, '):')),
                              textOutput('currentTime2')),
                            br(), 
                            plotOutput("plotBidPrice"), 
                            tags$hr(),
                            plotOutput("plotAskPrice")), 
                   tabPanel('Data', 
                            h3('Data Download'), 
                            p(strong(paste0('Current time (', zone, '):')),
                              textOutput('currentTime3')),
                            p('The time zone of data in GMT.'), 
                            dataTableOutput('fxDataTable'), 
                            p(strong('Refresh'), 'button will collect the latest dataset ', 
                              '(time unit in seconds).'), 
                            p('Please becareful, once you click on', 
                              strong('Reset'), 'button, ', 
                              'all data will be lost. Kindly download the dataset ', 
                              'as csv format prior to reset it.'), 
                            actionButton('refresh', 'Refresh', class = 'btn-primary'), 
                            downloadButton('downloadData', 'Download'), 
                            actionButton('reset', 'Reset', class = 'btn-danger')))), 
        
        tabPanel('Appendix', 
                 tabsetPanel(
                   tabPanel('Reference', 
                            h3('Speech'), 
                            p('I try to refer to the idea from below reference to create this web ', 
                              'application for data collection.'), 
                            p(HTML("<a href='https://beta.rstudioconnect.com/content/3138/'>Q1App2</a>"), 
                              '(', strong('Q1App2'), 'inside 2nd reference link at below', 
                              strong('Reference'), 'tab) for algorithmic trading. Kindly browse over', 
                              HTML("<a href='https://github.com/scibrokes/real-time-fxcm'>Real Time FXCM</a>"), 
                              'for more information about high frequency algorithmic trading.'), 
                            br(), 
                            h3('Reference'), 
                            p('01. ', HTML("<a href='https://github.com/cran/TFX'>TFX r package</a>")), 
                            p('02. ', HTML("<a href='https://www.fxcmapps.com/apps/basic-historical-data-downloader/'>Basic Historical Data Downloader</a>")), 
                            p('03. ', HTML("<a href='https://github.com/englianhu/binary.com-interview-question'>binary.com : Job Application - Quantitative Analyst</a>"))), 
                   
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
  
  fetchData <- reactive({
    if (!input$pause)
      invalidateLater(750)
    qtf <- QueryTrueFX()
    qtf %<>% mutate(TimeStamp = as.character(TimeStamp))
    names(qtf)[6] <- 'TimeStamp (GMT)'
    return(qtf)
  })
  
  output$fxdata <- renderTable({
    fetchData()
  }, digits = 5, row.names = FALSE)
  
  # Function to get new observations
  get_new_data <- function(){
    readLines('http://webrates.truefx.com/rates/connect.html')
    }
  
  ## ----------------- Start fxData ---------------------------
  # Initialize fxData
  fxData <<- get_new_data()
  
  # Function to update fxData, latest data will be showing upside.
  update_data <- function(){
    fxData <<- rbind(get_new_data(), fxData)#  %>% unique
    saveRDS(fxData, paste0(str_replace_all(now('GMT'), ':', 'T'), 'GMT.rds'))
    }
  
  output$plotBidPrice <- renderPlot({
    invalidateLater(1000, session)
    update_data()
    
    dt <- terms()
    if(nrow(dt) > 40) dt <- tail(dt, 40)
    
    ggplot(data = dt, aes(x = TimeStamp, y = Bid.Price, 
                               group = Symbol, colour = Symbol)) +
      geom_line() + geom_point( size = 4, shape = 21, fill = 'white') + 
      ggtitle('Real Time Graph 1 : Forex Bid Price')
    })
  
  output$plotAskPrice <- renderPlot({
    invalidateLater(1000, session)
    update_data()
    
    dt <- terms()
    if(nrow(dt) > 40) dt <- tail(dt, 40)
    
    ggplot(data = dt, aes(x = TimeStamp, y = Ask.Price, 
                               group = Symbol, colour = Symbol)) +
      geom_line() + geom_point( size = 4, shape = 21, fill = 'white') + 
      ggtitle('Real Time Graph 2 : Forex Ask Price')
  })
  ## ------------------ End fxData ----------------------------
  
  terms <- reactive({
    input$refresh
    
    if(any(file.exists(paste0(dir(pattern = '.rds'))))) {
      realData <<- llply(dir(pattern = '.rds'), readRDS)
      realData <<- do.call(rbind, realData) %>% unique
      realData <<- ldply(realData, ParseTrueFX) %>% unique
    }
  })
  
  # Downloadable csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('fxData.csv', sep = '')
    },
    content = function(file) {
      fwrite(terms(), file, row.names = FALSE)
    }
  )
  
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