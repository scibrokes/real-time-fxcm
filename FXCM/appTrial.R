#server.R
library(shiny)

server <- shinyServer(function(input, output) {
  
  observe({
    file1 = input$file1
    if (is.null(file1)) {
      return(NULL)
    }
    
    doc <- xmlParse(file1$datapath)
    docll <- xmlToList(doc)$Worksheet$Table
    
    ## https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists-in-r
    docllt <- do.call(c, unlist(docll, recursive = FALSE))
    doctbl <- ldply(docllt, data.frame) %>% tbl_df %>% .$text
    doctbl <- matrix(na.omit(unlist(doctbl)), nc = 12, byrow = TRUE) %>% tbl_df
    names(doctbl) <- doctbl[1,]
    doctbl <- doctbl[-1,]
    
    doctbl %<>% data.frame %>% tbl_df %>% 
      mutate(Date = as.POSIXct(strptime(str_replace_all(Date, 'T', ' '), 
                                        '%Y-%m-%d %H:%M:%S')), 
             EUR.USD.Open..Ask. = as.numeric(EUR.USD.Open..Ask.), 
             EUR.USD.High..Ask. = as.numeric(EUR.USD.High..Ask.), 
             EUR.USD.Low..Ask. = as.numeric(EUR.USD.Low..Ask.), 
             EUR.USD.Close..Ask. = as.numeric(EUR.USD.Close..Ask.), 
             EUR.USD.Open..Bid.. = as.numeric(EUR.USD.Open..Bid..), 
             EUR.USD.High..Bid.. = as.numeric(EUR.USD.High..Bid..), 
             EUR.USD.Low..Bid.. = as.numeric(EUR.USD.Low..Bid..), 
             EUR.USD.Close..Bid.. = as.numeric(EUR.USD.Close..Bid..), 
             MVA.EUR.USD.Open.7. = as.numeric(MVA.EUR.USD.Open.7.), 
             Transactions = as.numeric(Transactions), 
             Real.Volume = as.numeric(Real.Volume))
    
    
    output$plot <- renderTable({
      doctbl
    })
  })
})

#ui.R
# Define UI for random distribution application 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("ORR Simulator"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Select the XXX.xml file',
                accept = '.xml')),
      tags$hr()),
    mainPanel(
      tableOutput("plot")
    )
  )
)

shinyApp(server, ui)