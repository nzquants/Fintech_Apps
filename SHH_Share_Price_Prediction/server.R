#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(prophet)
library(quantmod)
library(data.table)
library(dplyr)
library(dygraphs)
library(shinycssloaders)

dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("plotters/barchart.js",package = "dygraphs"))
}

dyMultiColumn <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "MultiColumn",
            path = system.file("plotters/multicolumn.js",
                               package = "dygraphs"))
}

dyCrosshair <- function(dygraph, 
                        direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("plugins/crosshair.js", 
                       package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

function(input, output,session) {
  
  data <- reactive({
    
    getSymbols(input$sym, src = "yahoo", from = Sys.Date()-(input$lookback*30), to = Sys.Date(), auto.assign = FALSE)
    
    
  }) 
  
  
  output$plot1 <- renderDygraph({
 #   mydf <- getSymbols(input$sym, src = "yahoo", from = Sys.Date()-(input$lookback*30), to = Sys.Date(), auto.assign = FALSE)
     mydf <- data()   
     mydf <- data.frame(mydf[,6])
    mydf <- copy(mydf)
    setDT(mydf, keep.rownames = TRUE)
    colnames(mydf)<- c("ds", "y")
    m <- prophet(mydf,yearly.seasonality = input$seasonal,  daily.seasonality=TRUE, weekly.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = input$forward*30)
    forecast <- predict(m, future)
    g1 <- dyplot.prophet(m, forecast) %>%
      dyAxis("y", label = "股票价格（元）") %>% 
      dyCrosshair(direction = "vertical")
    
    
    g1
  })
  
  
  output$plot2 <- renderDygraph({
    #   mydf <- getSymbols(input$sym, src = "yahoo", from = Sys.Date()-(input$lookback*30), to = Sys.Date(), auto.assign = FALSE)
    mydf <- data()   
    mydf <- data.frame(mydf[,2])
    mydf <- copy(mydf)
    setDT(mydf, keep.rownames = TRUE)
    colnames(mydf)<- c("ds", "y")
    m <- prophet(mydf,yearly.seasonality = input$seasonal,  daily.seasonality=TRUE, weekly.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = input$forward*30)
    forecast <- predict(m, future)
    g2 <- dyplot.prophet(m, forecast) %>%
      dyAxis("y", label = "股票价格（元）") %>% 
      dyCrosshair(direction = "vertical")
    
    
    g2
  })
  
  
  output$plot3 <- renderDygraph({
    #   mydf <- getSymbols(input$sym, src = "yahoo", from = Sys.Date()-(input$lookback*30), to = Sys.Date(), auto.assign = FALSE)
    mydf <- data()   
    mydf <- data.frame(mydf[,3])
    mydf <- copy(mydf)
    setDT(mydf, keep.rownames = TRUE)
    colnames(mydf)<- c("ds", "y")
    m <- prophet(mydf,yearly.seasonality = input$seasonal,  daily.seasonality=TRUE, weekly.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = input$forward*30)
    forecast <- predict(m, future)
    g3 <- dyplot.prophet(m, forecast) %>%
      dyAxis("y", label = "股票价格（元）") %>% 
      dyCrosshair(direction = "vertical")
    
    
    g3
  })
  
  
  
  
  output$plot4 <- renderDygraph({
    #   mydf <- getSymbols(input$sym, src = "yahoo", from = Sys.Date()-(input$lookback*30), to = Sys.Date(), auto.assign = FALSE)
    mydf <- data()   
    mydf <- data.frame(mydf[,5])/10^6
    mydf <- copy(mydf)
    setDT(mydf, keep.rownames = TRUE)
    colnames(mydf)<- c("ds", "y")
    m <- prophet(mydf,yearly.seasonality = input$seasonal,  daily.seasonality=TRUE, weekly.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = input$forward*30)
    forecast <- predict(m, future)
    g3 <- dyplot.prophet(m, forecast) %>%
      dyAxis("y", label = "成交量（百万）") %>%
      dyMultiColumn() %>% 
      dyCrosshair(direction = "vertical")
    
    
    g3
  })
  
  
  
  
}