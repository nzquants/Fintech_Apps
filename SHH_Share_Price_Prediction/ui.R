library(shiny)
library(dygraphs)
library(shinycssloaders)

symbols<-read.csv("SHH.csv",sep=",")
symbols$Symbol<-symbols$Ticker


fluidPage(
  titlePanel("智能股票走势预测"),
  fluidRow(
    column(12, wellPanel(
      selectizeInput("sym", "选择股票代码", symbols$Symbol,options = list(maxOptions=100000)),
       h3("模型参数设置"), 
      sliderInput("lookback", "选择用于训练预测模型的时间（月数）:", min = 12, max = 60, value = 12, step = 1),
      sliderInput("forward", "选择向前预测的时间（月数）:", min = 1, max = 12, value = 1, step = 1),
      
    #  selectInput("sym", "Symbol (Yahoo Finance!)", c("Adalta Limited"="1AD.AX")),
  #  selectInput("sym", "Symbol (Yahoo Finance!)", as.vector(rbind(as.character(symbols$Symbol),as.character(symbols$Description)))),
      checkboxInput("seasonal","加入年度变动？", TRUE)
    )),
    column(12,
           h3("日收盘价预测"),br(),
           dygraphOutput("plot1") %>% withSpinner(),
           h3("日最高价预测"),br(),
           dygraphOutput("plot2") %>% withSpinner(),
           h3("日最低价预测"),br(),
           dygraphOutput("plot3") %>% withSpinner(),
           h3("日成交量预测"),br(),
           dygraphOutput("plot4") %>% withSpinner()
    )
  )
)