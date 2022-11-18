library(tidyverse)
library(tidyquant)
library(corrplot)
library(ggtext)
library(shiny)

symbol1 <- read.csv("./symbol_list/nasdaq_screener_1667510028096.csv")
symbol2 <- read.csv("./symbol_list/nasdaq_screener_1667510197492.csv")
symbol3 <- read.csv("./symbol_list/nasdaq_screener_1667510230035.csv")
symbol <- rbind(symbol1,symbol2,symbol3)

ui <- fluidPage(
  # below are for one stock data request
  wellPanel(
    h4("Requesting Data from a single Stock"),
    selectInput('symbol','Stock Symbol',symbol$Symbol,multiple = FALSE),
    sliderInput('time','Select Time period',Sys.Date()-3650,Sys.Date(),value = c(Sys.Date()-1000,Sys.Date())),
    actionButton('get_data',"Request Data")
  ),
  fluidRow(
    column(9,
           wellPanel(
             plotOutput('linechart',brush = brushOpts("area", direction = "xy")),
             plotOutput('hist1',height = 100)
           )
    )),
  fluidRow(
    column(9 , 
           wellPanel(
             plotOutput('candlestickplot',hover = hoverOpts(id ="plot_hover", delay = 100)),
             plotOutput('hist2',height = 100))),
    column(3 , wellPanel(
      htmlOutput("text"),#, style = "font-size:25px;"
      tableOutput("select_table")))
  ),
  
  # below are for correlation calculation
  wellPanel(
    h4("Calculating Stocks correlation"),
    selectInput('sym_set','Select Stocks',symbol$Symbol,multiple = TRUE),
    sliderInput('com_date','Select comparing period',Sys.Date()-365,Sys.Date(),value = c(Sys.Date()-100,Sys.Date())),
    actionButton('do_cor',"Calculating Stocks correlation")
  ),
  wellPanel(
    plotOutput('cor_plot')
  )
)