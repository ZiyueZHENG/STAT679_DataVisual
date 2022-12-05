library(tidyverse)
library(tidyquant)
library(corrplot)
library(ggtext)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

symbol <- read.csv("./symbol_list/symbols.csv")

header <- dashboardHeader(title = "Stock Data Visual")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Single stock", tabName = "data"),
    menuItem("Table", tabName = "table"),
    menuItem("Multiple stocks", tabName = "cor")
  )
)
<<<<<<< HEAD

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "data",
            fluidPage(
              # below are for one stock data request
              wellPanel(
                h4("Requesting Data from a single Stock"),
                selectInput('symbol','Stock Symbol',symbol$Symbol,multiple = FALSE,selected = 'AACG'),
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
              )
            )
            
    ),
    tabItem(tabName = "table",
            fixedPage(htmlOutput("text1", style = "font-size:25px;"), dataTableOutput('table'))
    ),   
    tabItem(tabName = "cor",
            # below are for correlation calculation
            fluidPage(
            wellPanel(
              h4("Calculating Stocks correlation"),
              selectInput('sym_set','Select Stocks',symbol$Symbol,multiple = TRUE,selected = c('AACG','AACI')),
              sliderInput('com_date','Select comparing period',Sys.Date()-365,Sys.Date(),value = c(Sys.Date()-100,Sys.Date())),
              actionButton('do_cor',"Calculating Stocks correlation")
            ),
            wellPanel(
              plotOutput('cor_plot'),
              plotOutput('hor_plot')
            ))
    )
  )
)


ui <- dashboardPage(header,
                    sidebar, 
                    body,
                    controlbar = NULL)
=======
>>>>>>> 747b81dd102367e2d1cc43069977af6489bda12a
