library(tidyverse)
library(tidyquant)
library(corrplot)

symbol1 <- read.csv("./symbol_list/nasdaq_screener_1667510028096.csv")
symbol2 <- read.csv("./symbol_list/nasdaq_screener_1667510197492.csv")
symbol3 <- read.csv("./symbol_list/nasdaq_screener_1667510230035.csv")
symbol <- rbind(symbol1,symbol2,symbol3)

# shiny
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
        ),
      wellPanel(
        plotOutput('candlestickplot',hover = hoverOpts(id ="plot_hover", delay = 0)),
        plotOutput('hist2',height = 100)
      )
    ),
    tableOutput("hover_info")
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

server <- function(input,output){
  # below are for one stock data request
  observeEvent(input$get_data,{
    df <- getSymbols(input$symbol,from = input$time[1],to = input$time[2],warnings = FALSE,auto.assign = FALSE)
    df <- as.data.frame(df)
    names(df)[1:6] <- c("open","high","low","close","volume","adjusted")
    df <- df %>%
      mutate(date = as.Date(rownames(df)),return_percent = (close-open)*100/open)
    
    selection <- reactiveVal(rep(TRUE, nrow(df)))
    observeEvent(
      input$area,
      selection(brushedPoints(df, input$area, allRows = TRUE)$selected_)
    )
    
    selected_data <- reactive({
      df %>%
        mutate(
          selected = selection(),
          selected = factor(selected,levels = c("TRUE", "FALSE"))
        )
    })
    
    output$linechart <- renderPlot({
      df%>%
        ggplot(aes(x=date,y=close))+
        geom_line() 
    })
    
    output$hist1 <- renderPlot({
      df%>%
        ggplot(aes(x=date,y=volume,col=(return_percent>0)))+
        geom_bar(stat='identity', show.legend = FALSE) 
    })
    
    output$candlestickplot <- renderPlot({
      filter(selected_data(), selection())%>%
        ggplot(aes(x=date,y=close))+
        geom_candlestick(aes(open = open,high=high,low=low,close=close))
    })
    
    output$hist2 <- renderPlot({
      filter(selected_data(), selection())%>%
        ggplot(aes(x=date,y=volume,col=(return_percent>0)))+
        geom_bar(stat='identity', show.legend = FALSE)
    })
    
    output$hover_info <- renderTable({
      hover <- round(input$plot_hover$x)
      df[as.numeric(df$date) == hover,]
    })
  })
  
  # below are for correlation calculation
  observeEvent(input$do_cor,{
    name_set <- input$sym_set
    n <- length(name_set)
    for(i in 1:n){
      temp <- getSymbols(input$sym_set[i],from = input$com_date[1],to = input$com_date[2],warnings = FALSE,auto.assign = FALSE)
      names(temp)[1:6] <- c("open","high","low","close","volume","adjusted")
      if(i == 1){
        comb_data <- data.frame(x1=as.data.frame(temp)$close)
      }
      else{
        comb_data <- cbind(comb_data,as.data.frame(temp)$close)
      }
    }
    colnames(comb_data) <- name_set
    
    output$cor_plot <- renderPlot({
      corrplot(cor((comb_data))) ###give a plot on this matrix
    })
  })
}
shinyApp(ui,server)

corrplo