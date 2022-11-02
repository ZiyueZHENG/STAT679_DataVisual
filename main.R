library(tidyverse)
library(tidyquant)
#### 我觉得现在最重要的是想一下怎么排版 ####
#### 然后想一想需要实现哪些功能 ####

# get data
getSymbols("^GSPC", from = Sys.Date()-3650,to = Sys.Date(),warnings = FALSE,auto.assign = TRUE)
df <- as.data.frame(GSPC) #这里得要设定成读取的股票或指数名
names(df)[1:6] <- c("open","high","low","close","volume","adjusted")
df <- df %>%
  mutate(date = as.Date(rownames(df)),return_percent = (close-open)*100/open)

# shiny
ui <- fluidPage(
  plotOutput('linechart',brush = brushOpts("area", direction = "xy")),
  plotOutput('hist1',height = 100),
  plotOutput('candlestickplot',hover = hoverOpts(id ="plot_hover", delay = 0)),
  plotOutput('hist2',height = 100),
  tableOutput("hover_info")
)

server <- function(input,output){
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
}
shinyApp(ui,server)


