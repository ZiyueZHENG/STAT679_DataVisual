library(tidyverse)
library(tidyquant)
library(corrplot)
library(ggtext)
library(shiny)

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
    
    # detail information 
    output$text <- renderUI({
      if (is.null(input$plot_hover$x)) {
        label = ""
      }else{
        hover <- round(input$plot_hover$x)
        hov <- df[as.numeric(df$date) == hover,]
        label = paste0('date:',hov$date , '<br/>','open:',round(hov$open,2) ,'<br/>', 'close:',round(hov$close,2) ,'<br/>', 'volume:',hov$volume,'<br/>',
                       'high:',round(hov$high,2),'<br/>','low:',round(hov$low,2) , '<br/>','return percent:',round(hov$return_percent,2))
      }
      HTML(label)
      
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