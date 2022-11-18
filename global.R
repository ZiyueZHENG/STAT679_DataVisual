library(tidyverse)
library(tidyquant)


symbol1 <- read.csv("./symbol_list/nasdaq_screener_1667510028096.csv")
symbol2 <- read.csv("./symbol_list/nasdaq_screener_1667510197492.csv")
symbol3 <- read.csv("./symbol_list/nasdaq_screener_1667510230035.csv")
symbol <- rbind(symbol1,symbol2,symbol3)
symbols <- symbol$Symbol


for (sy in symbols) {
  fit<-try(getSymbols(sy,warnings = FALSE,auto.assign = FALSE),silent=TRUE)
  if('try-error' %in% class(fit)){
    next
  }else{
    av_sy <- append(av_sy , sy)
  }
}

symbol <- symbol %>% filter(Symbol %in% av_sy)
   
write.csv(symbol , './symbol_list/symbols.csv',row.names = FALSE)


 
