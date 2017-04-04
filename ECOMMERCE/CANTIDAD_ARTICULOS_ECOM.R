#install.packages("gridExtra")

library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

pv$TOTAL <- as.numeric(as.character(pv$MontoItem))



ll <- vector()
for(i in 1:length(unique(pv$NumeroOrden))){
  SUB <- filter(pv, NumeroOrden==unique(unique(pv$NumeroOrden)[i]))
  #TP <- aggregate(SUB[,c("MontoItem")], by=list(SUB$NumeroOrden), "sum")
  #TP <- TP[order(TP$x, decreasing = TRUE),]
  ll[i] <- (length(SUB[[1]]))
  
}

prom <- sum(ll)/length(ll)

cantidades <- as.data.frame(table(ll))
