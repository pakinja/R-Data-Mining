#install.packages("gridExtra")

library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))



ll <- vector()
for(i in 1:length(unique(pv$ID_TIENDA))){
  SUB <- filter(pv, ID_TIENDA==unique(unique(pv$ID_TIENDA)[i]))
  TP <- aggregate(SUB[,c("CANTIDAD")], by=list(SUB$NO_TRANSACCION), "sum")
  TP <- TP[order(TP$x, decreasing = TRUE),]
  ll[i] <- (table(TP$x)/length(TP$Group.1))*100
  
}

prom <- sum(ll)/length(ll)


