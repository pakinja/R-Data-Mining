#install.packages("gridExtra")

library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

pv$MontoItem<- as.numeric(as.character(pv$MontoItem))

AAA <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$NumeroOrden,
                                              pv$LugarEnvio), "sum")

TICKET_PROMEDIO <- mean(AAA$x)
