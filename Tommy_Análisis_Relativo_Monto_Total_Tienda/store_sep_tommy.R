###################
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
###################
#pv <- read.csv2('Tommy DB Sales.csv', fileEncoding = "latin1")

################
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")
#pv <- pv[order(pv$FECHA_CREACION),]
pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
pv$TALLA <- as.factor(pv$TALLA)

tienda_id <- unique(pv$ID_TIENDA)
pv$NO_TRANSACCION <- as.factor(pv$NO_TRANSACCION)
pv$CATEGORIA <- as.factor(pv$CATEGORIA)
pv$NO_TRANSACCION <- factor(pv$NO_TRANSACCION )
pv$CATEGORIA <- factor(pv$CATEGORIA )


#################

tienda_id <- unique(pv$ID_TIENDA)
pv<- pv[order(pv$NO_TRANSACCION),]

####### metricas

#ptot <- sum(pv$TOTAL)
#pv$TOTAL <- (pv$TOTAL/ptot)*100

#######
for (i in tienda_id)
{
  assign(paste("T",i, sep = ""), filter(pv, pv$ID_TIENDA == i))
}