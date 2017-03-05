library(dplyr)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

TP <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA, pv$NO_TRANSACCION), "sum")
TP <- TP[order(TP$x, decreasing = TRUE),]

pvA <- filter(pv, CATEGORIA != 'sincat')
pvA <- aggregate(pvA[,c("CANTIDAD")], by=list(pvA$ID_TIENDA,pvA$NO_TRANSACCION), "sum")

pvB <- filter(pv, CATEGORIA == 'sincat')
pvB <- aggregate(pvB[,c("CANTIDAD")], by=list(pvB$ID_TIENDA,pvB$NO_TRANSACCION), "sum")

total_tickets <- length(TP$Group.2)
ticket_sin <- length(pvA$Group.2)
ticket_unicamente_con <- length(pvB$Group.2)

tiucon <- (ticket_unicamente_con/total_tickets)*100
tisin <- (ticket_sin/total_tickets)*100

###########

pvC <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA,pv$NO_TRANSACCION,
                                              pv$CATEGORIA), "sum")








