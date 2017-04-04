

AAA <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$FechaRegistro,
                                              pv$FechaOrden), "sum")
#AAA <- AAA[order(AAA[[2]]),]
#AAA <- AAA[!duplicated(AAA$Group.1),]

intcompra <- as.data.frame(as.numeric(AAA$Group.3 -AAA$Group.2))
names(intcompra) <- c("Difdias")
difer <- as.data.frame(table(intcompra))

names(difer) <- c("Cantidaddias", "Frecuencia")

difer["Porcentaje"] <- difer$Frecuencia/sum(difer$Frecuencia)*100
