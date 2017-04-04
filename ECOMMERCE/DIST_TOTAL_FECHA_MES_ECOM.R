#install.packages("scatterplot3d")

library(lubridate)
library(ggplot2)
library(data.table)
library(scatterplot3d)

pv$MontoItem <- as.numeric(as.character(pv$MontoItem))
#pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FechaOrden <- as.Date(pv$FechaOrden,  "%m/%d/%Y")

pv$FechaOrden <- month(pv$FechaOrden)

pvA <- aggregate(pv[,c("MontoItem")], by=list(pv$FechaOrden), "sum")
colnames(pvA)[1] <- "Mes"
#colnames(pvA)[2] <- "Tienda"
colnames(pvA)[2] <- "Monto"
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100

#pvA$Tienda <- factor(pvA$Tienda)
pvA$Mes <- as.numeric(pvA$Mes)

ggplot(pvA, aes(x=Mes, y=Monto)) + 
  geom_point(colour="blue")+
  #scale_x_continuous(breaks = seq(1, 31))+
  #scale_y_continuous(breaks = seq(0, 200000, by = 10000))+
  #geom_vline(xintercept = c(5,7,11,18,20,28,29), colour="red")+
  #geom_vline(xintercept = c(14,17,24,27), colour="black")+
  labs(title="Rapsodia", x="Mes", y="Monto")

