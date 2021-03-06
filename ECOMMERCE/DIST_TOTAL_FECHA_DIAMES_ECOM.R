#install.packages("scatterplot3d")

library(lubridate)
library(ggplot2)
library(data.table)
library(scatterplot3d)

pv$MontoItem <- as.numeric(as.character(pv$MontoItem))
#pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FechaOrden <- as.Date(pv$FechaOrden,  "%m/%d/%Y")

pv$FechaOrden <- day(pv$FechaOrden)

pvA <- aggregate(pv[,c("MontoItem")], by=list(pv$FechaOrden), "sum")
colnames(pvA)[1] <- "Día"
#colnames(pvA)[2] <- "Tienda"
colnames(pvA)[2] <- "Monto"
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100

#pvA$Tienda <- factor(pvA$Tienda)
pvA$Día <- as.numeric(pvA$Día)

ggplot(pvA, aes(x=Día, y=Monto)) + 
  geom_line(colour="blue")+
  scale_x_continuous(breaks = seq(1, 31))+
  scale_y_continuous(breaks = seq(0, 500000, by = 10000))+
  geom_vline(xintercept = c(9,15,18,21,28), colour="red")+
  geom_vline(xintercept = c(2,12,22,24,31), colour="black")+
  labs(title="Tommy", x="Día del Mes", y="Monto")

