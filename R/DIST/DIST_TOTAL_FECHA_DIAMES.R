#install.packages("scatterplot3d")

library(lubridate)
library(ggplot2)
library(data.table)
library(scatterplot3d)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

pv$FECHA_CREACION <- day(pv$FECHA_CREACION)

pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$FECHA_CREACION,pv$ID_TIENDA), "sum")
colnames(pvA)[1] <- "Día"
colnames(pvA)[2] <- "Tienda"
colnames(pvA)[3] <- "Monto"
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100

pvA$Tienda <- factor(pvA$Tienda)
pvA$Día <- as.numeric(pvA$Día)

ggplot(pvA, aes(x=Día, y=Monto, fill=Tienda)) + 
  geom_area(colour="black", size=.2, alpha=.4)+
  scale_x_continuous(breaks = seq(0, 31))+
  scale_y_continuous(breaks = seq(0, 100000000, by = 1000000))+
  geom_vline(xintercept = c(6,10,13,16,18,20,23,26,30), colour="red")+
  geom_vline(xintercept = c(4,8,11,15,17,19,22,25,31), colour="black")+
  labs(title="Tommy", x="Día del Mes", y="Monto")
