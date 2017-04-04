#install.packages("scatterplot3d")

library(lubridate)
library(ggplot2)
library(data.table)
library(scatterplot3d)

pv$MontoItem <- as.numeric(as.character(pv$MontoItem))
#pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FechaOrden <- as.Date(pv$FechaOrden,  "%m/%d/%Y")

pv$FechaOrden <- weekdays(pv$FechaOrden)

pvA <- aggregate(pv[,c("MontoItem")], by=list(pv$FechaOrden), "sum")
colnames(pvA)[1] <- "Día"
#colnames(pvA)[2] <- "Tienda"
colnames(pvA)[2] <- "Monto"
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100

pvA$Día <- factor(pvA$Día)
pvA$Día <- gsub("domingo","7", pvA$Día)
pvA$Día <- gsub("sábado","6", pvA$Día)
pvA$Día <- gsub("viernes","5", pvA$Día)
pvA$Día <- gsub("jueves","4", pvA$Día)
pvA$Día <- gsub("miércoles","3", pvA$Día)
pvA$Día <- gsub("martes","2", pvA$Día)
pvA$Día <- gsub("lunes","1", pvA$Día)
pvA$Día <- as.numeric(pvA$Día)

#pvA$Tienda <- factor(pvA$Tienda)
pvA$Día <- as.numeric(pvA$Día)

ggplot(pvA, aes(x=Día, y=Monto)) + 
  geom_line(colour="blue")+
  scale_x_continuous(breaks = seq(0, 7))+
  scale_y_continuous(breaks = seq(0, 500000, by = 50000))+
  geom_vline(xintercept = c(1,5,7), colour="red")+
  geom_vline(xintercept = c(4), colour="black")+
  labs(title="tommy", x="Día de la Semana", y="Monto")+
  expand_limits(y = 0)
