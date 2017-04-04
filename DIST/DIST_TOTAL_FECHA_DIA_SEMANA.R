#install.packages("scatterplot3d")

library(lubridate)
library(ggplot2)
library(data.table)
library(scatterplot3d)

pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

pv$FECHA_CREACION <- weekdays(pv$FECHA_CREACION)

pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$FECHA_CREACION,pv$ID_TIENDA), "sum")

pvA <- pvA[order(pvA[[3]],decreasing = TRUE), ]

pvA["Porcentaje"] <- (pvA$x/sum(pvA$x))*100
pvA["SumaAcumulada"] <- cumsum(pvA$x)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$x))*100


ggplot(pvA, aes(x=Group.1, y=x, fill=Group.2)) + 
  geom_area()
  #scale_x_discrete(breaks = seq(0, 7))
  #scale_y_continuous(breaks = seq(0, 9000000,500000))
  

ggplot(pvA, aes(x =Group.2 , y = Group.1)) + 
  geom_bin2d() +
  #labs(title="Cantidad Artículos por tienda", x="TIENDA", y="No. Artículos")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 1))
  #scale_y_continuous(breaks = seq(0, max(pvA$x), by = 1000))+
  #geom_text(aes(label= paste(round(Porcentaje,2),"%")),
  #          vjust=-0.5,hjust=-2,size=2.8, angle=45)

