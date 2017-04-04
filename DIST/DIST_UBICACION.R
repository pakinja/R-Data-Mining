library(ggplot2)
library(data.table)

pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$UBICACION), "sum")

pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$x/sum(pvA$x))*100
pvA["SumaAcumulada"] <- cumsum(pvA$x)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$x))*100

ggplot(pvA, aes(x =reorder(Group.1,-x) , y = x)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow") +
  labs(title="Cantidad Artículos por tienda", x="TIENDA", y="No. Artículos")+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$x), by = 1000))+
  geom_text(aes(label= paste(round(Porcentaje,2),"%")),
            vjust=-0.5,hjust=-2,size=2.8, angle=45)

