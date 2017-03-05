library(ggplot2)
library(data.table)

pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$CODIGO_SKU), "sum")

pvA <- pvA[order(pvA$x, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$x/sum(pvA$x))*100
pvA["SumaAcumulada"] <- cumsum(pvA$x)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$x))*100

nskus <- seq(1:length(pvA$PorcentajeAcumulado))
pvA["PorcentajeSkus"] <- (nskus /length(nskus))*100

ggplot(pvA , aes(x = nskus, y = PorcentajeAcumulado)) + 
  geom_line()+ 
  labs(title="Rapsodia Porcentaje de Venta & Cantidad de SKU's", x="No. de SKU's", y="Porcentaje de Venta")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$PorcentajeAcumulado), by = 10))+
  scale_x_continuous(breaks = seq(0, length(pvA$Group.1), by = 200))+
  geom_vline(xintercept = 3468, colour="red")+
  geom_hline(yintercept = 90, colour="red")+
  annotate("text", x = 6000, y = 70, label = "40% de Skus - 90% Ventas", size=7)
  

