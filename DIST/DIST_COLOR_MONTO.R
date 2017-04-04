library(ggplot2)
library(data.table)


pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$COLOR), "sum")
colnames(pvA)[1] <- "Color"
colnames(pvA)[2] <- "Monto"
#colnames(pvA)[3] <- "Monto"
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100



ncol <- seq(1:length(pvA$Color))
pvA["PorcentajeColor"] <- (ncol /length(ncol))*100

###########################################################
pvA <- pvA[1:50,]
ggplot(pvA , aes(x =reorder(Color,PorcentajeAcumulado) , y = PorcentajeAcumulado)) + 
  geom_point(stat = "identity")+ 
  stat_summary(geom="line")+
  labs(title="Tommy Color & Monto", x="Color", y="Porcentaje de Venta")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$PorcentajeAcumulado), by = 10))


