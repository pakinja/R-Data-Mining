library(ggplot2)
library(data.table)


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CATEGORIA), "sum")
colnames(pvA)[1] <- "Categoría"
colnames(pvA)[2] <- "Cantidad"
#colnames(pvA)[3] <- "Monto"
pvA <- pvA[order(pvA$Cantidad, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Cantidad/sum(pvA$Cantidad))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Cantidad)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Cantidad))*100



ncats <- seq(1:length(pvA$Categoría))
pvA["PorcentajeCats"] <- (ncats /length(ncats))*100

###########################################################
#pvA <- pvA[1:54,]
ggplot(pvA , aes(x =reorder(Categoría,PorcentajeAcumulado) , y = PorcentajeAcumulado)) + 
  geom_point(stat = "identity")+ 
  stat_summary(geom="line")+
  labs(title="Tommy Cantidad de Artículos & Categorías", x="Categorías", y="Porcentaje de Artículos")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$PorcentajeAcumulado), by = 10))


