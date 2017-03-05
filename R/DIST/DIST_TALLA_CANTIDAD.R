library(ggplot2)
library(data.table)


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TALLA), "sum")
colnames(pvA)[1] <- "Talla"
colnames(pvA)[2] <- "Cantidad"
#colnames(pvA)[3] <- "Monto"
pvA <- pvA[order(pvA$Cantidad, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Cantidad/sum(pvA$Cantidad))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Cantidad)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Cantidad))*100



ntall <- seq(1:length(pvA$Talla))
pvA["PorcentajeTallas"] <- (ntall /length(ntall))*100

###########################################################
pvA <- pvA[1:50,]
ggplot(pvA , aes(x =reorder(Talla,PorcentajeAcumulado) , y = PorcentajeAcumulado)) + 
  geom_point(stat = "identity")+ 
  stat_summary(geom="line")+
  labs(title="Tommy Tallas & Cantidad", x="Talla", y="Porcentaje de Artículos")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$PorcentajeAcumulado), by = 10))


