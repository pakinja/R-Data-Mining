library(ggplot2)
library(data.table)


pvA <- aggregate(pv[,c("MontoItem")], by=list(pv$Producto), "sum")
colnames(pvA)[1] <- "Producto"
colnames(pvA)[2] <- "Monto"
#colnames(pvA)[3] <- "Monto"
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Cantidad/sum(pvA$Cantidad))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Cantidad)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Cantidad))*100



nprods <- seq(1:length(pvA$Producto))
pvA["PorcentajeProds"] <- (nprods /length(nprods))*100

###########################################################
#pvA <- pvA[1:54,]
ggplot(pvA , aes(x =reorder(nprods,PorcentajeAcumulado) , y = PorcentajeAcumulado)) + 
  geom_point(stat = "identity")+ 
  stat_summary(geom="line")+
  labs(title="Tommy Cantidad de Artículos & Categorías", x="Categorías", y="Porcentaje de Artículos")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = nprods))


