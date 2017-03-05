library(ggplot2)
library(data.table)


pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$CATEGORIA, pv$ID_TIENDA), "sum")
colnames(pvA)[1] <- "Categoría"
colnames(pvA)[2] <- "Tienda"
colnames(pvA)[3] <- "Monto"
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100



ncats <- seq(1:length(pvA$Categoría))
pvA["PorcentajeCats"] <- (ncats /length(ncats))*100

###########################################################
ggplot(pvA , aes(x = ncats, y = PorcentajeAcumulado)) + 
  geom_line()+ 
  labs(title="Tommy Porcentaje de Venta & Cantidad de Categorías", x="No. de Categorías", y="Porcentaje de Venta")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$PorcentajeAcumulado), by = 10))+
  scale_x_continuous(breaks = seq(0, length(pvA$Group.1), by = 1))+
  geom_vline(xintercept = 9.2, colour="red")+
  geom_hline(yintercept = 90, colour="red")+
  annotate("text", x = 30, y = 70, label = "13.6% de Categorías - 90% Ventas", size=7)
