library(ggplot2)
library(data.table)
library(dplyr)


pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$CATEGORIA), "sum")


pvA <- pvA[order(pvA$x, decreasing = TRUE),]


colnames(pvA)[1] <- "Categoria"
#colnames(pvA)[2] <- "Categoria"
colnames(pvA)[2] <- "Monto"
#colnames(pvA)[3] <- "Monto"


pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100

#write.csv(pvA,"")
#pvA <- pvA[1:49, ]

ggplot(pvA , aes(x =reorder(Categoria,PorcentajeAcumulado) , y = PorcentajeAcumulado)) + 
  geom_point(stat = "identity")+ 
  stat_summary(geom="line")+
  labs(title="Tommy Porcentaje de Venta & Categorías", x="Categorías", y="Porcentaje de Venta")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$PorcentajeAcumulado), by = 10))
  #scale_x_d(breaks = seq(0, length(pvA$Group.1), by = 200))+
  #geom_vline(xintercept = 3468, colour="red")+
  #geom_hline(yintercept = 90, colour="red")+
  #annotate("text", x = 6000, y = 70, label = "40% de Skus - 90% Ventas", size=7)


