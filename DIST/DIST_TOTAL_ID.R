library(ggplot2)
library(data.table)
library(dplyr)
pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$ID_TIENDA), "sum")

pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]


pvA["Porcentaje"] <- (pvA$x/sum(pvA$x))*100
pvA["SumaAcumulada"] <- cumsum(pvA$x)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$x))*100

colnames(pvA)[1] <- "Tienda"
colnames(pvA)[2] <- "Monto"
#colnames(pvA)[2] <- "SKU"
#colnames(pvA)[2] <- "Monto"

#pvA["Tienda"] <- factor(pvA$Tienda)

#write.csv(pvA, file = "MontoTiendaTommy.csv")

pp <- ggplot(pvA ,aes(x = reorder(Tienda,-Monto), y=Monto )) +
  geom_bar(stat="identity", fill="purple", colour="yellow")+
  labs(title="Tommy Monto de Venta & ID_TIENDA", x="ID_TIENDA", y="Monto")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$Monto), by = 5000000))+
  geom_text(aes(label=paste(round(Porcentaje,2),"%")), vjust=0.2,hjust=0.5,angle=90,size=4)
  #scale_x_continuous(breaks = seq(0, length(pvA$Group.1), by = 200))+
  #geom_vline(xintercept = , colour="red")+
  #geom_hline(yintercept = 90, colour="red")+
  #annotate("text", x = 6000, y = 70, label = "40% de Skus - 90% Ventas", size=7)

print(pp)
