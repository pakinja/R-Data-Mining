library(ggplot2)
library(data.table)
library(dplyr)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$TOTAL,pv$ID_TIENDA,pv$CATEGORIA), "sum")


pvA <- pvA[order(pvA$x, decreasing = TRUE),]
#fil.tick <- pvA[pvA$Group.2 %in% c("3044", "3011", "3005", "3004", "3016", "3022", "3045", 
#                                   "3048", "3025", "3042", "3031", "3013", "3021", "3034",
#                                   "3012", "3002", "3020", "3007", "3032", "3001", "3033", 
#                                   "3024", "3030", "3006", "3014", "3047", "3023", "3029", 
#                                   "3041", "3028", "3003"),]

fil.tick <- pvA[pvA$Group.2 %in% c("3046", "3026", "3010", "3040", "3036", "3037", "3039", 
                                   "3015", "3038", "3019", "3017", "3018", "3008",
                                   "3027","3009", "3035", "3049"),]

fil.tick <- fil.tick[order(fil.tick[[4]], decreasing = TRUE),]

colnames(pvA)[1] <- "Categoria"
#colnames(pvA)[2] <- "Categoria"
colnames(pvA)[2] <- "Monto"
#colnames(pvA)[3] <- "Monto"


pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100

#write.csv(pvA,"")
pvA <- pvA[1:49, ]
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


