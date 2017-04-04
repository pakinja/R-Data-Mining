
pvA<- aggregate(pv[,c("MontoItem")], by=list(pv$Color), "sum")
#BBB <- count(na.omit(AAA$Group.2))

names(pvA) <- c("Color", "Monto")
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]

pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100


#pvA <- pvA[1:50,]
ggplot(pvA , aes(x =reorder(Color,PorcentajeAcumulado) , y = PorcentajeAcumulado)) + 
  geom_point(stat = "identity")+ 
  stat_summary(geom="line")+
  labs(title="Rapsodia Color & Monto", x="Color", y="Porcentaje de Venta")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$PorcentajeAcumulado), by = 10))
