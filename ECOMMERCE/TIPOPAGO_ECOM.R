
pvA <- aggregate(pv[,c("MontoItem")], by=list(pv$MetodoPago), "sum")
#BBB <- count(na.omit(AAA$Group.2))
names(pvA) <- c("MetodoPago", "Monto")
pvA <- pvA[order(pvA$Monto, decreasing = TRUE),]


pvA["Porcentaje"] <- (pvA$Monto/sum(pvA$Monto))*100
pvA["SumaAcumulada"] <- cumsum(pvA$Monto)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$Monto))*100

ggplot(pvA, aes(x = reorder(MetodoPago, -Monto), y = Monto))+
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(text = element_text(size=8))+
  #coord_flip()+
  geom_text(aes(label=Monto), vjust=-0.5,hjust=0.5,size=2.8)+
  labs(title="Método de Pago Tommy ecommerce", x="Método de Pago", y="Monto")
