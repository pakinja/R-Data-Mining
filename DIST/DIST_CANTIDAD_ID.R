pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA,pv$NO_TRANSACCION), "sum")

pvA <- pvA[order(pvA[[3]], decreasing = TRUE),]
fivenum(pvA$x)
cumsum(pvA$x)
pvA["Porcentaje"] <- (pvA$x/sum(pvA$x))*100
pvA["SumaAcumulada"] <- cumsum(pvA$x)
pvA["PorcentajeAcumulado"] <- (pvA$SumaAcumulada/sum(pvA$x))*100


aaa <- as.data.frame(table(pvA$Group.1,pvA$x))


ggplot(pvA, aes(x =reorder(Group.1,-x) , y = x)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow") +
  labs(title="Cantidad Artículos por tienda", x="TIENDA", y="No. Artículos")+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$x), by = 1000))+
  geom_text(aes(label= paste(round(Porcentaje,2),"%")),
            vjust=-0.5,hjust=0.5,size=2.8)
