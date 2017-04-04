library(ggplot2)

pvB <- aggregate(pv[,c("MontoItem")], by=list(pv$SKU), "sum")
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

print(length(unique(pv$SKU)))

colnames(pvB)[1] <- "SKU"
colnames(pvB)[2] <- "Monto"
#colnames(pvA)[3] <- "Monto"


pvB["Porcentaje"] <- (pvB$Monto/sum(pvB$Monto))*100
pvB["SumaAcumulada"] <- cumsum(pvB$Monto)
pvB["PorcentajeAcumulado"] <- (pvB$SumaAcumulada/sum(pvB$Monto))*100


pvB <- pvB[1:30,]

ggplot(pvB, aes(x = reorder(Group.1,-x), y = x)) + 
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(axis.text.x = element_text(angle = 45,vjust=1.5, hjust = 1, size =6))+
  scale_y_continuous(limits=c(0, max(pvB$x)), breaks = seq(0,max(pvB$x),by = 1000))+
  labs(x="Top 30 SKU's (2808 skus)", y="Monto")+
  ggtitle("Tommy Top 30 SKUS")
