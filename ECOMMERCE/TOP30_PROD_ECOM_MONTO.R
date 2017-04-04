library(ggplot2)

pvB <- aggregate(pv[,c("MontoItem")], by=list(pv$Producto), "sum")
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

print(length(unique(pv$Producto)))


pvB <- pvB[1:30,]

ggplot(pvB, aes(x = reorder(Group.1,-x), y = x)) + 
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(axis.text.x = element_text(angle = 45,vjust=1.5, hjust = 1, size =8))+
  scale_y_continuous(limits=c(0, max(pvB$x)), breaks = seq(0,max(pvB$x),by = 10000))+
  labs(x="Top 30 Productos (863 productos)", y="Monto")+
  ggtitle("Tommy Top 30 Productos")
