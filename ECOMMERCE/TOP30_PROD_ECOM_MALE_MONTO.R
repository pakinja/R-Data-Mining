library(ggplot2)
pvB <- filter(pv, Sexo=="male")
pvB <- aggregate(pvB[,c("MontoItem")], by=list(pvB$Producto), "sum")
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

print(length(unique(pv$Producto)))


pvB <- pvB[1:30,]

ggplot(pvB, aes(x = reorder(Group.1,-x), y = x)) + 
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(axis.text.x = element_text(angle = 45,vjust=1, hjust = 1, size =8))+
  scale_y_continuous(limits=c(0, max(pvB$x)), breaks = seq(0,max(pvB$x),by = 10000))+
  labs(x="Productos Hombres", y="Monto")+
  ggtitle("Tommy Productos (Hombres)")
