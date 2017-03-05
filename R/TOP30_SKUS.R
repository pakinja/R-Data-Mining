library(ggplot2)

pvB <- aggregate(pv[,c("TOTAL")], by=list(pv$CODIGO_SKU), "sum")
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

print(length(unique(pv$CODIGO_SKU)))


pvB <- pvB[1:30,]

ggplot(pvB, aes(x = reorder(Group.1,-x), y = x)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))+
  scale_y_continuous(limits=c(0, max(pvB$x)), breaks = seq(0,max(pvB$x),by = 100))+
  labs(x="Top 30 SKU's (63777 skus)", y="Cantidad")+
  ggtitle("TOMMY: 1,079,262 artículos @473,364 transacciones @427 días 2015-11-01 - 2016-12-31 total = $800,726,791")
