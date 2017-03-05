library(ggplot2)

pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$EMAIL), "sum")
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

print(length(unique(pv$EMAIL)))


pvB <- pvB[1:30,]

ggplot(pvB, aes(x = reorder(Group.1,-x), y = x)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow")+
  theme(axis.text.x = element_text(angle = 75, hjust = 0.5))+
  scale_y_continuous(limits=c(0, max(pvB$x)), breaks = seq(0,max(pvB$x),by = 1000))+
  labs(x="Top 30 SKU's (5356 skus)", y="CANTIDAD")+
  ggtitle("COACH: 37,776 artículos @28,371 transacciones @427 días 2015-11-01 - 2016-12-31 total = $121,624,810")