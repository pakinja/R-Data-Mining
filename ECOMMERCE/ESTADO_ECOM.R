
AAA <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$NumeroOrden,
                                              pv$Estado), "sum")

BBB <- count(AAA$Group.2)

#BBB <- BBB[1:30,]
ggplot(BBB, aes(x = reorder(x, freq), y = freq))+
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(text = element_text(size=8))+
  coord_flip()
