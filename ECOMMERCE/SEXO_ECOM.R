
AAA <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,
                                          pv$Sexo), "sum")
#BBB <- count(na.omit(AAA$Group.2))

names(AAA) <- c("NumeroOrden","Sexo", "Monto")

sexo <- as.data.frame(table(AAA$Sexo))


ggplot(sexo, aes(x = reorder(Var1, -Freq), y = Freq))+
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(text = element_text(size=8))+
  geom_text(aes(label=Freq), vjust=-0.5,hjust=0.5,size=2.8)+
  labs(title="Sexo Tommy ecommerce", x="Sexo", y="No. Registros")



ggplot(AAA, aes(x = reorder(Var1, -Freq), y = Freq))+
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(text = element_text(size=8))+
  geom_text(aes(label=Freq), vjust=-0.5,hjust=0.5,size=2.8)+
  labs(title="Sexo Rapsodia ecommerce", x="Sexo", y="No. Órdenes")
