
AAA <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,
                                          pv$Cumpleaños), "sum")
#AAA <- AAA[!duplicated(AAA$Group.1),]

ed <- as.data.frame(as.numeric((Sys.Date() -AAA$Group.2)/365))
names(ed) <- c("Edad")
ed$Edad <- as.numeric(ed$Edad)
ed$Edad <- round(ed$Edad)


edades <- as.data.frame(table(ed))
names(edades) <- c("Edad", "Frecuencia")

edades["Porcentaje"] <- as.numeric(edades$Frecuencia)/sum(as.numeric(edades$Frecuencia))*100


ggplot(edades, aes(x = Edad, y = Porcentaje))+
  geom_bar(stat="identity", fill="brown", colour="yellow")+
  #geom_density()+
  labs(title="Edad Tommy ecommerce", x="Edad", y="Porcentaje")+
  #geom_text(aes(label=(round(Porcentaje,2)), vjust=0.5,hjust=0.5, angle=45 ,size=1))
  #scale_x_continuous(breaks = seq(min(edad$Edad), max(edad$Edad), by = 1))
  scale_y_continuous(breaks = seq(0, max(edades$Porcentaje), by = 0.5))
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))

media <- mean(ed$Edad)
