AAA <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,
                                              pv$Cumpleaños), "sum")

ed <- as.data.frame(as.numeric((Sys.Date() -AAA$Group.2)/365))
names(ed) <- c("Edad")
names(AAA) <- c("Email", "Cumpleaños", "Monto")
ed$Edad <- as.numeric(ed$Edad)
ed$Edad <- round(ed$Edad)
AAA["Edad"] <- ed$Edad

BBB <- aggregate(AAA[,c("Monto")], by=list(AAA$Edad), "sum")

names(BBB) <- c("Edad","Monto")

BBB <- BBB[order(BBB$Monto, decreasing = TRUE),]

BBB["Porcentaje"] <- BBB$Monto/sum(pv$MontoItem)*100
BBB["SumaAcumulada"] <- cumsum(BBB$Monto)
BBB["PorcentajeAcumulado"] <- (BBB$SumaAcumulada/sum(BBB$Monto))*100


ggplot(BBB, aes(x =reorder(Edad, -Monto), y = Monto))+
  geom_bar(stat="identity", fill="brown", colour="yellow")+
  #geom_density()+
  labs(title="Tommy", x="Edad", y="Monto")+
  #geom_text(aes(label=(round(Porcentaje,2)), vjust=0.5,hjust=0.5, angle=45 ,size=1))
  #scale_x_continuous(breaks = seq(min(BBB$Edad), max(BBB$Edad), by = 1))
  scale_y_continuous(breaks = seq(0, max(BBB$Monto), by = 10000))
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################################

ggplot(BBB, aes(x =Edad, y = Monto))+
  geom_bar(stat="identity", fill="brown", colour="yellow")+
  #geom_density()+
  labs(title="Tommy", x="Edad", y="Monto")+
  #geom_text(aes(label=(round(Porcentaje,2)), vjust=0.5,hjust=0.5, angle=45 ,size=1))
  #scale_x_continuous(breaks = seq(min(BBB$Edad), max(BBB$Edad), by = 1))
  scale_y_continuous(breaks = seq(0, max(BBB$Monto), by = 10000))
#theme(axis.text.x = element_text(angle = 45, hjust = 1))