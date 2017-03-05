library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)
pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

pv$FECHA_CREACION <- day(pv$FECHA_CREACION)
pv$TIPO_PAGO <- as.factor(gsub(" ","", pv$TIPO_PAGO))


pvA <- aggregate(pv[,c("TOTAL")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("TOTAL")], by=list(pv$TIPO_PAGO), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

ggplot(pvA, aes(x = reorder(Group.1,-x) , y = x)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow")+ 
  labs(title ="Monto total Tommy", x = "Día del Mes", y="MXN")+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$x), by = 5000000))+
  geom_text(aes(label=round(pvA$x,2)), vjust=0.2, hjust=1.5,angle=90, size=5)
  #scale_fill_brewer()+
  #annotate("text",x = 5, y = max(Ticket_Promedio+100), label = main)+
  #geom_text(aes(label=round(Ticket_Promedio,2)), vjust=0.2,hjust=0.2, angle=45,size=2.8)

ggplot(pvB, aes(x = reorder(Group.1,-x) , y = x)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow")+ 
  labs(title ="Monto total Tommy", x = "Tipo Pago", y="MXN")+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvB$x), by = 50000000))+
  geom_text(aes(label=round(pvB$x,2)), vjust=0, size=4)
