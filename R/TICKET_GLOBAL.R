#install.packages("gridExtra")

library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))

Ticket_Promedio <- vector()
MAXS <- vector()
SDS <- vector()
len <- vector()
SUMA <- vector()

for(i in 1:length(unique(pv$ID_TIENDA))){
  SUB <- filter(pv, ID_TIENDA==unique(unique(pv$ID_TIENDA)[i]))
  TP <- aggregate(SUB[,c("TOTAL")], by=list(SUB$NO_TRANSACCION), "sum")
  #TP <- TP[order(TP$x, decreasing = TRUE),]
  len[i] <- length(TP$x)
  #MAXS[i] <- max(TP$x)
  #SDS[i] <- sd(TP$x)
  Ticket_Promedio[i] <- mean(TP$x)
  SUMA[i] <- sum(TP$x)
  
}
MGLOBAL <- sum(SUMA)/sum(len)

Tienda <- unique(pv$ID_TIENDA)
MT <- as.data.frame(cbind(Tienda, Ticket_Promedio))
MM <- as.data.frame(cbind(Tienda, MAXS))


ggplot(MT, aes(x = reorder(Tienda,-Ticket_Promedio) , y = Ticket_Promedio)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow") +
  labs(title="Ticket Promedio", x="TIENDA", y="MXN")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(MT$Ticket_Promedio), by = 500))+
  scale_fill_brewer()+
  annotate("text",x = 20, y = max(Ticket_Promedio+100), label = main)

ggplot(MM, aes(x = reorder(Tienda,-MAXS) , y = MAXS)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow") +
  labs(title="Ticket Máximo", x="TIENDA", y="MXN")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(MM$MAXS), by = 10000))+
  scale_fill_brewer()+
  annotate("text",x = 20, y = max(MAXS+5000), label = main)

