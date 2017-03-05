#install.packages("gridExtra")

library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))


NO_TICK <- vector()


for(i in 1:length(unique(pv$ID_TIENDA))){
  SUB <- filter(pv, ID_TIENDA==unique(unique(pv$ID_TIENDA)[i]))
  TP <- aggregate(SUB[,c("TOTAL")], by=list(SUB$NO_TRANSACCION), "sum")
  TP <- TP[order(TP$x, decreasing = TRUE),]
  NO_TICK[i] <- length(TP$x)
}

Tienda <- unique(pv$ID_TIENDA)
MT <- as.data.frame(cbind(Tienda, NO_TICK))



ggplot(MT, aes(x = reorder(Tienda,-NO_TICK) , y = NO_TICK)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow") +
  labs(title="Tommy No. Tickets por Tienda", x="TIENDA", y="No. Tickets")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(MT$NO_TICK), by = 1000))+
  scale_fill_brewer()+
  #annotate("text",x = 5, y = max(Ticket_Promedio+100), label = main)+
  geom_text(aes(label=round(NO_TICK,2)),  vjust=0.5,hjust=0.5,angle=90,size=4)

