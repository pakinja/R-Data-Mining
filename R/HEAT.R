library(ggplot2)
library(data.table)

#pv$NO_TRANSACCION <- as.factor(gsub(" ","", pv$NO_TRANSACCION))
#mT4008 <- T4008[duplicated(T4008)|duplicated( T4008, fromLast=TRUE),]

sub <- sub[1:198,]
sub$NO_TRANSACCION <- as.factor(sub$NO_TRANSACCION)
sub$CATEGORIA <- as.factor(sub$CATEGORIA)
sub$NO_TRANSACCION <- factor(sub$NO_TRANSACCION )
sub$CATEGORIA <- factor(sub$CATEGORIA )
sub <- sub[order(sub[[5]], decreasing = TRUE),]

pp <- ggplot(sub, aes(x=NO_TRANSACCION, y=CATEGORIA))+
  geom_bin2d()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  panel.grid.major = element_line(colour = "red", size =.7)) +
  ggtitle("Coach 4010 Muestra Tickets Mixtos")
 
newdat <- ggplot_build(pp)$data[[1]]
pp + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                               label=count), col="white")
