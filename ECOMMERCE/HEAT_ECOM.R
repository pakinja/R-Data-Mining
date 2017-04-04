library(ggplot2)
library(data.table)

#pv$NO_TRANSACCION <- as.factor(gsub(" ","", pv$NO_TRANSACCION))
#mT4008 <- T4008[duplicated(T4008)|duplicated( T4008, fromLast=TRUE),]
n <- 1
tbl <- table(pv$NumeroOrden) > n
pv <- subset(pv, NumeroOrden %in% names(tbl)[tbl])

pv$NumeroOrden <- as.factor(pv$NumeroOrden)
pv$Producto <- as.factor(pv$Producto)
pv$NumeroOrden <- factor(pv$NumeroOrden)
pv$Producto  <- factor(pv$Producto)
pv<- pv[order(pv[[3]], decreasing = TRUE),]

pv <- pv[1:70,]

pp <- ggplot(pv, aes(x=NumeroOrden, y=Producto))+
  geom_bin2d()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  panel.grid.major = element_line(colour = "red", size =.7)) +
  theme(axis.text.y = element_text(size=7))+
  ggtitle("")
 
newdat <- ggplot_build(pp)$data[[1]]
pp + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                               label=count), col="white")

