
n <- 1
tbl <- table(pv$Email) > n
pv <- subset(pv, Email %in% names(tbl)[tbl])

#no_tran <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$Producto,
#                                                  pv$FechaOrden), "sum")

no_tran <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,
                                                  pv$FechaOrden), "sum")
no_tran <- no_tran[order(no_tran[[1]], decreasing = TRUE),]
names(no_tran) <- c("Email", "FechaOrden", "Monto")



tbl <- table(no_tran$Email) > n
no_tran <- subset(no_tran, Email %in% names(tbl)[tbl])


#no_tran$Group.2 <- as.Date(no_tran$Group.2,  "%m/%d/%Y")

#no_tran$Email <- as.factor(no_tran$Email)
#no_tran$Producto <- as.factor(no_tran$Producto)
#no_tran$Email <- factor(no_tran$Email)
#no_tran$Producto  <- factor(no_tran$Producto)
#no_tran<- pv[order(pv[[3]], decreasing = TRUE),]
no_tran$FechaOrden  <- as.factor(no_tran$FechaOrden)
no_tran$FechaOrden  <- factor(no_tran$FechaOrden)
write.csv(no_tran, "Tommy_frecuentes.csv")


names(no_tran) <- c("Email", "FechaOrden", "Monto")

no_tran <-no_tran[1:110,]

pp <- ggplot(no_tran, aes(x=Group.1, y=Group.2))+
  geom_bin2d()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "red", size =.7)) +
  theme(axis.text.y = element_text(size=7))+
  ggtitle("")

newdat <- ggplot_build(pp)$data[[1]]
pp + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                label=count), col="white")
############################

pp <- ggplot(no_tran, aes(x=Email, y=FechaOrden))+
  geom_bin2d()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "red", size =.7)) +
  theme(axis.text.y = element_text(size=7))+
  ggtitle("")

newdat <- ggplot_build(pp)$data[[1]]
pp + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                label=count), col="white")
###############################3


cc <- count(no_tran$Email)
cc <- cc[order(cc[[2]], decreasing = TRUE),]

ggplot(cc, aes(x = reorder(x,-freq) , y = freq)) + 
  geom_bar(stat = "identity", fill="brown", colour="yellow")+ 
  labs(title ="Tommy", x = "ID", y="Frecuencia")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))
  #scale_y_continuous(breaks = seq(0, max(pvA$x), by = 50000))+
  #geom_text(aes(label=round(pvA$x,2)), vjust=0.2, hjust=1,angle=90, size=5)



