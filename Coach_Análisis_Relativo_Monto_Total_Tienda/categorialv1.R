###################
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
###################
pv <- read.csv2('Rapsodia DB Sales.csv', fileEncoding = "latin1")
pv$CATEGORIA <- tolower(as.factor(gsub(" ","", pv$CATEGORIA)))
pv$CATEGORIA <- as.factor(gsub("knits","knit", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("kit","knit", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("accesories","accessories", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("tshirt","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("t-shirts","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("shi","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("sweater","sweaters", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("sweaterss","sweaters", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("swimwear","swimwears  ", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("swimwears  s","swimwears  ", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("skirt","skirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("skirtss","skirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("mensshoes","menshoes", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("tshirtsrts","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("ttshirts","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("tttshirtsrts","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("tttshirtsrtss","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("tshirtsrt","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("tshirtss","tshirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("pant","pants", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("skirtsss","skirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("skirtssss","skirts", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("jacketss","jackets", pv$CATEGORIA))
pv$CATEGORIA <- as.factor(gsub("jacket","jackets", pv$CATEGORIA))
pv$TALLA<- as.factor(gsub(" ","", pv$TALLA))
pv$COLOR <- as.factor(gsub(" ","", pv$COLOR))
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))
pv$UBICACION <- as.factor(gsub(" ","", pv$UBICACION))

################
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")
#pv <- pv[order(pv$FECHA_CREACION),]
pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
#pv$TALLA <- as.factor(pv$TALLA)
#CATEGORIA <- as.factor(gsub(" ","", pv$CATEGORIA))

# variables de interes, frecuencias y top 30

pvcat <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CATEGORIA), "sum")
pvcolor <- aggregate(pv[,c("CANTIDAD")], by=list(pv$COLOR), "sum")
pvtalla <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TALLA), "sum")
pvid <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvubicacion <- aggregate(pv[,c("CANTIDAD")], by=list(pv$UBICACION), "sum")

pvcat <- pvcat[order(pvcat[[2]], decreasing = TRUE),]
pvtalla <- pvtalla[order(pvtalla[[2]], decreasing = TRUE),]
pvcolor <- pvcolor[order(pvcolor[[2]], decreasing = TRUE),]
pvid <- pvid[order(pvid[[2]], decreasing = TRUE),]
pvubicacion <- pvubicacion[order(pvubicacion[[2]], decreasing = TRUE),]

muestra <- 10

cats <- pvcat[1:muestra, 1]
tallas <- pvtalla[1:muestra,1]
colores <- pvcolor[1:muestra,1]
ids <- pvid[1:muestra,1]
ubics <- pvubicacion[1:muestra,1]

# filtramos dataframe por top categorias
fil.cat <- pv[pv$CATEGORIA %in% cats,]

#
attach(fil.cat)
CATEGORIA <- factor(CATEGORIA, levels=cats)
TALLA <- factor(TALLA, levels=tallas)
COLOR <- factor(COLOR, levels=colores)
ID_TIENDA <- factor(ID_TIENDA, levels=ids)
UBICACION <- factor(UBICACION, levels=ubics)

#

freqCAT <- table(CATEGORIA)
relfreqCAT <- table(CATEGORIA)/length(CATEGORIA)
cbind(freqCAT, relfreqCAT)

freqTALLA <- table(TALLA)
relfreqTALLA <- table(TALLA)/length(TALLA)
cbind(freqTALLA, relfreqTALLA)

freqCOLOR <- table(COLOR)
relfreqCOLOR <- table(COLOR)/length(COLOR)
cbind(freqCOLOR, relfreqCOLOR)

freqID <- table(ID_TIENDA)
relfreqID <- table(ID_TIENDA)/length(ID_TIENDA)
cbind(freqID, relfreqID)

freqUBICACION <- table(UBICACION)
relfreqUBICACION <- table(UBICACION)/length(UBICACION)
cbind(freqUBICACION, relfreqUBICACION)

##### joint distribution and conditional distribution

joint_cat_talla <- CrossTable(CATEGORIA, TALLA, prop.chisq = FALSE)
joint_counts_cat_talla <- joint_cat_talla$t

barplot(joint_counts_cat_talla, beside = TRUE, col=rainbow(10), ylab = "Frequency", xlab = "TALLA")
title("COACH")
legend('topright', levels(CATEGORIA), pch=15, col=rainbow(10), ncol = 4,  cex = 0.75)


joint_cat_color <- CrossTable(CATEGORIA, COLOR, prop.chisq = FALSE)
joint_counts_color <- joint_cat_color$t

barplot(joint_counts_color, beside = TRUE, col=rainbow(10), ylab = "Frequency", xlab = "COLOR")
title("COACH")
legend('topright', levels(CATEGORIA), pch=15, col=rainbow(10), ncol = 4,  cex=0.75)

joint_cat_id <- CrossTable(CATEGORIA, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id <- joint_cat_id$t

barplot(joint_counts_id, beside = TRUE, col=rainbow(10), ylab = "Frequency", xlab = "ID_TIENDA")
title("COACH")
legend('topright', levels(CATEGORIA), pch=15, col=rainbow(10), ncol = 4,  cex=0.75)

joint_cat_ubicacion <- CrossTable(CATEGORIA, UBICACION, prop.chisq = FALSE)
joint_counts_ubicacion <- joint_cat_ubicacion$t

barplot(joint_counts_ubicacion, beside = TRUE, col=rainbow(10), ylab = "Frequency", xlab = "UBICACION",las=2)
title("COACH")
legend('topright', levels(CATEGORIA), pch=15, col=rainbow(10), ncol = 4,  cex=0.75)

