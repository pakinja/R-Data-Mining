###################
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
###################
pv <- read.csv2('Coach DB Sales.csv', fileEncoding = "latin1")
C <- as.factor(gsub(" ","", pv$CATEGORIA))
Ta <- as.factor(gsub(" ","", pv$TALLA))
clr <- as.factor(gsub(" ","", pv$COLOR))
idt <- as.factor(gsub(" ","", pv$ID_TIENDA))
ub <- as.factor(gsub(" ","", pv$UBICACION))
sku <- as.factor(gsub(" ","", pv$CODIGO_SKU))
pv$CATEGORIA <- C
pv$TALLA <- Ta
pv$COLOR <- clr
pv$ID_TIENDA <- idt
pv$UBICACION <- ub
pv$CODIGO_SKU <- sku
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
pvsku <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CODIGO_SKU), "sum")

pvcat <- pvcat[order(pvcat[[2]], decreasing = TRUE),]
pvtalla <- pvtalla[order(pvtalla[[2]], decreasing = TRUE),]
pvcolor <- pvcolor[order(pvcolor[[2]], decreasing = TRUE),]
pvid <- pvid[order(pvid[[2]], decreasing = TRUE),]
pvubicacion <- pvubicacion[order(pvubicacion[[2]], decreasing = TRUE),]
pvsku <- pvsku[order(pvsku[[2]], decreasing = TRUE),]

muestra <- 10

cats <- pvcat[1:muestra, 1]
tallas <- pvtalla[1:muestra,1]
colores <- pvcolor[1:muestra,1]
ids <- pvid[1:muestra,1]
ubics <- pvubicacion[1:muestra,1]
skus <- pvsku[1:muestra,1]
# filtramos dataframe por top categorias
fil.cat <- pv[pv$CATEGORIA %in% cats,]

#
attach(fil.cat)
CATEGORIA <- factor(CATEGORIA, levels=cats)
TALLA <- factor(TALLA, levels=tallas)
COLOR <- factor(COLOR, levels=colores)
ID_TIENDA <- factor(ID_TIENDA, levels=ids)
UBICACION <- factor(UBICACION, levels=ubics)
CODIGO_SKU <- factor(UBICACION, levels=ubics)

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

freqSKU <- table(CODIGO_SKU)
relfreqSKU <- table()/length(CODIGO_SKU)
cbind(freqSKU, relfreqSKU)

##### joint distribution and conditional distribution

joint_cat_talla <- CrossTable(CATEGORIA, TALLA, prop.chisq = FALSE)
joint_counts_cat_talla <- joint_cat_talla$t

barplot(joint_counts_cat_talla, beside = TRUE, col=1:10, ylab = "Frequency", xlab = "TALLA")
title("TOMMY")
legend('topright', levels(CATEGORIA), pch=15, col=1:10, ncol = 4,  cex = 0.75)


joint_cat_color <- CrossTable(CATEGORIA, COLOR, prop.chisq = FALSE)
joint_counts_color <- joint_cat_color$t

barplot(joint_counts_color, beside = TRUE, col=1:10, ylab = "Frequency", xlab = "COLOR")
title("TOMMY")
legend('topright', levels(CATEGORIA), pch=15, col=1:10, ncol = 4,  cex=0.75)

joint_cat_id <- CrossTable(CATEGORIA, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id <- joint_cat_id$t

barplot(joint_counts_id, beside = TRUE, col=1:10, ylab = "Frequency", xlab = "ID_TIENDA")
title("TOMMY")
legend('topright', levels(CATEGORIA), pch=15, col=1:10, ncol = 4,  cex=0.75)

joint_cat_ubicacion <- CrossTable(CATEGORIA, UBICACION, prop.chisq = FALSE)
joint_counts_ubicacion <- joint_cat_ubicacion$t

barplot(joint_counts_ubicacion, beside = TRUE, col=1:10, ylab = "Frequency", xlab = "UBICACION",las=2)
title("TOMMY")
legend('topright', levels(CATEGORIA), pch=15, col=1:10, ncol = 4,  cex=0.75)

joint_cat_sku <- CrossTable(CODIGO_SKU, TALLA, prop.chisq = FALSE)
joint_counts_sku <- joint_cat_sku$t

barplot(joint_counts_sku, beside = TRUE, col=1:10, ylab = "Frequency", xlab = "TALLA",las=2)
title("TOMMY")
legend('topright', levels(CODIGO_SKU), pch=15, col=1:10, ncol = 4,  cex=0.75)



