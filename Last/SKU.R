###################
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)
###################
#pv <- read.csv2('Coach DB Sales.csv', fileEncoding = "latin1")

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
fil.cat <- pv[pv$CODIGO_SKU %in% skus,]

#
attach(fil.cat)
CATEGORIA <- factor(CATEGORIA, levels=cats)
TALLA <- factor(TALLA, levels=tallas)
COLOR <- factor(COLOR, levels=colores)
ID_TIENDA <- factor(ID_TIENDA, levels=ids)
UBICACION <- factor(UBICACION, levels=ubics)
CODIGO_SKU <- factor(CODIGO_SKU, levels=skus)

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
relfreqSKU <- table(CODIGO_SKU)/length(CODIGO_SKU)
cbind(freqSKU, relfreqSKU)

##### joint distribution and conditional distribution

joint_sku_talla <- CrossTable(CODIGO_SKU, TALLA, prop.chisq = FALSE)
joint_counts_sku_talla <- joint_sku_talla$t

barplot(joint_counts_sku_talla, beside = TRUE, col=brewer.pal(10,"Paired"), ylab = "Frequency", xlab = "")
title("")
legend('topleft', levels(CODIGO_SKU), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 0.75)

joint_sku_id <- CrossTable(CODIGO_SKU, ID_TIENDA, prop.chisq = FALSE)
joint_counts_sku_id <- joint_sku_id$t

barplot(joint_counts_sku_id, beside = TRUE, col=brewer.pal(10,"Paired"), ylab = "Frequency", xlab = "")
title("")
legend('topright', levels(CODIGO_SKU), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 0.75)

joint_sku_color <- CrossTable(CODIGO_SKU, COLOR, prop.chisq = FALSE)
joint_counts_sku_color <- joint_sku_color$t

barplot(joint_counts_sku_color, beside = TRUE, col=brewer.pal(10,"Paired"), ylab = "Frequency", xlab = "")
title("")
legend('topright', levels(CODIGO_SKU), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 0.75)


joint_sku_cat <- CrossTable(CODIGO_SKU, CATEGORIA, prop.chisq = FALSE)
joint_counts_sku_cat <-joint_sku_cat$t

barplot(joint_counts_sku_cat, beside = TRUE, col=brewer.pal(10,"Paired"), ylab = "Frequency", xlab = "")
title("")
legend('topright', levels(CODIGO_SKU), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 0.75)

joint_sku_ubicacion <- CrossTable(CODIGO_SKU, UBICACION, prop.chisq = FALSE)
joint_counts_sku_ubicacion <-joint_sku_ubicacion$t

barplot(joint_counts_sku_ubicacion , beside = TRUE, col=brewer.pal(10,"Paired"), ylab = "Frequency", xlab = "")
title("")
legend('topright', levels(CODIGO_SKU), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 0.75)


