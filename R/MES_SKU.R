library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

#pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
#pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

#pv$FECHA_CREACION <- month(pv$FECHA_CREACION)
pv$CODIGO_SKU <- as.factor(gsub(" ","", pv$CODIGO_SKU))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CODIGO_SKU), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:12, 1] & pv$CODIGO_SKU %in% pvB[1:10, 1],]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$CODIGO_SKU <- factor(fil.cat$CODIGO_SKU)

joint_mes_sku <- CrossTable(CODIGO_SKU, FECHA_CREACION, prop.chisq = FALSE)
joint_counts_mes_sku <- joint_mes_sku$t

barplot(joint_counts_mes_sku, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "Mes & CODIGO_SKU")
title(main)
legend('top', levels(CODIGO_SKU), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 1)

