library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

pv$CODIGO_SKU <- as.factor(gsub(" ","", pv$CODIGO_SKU))
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))

pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CODIGO_SKU), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$ID_TIENDA %in% pvA[1:9, 1] & pv$CODIGO_SKU %in% pvB[1:10, 1],]

attach(fil.cat)

fil.cat$CODIGO_SKU <- factor(fil.cat$CODIGO_SKU)
fil.cat$ID_TIENDA <- factor(fil.cat$ID_TIENDA)

joint_id_sku <- CrossTable(CODIGO_SKU, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id_sku <- joint_id_sku$t

barplot(joint_counts_id_sku, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "ID_TIENDA & SKU")
title(main)
  legend('topleft', levels(CODIGO_SKU), pch=15, col=brewer.pal(10,"Paired"), ncol = 3,  cex = 1)

  