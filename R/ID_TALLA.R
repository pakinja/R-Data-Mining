library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

pv$TALLA <- as.factor(gsub(" ","", pv$TALLA))
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))

pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TALLA), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$ID_TIENDA %in% pvA[1:9, 1] & pv$TALLA %in% pvB[1:10, 1],]

attach(fil.cat)

fil.cat$TALLA <- factor(fil.cat$TALLA)
fil.cat$ID_TIENDA <- factor(fil.cat$ID_TIENDA)

joint_id_talla <- CrossTable(TALLA, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id_talla <- joint_id_talla$t

barplot(joint_counts_id_talla, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "ID_TIENDA & TALLA")
title(main)
legend('topright', levels(TALLA), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 1)
