library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

pv$COLOR <- as.factor(gsub(" ","", pv$COLOR))
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))

pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$COLOR), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$ID_TIENDA %in% pvA[1:10, 1] & pv$COLOR %in% pvB[1:10, 1],]

attach(fil.cat)

fil.cat$COLOR <- factor(fil.cat$COLOR)
fil.cat$ID_TIENDA <- factor(fil.cat$ID_TIENDA)

joint_id_color <- CrossTable(COLOR, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id_color <- joint_id_color$t

barplot(joint_counts_id_color, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "ID_TIENDA & COLOR")
title(main)
legend('topright', levels(COLOR), pch=15, col=brewer.pal(10,"Paired"), ncol = 3,  cex = 1)
