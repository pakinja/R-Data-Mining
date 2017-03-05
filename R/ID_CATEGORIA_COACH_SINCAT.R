library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

pv$CATEGORIA <- as.factor(gsub(" ","", pv$CATEGORIA))
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))

pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CATEGORIA), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

cats <-c("sneakers", "totes", "crossbody", "shoulderbag",
                  "smallwristlets", "carryalls", "satchels", "moccasins",
                  "runner", "miniskinny")

#fil.cat <- pv[pv$ID_TIENDA %in% pvA[1:9, 1] ,]
fil.cat <- pv[pv$CATEGORIA %in% cats, ]


attach(fil.cat)

fil.cat$CATEGORIA <- factor(fil.cat$CATEGORIA)
fil.cat$ID_TIENDA <- factor(fil.cat$ID_TIENDA)

joint_id_cat <- CrossTable(CATEGORIA, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id_cat <- joint_id_cat$t

barplot(joint_counts_id_cat, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "ID_TIENDA & CATEGORIA")
title(main)
legend('topright', levels(CATEGORIA), pch=15, col=brewer.pal(10,"Paired"), ncol = 2,  cex = 1)

