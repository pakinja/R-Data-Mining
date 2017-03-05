library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

pv$FECHA_CREACION <- day(pv$FECHA_CREACION)
pv$CATEGORIA <- as.factor(gsub(" ","", pv$CATEGORIA))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CATEGORIA), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]
cats <-c("sneakers", "totes", "crossbody", "shoulderbag",
         "smallwristlets", "carryalls", "satchels", "moccasins",
         "runner", "miniskinny")
fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:31, 1],]
fil.cat <- fil.cat[fil.cat$CATEGORIA %in% cats,]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$CATEGORIA <- factor(fil.cat$CATEGORIA)

joint_diames_categoria <- CrossTable(CATEGORIA, FECHA_CREACION, prop.chisq = FALSE)
joint_counts_diames_categoria <- joint_diames_categoria$t

barplot(joint_counts_diames_categoria, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "Día del Mes & CATEGORIA")
title(main)
legend('topleft', levels(CATEGORIA), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 1)

