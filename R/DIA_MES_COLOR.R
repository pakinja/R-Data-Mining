library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

#pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
#pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

#pv$FECHA_CREACION <- day(pv$FECHA_CREACION)
pv$COLOR <- as.factor(gsub(" ","", pv$COLOR))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$COLOR), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:31, 1] & pv$COLOR %in% pvB[1:10, 1],]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$COLOR <- factor(fil.cat$COLOR)

joint_diames_color <- CrossTable(COLOR, FECHA_CREACION, prop.chisq = FALSE)
joint_counts_diames_color <- joint_diames_color$t

barplot(joint_counts_diames_color, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "Día del Mes & COLOR")
title(main)
legend('topleft', levels(COLOR), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 1)

