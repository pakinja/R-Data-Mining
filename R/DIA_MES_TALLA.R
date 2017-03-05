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
pv$TALLA <- as.factor(gsub(" ","", pv$TALLA))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TALLA), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:31, 1] & pv$TALLA %in% pvB[1:10, 1],]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$TALLA <- factor(fil.cat$TALLA)

joint_diames_talla <- CrossTable(TALLA, FECHA_CREACION, prop.chisq = FALSE)
joint_counts_diames_talla <- joint_diames_talla$t

barplot(joint_counts_diames_talla, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "Día del Mes & TALLA")
title(main)
legend('topleft', levels(TALLA), pch=15, col=brewer.pal(10,"Paired"), ncol = 5,  cex = 1)

