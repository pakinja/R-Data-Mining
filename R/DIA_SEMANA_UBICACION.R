library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

pv$FECHA_CREACION <- weekdays(pv$FECHA_CREACION)
pv$UBICACION <- as.factor(gsub(" ","", pv$UBICACION))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$UBICACION), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:7, 1] & pv$UBICACION %in% pvB[1:10, 1],]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$UBICACION <- factor(fil.cat$UBICACION)

joint_diasemana_ubicacion <- CrossTable(UBICACION, FECHA_CREACION, prop.chisq = FALSE)
joint_counts_diasemana_ubicacion <- joint_diasemana_ubicacion $t

barplot(joint_counts_diasemana_ubicacion, beside = TRUE, col=brewer.pal(10,"Paired"),
        ylab = "No. Artículos", xlab = "Día de la semana & UBICACION")
title(main)
legend('top', levels(UBICACION), pch=15, col=brewer.pal(10,"Paired"), ncol = 3,  cex = 1)

