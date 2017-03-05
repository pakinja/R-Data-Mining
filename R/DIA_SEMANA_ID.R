library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

#pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

pv$FECHA_CREACION <- weekdays(pv$FECHA_CREACION)
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:7, 1] & pv$ID_TIENDA %in% pvB[1:9, 1],]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$ID_TIENDA <- factor(fil.cat$ID_TIENDA)

joint_diasemana_id <- CrossTable(ID_TIENDA, FECHA_CREACION, prop.chisq = FALSE)
joint_counts_diasemana_id <- joint_diasemana_id$t

barplot(joint_counts_diasemana_id, beside = TRUE, col=brewer.pal(9,"Paired"),
        ylab = "No. Artículos", xlab = "Día de la semana & ID_TIENDA")
title(main)
legend('top', levels(ID_TIENDA), pch=15, col=brewer.pal(9,"Paired"), ncol = 4,  cex = 1)

