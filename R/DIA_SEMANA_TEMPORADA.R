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
pv$TEMPORADA <- as.factor(gsub(" ","", pv$TEMPORADA))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TEMPORADA), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:7, 1] & pv$TEMPORADA %in% pvB[1:9, 1],]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$TEMPORADA <- factor(fil.cat$TEMPORADA)

joint_diames_temporada <- CrossTable(TEMPORADA,FECHA_CREACION, prop.chisq = FALSE)
joint_counts_diames_temporada <- joint_diames_temporada$t

barplot(joint_counts_diames_temporada, beside = TRUE, col=brewer.pal(9,"Paired"),
        ylab = "No. Artículos", xlab = "Día de la Semana & TEMPORADA")
title(main)
legend("top",levels(TEMPORADA), pch=15, col=brewer.pal(9,"Paired"), ncol = 4,  cex = 1)

