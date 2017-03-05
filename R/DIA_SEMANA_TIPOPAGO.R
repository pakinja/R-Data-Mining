library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

#pv$FECHA_CREACION <- as.factor(gsub(" ","", pv$FECHA_CREACION))
#pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")

#pv$FECHA_CREACION <- weekdays(pv$FECHA_CREACION)
pv$TIPO_PAGO <- as.factor(gsub(" ","", pv$TIPO_PAGO))


pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TIPO_PAGO), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$FECHA_CREACION %in% pvA[1:7, 1] & pv$TIPO_PAGO %in% pvB[1:4, 1],]

attach(fil.cat)

fil.cat$FECHA_CREACION <- factor(fil.cat$FECHA_CREACION)
fil.cat$TIPO_PAGO <- factor(fil.cat$TIPO_PAGO)

joint_diasemana_tipopago <- CrossTable(TIPO_PAGO, FECHA_CREACION, prop.chisq = FALSE)
joint_counts_diasemana_tipopago <- joint_diasemana_tipopago$t

barplot(joint_counts_diasemana_tipopago, beside = TRUE, col=brewer.pal(4,"Paired"),
        ylab = "No. Artículos", xlab = "Día de la Semana & TIPO_PAGO")
title(main)
legend('topright', levels(TIPO_PAGO), pch=15, col=brewer.pal(4,"Paired"), ncol = 2,  cex = 1)

