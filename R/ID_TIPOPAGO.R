library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

pv$TIPO_PAGO <- as.factor(gsub(" ","", pv$TIPO_PAGO))
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))

pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TIPO_PAGO), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
fil.cat <- pv[pv$ID_TIENDA %in% pvA[1:9, 1] & pv$TIPO_PAGO %in% pvB[1:4, 1],]

attach(fil.cat)

fil.cat$TIPO_PAGO <- factor(fil.cat$TIPO_PAGO)
fil.cat$ID_TIENDA <- factor(fil.cat$ID_TIENDA)

joint_id_tipopago <- CrossTable(TIPO_PAGO, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id_tipopago <- joint_id_tipopago$t

barplot(joint_counts_id_tipopago, beside = TRUE, col=brewer.pal(4,"Paired"),
        ylab = "No. Artículos", xlab = "ID_TIENDA & TIPO_PAGO")
title(main)
legend('topright', levels(TIPO_PAGO), pch=15, col=brewer.pal(4,"Paired"), ncol = 1,  cex = 1)





