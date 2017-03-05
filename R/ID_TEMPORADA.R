#library(lubridate)
#library(ggplot2)
#library(dplyr)
#library(data.table)
#library(plyr)
#library(gmodels)
#library(RColorBrewer)

pv$TEMPORADA <- as.factor(gsub(" ","", pv$TEMPORADA))
pv$ID_TIENDA <- as.factor(gsub(" ","", pv$ID_TIENDA))

pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA), "sum")
pvB <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TEMPORADA), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]
pvB <- pvB[order(pvB[[2]], decreasing = TRUE),]

fil.cat <- pv[pv$ID_TIENDA %in% pvA[1:9, 1] & pv$TEMPORADA %in% pvB[1:9, 1],]

attach(fil.cat)

fil.cat$TEMPORADA <- factor(fil.cat$TEMPORADA)
fil.cat$ID_TIENDA <- factor(fil.cat$ID_TIENDA)

joint_id_temporada <- CrossTable(TEMPORADA, ID_TIENDA, prop.chisq = FALSE)
joint_counts_id_temporada <- joint_id_temporada$t

barplot(joint_counts_id_temporada, beside = TRUE, col=brewer.pal(9,"Paired"),
        ylab = "No. Artículos", xlab = "ID_TIENDA & TEMPORADA")
title(main)
legend('topright', levels(TEMPORADA), pch=15, col=brewer.pal(9,"Paired"), ncol = 3,  cex = 1)

