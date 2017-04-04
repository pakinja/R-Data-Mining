###################
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)

################### LECTURA DE DDBB
pv <- read.csv2('Coach DB Sales.csv', fileEncoding = "latin1")

################### PREPARACION DE VARIBLE
pv$CATEGORIA<- tolower(as.factor(gsub(" ","", pv$CATEGORIA)))
muestra <- 10
pvcat <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CATEGORIA), "sum")
pvcat <- pvcat[order(pvcat[[2]], decreasing = TRUE),]
cats <- pvcat[1:muestra, 1]
pv$CATEGORIA <- factor(pv$CATEGORIA, levels=cats)

################## FILTRAMOS DDBB POR NIVELES ELEGIDOS
pvan <- pv[pv$CATEGORIA %in% cats,]

################## RELACIONES ENTRE VARIABLES
joint_cat_id <- CrossTable(pvan$CATEGORIA, pvan$ID_TIENDA, prop.chisq = FALSE)
joint_counts_cat_id <- joint_cat_id$t


################## VISUALIZACIONES
barplot(joint_counts_cat_id, beside = TRUE, col=brewer.pal(10,"Paired"), ylab = "Frequency", xlab = "")
title("")
legend('topright', levels(pvan$CATEGORIA), pch=15, col=brewer.pal(10,"Paired"), ncol = 4,  cex = 0.75)
