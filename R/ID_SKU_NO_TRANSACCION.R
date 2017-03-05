pvA <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA, pv$NO_TRANSACCION,pv$CODIGO_SKU), "sum")
pvA <- pvA[order(pvA[[2]], decreasing = TRUE),]


tab <- as.data.frame(table(pvA[1:100,2:3]))
