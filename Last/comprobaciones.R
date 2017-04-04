aaa <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CODIGO_SKU,pv$COLOR),"sum")
aaa <- aaa[order(aaa[[3]], decreasing = TRUE),]

sku <- filter(pv, CODIGO_SKU==888542839131)
bbb <- aggregate(pv[,c("CANTIDAD")], by=list(pv$COLOR),"sum")
bbb <- bbb[order(bbb[[2]], decreasing = TRUE),]


ccc <- aggregate(pv[,c("CANTIDAD")], by=list(pv$FECHA_CREACION,pv$CATEGORIA),"sum")
ccc <- ccc[order(ccc[[3]], decreasing = TRUE),]

ddd <- aggregate(fil.cat[,c("CANTIDAD")], by=list(fil.cat$CATEGORIA,
                                                  fil.cat$FECHA_CREACION),"sum")
ddd <- ddd[order(ddd[[2]], decreasing = TRUE),]


eee <- aggregate(pv[,c("CANTIDAD")], by=list(pv$CATEGORIA,
                                                  pv$ID_TIENDA),"sum")
eee <- eee[order(eee[[3]], decreasing = TRUE),]


fff <- aggregate(pv[,c("CANTIDAD")], by=list(pv$NO_TRANSACCION, pv$TIPO_PAGO), "sum")
fff <- fff[order(fff[[3]], decreasing = TRUE),]

ggg <- aggregate(pv[,c("CANTIDAD")], by=list(pv$NO_TRANSACCION), "sum")
ggg <- ggg[order(ggg[[2]], decreasing = TRUE),]

hhh <- aggregate(pv[,c("CANTIDAD")], by=list(pv$TIPO_PAGO), "sum")
hhh <- hhh[order(hhh[[2]], decreasing = TRUE),]

iii <- filter(pv, ID_TIENDA == 3010)

jjj <- as.data.frame(table(iii$CATEGORIA))
jjj <- jjj[order(jjj[[2]], decreasing = TRUE),]


barplot(jjj$Var1, jjj$Freq)
