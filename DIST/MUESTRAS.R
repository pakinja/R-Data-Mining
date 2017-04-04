


#pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
ct3044 <- aggregate(sub[,c("TOTAL")], by=list(sub$EMAIL,sub$ID_TIENDA,sub$NO_TRANSACCION,
                                              sub$FECHA_CREACION,sub$CATEGORIA,
                                              sub$CANTIDAD,sub$UBICACION), "sum")
names(ct3044)<- c("EMAIL","ID_TIENDA", "NO_TRANSACCION", "FECHA", "CATEGORIA","CANTIDAD","UBICACION", "MONTO")
ct3044 <-ct3044[order(ct3044$EMAIL, decreasing = TRUE),]

write.csv(ct3044, "tommy_3044.csv")
