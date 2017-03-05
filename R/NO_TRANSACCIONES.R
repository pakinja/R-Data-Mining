library(lubridate)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))
pv$FECHA_CREACION <- as.Date(pv$FECHA_CREACION,  "%d/%m/%Y")
TP <- aggregate(pv[,c("CANTIDAD")], by=list(pv$ID_TIENDA, pv$NO_TRANSACCION), "sum")
TP <- TP[order(TP$x, decreasing = TRUE),]

print(length(TP$Group.1))
print(sum(pv$CANTIDAD))
print(min(pv$FECHA_CREACION))
print(max(pv$FECHA_CREACION))
print(length(unique(pv$FECHA_CREACION)))
print(sum(pv$TOTAL))
