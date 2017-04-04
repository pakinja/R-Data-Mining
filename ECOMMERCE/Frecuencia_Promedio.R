library(data.table)
library(plyr)
library(dplyr)

no_tran <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,
                                                  pv$FechaOrden), "sum")
no_tran <- no_tran[order(no_tran[[1]], decreasing = TRUE),]

dt <- data.table(no_tran)

cc <- dt[,list(dif=mean(diff(Group.2))),by=Group.1]

cc[is.na(cc)] <- 0 
names(cc) <- c("Email", "Frecuencia_Promedio (dias)")

#clientes <- read.csv("ClientesRapsodiaecom_2.csv")
clientes <- read.csv("ClientesTommyecom_2.csv")
clientes$X <- NULL

clientes <- cbind(clientes, cc$Frecuencia_Promedio)
names(clientes) <- c("Email", "No_Transacciones", "Cantidad_dias_Diferentes",
                  "No_Productos","Monto", "Estado","Frecuencia_Promedio (dias)")

clientes <- clientes[c(1,2,3,4,7,5,6)]
write.csv(clientes, "ClientesTommy.csv")

