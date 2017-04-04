
library(data.table)
library(dplyr)
library(plyr)

pv$TOTAL <- as.numeric(as.character(pv$TOTAL))

intpunto <- pv[pv$EMAIL %in% int, ]


no_tran <- aggregate(intpunto[,c("TOTAL")], by=list(intpunto$EMAIL,intpunto$NO_TRANSACCION,
                              intpunto$FECHA_CREACION,intpunto$UBICACION), "sum")

names(no_tran) <- c("Email","No_Transaccion","Fecha_Creacion", "Ubicacion", "Monto")
no_tran <- no_tran[order(no_tran$Email, decreasing = TRUE),]
#no_tran$Email <- as.factor(no_tran$Email)

mail_monto <- aggregate(intpunto[,c("TOTAL")], by=list(intpunto$EMAIL), "sum")
mail_monto <-mail_monto[order(mail_monto[[1]], decreasing = TRUE),]

c_mail <- count(no_tran$Email)
names(c_mail) <- c("Email", "NoTransacciones")
c_mail <-c_mail[order(c_mail$Email, decreasing = TRUE),]

mail_dias <- aggregate(intpunto[,c("TOTAL")], by=list(intpunto$EMAIL,intpunto$FECHA_CREACION), "sum")
c_dias <- count(mail_dias[1])
names(c_dias) <- c("Email", "CantidadDiasDiferentes")
c_dias <-c_dias[order(c_dias$Email, decreasing = TRUE),]

mail_noart<- aggregate(intpunto[,c("CANTIDAD")], by=list(intpunto$EMAIL), "sum")
names(mail_noart) <- c("Email","No_Productos")
mail_noart <-mail_noart[order(mail_noart$Email, decreasing = TRUE),]


cc <- count(intpunto$EMAIL)
cc <-cc[order(cc[[1]], decreasing = TRUE),]

no_estado <- aggregate(intpunto[,c("TOTAL")], by=list(intpunto$EMAIL,intpunto$UBICACION), "sum")
no_estado <- no_estado[order(no_estado[[1]], decreasing = TRUE),]
dup <- which(duplicated(no_estado$Group.1))
no_estado <- no_estado[-dup, ] 
#########################################
final <- cbind(c_mail, c_dias$CantidadDiasDiferentes,cc$freq,mail_monto$x,
               no_estado$Group.2)

names(final) <- c("Email", "NoTransacciones", "CantidaddiasDiferentes",
                  "NoProductos","Monto", "Estado")

write.csv(final, "ClientesTommyPunto-Ecom.csv")



