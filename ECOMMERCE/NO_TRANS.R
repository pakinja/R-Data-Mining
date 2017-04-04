no_tran <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$NumeroOrden,
                                        pv$FechaOrden,pv$Estado), "sum")

names(no_tran) <- c("Email","NoOrden","FechaOrden", "Estado", "Monto")
no_tran <- no_tran[order(no_tran$Email, decreasing = TRUE),]


mail_monto<- aggregate(pv[,c("MontoItem")], by=list(pv$Email), "sum")
mail_monto <-mail_monto[order(mail_monto[[1]], decreasing = TRUE),]

c_mail <- count(no_tran$Email)
names(c_mail) <- c("Email", "NoTransacciones")
c_mail <-c_mail[order(c_mail$Email, decreasing = TRUE),]


mail_dias <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$FechaOrden), "sum")
c_dias <- count(mail_dias[1])
names(c_dias) <- c("Email", "CantidadDiasDiferentes")
c_dias <-c_dias[order(c_dias$Email, decreasing = TRUE),]


mail_noart<- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$NumeroOrden,pv$Producto), "sum")
names(mail_noart) <- c("Email","NoOrden", "Producto", "Monto")

mail_noart <- mail_noart[order(mail_noart$Email, decreasing = TRUE),]
c_noart <- count(mail_noart$Email)
names(c_noart) <- c("Email", "CantidadProductos")
c_noart <-c_noart[order(c_noart$Email, decreasing = TRUE),]


cc <- count(pv$Email)
cc <-cc[order(cc[[1]], decreasing = TRUE),]

#################################################################

no_estado <- aggregate(pv[,c("MontoItem")], by=list(pv$Email,pv$Estado), "sum")
no_estado <- no_estado[order(no_estado[[1]], decreasing = TRUE),]
no_estado <- no_estado[-c(174,267,305,622), ] 
#no_estado <- no_estado[-c(76,81), ]
#no_estado <- no_estado[-(267), ]
#no_estado <- no_estado[-(305), ]
#no_estado <- no_estado[-(622), ]

final <- cbind(c_mail, c_dias$CantidadDiasDiferentes,cc$freq,mail_monto$x,
               no_estado$Group.2)

names(final) <- c("Email", "NoTransacciones", "CantidaddiasDiferentes",
                    "NoProductos","Monto", "Estado")
write.csv(final, "ClientesTommyecom_2.csv")

