
pv$EMAIL <- tolower(as.factor(gsub(" ","", pv$EMAIL)))
pvecom$Email <- tolower(as.factor(gsub(" ","", pvecom$Email)))

pv <- pv[order(pv[[4]]),]
pvecom <- pvecom[order(pvecom[[1]]),]

mailpv <- data.frame(unique(pv$EMAIL))
mailecom <- data.frame(unique(pvecom$Email))

names(mailpv) <- c("Email")
names(mailecom) <- c("Email")
#mailpv <- mailpv[order(mailpv[[1]], decreasing = TRUE),]
#mailecom <- mailecom[order(mailecom[[1]], decreasing = TRUE),]

#which(mailecom %in% mailpv)
int <- intersect(mailecom$Email, mailpv$Email)

#sss <- filter(pv, EMAIL == "yunkonk@yahoo.com.mx")
#sss <- filter(pvecom, Email == "yunkonk@yahoo.com.mx")

sel <- pvecom[pvecom$Email %in% int, ]

#selu <- sel[!duplicated(sel$Email), ]

selu <- aggregate(sel[,c("MontoItem")], by=list(sel$Email, sel$Estado), "sum")
selu <- selu[order(selu[[1]]),]

names(selu) <- c("Email", "Estado")
#write.csv(selu, "InterseccionTommy.csv")

