
n <- 1
tienda_id <- unique(pv$ID_TIENDA)

tbl <- vector()
tienda <- d4010
tbl <- table(tienda$NO_TRANSACCION) > n

sub <- subset(tienda, NO_TRANSACCION %in% names(tbl)[tbl])
sub$CATEGORIA <- factor(sub$CATEGORIA)

pvA <- as.data.frame(table(sub$CATEGORIA))
pvA <-pvA[order(pvA$Freq, decreasing=TRUE), ]

colnames(pvA)[1] <- "Categoria"
pvA["Porcentaje"] <- (pvA$Freq/sum(pvA$Freq))*100

por <- length(unique(sub$NO_TRANSACCION))


pp <- ggplot(pvA , aes(x =reorder(Categoria, -Porcentaje) , y = Porcentaje)) + 
  geom_bar(stat = "identity", fill="purple", colour="yellow")+ 
  labs(title="Tommy Porcentaje Categorías Tickets Mixtos 3049", x="Categorías", y="")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(pvA$Porcentaje), by = 1))

print(pp)
