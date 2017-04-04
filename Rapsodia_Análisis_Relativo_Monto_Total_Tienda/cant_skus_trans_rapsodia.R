
#tienda_id <- unique(pv$ID_TIENDA)

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
no_tiendas <- length(unique(pv$ID_TIENDA))
##########################################################

agg <- function(tienda){
 aggregate(tienda[,c("CANTIDAD")], by=list(tienda$CODIGO_SKU,
                       tienda$NO_TRANSACCION),"sum")
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("v_",i, sep = ""), agg(get(i)))
}

for(i in tienda_id){
  rm(list = c(paste("T",i, sep="")))
}

rm(dfs)
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
###########################################################

tab_cant <- function(tienda){
  as.data.frame(table(tienda$Group.2))
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("",i, sep = ""), tab_cant(get(i)))
}

############################################################

cant_prom <- function(i){
  sum(i$Freq)/length(i$Freq)
}

cc <- vector()
for(i in dfs[2:length(dfs)])
{
  cc[i] <- cant_prom(get(i))
  
}


cant <- setDT(as.data.frame(cc), keep.rownames = TRUE)[]


###########################################################

pl <- ggplot(cant, aes(x = reorder(rn,-cc), y = cc)) + 
  geom_bar(stat = "identity", fill="brown", colour="yellow")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits=c(0, 2.5), breaks = seq(0,2.5,by = 0.5))+
  labs(x="ID_TIENDA", y="# Promedio de SKU's")+
  ggtitle("Cantidad promedio de SKU's por Transaccion")

print(pl)
