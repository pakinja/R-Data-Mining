#library(ggplot2)


tienda_id <- unique(pv$ID_TIENDA)
pv$NO_TRANSACCION <- as.factor(pv$NO_TRANSACCION)
pv$CATEGORIA <- as.factor(pv$CATEGORIA)
pv$NO_TRANSACCION <- factor(pv$NO_TRANSACCION )
pv$CATEGORIA <- factor(pv$CATEGORIA )

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]

agg <- function(tienda){
  tienda[duplicated(tienda)|duplicated(tienda, fromLast=TRUE),]
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("pv_",i, sep = ""), agg(get(i)))
}

for(i in tienda_id){
rm(list = c(paste("T",i, sep="")))
}

rm(dfs)
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]

ord <- function(tienda){
 tienda[order(tienda[[5]], decreasing = TRUE),]
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("",i, sep = ""), ord(get(i)))
}


#####
pl<- function(tienda,k){
  
  pp <- ggplot(tienda, aes(x=NO_TRANSACCION, y=CATEGORIA))+
    geom_bin2d()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(colour = "orange", size =.6))+
    #labs(x="NO_TRANSACCION", y="")+
    ggtitle(paste("TOMMY", as.character(k)))
  
  newdat <- ggplot_build(pp)$data[[1]]
  pp + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                  label=count), col="white")
  
  }

for(i in dfs[2:length(dfs)])
{
  assign(paste("p",i, sep = ""), pl(get(i),i))
  
}

plots <- ls()[sapply(mget(ls(), .GlobalEnv), is.ggplot)]

for(i in 1:length(plots)){
print(get(plots[i]))
}
