
#tienda_id <- unique(pv$ID_TIENDA)

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
no_tiendas <- length(unique(pv$ID_TIENDA))
##########################################################

agg <- function(tienda){
 aggregate(tienda[,c("CANTIDAD")], by=list(tienda$CODIGO_SKU),"sum")
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

sub <- function(tienda){
  tienda[1:30,]
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("",i, sep = ""), sub(get(i)))
}

###########################################################

pl<- function(tienda,k){
  
 ggplot(tienda, aes(x = reorder(Group.1,-x), y = x)) + 
    geom_bar(stat = "identity", fill="pink", colour="yellow")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(limits=c(0, 40), 
                       breaks = seq(0,40,by = 10))+
    labs(x="SKU", y="CANTIDAD")+
    ggtitle(as.character(k))
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("p",i, sep = ""), pl(get(i),i))
  
}

plots <- ls()[sapply(mget(ls(), .GlobalEnv), is.ggplot)]

for(i in 1:length(plots)){
  print(get(plots[i]))
}