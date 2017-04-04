#library(ggplot2)

tienda_id <- unique(pv$ID_TIENDA)
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]

agg <- function(tienda){
  aggregate(tienda[,c("TOTAL")], by=list(tienda$TALLA), "sum")
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

ord <- function(tienda){
  tienda[order(tienda[[2]], decreasing = TRUE),]
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("",i, sep = ""), ord(get(i)))
}

sub <- function(tienda){
  tienda[1:20,]
}

for(i in dfs[2:length(dfs)])
{
  assign(paste("",i, sep = ""), sub(get(i)))
}


#####
pl<- function(tienda,k){
  
  ggplot(tienda, aes(x = reorder(Group.1,-x), y = x)) + 
    geom_bar(stat = "identity", fill="brown", colour="yellow")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(limits=c(0, 8), breaks = seq(0,8,by = 0.5))+
    labs(x="TALLA", y="% respecto al total de venta")+
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
