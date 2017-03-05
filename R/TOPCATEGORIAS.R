


pvBsub <- pvB[1:100,]
ggplot(pvBsub, aes(reorder(x = Group.1, -x), y = x)) + 
  geom_bar(stat="identity", fill = "pink", colour="purple")+
theme(axis.text.x = element_text(angle = 45, hjust = 1))+
labs(title="TOP 100 CATEGORÍAS", x="CATEGORÍA", y="NO. ARTÍCULOS VENDIDOS")+
annotate("text",x = 50, y = max(pvBsub$x), label = main)
