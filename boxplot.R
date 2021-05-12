library(tidyverse)
library(ggplot2)
attach(governance)
summary(governance)
which.min(governance$Score)


?geom_boxplot

p = ggplot(data=governance, mapping=aes(x=reorder(Elements,Score,mean), y=Score, fill=Elements))+
             geom_boxplot(show.legend=False, outlier.color = 'red', outlier.shape = 8, outlier.size = 3)+
             ggtitle("Elements of Good Governance")+
             labs(x=" ", ylab='Score')+
  theme_get()+
  coord_flip()
p
q=p + theme(legend.position = "none")
q

