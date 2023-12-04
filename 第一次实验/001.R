library(ggplot2)
library(dplyr)

rt<-read.table("beenswax.txt",head=TRUE);
#x11(1)
ggplot(rt,aes(x = MeltingPoint))+
  stat_ecdf(color = "red")+
  labs(title = "混合物累积分布图")+
  theme(plot.title = element_text(hjust = 0.5))
#x11(2)
ggplot(rt,aes(x = Hydrocarbon))+
  stat_ecdf(color = "red")+
  labs(title = "碳氢化合物累积分布图")+
  theme(plot.title = element_text(hjust = 0.5))
#直方图
#x11()
with(rt,hist(MeltingPoint, main="直方图",xlab = "MeltingPoint", ylab="quantity",col = "yellow",border = "blue"));
#x11()
with(rt,hist(Hydrocarbon, main="直方图",xlab = "MeltingPoint", ylab="quantity",col = "yellow",border = "blue"));
#Q-Q图
#()
tibble(y = rt$MeltingPoint)%>% 
  ggplot(aes(sample = y)) +
  geom_qq() + geom_qq_line()
#x11()
tibble(y = rt$Hydrocarbon)%>% 
  ggplot(aes(sample = y)) +
  geom_qq() + geom_qq_line()
#计算分位数
with(rt,
  quantile(rt$MeltingPoint,probs = c(0.90,0.75,0.50,0.25,0.10))
  
)
with(rt,
     quantile(rt$Hydrocarbon,probs = c(0.90,0.75,0.50,0.25,0.10))
     
)


