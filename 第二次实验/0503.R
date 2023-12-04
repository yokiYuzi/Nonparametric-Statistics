#Mantel-Haenszel检验
c1<-c(87,70,45);
c2<-c(91,86,15);
c3<-c(41,38,10);
data1<-rbind(c1,c2,c3)
#假设H0：三种年龄的关注度一样
x=array(data1,c(3,3,3))
mantelhaen.test(x)