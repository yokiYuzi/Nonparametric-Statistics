a<-c(8.8,8.2,5.4,4.9,8.9,4.2,5.6,7.1,5.5,8.6,6.3,3.9);
b<-c(13.0,14.5,16.5,22.8,20.7,19.6,18.4,21.3,24.2,19.6,11.7,18.9,14.6,19.8,14.5);
#用Mood方差检验法
#不妨假设H0：两种样品的方差相同
length.a<-length(a);
length.b<-length(b);
m=n=(12+15+1)/2;
c<-c(a,b);
#用平均秩法求秩
d<-rank(c,ties.method = "average")
data1<-data.frame(cbind(c,d))
M<-0;
for (i in 1:12){
  br<-(data1[i,2]-m)^2;
  M<-M+br;
}
E<-m*(m+n+1)*(m+n-1)/12;
varM<-m*n*(m+n+1)*(m+n+2)*(m+n-2)/180;
Z<-(M-E+0.5)/sqrt(varM)
print(Z)
# -0.6053207<Z_{0.05/2}=1.96  得到结论是：不能拒绝原假设 既两种样品方差相同

#用Moses中位数检验法
#将数据a分成4组，将数据b分成5组
#求各小组的离差平方和应有
amean<-c(mean(a[1:3]),mean(a[4:6]),mean(a[7:9]),mean(a[10:12]));
SSA1=SSA2=SSA3=SSA4=SSB1=SSB2=SSB3=SSB4=SSB5=0
for (i in 1:3){
  br<-(a[i]-amean[1])^2
  SSA1=SSA1+br;
}
for (i in 4:6){
  br<-(a[i]-amean[2])^2
  SSA2=SSA2+br;
}
for (i in 7:9){
  br<-(a[i]-amean[3])^2
  SSA3=SSA3+br;
}
for (i in 10:12){
  br<-(a[i]-amean[4])^2
  SSA4=SSA4+br;
}
bmean<-c(mean(b[1:3]),mean(b[4:6]),mean(b[7:9]),mean(b[10:12]),mean(b[13:15]));
for (i in 1:3){
  br<-(b[i]-bmean[1])^2
  SSB1=SSB1+br;
}
for (i in 4:6){
  br<-(b[i]-bmean[1])^2
  SSB2=SSB2+br;
}
for (i in 7:9){
  br<-(b[i]-bmean[1])^2
  SSB3=SSB3+br;
}
for (i in 10:12){
  br<-(b[i]-bmean[1])^2
  SSB4=SSB4+br;
}
for (i in 13:15){
  br<-(b[i]-bmean[1])^2
  SSB5=SSB5+br;
}
shujuA<-c(SSA1,SSA2,SSA3,SSA4);
shujuB<-c(SSB1,SSB2,SSB3,SSB4,SSB5);
zongshuju<-c(shujuA,shujuB);
rankshuju<-rank(zongshuju,ties.method = "average")
#则有第一组平方和的秩和S为
S<-0;
for(i in 1:4){
  S=S+rankshuju[i]
}
TM<-S-4*(4+1)/2
print(TM)
#TM=3 查询Moses的附表应有：W_{0.05，4，5}=3 拒绝H0
#既认为两组数据的方差不相等 ，存在差异




