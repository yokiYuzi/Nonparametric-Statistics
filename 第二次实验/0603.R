#Kendall检验法
#载入数据
data1<-read.table("result2.txt")
data2<-as.numeric(data1$V1)
data<-matrix(data2,nrow=10,ncol=12)
data=t(data)
#不妨假设H0：裁判打分是一致的
zhibiao<-rep(0,120)
rzhibiao<-matrix(zhibiao,12,10)
#求每组的秩和
for(i in 1:10)
{
   
  br1<-rank(data[,i])
  print(br1)
   for(t in 1:12){
        rzhibiao[t,i]=br1[t]
      }
}
#发现原秩表是有结表
#计算R_i
R.i<-rep(0,12)
for(i in 1:12){
  br2=0
  for(t in 1:10){
    br2=br2+rzhibiao[i,t]
  }
  R.i[i]<-br2
}
print(R.i)
qiuheR.j=0
for(i in 1:12){
  qiuheR.j=qiuheR.j+(R.i[i])^2;
}
#求有结系数T
T<-11*(2^3-2)+(4^3-4)
#计算总系数W_c
W=(12*qiuheR.j-(3*10^2*12*(12+1)^2))/(10^2*(12^3-12)-10*T)
print(W)
#得到计算的W值为0.1135611
#近似卡方值应有
Xc<-10*(12-1)*W
print(Xc)
#得到卡方近似值为12.49172，查表有X_0.95,11为19.675
#12.49172<19.675
#拒绝原假设 认为评委打分是不一致的