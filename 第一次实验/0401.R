youhao<-c(20.3,21.2,18.2,18.6,18.5,25.6,24.7,29.3,19.3,20.7,24.0,23.1,20.6,19.8,21.4);
treat.YH<-c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5);
block.YH<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3);
#通过Fieldman检验应有：H0：其品质不存在差异
friedman.test(youhao,treat.YH,block.YH)
#五类数据，自由度应该为4
#此时有： chi-squared = 6.1333<X^2_{0.05,4}=9.488
#认为可以接受H0，既油耗没有差异 
meanyh<-rep(0,3)
meanyh[1]<-mean(youhao[1:5]);meanyh[2]<-mean(youhao[6:10]);meanyh[3]<-mean(youhao[10:15]);
tzyh<-rep(0,15)
for(i in 1:5){
  tzyh[i]<-youhao[i]-meanyh[1]
}
for(i in 6:10){
  tzyh[i]<-youhao[i]-meanyh[2]
}
for(i in 11:15){
  tzyh[i]<-youhao[i]-meanyh[3]
}
rank.tzyh<-rank(tzyh,ties.method = "average")
data1<-data.frame(tzyh,rank.tzyh)
meanRj=rep(0,5)
meanRj[1]<-(1/3)*(rank.tzyh[1]+rank.tzyh[6]+rank.tzyh[11])
meanRj[2]<-(1/3)*(rank.tzyh[2]+rank.tzyh[7]+rank.tzyh[12])
meanRj[3]<-(1/3)*(rank.tzyh[3]+rank.tzyh[8]+rank.tzyh[13])
meanRj[4]<-(1/3)*(rank.tzyh[4]+rank.tzyh[9]+rank.tzyh[14])
meanRj[5]<-(1/3)*(rank.tzyh[5]+rank.tzyh[10]+rank.tzyh[15])
meanRi=rep(0,3)
meanRi[1]<-mean(rank.tzyh[1:5])
meanRi[2]<-mean(rank.tzyh[6:10])
meanRi[3]<-mean(rank.tzyh[11:15])
R.j<-rep(0,3);
for(i in 1:5){
  br<-rank.tzyh[i]
  R.j[1]=R.j[1]+br;
}
for(i in 6:10){
  br<-rank.tzyh[i]
  R.j[2]=R.j[2]+br;
}
for(i in 11:15){
  br<-rank.tzyh[i]
  R.j[3]=R.j[3]+br;
}
totalR.j<-R.j[1]^2+R.j[2]^2+R.j[3]^2;
R.i<-rep(0,5)
R.i[1]<-rank.tzyh[1]+rank.tzyh[6]+rank.tzyh[11]
R.i[2]<-rank.tzyh[2]+rank.tzyh[7]+rank.tzyh[12]
R.i[3]<-rank.tzyh[3]+rank.tzyh[8]+rank.tzyh[13]
R.i[4]<-rank.tzyh[4]+rank.tzyh[9]+rank.tzyh[14]
R.i[5]<-rank.tzyh[5]+rank.tzyh[10]+rank.tzyh[15]
totalR.i<-R.i[1]^2+R.i[2]^2+R.i[3]^2+R.i[4]^2+R.i[5]^2; 
#计算检验统计量Q
Q=((5-1)*(totalR.j-5*3^2*((5*3+1)^2)/4))/((1/6)*3*5*(5*3+1)*(2*5*3+1)-(1/5)*totalR.i)
print(Q)
#得到统计量为Q= 13.43117>X^2_{0.05,4}=9.488
#既通过HL检验认为油耗存在差异