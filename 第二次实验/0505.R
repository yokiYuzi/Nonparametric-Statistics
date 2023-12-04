data<-data.frame(read.table("result.txt"))
c1<-c(data[1:5,1])
c2<-c(data[7:11,1])
c3<-c(data[13:17,1])
c4<-c(data[19:23,1])
c5<-c(data[25:29,1])
manyi<-rbind(c1,c2,c3,c4,c5);
ridi.test<-function(x)
{
  order.num=ncol(x)
  treat.num=nrow(x)
  rowsum=rowSums(x)#O_i.
  colsum=colSums(x)#O_.i
  total=sum(rowsum)
  N=(colsum/2)[1:order.num]+c(0,(cumsum(colsum))[1:order.num-1])
  ri=N/total#每个顺序类的得分
  p_coni=x/outer(rowsum,rep(1,order.num),"*")##概率阵——i水平下属于第j顺序类的概率、
  pi.=rowsum/total
  score=p_coni%*%ri
  confi_inter=matrix(c(score-1/sqrt(3*rowsum),score+1/sqrt(3*rowsum)))
  
  if(length(rle(sort(ri))$lengths)==length(ri))#不打结
  {
    w=(12*total/(total+1)*sum(rowsum*(score-0.5)^2))
  }
  if(length(rle(sort(ri))$lengths)<=length(ri))#打结
  {
    tao<-rle(sort(ri))$lengths
    T=1-sum(tao^3-tao)/(order.num^3-order.num)
    w=(12*total/((total+1)*T))*sum(rowsum*(score-0.5)^2)}
  pvalue=pchisq(w,treat.num-1,lower.tail = FALSE)
  list(score,confi_inter=confi_inter,W=w,pvalue=pvalue)
}
options(digits = 4)##设结果为4位有效数字

res_data=ridi.test(manyi)
graph_data<-res_data$confi_inter
x11()
plot(0,0,ylim = c(0,1),xlim = c(1,5),xlab = "function",ylab = "",main="Ridgit value confidence interval",col="gray7")

abline(h=0.5)
for(i in 1:(nrow(graph_data)/2))lines(c(i,i),c(graph_data[i],graph_data[i+5]))
