iliteracy<-c(7.33,10.80,15.60,8.86,9.70,18.52,17.71,21.24,23.20,14.24,13.82,17.97,10.00,10.15,17.05,10.94,20.97,16.40,16.59,17.40,14.12,18.99,30.18,28.48,61.13,21.00,32.88,42.14,25.02,14.65)
GDP<-c(15044,12270,5345,7730,22275,8447,9455,8136,6834,9513,4081,5500,5163,4220,4259,6468,3881,3715,4032,5122,4130,3763,2093,3715,2732,3313,2901,3748,3731,5167)
par(mfrow = c(1, 3))
hist(iliteracy,border = F,col = "gray7")
hist(GDP,border = F,col = "gray7")
plot(iliteracy,GDP,main="Scatter plot of Iliteracy rate and GDP")
#编写cor.pearson、cor.spearman,cor.kendall函数
cor.pearson<-function(x,y)
{
  x1<-x-mean(x)
  y1<-y-mean(y)
  numerator<-sum(x1*y1)
  denominator<-sqrt(sum(x1^2)*sum(y1^2))
  cor<-numerator/denominator
  Z<-cor*sqrt(length(x)-2)/sqrt(1-cor^2)
  p_value<-2*pt(Z,length(x)-2)
  list(p_value=p_value,cor=cor)
  
}


cor.spearman<-function(x,y)
{
  x.rank<-rank(x)
  y.rank<-rank(y)
  x1<-x.rank-mean(x.rank)
  y1<-y.rank-mean(y.rank)
  numerator<-sum(x1*y1)
  denominator<-sqrt(sum(x1^2)*sum(y1^2))
  cor<-numerator/denominator
  Z<-cor*sqrt(length(x)-2)/sqrt(1-cor.test^2)
  p_value<-2*pt(Z,length(x)-2)
  list(p_value=p_value,cor=cor)
}

cor.kendall<-function(x,y)
{
  options(digits = 4)
  n<-length(x)
  s=0
  c=0
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      s=s+sign((iliteracy[i]-iliteracy[j])*(GDP[i]-GDP[j]))
      c=c+1
    }
  }
  tau <- 2/(n*(n-1))*s
  Z <- tau*sqrt((9*n*(n-1))/(2*(2*n+5)))
  p_value<-2*pnorm(Z)
  list(p_value=p_value,cor=cor)
}
#则有以下的结果
x11()
cor.test(iliteracy,GDP)
cor.pearson(iliteracy,GDP)
cor.test(iliteracy,GDP,method="spearman")
cor.spearman(iliteracy,GDP)
cor.kendall(iliteracy,GDP)  
cor.test(iliteracy,GDP,method="kendall")
