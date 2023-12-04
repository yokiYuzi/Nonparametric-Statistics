qiuzhi=function(x){
  n1=length(x)
  n2=length(unique(x))
  zhi=NULL;
  if(n1==n2){
    for(i in 1:n1){
      zhi=c(zhi,sum(x<=x[i]))
    }
  }
  else{
    for(i in 1:n1){
      zhi=c(zhi,mean(sum(x<=x[i],1):sum(x<=x[i])))
    }
  }
  zhi
}
jiedian=function(x1){
  n1=length(x1)
  x2=unique(x1);
  n2=length(x2)
  jie=NULL
  for(i in 1:n2){
    n=sum(x1==x2[i])
    if(n>1){jie=c(jie,n)}
  }
  jie
}  
a1<-c(85,82,82,79);
a2<-c(87,75,86,82);
zhi1=qiuzhi(a1);
zhi1
rank(a1,ties.method = "average")