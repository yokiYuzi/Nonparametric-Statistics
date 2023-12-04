#Cochran检验法
#不妨假设原假设H0：三种推销效果相同
candid1<-c(rep(1,6),0,0,1,1,1,0);
candid2<-c(0,1,0,1,0,0,0,1,0,0,0,0)
candid3<-c(1,0,1,0,0,1,0,1,0,0,0,1)
candid<-matrix(c(candid1,candid2,candid3),nrow = 12,ncol = 3);
nidot.candid=apply(candid, 1, sum)
ndotj.candid=apply(candid, 2, sum)
k=ncol(candid)
Q=(k-1)*((k*sum(ndotj.candid^2)-(sum(ndotj.candid))^2))/+(k*sum(nidot.candid)-sum(nidot.candid^2))
pvalue.candid=pchisq(Q,k-1,lower.tail = F)  
pvalue.candid
# pvalue=0.07843739>0.05，接受原假设H0，既效果相同