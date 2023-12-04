x<-c(33,45,30,20,39,34,34,21,27,38,30)
y<-c(76,103,69,50,86,85,74,58,62,88,210)

cyx=coef(lm(x~y))
md=median(x)
x1<-x[x<=md]
x2<-x[x>md]
y1<-y[y<=md]
y2<-y[y>md]
md1=median(x1)
md2=median(x2)
mw1=median(y1)
mw2=median(y2)
beta=(mw2-0)/(md2-md1)
alpha<-median(x-beta*x)
x11()
plot(x,y)
abline(alpha,beta)
abline(c(cyx),lty=2)
#两线重合 是一致的
#认为出现了离群点：(30,210)
#删去后作图如下