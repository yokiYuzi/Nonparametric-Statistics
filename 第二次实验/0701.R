require(stats); require(graphics)
f.tit <-  "faithful data: Eruptions of Old Faithful"

ne60 <- round(e60 <- 60 * faithful$eruptions)
all.equal(e60, ne60)             # 相对差异 ~ 1/10000
table(zapsmall(abs(e60 - ne60))) # 0、0.02 或 0.04
faithful$better.eruptions <- ne60 / 60
te <- table(ne60)
te[te >= 4]                      # (太多) 5 的许多倍数！
plot(names(te), te, type = "h", main = f.tit, xlab = "Eruption time (sec)")

plot(faithful[, -3], main = f.tit,
     xlab = "Eruption time (min)",
     ylab = "Waiting time to next eruption (min)")
lines(lowess(faithful$eruptions, faithful$waiting, f = 2/3, iter = 3),
      col = "red")

ker.density=function(x,h){
  x=sort(x)
  n=length(x);s=0;t=0;y=0
  for(i in 2:n)
    s[i]=0
  for(i in 1:n){
    for(j in 1:n)
      s[i]=s[i]+exp(-((x[i]-x[j])^2)/(2*h*h))
    t[i]=s[i]
  }
  for(i in 1:n)
    y[i]=t[i]/(n*h*sqrt(2*pi))
  z=complex(re=x,im=y)
  hist(x,freq=FALSE)
  lines(z)
}
x11()
#分别作三个数据的核估计如下：
ker.density(faithful$better.eruptions,0.8)
