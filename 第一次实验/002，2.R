联赛1<-c(91,46,108,99,110,105,191,57,34,81);
联赛2<-c(81,51,63,51,46,45,66,64,90,28);
data1<-data.frame(联赛1,联赛2);
zhw1<-median(data1$联赛1);
zhw2<-median(data1$联赛2);
#对两组数据做符号检验 binom.test
binom.test(sum(data1$联赛1>zhw1),length(data1$联赛1),al="l");
binom.test(sum(data1$联赛2>zhw2),length(data1$联赛2),al="l");
#对两组数据作Wilcoxon符号秩检验
#对于已经计算出其中位数，直接检验其是否存在显著性差异
wilcox.test(data1$联赛1,mu=zhw1,alternative = "less",exact = FALSE,correct = FALSE,conf.int = TRUE)
wilcox.test(data1$联赛2,mu=zhw2,alternative = "less",exact = FALSE,correct = FALSE,conf.int = TRUE)
#用T检验检验样本数据
t.test(data1$联赛1,data1$联赛2)

#综上所述，这些数据中Wilcox检验更好，能够更加直观的反映数据与中位数之间的检验差异