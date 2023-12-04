#Fisher精确性检验 
compare<-matrix(c(6,4,1,9),nrow=2, ncol=2 )

fisher.test(compare, alternative = "greater")