rm(list=ls())

pt = 0
pw = 0
for (i in 1:10000) {
  x = runif(20, -sqrt(3), sqrt(3))
  y = runif(20, -sqrt(3)+0.5, sqrt(3) + 0.5)
  pt = pt + as.numeric(t.test(x, y, paired = F)$p.value<0.05)
  pw = pw + as.numeric(wilcox.test(x, y, paired = F)$p.value<0.05)
  cat("循环已经进行了",i,"次.\r")
}
pt = pt / 10000
pw = pw / 10000

pw/pt



pt = 0
pw = 0
for (i in 1:10000) {
  x = rnorm(100, 0, 1)
  y = rnorm(100, 0.3/sqrt(200), 1)
  pt = pt + as.numeric(t.test(x, y, paired = F)$p.value<0.05)
  pw = pw + as.numeric(wilcox.test(x, y, paired = F)$p.value<0.05)
  cat("循环已经进行了",i,"次.\r")
}
pt = pt / 10000
pw = pw / 10000

pw/pt
n=20
norm.power<-function(n){
  dd<-rep(0,1000)
  for(s in 1:1000){
    pt = 0
    pw = 0
    for (i in 1:n) {
      x = rnorm(n, 0, 1)
      y = rnorm(n, 2/sqrt(n), 1)
      pt = pt + as.numeric(t.test(x, y, paired = F)$p.value<0.05)
      pw = pw + as.numeric(wilcox.test(x, y, paired = F)$p.value<0.05)
    }
    dd[s]<-pw/pt
  }
  
  return(mean(dd))
}

norm.power(20)

seq(-5,5,0.2)






