rm(list = ls())

#单总体正态检验


#单侧检验，已知原假设均值与方差，以mu>mu0为备择假设
ztest1 <- function(data, alpha=0.05, mu0=0, sigma0=1){
  alpha1 <- alpha
  Z <- sqrt(length(data))*(mean(data)-mu0)/sigma0  #正态检验统计量
  if(Z>qnorm(1 - alpha, mu0, sigma0)){     #单边检验，拒绝域包含Z
    cat("Reject H0 at the significance level of", alpha1, ".")
  }else{
    cat("Accept H0 at the significance level of", alpha1,".")
  }
}

data <- rnorm(100,3,1)  #均值为3，方差为1的正态数据
ztest1(data, 0.05, 0, 1)  

#输出p值的单侧正态检验
ztest2 <- function(data, mu0, sigma0){
  alpha1 <- alpha
  Z <- sqrt(length(data))*(mean(data)-mu0)/sigma0  #正态检验统计量
  pvalue <- 1 - pnorm(Z, mu0, sigma0)     #p值
  cat("The p-value of the test is:",pvalue) 
}
data2 <- rnorm(100, 0.1, 1) 
ztest2(data2, 0, 1)

###思考：双侧检验的函数如何实现？

#方差未知的均值t检验
#以mu>mu0为例，mu0=0
data3 <- rnorm(100,0.2,1)
t.test(data3, mu = 0, alternative = "greater")

#直接实现单边检验
ttest <- function(data, alpha = 0.05, mu = 0){
  alpha2 = alpha
  T1 = sqrt(length(data))*(mean(data)-mu0)/var(data) #T统计量
  pvalue <- 1 - pt(T1, length(data)-1)     #p值
  cat("The p-value of the test is:",pvalue)
}
ttest(data3)


#卡方检验
#如何编写卡方检验的函数？


#两样本检验
##方差相等的均值检验
data4 <- rnorm(100, 3, 1)
data5 <- rnorm(150, 2, 1)
t.test(data4, data5, alternative = "two.sided")

##方差不等均值检验
data6 <- rnorm(150, 2, 4)
t.test(data4, data6, alternative = "two.sided", var.equal = F)

#想一想，如何根据两样本t检验过程编写两样本t检验的函数？

##方差的F检验
var.test(data5, data6)

#功效模拟计算
u <- rep(0, 1000)
for(i in 1:1000){
  d1 <- rnorm(100, 0.3, 1)  #原假设mu0=0，事实上是错误的
  T2 <- sqrt(length(d1))*(mean(d1)-mu0)/var(d1)
  if(T2>qt(0.95,length(d1)-1 )){     #单边检验，拒绝域包含Z，即判断正确
    u[i] = 1    #判断正确记为1
  }
}
mean(u)

