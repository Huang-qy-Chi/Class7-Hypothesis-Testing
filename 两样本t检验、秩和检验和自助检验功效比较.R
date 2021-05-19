#本程序比较两样本t检验、秩和检验与自助检验在不同条件下的检验效果，作者：欧阳乾弘、黄启岳
#黄启岳主要完成分布参数给定、功效算法设计；欧阳乾弘主要完成编程实现以及绘图工作。
#程序经过同为作者的欧阳乾弘同意后发布
rm(list=ls())


bootstrap.test <- function(x, y, B){
  n = length(x)
  m = length(y)
  origin_stat = (mean(x) - mean(y)) / sqrt(var(x)/n + var(y)/m)
  
  data = c(x, y)
  bs_sample = sample(data, size = (n+m)*B, replace = T)
  bs_sample = matrix(bs_sample, n+m, B)
  
  bs_stat = rep(0, B)
  for (i in 1:B) {
    x_s = bs_sample[1:n, i]
    y_s = bs_sample[(n+1):(n+m), i]
    bs_stat[i] = (mean(x_s) - mean(y_s)) / sqrt(var(x_s)/n + var(y_s)/m)
  }
  
  p = mean(abs(bs_stat)>=abs(origin_stat))
  return(p)
}

t_w_b_test <- function(para, type, size, alpha, att, B){
  pt = 0
  pw = 0
  pb = 0
  if ((type[1]=='Laplace')|(type[2]=='Laplace')) library(extraDistr)
  
  for (i in 1:att) {
    xy = list(rep(0, size[1]), rep(0, size[2]))
    for (j in 1:2) 
      xy[[j]] = switch(type[j],
        Uniform = runif(size[j], para[j*2-1], para[j*2]),
        Normal = rnorm(size[j], para[j*2-1], para[j*2]),
        Laplace = rlaplace(size[j], para[j*2-1], para[j*2]),
        Logistic = rlogis(size[j], para[j*2-1], para[j*2]),
        Cauchy = rcauchy(size[j], para[j*2-1], para[j*2]))
    
    x = xy[[1]]
    y = xy[[2]]
    pt = pt + as.numeric(t.test(x, y, paired = F, var.equal = T)$p.value<alpha)
    pw = pw + as.numeric(wilcox.test(x, y, paired = F)$p.value<alpha)
    if (B!=0) #use the given value
      pb = pb + as.numeric(bootstrap.test(x, y, B)<alpha)
    else #if B is not given, let B = mean(size)
      pb = pb + as.numeric(bootstrap.test(x, y, mean(size))<alpha)
  }
  return(c(pt, pw, pb) / att)
}

plot3 <- function(dat, t, s, g, l) {
  plot(g, dat[1, ], type = "l", col = "red", ylim = c(0, 1),  cex.main = 1.5,
       xlab = "Location Shift", ylab = "Power", lwd = 3, cex.lab = 1.5, 
       cex.axis = 1.2, main = paste0(t, ', ', 'size = ', as.character(s)))
  par(new = T)
  plot(g, dat[2, ], type = "l", col = "blue", ylim = c(0, 1), xlab = "", 
       ylab = "", lwd = 3, cex.axis = 1.2)
  par(new = T)
  plot(g, dat[3, ], type = "l", col = "green", ylim = c(0, 1), xlab = "", 
       ylab = "", lwd = 3, cex.axis = 1.2)
  if (l==1) 
    legend("topleft", c('Student t Test', 'Wilcoxon Test', 'Bootstrap Test'),
           col = c("red", "blue", "green"), lwd = c(3, 3, 3), bty = "n",
           y.intersp = 0.8, cex = 1.2, x.intersp = 0.9, inset = -0.02)
  par(new = F)
  w_t = dat[2, ] / dat[1, ]
  b_t = dat[3, ] / dat[1, ]
  r = range(c(w_t, b_t))
  plot(g, w_t, type = "l", col = "purple", lwd = 3, xlab = "Location Shift",
       ylim = c(r[1] - 0.05, r[2] + 0.05), ylab = "Power Ratio", cex.lab = 1.5,
       cex.axis = 1.2, main = "Power Ratio", cex.main = 1.5)
  par(new = T)
  plot(g, b_t, type = "l", col = "brown", lwd = 3, xlab = "",
       ylim = c(r[1] - 0.05, r[2] + 0.05), ylab = "", cex.axis = 1.2)
  hori = switch(t, Uniform = 1,
                Normal = 3 / pi,
                Laplace = 1.5,
                Logistic = pi^2 / 9,
                Cauchy = 1)
  if (t!="Cauchy") 
    abline(h = hori, lty = 2, lwd = 2)
  par(new = F)
  if (l==1)
    legend("topright", c("P(W) / P(t)", "P(B) / P(t)"), lwd = c(3, 3), 
           bty = "n", col = c("purple", "brown"), y.intersp = 0.8, cex = 1.2, 
           x.intersp = 0.9, inset = -0.01)
}

main <- function(type, att, B, grid) {
  outdat = list(0, 0, 0)
  para = switch(type, Uniform = c(0, 2 * sqrt(3)),
                Normal = c(0, 1),
                Laplace = c(0, 1 / sqrt(2)),
                Logistic = c(0, pi / sqrt(3)),
                Cauchy = c(0, 1))
  paragrid = as.list(rep(0, length(grid)))
  for (i in 1:length(grid)) {
    if (type=='Uniform') paragrid[[i]] = c(para, para + grid[i])
      else paragrid[[i]] = c(para, grid[i], para[2])
  }
  size = c(10, 30, 50)
  leg = c(1, 0, 0)
  for (i in 1:3) {
    dat = sapply(paragrid, t_w_b_test, type = c(type, type), 
                 size = c(size[i], size[i]), alpha = 0.05, att = att, B = B)
    plot3(dat, type, size[i], grid, leg[i])
    outdat[[i]] = dat
  }
  return(outdat)
}

att = 500
B = 0
grid = seq(0.1, 2, by = 0.05)
testseq = c('Uniform', 'Normal', 'Uniform', 'Logistic', 'Cauchy')
fulldata = list(0, 0, 0, 0, 0)
for (i in 1:5) {
  jpeg(file = paste0(testseq[i], ".jpeg"), height = 500, width = 900,
       quality = 90)
  par(mfcol = c(2, 3))
  fulldata[[i]] = main(testseq[i], att, B, grid)
  dev.off()
  cat("循环已进行",i,"次.\r")
}




