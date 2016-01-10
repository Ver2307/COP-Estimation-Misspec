
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **COPmisspecClayGum** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml
Name of Quantlet: COPmisspecClayGum

Published in: Estimation of the Dependence Parameter in Bivariate Archimedean
              Copula Models under Misspecification

Description: Simulates 150 random samples of size 500 from a bivariate
             Clayton copula and estimates the dependence parameter theta
             via maximum likelihood and the inversion of Kendall's tau
             assuming a Gumbel copula. The obtained estimates are summarised
             in a table.

Keywords: Copula, Gumbel, Clayton, estimation, dependence parameter,
          misspecification

Author: Verena Weber

Submitted:  
```



```R
# Close windows and clear variables
rm(list = ls(all = TRUE))
graphics.off()

# please install these packages if necessary
# install.packages("copula")
# install.packages("parallel")
# install.packages("doPparallel")
# install.packages("foreach")


library(copula)
library(parallel)
library(foreach)
library(doParallel)


theta.C.0.5 = iTau(claytonCopula(), 0.5) # theta=2

reps = 150 
dim  = 2
n    = 500

cores = detectCores()
registerDoParallel(cores = cores)
getDoParWorkers()

model.fit = function(x){
  gumbel.cop = gumbelCopula(2)
  u          = pobs(x)
  #ML estimation 
  gumbel.fit.ML  = fitCopula(gumbel.cop, u, method = "mpl")
  theta.gum.ML   = gumbel.fit.ML@estimate
  gumbel.fit.tau = fitCopula(gumbel.cop, u, method = "itau")
  theta.gum.tau  = gumbel.fit.tau@estimate
  diff           = theta.gum.ML - theta.gum.tau
  abs.diff       = abs(theta.gum.ML - theta.gum.tau)
  bias.ML        = (theta.gum.ML - theta.C.0.5)
  bias.tau       = (theta.gum.tau - theta.C.0.5)
  results        = cbind(theta.gum.ML, theta.gum.tau, diff, 
                         abs.diff, bias.ML, bias.tau)
  return(results)
}

estimates.theta = foreach(icount(reps)) %dopar% {
  library(copula)
  x = rCopula(n, claytonCopula(theta.C.0.5, dim = dim))
  model.fit(x)
}


estimates.theta2 = do.call(rbind, estimates.theta)
estimates.theta  = as.data.frame((estimates.theta2))

sum.estimates.theta           = matrix(NA, nrow = 4, ncol = 4) 
colnames(sum.estimates.theta) = c("Min.","Mean", "Max.", "Bias")
rownames(sum.estimates.theta) = c("theta.ML", "","theta.tau","")

diff           = matrix(NA, nrow = 2, ncol = 3)
colnames(diff) = c("Min.", "Mean", "Max.")



sum1 = summary(estimates.theta$theta.gum.ML)

sum2 = summary(estimates.theta$theta.gum.tau)
sd(estimates.theta$theta.gum.tau)

sum3 = summary(estimates.theta$diff)
sd(estimates.theta$diff)

sum.estimates.theta[1, ]  = c(sum1[c(1, 4, 6)], mean(estimates.theta$bias.ML))
sum.estimates.theta[2, 2] = sd(estimates.theta$theta.gum.ML)
sum.estimates.theta[3, ]  = c(sum2[c(1, 4, 6)], mean(estimates.theta$bias.tau))
sum.estimates.theta[4, 2] = sd(estimates.theta$theta.gum.tau)

diff[1, ]  = sum3[c(1, 4, 6)]
diff[2, 2] = sd(estimates.theta$diff)
```
