
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CopVaRBackTesRes2** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml

Name of Quantlet: CopVaRBackTesRes2

Published in: Estimation of the Dependence Parameter in Bivariate Archimedean
              Copula Models under Misspecification

Description: Calculates the ratio of exceedances and the relative distance 
             between theoretical and empirical alpha for VaR estimates 
             obtained using the maximum likelihood estimator, the
             inversion of Kendall's tau and a p-value weighted average of the
             two in combination with the Frank, the Gumbel, and the Clayton
             copula for a series of AR-GJR-GARCH residuals from returns  of
             Volkswagen and Thyssen-Krupp. The considered time span is
             26.08.2005 to 13.08.2015. The moving window is 250.

Keywords: Copula, Gumbel, Frank, Clayton, estimation, dependence parameter

See also: CopDynEst, COPretaparch, CopVaRBackTesRes1

Author: Verena Weber

Submitted:  

Datafile:  VaRresStocks.txt

Input:  VaRresStocks.txt

Output:  Returns two tables: one contains the empirical alpha (exceedances
         ratio) and the other the relative distance between the empirical
         alpha and the theoretical alpha.

```





```R
# Close windows and clear variables
rm(list = ls(all = TRUE))
graphics.off()

# please install these packages if necessary
# install.packages("fGarch")

library(fGarch)

#Load results Value-at-Risk estimation
VaR.sto = read.table("VaRresStocks.txt",  header = TRUE)

quantiles.points = c(5, 1, 0.5, 0.1)/100
steps = 2350

exceedances.ratio = function(copula, quantile.one, estimator, dataset) {
  #selecting the L.real.left column   
  PL = as.vector(dataset[, dim(dataset)[2] - 4])     
  
  # gives the 'position' in the vector, e.g. for second entry 2
  q = which(quantiles.points == quantile.one)  
  if (estimator == "ML") {
    z = 0
  }
  if (estimator == "tau") {
    z = 4
  }
  if (estimator == "new") {
    z = 8
  }
  if (copula == "gumbel") {
    q = q + z
    copula.name = "Gumbel"
  }
  if (copula == "clayton") {
    q = length(quantiles.points) * 3 + q + z
    copula.name = "Clayton"
  }
  if (copula == "frank") {
    q = length(quantiles.points) * 6 + q + z
    copula.name = "Frank"
  }
  
  VaR.v = as.vector(dataset[, q])
  
  alpha = length(PL[PL < VaR.v])/steps
  
  return(alpha)
}

rel.exceedance = function(x, alpha) {
  e = abs(x - alpha)/alpha
  return(e)
}

######## Stocks

exceedances = matrix(NA, nrow = 9, ncol = length(quantiles.points))
colnames(exceedances) = quantiles.points
rownames(exceedances) = c("Frank_ML", "Frank_tau", "Frank_new", "Gumbel_ML", "Gumbel_tau", 
                          "Gumbel_new", "Clayton_ML", "Clayton_tau", "Clayton_new")

res_frank   = rep(0, 3)
res_gumbel  = rep(0, 3)
res_clayton = rep(0, 3)

for (i in quantiles.points) {
  ML  = exceedances.ratio("frank", i, "ML",  VaR.sto)
  tau = exceedances.ratio("frank", i, "tau", VaR.sto)
  new = exceedances.ratio("frank", i, "new", VaR.sto)
  
  res_frank = cbind(res_frank, c(ML, tau, new))
}

for (i in quantiles.points) {
  ML  = exceedances.ratio("gumbel", i, "ML",  VaR.sto)
  tau = exceedances.ratio("gumbel", i, "tau", VaR.sto)
  new = exceedances.ratio("gumbel", i, "new", VaR.sto)
  
  res_gumbel = cbind(res_gumbel, c(ML, tau, new))
}

for (i in quantiles.points) {
  ML  = exceedances.ratio("clayton", i, "ML", VaR.sto)
  tau = exceedances.ratio("clayton", i, "tau", VaR.sto)
  new = exceedances.ratio("clayton", i, "new", VaR.sto)
  
  res_clayton = cbind(res_clayton, c(ML, tau, new))
}

res_frank   = res_frank  [, -1]
res_gumbel  = res_gumbel [, -1]
res_clayton = res_clayton[, -1]

exceedances[c(1:3), ] = res_frank
exceedances[c(4:6), ] = res_gumbel
exceedances[c(7:9), ] = res_clayton


# relative distance between alpha_hat and alpha

rel.exceedances = exceedances
rel.exceedances[, 1] = rel.exceedance(exceedances[, 1], 0.05)
rel.exceedances[, 2] = rel.exceedance(exceedances[, 2], 0.01)
rel.exceedances[, 3] = rel.exceedance(exceedances[, 3], 0.005)
rel.exceedances[, 4] = rel.exceedance(exceedances[, 4], 0.001)

exceedances
rel.exceedances 
```
