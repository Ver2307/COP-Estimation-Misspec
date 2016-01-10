# Close windows and clear variables
rm(list = ls(all = TRUE))
graphics.off()

# please install these packages if necessary
# install.packages("fGarch")

library(fGarch)

#Load results Value-at-Risk estimation
VaR.ind = read.table("VaRresIndices.txt", header = TRUE)

quantiles.points = c(5, 1, 0.5, 0.1)/100
steps = 2350

exceedances.ratio = function(copula, quantile.one, estimator, dataset) {
  # selecting the L.real.left column
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

########### Indices

exceedances = matrix(NA, nrow = 9, ncol = length(quantiles.points))
colnames(exceedances) = quantiles.points
rownames(exceedances) = c("Frank_ML", "Frank_tau", "Frank_new", "Gumbel_ML", "Gumbel_tau", 
                          "Gumbel_new", "Clayton_ML", "Clayton_tau", "Clayton_new")
res_frank   = rep(0, 3)
res_gumbel  = rep(0, 3)
res_clayton = rep(0, 3)


for (i in quantiles.points) {
  ML  = exceedances.ratio("frank", i, "ML",  VaR.ind)
  tau = exceedances.ratio("frank", i, "tau", VaR.ind)
  new = exceedances.ratio("frank", i, "new", VaR.ind)
  
  res_frank = cbind(res_frank, c(ML, tau, new))
}

for (i in quantiles.points) {
  ML  = exceedances.ratio("gumbel", i, "ML",  VaR.ind)
  tau = exceedances.ratio("gumbel", i, "tau", VaR.ind)
  new = exceedances.ratio("gumbel", i, "new", VaR.ind)
  
  res_gumbel = cbind(res_gumbel, c(ML, tau, new))
}

for (i in quantiles.points) {
  ML  = exceedances.ratio("clayton", i, "ML",  VaR.ind)
  tau = exceedances.ratio("clayton", i, "tau", VaR.ind)
  new = exceedances.ratio("clayton", i, "new", VaR.ind)
  
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

