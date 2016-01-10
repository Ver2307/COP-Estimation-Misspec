# Close windows and clear variables
rm(list = ls(all = TRUE))
graphics.off()

# If necessary, please install the following package.
# install.packages("copula")

library(copula)

# Load results from simulation study
res.frank.0.25.l   = read.table("ResFrankLarge0.25.txt",   header = TRUE)
res.frank.0.5.l    = read.table("ResFrankLarge0.5.txt",    header = TRUE)
res.frank.0.75.l   = read.table("ResFrankLarge0.75.txt",   header = TRUE)
res.gumbel.0.25.l  = read.table("ResGumbelLarge0.25.txt",  header = TRUE)
res.gumbel.0.5.l   = read.table("ResGumbelLarge0.5.txt",   header = TRUE)
res.gumbel.0.75.l  = read.table("ResGumbelLarge0.75.txt",  header = TRUE)
res.clayton.0.25.l = read.table("ResClaytonLarge0.25.txt", header = TRUE)
res.clayton.0.5.l  = read.table("ResClaytonLarge0.5.txt",  header = TRUE)
res.clayton.0.75.l = read.table("ResClaytonLarge0.75.txt", header = TRUE)

tau = c(0.25, 0.5, 0.75)

# Calculate corresponding theoretical theta for Kendall's tau of 0.25, 0.5 and 0.75 

# Frank
theta.F.0.25 = iTau(frankCopula(), 0.25) 
theta.F.0.5  = iTau(frankCopula(), 0.5) 
theta.F.0.75 = iTau(frankCopula(), 0.75) 


# Gumbel
theta.G.0.25 = iTau(gumbelCopula(), 0.25) 
theta.G.0.5  = iTau(gumbelCopula(), 0.5) 
theta.G.0.75 = iTau(gumbelCopula(), 0.75) 

# Clayton 
theta.C.0.25 = iTau(claytonCopula(), 0.25) 
theta.C.0.5  = iTau(claytonCopula(), 0.5) 
theta.C.0.75 = iTau(claytonCopula(), 0.75) 

# Function that calculates the percentage relative 
# bias for each estimator, dependence level
# and true copula 

rel.bias.plot = function(x.1, x.2, x.3, true.theta.1, true.theta.2, 
                         true.theta.3, copula, estim.method) {
  if ((estim.method == "ML") & (copula == "frank")) {
    Rel.Bias.ML.frank = c(mean(x.1$theta.frank.ML/true.theta.1), 
                          mean(x.2$theta.frank.ML/true.theta.2), 
                          mean(x.3$theta.frank.ML/true.theta.3))
    return(Rel.Bias.ML.frank)
  } else if ((estim.method == "ML") & (copula == "gumbel")) {
    Rel.Bias.ML.gumbel = c(mean(x.1$theta.gum.ML/true.theta.1), 
                           mean(x.2$theta.gum.ML/true.theta.2), 
                           mean(x.3$theta.gum.ML/true.theta.3))
    return(Rel.Bias.ML.gumbel)
  } else if ((estim.method == "ML") & (copula == "clayton")) {
    Rel.Bias.ML.clayton = c(mean(x.1$theta.clay.ML/true.theta.1), 
                            mean(x.2$theta.clay.ML/true.theta.2), 
                            mean(x.3$theta.clay.ML/true.theta.3))
    return(Rel.Bias.ML.clayton)
  } else if ((estim.method == "tau") & (copula == "frank")) {
    Rel.Bias.tau.frank = c(mean(x.1$theta.frank.tau/true.theta.1), 
                           mean(x.2$theta.frank.tau/true.theta.2), 
                           mean(x.3$theta.frank.tau/true.theta.3))
    return(Rel.Bias.tau.frank)
  } else if ((estim.method == "tau") & (copula == "gumbel")) {
    Rel.Bias.tau.gumbel = c(mean(x.1$theta.gum.tau/true.theta.1), 
                            mean(x.2$theta.gum.tau/true.theta.2), 
                            mean(x.3$theta.gum.tau/true.theta.3))
    return(Rel.Bias.tau.gumbel)
  } else if ((estim.method == "tau") & (copula == "clayton")) {
    Rel.Bias.tau.clayton = c(mean(x.1$theta.clay.tau/true.theta.1), 
                             mean(x.2$theta.clay.tau/true.theta.2), 
                             mean(x.3$theta.clay.tau/true.theta.3))
    return(Rel.Bias.tau.clayton)
  } else if ((estim.method == "new") & (copula == "frank")) {
    Rel.Bias.new.frank = c(mean(x.1$theta.new.frank/true.theta.1), 
                           mean(x.2$theta.new.frank/true.theta.2), 
                           mean(x.3$theta.new.frank/true.theta.3))
    return(Rel.Bias.new.frank)
  } else if ((estim.method == "new") & (copula == "gumbel")) {
    Rel.Bias.new.gumbel = c(mean(x.1$theta.new.gumbel/true.theta.1), 
                            mean(x.2$theta.new.gumbel/true.theta.2), 
                            mean(x.3$theta.new.gumbel/true.theta.3))
    return(Rel.Bias.new.gumbel)
  } else if ((estim.method == "new") & (copula == "clayton")) {
    Rel.Bias.new.clayton = c(mean(x.1$theta.new.clayton/true.theta.1), 
                             mean(x.2$theta.new.clayton/true.theta.2), 
                             mean(x.3$theta.new.clayton/true.theta.3))
    return(Rel.Bias.new.clayton)
  }
  
}


reset = function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0:1, 0:1, type = "n", xlab = " ", ylab = " ", axes = FALSE)
}

legend1 = c(expression(paste(hat(theta)["ML"])), expression(paste(hat(theta)[tau])), 
            expression(paste(hat(theta)["new"])), "F", "G", "C")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.9, 5.5, 1.5, 1), new = TRUE)
par(mfrow = c(3, 1))

# Frank cop = true cop
par(xpd = T, mar = par()$mar + c(0, 0, 0, 7))
plot(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                        theta.F.0.5, theta.F.0.75, "frank", "new"), 
     type = "b", pch = 22, lty = "solid", 
     bg = "gray", col = "blue", cex = 6.5, ylim = c(0.17, 1.1), lwd = 3, las = 1, 
     main = "Frank", xlab = NA, xaxt = "n", mgp = c(2.7, 1, 0), ylab = "Rel. Perc. Bias", 
     cex.axis = 1.7, cex.main = 2.5, cex.lab = 1.5)
axis(1, at = tau, cex.axis = 1.7)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "gumbel", "new"), 
       type = "b", col = "blue", pch = 22, lty = "solid", bg = "green", cex = 6.5, lwd = 3)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "clayton", "new"), 
       type = "b", pch = 22, lty = "solid", col = "blue", bg = "yellow", cex = 6.5, lwd = 3)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "frank", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "gray", cex = 5, lwd = 1.8)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "gumbel", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "green", cex = 5, lwd = 1.8)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "clayton", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "yellow", cex = 5, lwd = 1.8)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "frank", "ML"), 
       type = "b", pch = 21, "twodash", bg = "gray", cex = 3)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "gumbel", "ML"), 
       type = "b", pch = 21, "twodash", bg = "green", cex = 3)

points(tau, rel.bias.plot(res.frank.0.25.l, res.frank.0.5.l, res.frank.0.75.l, theta.F.0.25, 
                          theta.F.0.5, theta.F.0.75, "clayton", "ML"), 
       type = "b", pch = 21, lty = "twodash", bg = "yellow", cex = 3)



# Gumbel = true copula
plot(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, theta.G.0.25, 
                        theta.G.0.5, theta.G.0.75, "frank", "new"), 
     type = "b", pch = 22, lty = "solid", bg = "gray", col = "blue", cex = 6.5,
     ylim = c(0.17, 4), lwd = 3, ylab = "Rel. Perc. Bias",  las = 1, main = "Gumbel",
     xlab = NA, xaxt = "n", mgp = c(2.7, 1, 0), cex.axis = 1.7, cex.main = 2.5, cex.lab = 1.5)
axis(1, at = tau, cex.axis = 1.7)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "gumbel", "new"), 
       type = "b", col = "blue", pch = 22, lty = "solid", bg = "green", cex = 6.5, lwd = 3)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "clayton", "new"), 
       type = "b", pch = 22, lty = "solid", col = "blue", bg = "yellow", cex = 6.5, lwd = 3)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "frank", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "gray", cex = 5, lwd = 1.5)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "gumbel", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "green", cex = 5, lwd = 1.5)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "clayton", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "yellow", cex = 5, lwd = 1.5)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "frank", "ML"), 
       type = "b", pch = 21, lty = "twodash", bg = "gray", cex = 3)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "gumbel", "ML"), 
       type = "b", pch = 21, lty = "twodash", bg = "green", cex = 3)

points(tau, rel.bias.plot(res.gumbel.0.25.l, res.gumbel.0.5.l, res.gumbel.0.75.l, 
                          theta.G.0.25, theta.G.0.5, theta.G.0.75, "clayton", "ML"), 
       type = "b", pch = 21, lty = "twodash", bg = "yellow", cex = 3)


# Clayton

plot(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                        theta.C.0.25, theta.C.0.5, theta.C.0.75, "frank", "new"), 
     type = "b", pch = 22, cex.main = 2.5, lty = "solid", bg = "gray", col = "blue", 
     cex = 6.5, ylim = c(0.17, 4),lwd = 3, cex.axis = 1.7, cex.lab = 1.5, las = 1, 
     main = "Clayton", xlab = expression(tau), xaxt = "n", mgp = c(2.7, 1, 0), 
     ylab = "Perc. Rel. Bias")
axis(1, at = tau, cex.axis = 1.7)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "gumbel", "new"), 
       type = "b", col = "blue", pch = 22, lty = "solid", bg = "green", cex = 6.5, lwd = 3)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "clayton", "new"), 
       type = "b", pch = 22, lty = "solid", col = "blue", bg = "yellow", cex = 6.5, lwd = 3)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "frank", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "gray", cex = 5, lwd = 1.5)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "gumbel", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "green", cex = 5, lwd = 1.5)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "clayton", "tau"), 
       type = "b", pch = 24, lty = "dashed", col = "red", bg = "yellow", cex = 5, lwd = 1.5)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "frank", "ML"), 
       type = "b", pch = 21, lty = "twodash", bg = "gray", cex = 3)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "gumbel", "ML"), 
       type = "b", pch = 21, lty = "twodash", bg = "green", cex = 3)

points(tau, rel.bias.plot(res.clayton.0.25.l, res.clayton.0.5.l, res.clayton.0.75.l, 
                          theta.C.0.25, theta.C.0.5, theta.C.0.75, "clayton", "ML"), 
       type = "b", pch = 21, lty = "twodash", bg = "yellow", cex = 3)

reset()
legend(0.92, 1.05, legend1, col = c("black", "red", "blue", "darkgray", "green", "yellow"), 
       pch = c(21, 24, 22, 20, 20, 20), bty = "n", cex = 1.25)
