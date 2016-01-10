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

table = function(x, true.theta) {
  Mean.ML  = c(mean(x$theta.frank.ML), sd(x$theta.frank.ML), mean(x$theta.gum.ML), 
               sd(x$theta.gum.ML), mean(x$theta.clay.ML), sd(x$theta.clay.ML))
  Mean.tau = c(mean(x$theta.frank.tau), sd(x$theta.frank.tau), mean(x$theta.gum.tau), 
               sd(x$theta.gum.tau), mean(x$theta.clay.tau), sd(x$theta.clay.tau))
  Mean.new = c(mean(x$theta.new.frank), sd(x$theta.new.frank), mean(x$theta.new.gumbel), 
               sd(x$theta.new.gumbel), mean(x$theta.new.clayton), sd(x$theta.new.clayton))
  
  Bias.ML  = c(mean(x$theta.frank.ML - true.theta), NA, mean(x$theta.gum.ML - true.theta), 
               NA, mean(x$theta.clay.ML - true.theta), NA)
  Bias.tau = c(mean(x$theta.frank.tau - true.theta), NA, mean(x$theta.gum.tau - true.theta), 
               NA, mean(x$theta.clay.tau - true.theta), NA)
  Bias.new = c(mean(x$theta.new.frank - true.theta), NA, mean(x$theta.new.gumbel - true.theta), 
               NA, mean(x$theta.new.clayton - true.theta), NA)
  
  KL.ML    = c(mean(x$KL.F.ML),  sd(x$KL.F.ML),  mean(x$KL.G.ML),  sd(x$KL.G.ML), 
               mean(x$KL.C.ML),  sd(x$KL.C.ML))
  
  KL.tau   = c(mean(x$KL.F.tau), sd(x$KL.F.tau), mean(x$KL.G.tau), sd(x$KL.G.tau), 
               mean(x$KL.C.tau), sd(x$KL.C.tau))
  
  KL.new   = c(mean(x$KL.F.new), sd(x$KL.F.new), mean(x$KL.G.new), sd(x$KL.G.new), 
               mean(x$KL.C.new), sd(x$KL.C.new))
  emp.tau  = c(mean(x$emp.tau),  sd(x$emp.tau))
  
  p.F      = c(mean(x$p.value.frank),   sd(x$p.value.frank))
  p.G      = c(mean(x$p.value.gumbel),  sd(x$p.value.gumbel))
  p.C      = c(mean(x$p.value.clayton), sd(x$p.value.clayton))
  t.F      = c(mean(x$t.F), sd(x$t.F))
  t.G      = c(mean(x$t.G), sd(x$t.G))
  t.C      = c(mean(x$t.C), sd(x$t.C))
  p.values = c(p.F, p.G, p.C)
  emp.tau  = c(emp.tau, NA, NA, NA, NA)
  time     = c(round(t.F, digits = 1), round(t.G, digits = 1), round(t.C, digits = 1))
  return(cbind(Mean.ML, KL.ML, Bias.ML, Mean.tau, KL.tau, Bias.tau, Mean.new, KL.new, 
               Bias.new, p.values, emp.tau, time))
}

options(scipen = 100, digits = 5)

table.0.25 = rbind(table(res.frank.0.25.l, theta.F.0.25), 
                   table(res.gumbel.0.25.l, theta.G.0.25), 
                   table(res.clayton.0.25.l, theta.C.0.25))

table.0.5  = rbind(table(res.frank.0.5.l, theta.F.0.5), 
                   table(res.gumbel.0.5.l, theta.G.0.5), 
                   table(res.clayton.0.5.l, theta.C.0.5))

table.0.75 = rbind(table(res.frank.0.75.l, theta.F.0.75), 
                   table(res.gumbel.0.75.l, theta.G.0.75), 
                   table(res.clayton.0.75.l, theta.C.0.75))

table = rbind(table.0.25, table.0.5, table.0.75)
table 