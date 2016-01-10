
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **COPSimStudyResMix** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml
Name of Quantlet: COPSimStudyResMix

Published in: Estimation of the Dependence Parameter in Bivariate Archimedean
              Copula Models under Misspecification

Description: Summarises the results of the simulation study for the samples
             from convex sums of Gumbel and Clayton. Estimates for the copula 
             dependence parameter theta were obtained using ML, the inversion
             of Kendall's tau, and a p-value weighted average of the two in
             combination with the Frank, the Gumbel, and the Clayton copula.
             The value of alpha s indicated in the name of each data  matrix.
             Standard deviations are given in the lines below the respective
             values.

Keywords: Copula, Gumbel, Frank, Clayton, estimation, dependence parameter,
          misspecification, simulation study

See also: COPtautotheta,  COPSimStuPercRelBiasMix

Author: Verena Weber

Submitted:  

Datafile: ResMix0.1.txt, ResMix0.2.txt, ResMix0.3.txt, ResMix0.4.txt, 
          ResMix0.5.txt, ResMix0.6.txt, ResMix0.7.txt, ResMix0.8.txt, 
          ResMix0.9.txt

Input: ResMix0.1.txt, ResMix0.2.txt, ResMix0.3.txt, ResMix0.4.txt, 
       ResMix0.5.txt, ResMix0.6.txt, ResMix0.7.txt, ResMix0.8.txt, 
       ResMix0.9.txt

Output: Returns a table of the summarised results.


```



```R
# Close windows and clear variables
rm(list = ls(all = TRUE))
graphics.off()

# If necessary, please install the following package.
# install.packages("copula")

library(copula)

# Load results from simulation study
res.mix.0.1  = read.table("ResMix0.1.txt", header = TRUE)
res.mix.0.2  = read.table("ResMix0.2.txt", header = TRUE)
res.mix.0.3  = read.table("ResMix0.3.txt", header = TRUE)
res.mix.0.4  = read.table("ResMix0.4.txt", header = TRUE)
res.mix.0.5  = read.table("ResMix0.5.txt", header = TRUE)
res.mix.0.6  = read.table("ResMix0.6.txt", header = TRUE)
res.mix.0.7  = read.table("ResMix0.7.txt", header = TRUE)
res.mix.0.8  = read.table("ResMix0.8.txt", header = TRUE)
res.mix.0.9  = read.table("ResMix0.9.txt", header = TRUE)

theta.C.0.5 = iTau(claytonCopula(), 0.5)
theta.G.0.5 = iTau(gumbelCopula(), 0.5)

options("scipen"=100, "digits"=5)

table = function (x, true.theta){
  Mean.ML  = c(mean(x$theta.G.ml),  sd(x$theta.G.ml),  
               mean(x$theta.C.ml),  sd(x$theta.C.ml))
  
  Mean.tau = c(mean(x$theta.G.tau), sd(x$theta.G.tau), 
               mean(x$theta.C.tau), sd(x$theta.C.tau))
  
  Mean.new = c(mean(x$theta.G.new), sd(x$theta.G.new), 
               mean(x$theta.C.new), sd(x$theta.C.new))
  
  Bias.ML  = c(mean(x$theta.G.ml  - true.theta), NA,
               mean(x$theta.C.ml  - true.theta), NA)
  Bias.tau = c(mean(x$theta.G.tau - true.theta), NA,
               mean(x$theta.C.tau - true.theta), NA)
  Bias.new = c(mean(x$theta.G.new - true.theta), NA,
               mean(x$theta.C.new - true.theta), NA)
  
  KL.ML    = c(mean(x$KL.G.ML), sd(x$KL.G.ML),
               mean(x$KL.C.ML), sd(x$KL.C.ML))
  
  KL.tau   = c(mean(x$KL.G.tau), sd(x$KL.G.tau),
               mean(x$KL.C.tau), sd(x$KL.C.tau))
  
  KL.new   = c(mean(x$KL.G.new), sd(x$KL.G.new),
               mean(x$KL.C.new), sd(x$KL.C.new))
  emp.tau  = c(mean(x$emp.tau),  sd(x$emp.tau))
  
  p.G      = c(mean(x$p.value.gumbel),  sd(x$p.value.gumbel))
  p.C      = c(mean(x$p.value.clayton), sd(x$p.value.clayton))
  t.G      = rbind(mean(x$t.G), sd(x$t.G))
  t.C      = rbind(mean(x$t.C), sd(x$t.C))
  p.values = c(p.G, p.C)
  emp.tau  = c(emp.tau, NA, NA)
  time     = rbind(round(t.G, digits = 1), round(t.C, digits = 1))
  return(cbind(Mean.ML, KL.ML,Bias.ML, Mean.tau,  KL.tau, Bias.tau, Mean.new, 
               KL.new,Bias.new, p.values, emp.tau, time))
}

table.mix = rbind(table(res.mix.0.1, theta.C.0.5), table(res.mix.0.2, theta.C.0.5),  
                  table(res.mix.0.3, theta.C.0.5), table(res.mix.0.4, theta.C.0.5),  
                  table(res.mix.0.5, theta.C.0.5), table(res.mix.0.6, theta.C.0.5),
                  table(res.mix.0.7, theta.C.0.5), table(res.mix.0.8, theta.C.0.5),  
                  table(res.mix.0.9, theta.C.0.5))

table.mix


```
