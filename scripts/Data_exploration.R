# Explorative analysis of exoplanet data

library(MASS)
library(reshape)
library(ggplot2)
library(VGAM) # dpospois - zero truncated poisson
source("https://raw.githubusercontent.com/cran/HMMpa/master/R/dgenpois.R")

exo_dat = read.table("../take_data/system_catalogue.dat",sep="\t",header=TRUE)
y = exo_dat$n_planets

table(y)

## fit count models
f1 = fitdistr(y, "poisson")
f1$loglik

f2 = fitdistr(y, "negative binomial")
f2$loglik

f3 = fitdistr(y, dgenpois, list(lambda1=2, lambda2=-0.1))
f3$loglik

f4 = fitdistr(y, dpospois, list(lambda=2))
f4$loglik

yy = 0:(max(y)+1)
C = data.frame(y=yy) 
C = merge(C, as.data.frame(table(y) / length(y)),all = TRUE)
names(C) = c("count", "empirical")
C$empirical[is.na(C$empirical)] = 0
C$pois = dpois(C$count,lambda = f1$estimate)
C$negbin = dnbinom(C$count, size = f2$estimate["size"], mu = f2$estimate["mu"])
C$genpois = dgenpois(C$count, lambda1 = f3$estimate["lambda1"], f3$estimate["lambda2"])
C$pospois = dpospois(C$count, lambda = f4$estimate["lambda"])
CC = melt(C,id="count")
names(CC)[2:3] = c("distribution", "density")

ggplot(CC,aes(x=count,y=density)) + 
   geom_line(aes(colour=distribution), size=1) + 
   scale_x_continuous(breaks=0:10) + 
   ggtitle(paste("Exoplanet count distribution for", length(y), "stars")) + 
   xlab("planet count")






