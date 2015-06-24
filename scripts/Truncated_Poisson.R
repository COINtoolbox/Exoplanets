library(R2jags)
library(rjags)
library(ggmcmc)
library(ggplot2)
library(ggthemes)
library(pander)
library(Cairo)
library(plyr)
library(MASS)
library(scales)
library(plyr)
require(gdata)
require(runjags)
require(gdata)
# Data
exo_dat<-read.table("../take_data/system_catalogue.dat",sep="\t",header=TRUE)
# select for now Period,  Planet radius, Planet mass

exo_dat<-exo_dat[complete.cases(exo_dat[,c("n_planets","star_radius","star_metallicity")]),]
y<-exo_dat$n_planets
#x1<-exo_dat$star_temperature
x1<-exo_dat$star_radius
x2<-exo_dat$star_metallicity

X <- model.matrix(~x1 + x2)
K <- ncol(X)
N<-nrow(exo_dat)

win.data1 <- list(
  Y    = y,
  N    = N,
  X    = X,
  bet0 = rep(0, K),          #Betas count
  Bet0 = diag(0.0001, K)   #Betas count
)
win.data1

########
#Model
sink("GPGLM.txt")
cat("
    model{
    #Priors regression parameters
    beta  ~ dmnorm(bet0[], Bet0[,])  
    # Prior for size

    size~dunif(0.001,10)
    #########
    #Likelihood 
    for (i in 1:N) {
    eta[i]<-inprod(X[i,], beta[]) 
    log(mu[i])<-max(-20,min(20,eta[i]))# Ensures that large beta values do not cause numerical problems. 

     p[i]<-size/(size+mu[i])
     Y[i]~dnegbin(p[i],size)T(1,)
#    Y[i]~dpois(mu[i])T(1,)
    # Prediction
    Pred[i]~dnegbin(p[i],size)T(1,10)
    }          
    }
    ",fill = TRUE)
sink()
#######

#Inits function
inits1  <- function () {
  list(beta  = rnorm(K, 0, 0.01)#,  #Regression parameters
       #phi   = 0
  )  }

#Parameters to estimate
params1 <- c("beta", 
             "Pred"
)

#Start Gibbs sampler
GP   <- jags(data       = win.data1,
             inits      = inits1,
             parameters = params1,
             model      = "GPGLM.txt",
             n.thin     = 10,
             n.chains   = 3,
             n.burnin   = 5000,
             n.iter     = 20000)

summary(GP)
plot(GP)


jagssamples <- as.mcmc(GP)
pred_exo<-summary(as.mcmc.list(jagssamples, vars="Pred"),quantiles=c(0.005,0.5,0.975))


gsamples<-ggs(jagssamples)

ggs_density(gsamples)

