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
exo_dat0<-read.table("../take_data/system_catalogue.dat",sep="\t",header=TRUE)
# select for now Period,  Planet radius, Planet mass

exo_dat<-exo_dat0[complete.cases(exo_dat0[,c("n_planets","star_radius","star_metallicity","star_temperature")]),]
y<-exo_dat$n_planets
#x1<-exo_dat$star_temperature
x1<-scale(exo_dat$star_radius)
x2<-scale(xo_dat$star_metallicity)
x3<-scale(exo_dat$star_temperature)

X <- model.matrix(~x1+x2)
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

    size~dgamma(0.001,0.001)
    #########
    #Likelihood 
    for (i in 1:N) {
    eta[i]<-inprod(X[i,], beta[]) 
    log(mu[i])<-max(-20,min(20,eta[i]))# Ensures that large beta values do not cause numerical problems. 

     p[i]<-size/(size+mu[i])
     Y[i]~dnegbin(p[i],size)T(1,10)
#    Y[i]~dpois(mu[i])T(1,)
    # Prediction
    Pred[i]~dnegbin(p[i],size)T(1,10)
# Discrepancy measures
YNew[i] ~dnegbin(p[i],size)T(1,10)
    expY[i] <- mu[i]
    varY[i] <- mu[i] + pow(mu[i],2) / size
    PRes[i] <-(Y[i] - expY[i])/sqrt(varY[i])
    PResNew[i] <-(YNew[i] - expY[i])/sqrt(varY[i])
    D[i]<-pow(PRes[i],2)
    DNew[i]<-pow(PResNew[i],2)
    }
Fit<-sum(D[1:N])
New<-sum(DNew[1:N])
    }
    ",fill = TRUE)
sink()
#######

#Inits function
inits0  <- function () {
  list(beta  = rnorm(K, 0, 0.01),  #Regression parameters
       size =runif(1,0.01,10)
  )  }

inits1 <- inits0() 
inits2 <- inits0() 
inits3 <- inits0() 

#Parameters to estimate
params1 <- c("beta", 
             "Pred",
             "PRes",
             "Fit",
             "New"
)

library(parallel)
cl <- makeCluster(3)
GP1 <- run.jags(method="rjparallel", method.options=list(cl=cl),
                     data = win.data1, 
                     inits = list(inits1,inits2,inits3),
                     model="GPGLM.txt",
                     n.chains = 3,
                     adapt=1000,
                     monitor=c(params1),
                     burnin=5000,
                     sample=10000,
                     summarise=FALSE,
                     plots=FALSE
)

jagssamples<- as.mcmc.list(GP1)
pred.NBerrx<-summary(as.mcmc.list(GP1,vars="Pred"),quantiles=c(0.005,0.025,0.25,0.5,0.75,0.975, 0.995))

# Model comparison 

# Predicted vs Observed 
Pred<-ggs(jagssamples.nb,family=c("New"))[,"value"]
Obs<-ggs(jagssamples.nb,family=c("Fit"))[,"value"]
sqrt(mean((Pred-Obs)^2))


# Dispersion parameter

require(scales)
Pres<-summary(as.mcmc.list(GP1, vars="PRes"),quantiles=0.5)$quantiles
Dispersion = sum(Pres^2)/(N-3)# beta.0, beta.1 and k, 3 parameters




gsamples<-ggs(jagssamples,family="beta")

ggs_density(gsamples)












