library(R2jags)

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
  Bet0 = diag(0.0001, K),   #Betas count
  Zeros   = rep(0, N)
)
win.data1

########
#Model
sink("GPGLM.txt")
cat("
    model{
    #Priors regression parameters
    beta  ~ dmnorm(bet0[], Bet0[,])  
    
    #########
    #Likelihood 
    C <- 10000
    phi <- 0
    for (i in 1:N) {
    #Log likelihood function of Generalized Poisson via zero trick: 
    Zeros[i] ~ dpois(Zeros.mean[i])
    Zeros.mean[i] <- -L[i] + C
    l1[i] <- log(theta[i])
    l2[i] <- (Y[i] - 1) * log(theta[i] + phi * Y[i])
    l3[i] <- (theta[i] + phi * Y[i])
    l4[i] <- loggam(Y[i] + 1)
    L[i]  <-  l1[i] + l2[i] - l3[i] - l4[i]
    
    theta[i] <- (1 - phi) * exp(eta[i])
    eta[i]   <- inprod(X[i,], beta[])           
    z[i]     <- max(-1, -theta[i] / 4)  #Condition    
    # theta[i] + phi * Y[i] must be >0..else the log is Inf
    zz[i]    <- -Y[i]/theta[i]     
    }          
    lb <- max(z[])
    lb2 <- min(zz[])
    #phi ~ dunif(lb, 0.9999)
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
             #"phi"
             "lb",
             "lb2"
)

#Start Gibbs sampler
GP   <- jags(data       = win.data1,
             inits      = inits1,
             parameters = params1,
             model      = "GPGLM.txt",
             n.thin     = 10,
             n.chains   = 3,
             n.burnin   = 5000,
             n.iter     = 10000)

summary(GP)