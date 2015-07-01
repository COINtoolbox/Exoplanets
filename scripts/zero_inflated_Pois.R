# Zero-Inflated Poisson GLM

library(R2jags)
library(rjags)
library(plyr)
library(ggmcmc)
library(ggplot2)
library(ggthemes)
library(pander)
library(Cairo)
library(MASS)
library(scales)
require(gdata)
require(runjags)
require(gdata)
# Data
exo_dat0<-read.table("../take_data/system_catalogue.dat",sep="\t",header=TRUE)
# select for now Period,  Planet radius, Planet mass

#exo_dat<-exo_dat0[complete.cases(exo_dat0[,c("n_planets","star_radius","star_metallicity","star_temperature")]),]
exo_dat<-exo_dat0[complete.cases(exo_dat0[,c("n_hab_planets","star_metallicity","star_radius")]),]
y<-exo_dat$n_hab_planets
#x1<-exo_dat$star_temperature
x1<-scale(exo_dat$star_metallicity)
x2<-scale(exo_dat$star_radius)
#x3<-scale(exo_dat$star_temperature)

X <- model.matrix(~x1+x2+x1*x2)
K <- ncol(X)
N<-nrow(exo_dat)

win.data1 <- list(
  Y    = y,
  N    = N,
  X    = X,
  bet0 = rep(0, K),          #Betas count
  Bet0 = diag(0.1, K),#Betas count
  aB0 = rep(0, K),
  AB0 = diag(0.1, K) 
)
win.data1


########
#Model
sink("ZIPGLM.txt")
cat("
    model{
    # 1. Priors 
    beta  ~ dmnorm(bet0[], Bet0[,])  
    aB ~ dmnorm(aB0[], AB0[,]) 
    # Prior for size


    # 2. Likelihood function
    for (i in 1:N) {

    # Logistic part

    W[i] ~ dbern(psi.min1[i])
    psi.min1[i] <- 1-psi[i]
    eta.psi[i]  <- inprod(X[i,], aB[]) 
    logit(psi[i]) <- eta.psi[i]

    # Poisson part
    Y[i]~dpois(mu.eff[i])
    mu.eff[i]<-W[i]*mu[i]+0.00001
    log(mu[i])<-max(-20,min(20,eta[i]))# Ensures that large beta values do not cause numerical problems. 
    eta[i]<-inprod(X[i,], beta[]) 

   # Discrepancy measures
    
    expY[i] <- mu[i]*(1-psi[i])
    varY[i] <- (1-psi[i])*(mu[i] + psi[i]*pow(mu[i], 2))
    PRes[i] <-(Y[i] - expY[i])/sqrt(varY[i])
    D[i]<-pow(PRes[i],2)
    
   
    # New Samples
    YNew[i] ~dpois(mu.eff[i])
    PResNew[i] <-(YNew[i] - expY[i])/sqrt(varY[i])
    DNew[i]<-pow(PResNew[i],2)
    # Prediction
    Pred[i]~dpois(mu.eff[i])
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
       aB = rnorm(K, 0, 0.01)
  )  }

inits1 <- inits0() 
inits2 <- inits0() 
inits3 <- inits0() 

#Parameters to estimate
params1 <- c("beta", 
              "aB",
             "Pred",
             "PRes",
             "Fit",
             "New"
)

library(parallel)
cl <- makeCluster(3)
ZIP1 <- run.jags(method="rjparallel", method.options=list(cl=cl),
                     data = win.data1, 
                     inits = list(inits1,inits2,inits3),
                     model="ZIPGLM.txt",
                     n.chains = 3,
                     adapt=3000,
                     monitor=c(params1),
                     burnin=10000,
                     sample=20000,
                     summarise=FALSE,
                     plots=FALSE
)

jagssamples<- as.mcmc.list(ZIP1)
summary<-extend.jags(ZIP1,drop.monitor=c("PRes","Fit","New","Pred"), summarise=TRUE)

pred.NBerrx<-summary(as.mcmc.list(ZIP1,vars="Pred"),quantiles=c(0.005,0.025,0.25,0.5,0.75,0.975, 0.995))
pred.NB2errx<-data.frame(star_radius=exo_dat$star_radius,mean=pred.NBerrx$quantiles[,4],lwr1=pred.NBerrx$quantiles[,3],lwr2=pred.NBerrx$quantiles[,2],lwr3=pred.NBerrx$quantiles[,1],upr1=pred.NBerrx$quantiles[,5],upr2=pred.NBerrx$quantiles[,6],upr3=pred.NBerrx$quantiles[,7])

limits3<-aes(ymax = pred.NB2errx$upr3, ymin=pred.NB2errx$lwr3)
limits2<-aes(ymax = pred.NB2errx$upr2, ymin=pred.NB2errx$lwr2)
limits1<-aes(ymax = pred.NB2errx$upr1, ymin=pred.NB2errx$lwr1)

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}
ggplot(exo_dat,aes(x=star_radius,y=n_hab_planets))+
  #geom_bar(data=pred.NB2errx,width=0.1,stat = "identity",aes(x=scale(star_radius),y=upr3))
  geom_errorbar(limits3, width=0.1,alpha=0.2,alpha=0.3)+
  geom_errorbar(limits2, width=0,alpha=0.2,alpha=0.3)+
  geom_errorbar(limits1, width=0,alpha=0.2,alpha=0.3)+
  geom_point(size=2,alpha=0.8,colour="blue3")+
  scale_colour_gdocs()+
  scale_shape_manual(values=c(19,2,8))+
  scale_x_continuous(trans = 'asinh',breaks=c(0,1,2.5,5,10,50),labels=c("0","1","2.5","5","10","50"))+
  theme_hc()+
  ylab(expression(N[planets]))+
  xlab(expression(R['\u0298']))+theme(legend.position="top",plot.title = element_text(hjust=0.5),
                                                axis.title.y=element_text(vjust=0.75),
                                                axis.title.x=element_text(vjust=-0.25),
                                                text = element_text(size=25))
# Model comparison 

# Predicted vs Observed 
Pred<-ggs(jagssamples,family=c("New"))[,"value"]
Obs<-ggs(jagssamples,family=c("Fit"))[,"value"]
sqrt(mean((Pred-Obs)^2))


# Dispersion parameter

require(scales)
Pres<-summary(as.mcmc.list(ZIP1, vars="PRes"),quantiles=0.5)$quantiles
Dispersion = sum(Pres^2)/(N-6)# beta.0, beta.1 and k, 3 parameters




gsamples<-ggs(jagssamples,family="beta")

ggs_density(gsamples)



p + stat_boxplot(colour="gray",geom ='errorbar')+geom_boxplot(aes(group=type,colour=type,fill=type),outlier.shape = 19,colour="gray",fatten=2,size=1,outlier.size=2,outlier.colour = "gray",notchwidth = 0.35,notch=F,data=clus_data)+
  theme_hc()+
  scale_fill_economist()+
  theme(strip.background = element_rect(fill="gray95"),plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position="none",plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(vjust=0.75),axis.text.x=element_text(size=25),
        strip.text.x=element_text(size=25),
        axis.title.x=element_text(vjust=-0.25),
        text = element_text(size=25))


