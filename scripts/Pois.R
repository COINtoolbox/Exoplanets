require(arm)
# Poisson model for planet occurence

exo_dat<-read.table("../take_data/system_catalogue.dat",sep="\t",header=TRUE)
# select for now Period,  Planet radius, Planet mass

y<-exo_dat$n_planets
#x1<-exo_dat$star_temperature
x2<-exo_dat$star_radius
M1<-bayesglm(y~x2,family=poisson(link="log"))
display(M1)
coefplot(M1)

yweight <- predict(M1, list(wt = x2),type="response")

plot(x2, y, pch = 16, xlab="Stelar radius", ylab="Number of planets")
lines(x2, yweight)
