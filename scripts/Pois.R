require(arm)
require(ggplot2)
# Poisson model for planet occurence

exo_dat<-read.table("../take_data/system_catalogue.dat",sep="\t",header=TRUE)
# select for now Period,  Planet radius, Planet mass

y<-exo_dat$n_planets
#x1<-exo_dat$star_temperature
x2<-exo_dat$star_radius
x3<-exo_dat$star_temperature
M1<-bayesglm(y~x2+x3,family=poisson(link="log"))
display(M1)
coefplot(M1)

yweight <- predict(M1, list(wt = x3),type="response")

par(mar=c(4, 4, 2, 2))
plot(x3, y, pch = 16, xlab="Stellar radius", ylab="Number of planets")
lines(x3, yweight)

ggplot(exo_dat,aes(x=star_metallicity,y=n_planets))+geom_point()
