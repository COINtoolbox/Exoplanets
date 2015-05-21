# Poisson model for planet occurence

exo_dat<-read.table("exoplanet.dat",sep="\t",header=FALSE)
# select for now Period,  Planet radius, Planet mass
exo_dat<-as.matrix(exo_dat)

colnames(exo_dat)

dim(aggregate(exo_dat, by = list(exo_dat$star_name),FUN=length))

names(exo_dat$name)