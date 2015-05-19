# Poisson model for planet occurence

exo_dat<-read.table("exoplanet.eu_catalog.dat",sep="\t",header=TRUE)
# select for now Period,  Planet radius, Planet mass


dim(aggregate(exo_dat, by = list(exo_dat$star_name),FUN=length))