
# logistic regression: planet in habitable zone (0/1)

D = read.table("../take_data/system_catalogue.dat",sep="\t",header=TRUE)

xtabs(~n_hab_planets, D)
xtabs(~n_hab_planets + n_planets, D)

D$habplnt = D$n_hab_planets > 0  # define binary outcome: planets in habitable zone (0/1)
xtabs(~habplnt, D)

fit = glm(formula = habplnt ~ n_planets + star_temperature + star_metallicity + star_radius + star_mass, 
         data = D,
         family = "binomial")

summary(fit)

aggregate(fit$fitted.values, by=list(fit$y), FUN=mean)

pSun = predict(fit, type="response", newdata=D[D$name=="Sun",])  # predicted value for the Sun
pSun # 0.952

hist(fit$fitted.values, breaks=20)
abline(v=pSun,col="red")

D[D$name=="Sun",] # Sun has 9 planets - that's counting Pluto in

