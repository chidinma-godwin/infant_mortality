## read the data
data<- read.csv("inf_data.csv", header = T)

## load libraries
library(R2jags)
library(mcmcplots)

## create jags model
mod_string<- "model{
for(i in 1:nobs){
dead[i] ~ dbern (p[i])
logit(p[i])<- b.state[state[i]] + b[1]*urbrur[i] + b[2]*middleclass[i] + b[3]*rich[i] +
b[4]*noedu[i] + b[5]*priedu[i] + b[6]*magebel20[i] + b[7]*mageabv34[i]
+b[8]*gender[i]+b[9]*firstbirth[i] + b[10]*birthintbel2[i] + b[11]*birthintabv2[i]
}
for(j in 1:nstate){
b.state[j]~dnorm(b.state.hat[j], tau.state)
b.state.hat[j]<- b.region[region[j]]
}
for(m in 1:nregion){
b.region[m]~dnorm(0, tau.reg)
}
for(k in 1:11){b[k]~dnorm(mu,tau)}
mu~dnorm(0,0.001)
sig~dunif(0,100)
tau<- pow(sig,-2)
#sig.state~dexp(2)
#tau.state<- pow(sig.state,-2)
sig.state<- pow(tau.state,-1/2)
tau.state~ dgamma(.1, .1)
tau.reg~ dgamma(.5, .5)
sig.reg<- abs(mu/sqrt(tau.reg))
icc.reg<- pow(sig.reg,2)/(3.29 + pow(sig.reg,2)+pow(sig.state,2))
icc.st<- pow(sig.state,2)/(3.29 + pow(sig.reg,2)+pow(sig.state,2))
}"

## read in the data frame for jags
data_jags<- as.list(data)
data_jags$nobs<- nrow(data)
data_jags$nregion<- length(unique(data$region))
data_jags$nstate<- length(unique(data$state))       

## define the parameters whose posterior distributions we need:
params<- c("sig.state","sig.reg","b","icc.st","icc.reg")

## specify initial values
inits<- list(list(.RNG.name="base::Super-Duper",.RNG.seed=333),
             list(.RNG.name="base::Super-Duper",.RNG.seed=222),
             list(.RNG.name="base::Super-Duper",.RNG.seed=444))
## fit the model. This may take few minutes
mod<- jags(model.file = textConnection(mod_string),data=data_jags,n.chains=3,
            parameters.to.save = params,n.iter=5000,n.burnin=1000,
            RNGname= "Super-Duper")

print(mod)

## save the model to disk
saveRDS(mod, "./model_2.rds")





