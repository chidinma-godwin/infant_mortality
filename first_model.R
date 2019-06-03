## read the data
data<- read.csv("inf_data.csv", header = T)

## load libraries
library(R2jags)
library(mcmcplots)

## create jags model
mod_string<- "model{
for(i in 1:nobs){
dead[i] ~ dbern (p[i])
logit(p[i])<- int + b[1]*urbrur[i] + b[2]*middleclass[i] + b[3]*rich[i] +
b[4]*noedu[i] + b[5]*priedu[i] + b[6]*magebel20[i] + b[7]*mageabv34[i]
+b[8]*gender[i]+b[9]*firstbirth[i] + b[10]*birthintbel2[i] + b[11]*birthintabv2[i]
}
int~dnorm(0,0.001)
for(k in 1:11){b[k]~dnorm(mu,tau)}
mu~dnorm(0,0.001)
sig~dunif(0,100)
tau<- pow(sig,-2)
}"

## read in the data frame for jags
data_jags<- as.list(data)
data_jags$nobs<- nrow(data)

## define the parameters whose posterior distributions we need:
params<- c("b")

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
saveRDS(mod, "./model_1.rds")

## load the model
model<- readRDS("./model_1.rds")

## make a caterpillar plot of the model output
mod.mcmc<- as.mcmc(model)
caterplot(mod.mcmc, parms =c("b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","b[7]","b[8]",
                             "b[9]","b[10]","b[11]"),style = "plain",
          labels=c("bintbel2","middleclass","no edu","male","m.agebel20","m.ageabv34","primary edu",
                   "firstbirth","rich","urban","bintabv2"))
abline(v=0)
