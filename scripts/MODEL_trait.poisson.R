model{ 
  
  #LIKELIHOOD
  for(i in 1:n){ 
    
    y[i] ~ dpois(lambda[i]) 
    log(lambda[i]) <- mu[i]
    mu[i] <- bCity[City[i]] + b[1]*prcnt_park_area[i] + b[2]*mean_park_EIratio[i] + 
      b[3]*park_clumpy[i] + b[4]*regional.C[i] + b[5]*Pop.Dens[i] +
      b[6]*Year[i] + b[7]*total.effort[i] + bSmall*small[i] + bLarge*large[i]
  } 
  
  
  #PRIORS ON COEFFICIENTS
  for(j in 1:npar){
    b[j] ~ dnorm(0,0.001)
  }
  
  
  for(c in 1:ncity){
    bCity[c] ~ dnorm(mu.city, tau.city)
  }
  
  
  bSmall ~ dnorm(0, 0.001) #coefficient for species whose trait value is smaller than mean trait value
  bLarge ~ dnorm(0, 0.001) #coefficient for species whose trait value is larger than mean trait value
  
  mu.city ~ dnorm(0,0.001)  #prior for precision of city intercept
  
  #tau.city <- 1/(sigma.city*sigma.city) #prior for SD for city random intercept. Tau is precision (1/variance)
  tau.city ~ dgamma(3,2) #estimated tau to feed into sigma (SD)
  
  #sigma.city ~ dunif(0,100) #originally: dunif(0.1,100) but not sure why.
  sigma.city <- 1/sqrt(tau.city) #derived sigma (SD)
  
  
}