model{ 
  
  #LIKELIHOOD
  for(i in 1:n){ 
    #tau is precision (1/variance)
    y[i] ~ dpois(lambda[i]) 
    log(lambda[i]) <- mu[i]
    mu[i] <- bCity[City[i]] + b[1]*prcnt_park_area[i] + b[2]*mean_park_EIratio[i] + 
      b[3]*park_clumpy[i] + b[4]*regional.C[i] + b[5]*Pop.Dens[i] +
      b[6]*Year[i] + b[7]*total.effort[i]
    
  } 
  
  #PRIORS ON COEFFICIENTS
  for(j in 1:npar){
    b[j] ~ dnorm(0,0.001)
  }
  
  
  for(c in 1:ncity){
    bCity[c] ~ dnorm(mu.city, tau.city)
  }
  
  mu.city ~ dnorm(0,0.001) 
  
  tau.city <- 1/(sigma.city*sigma.city) #prior for precision of city intercept
  
  sigma.city ~ dunif(0,100) #prior for SD for city random intercept
  
  
}