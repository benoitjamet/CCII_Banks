data{ # standardize data
 xm <- mean(x)
 xsd <- sd(x)
 GFIm <- mean(GFI)
 GFIsd <- sd(GFI)
 SIZEm <- mean(SIZE)
 SIZEsd <- sd(SIZE)
 GPSm <- mean(GPS)
 GPSsd <- sd(GPS)
 ym <- mean(y)
 ysd <- sd(y)
 
 zx    <- (x-xm)/xsd
 zGFI  <- (GFI-GFIm)/GFIsd
 zSIZE <- (SIZE - SIZEm)/SIZEsd
 zGPS  <- (GPS - GPSm)/GPSsd
 zy    <- (y - ym)/ysd
 
}


model{

  for(i in 1:N){
    mu[i] <- alpha0 + alpha1*zx[i] + alphaSIZE*zSIZE[i] + alphaGPS*zGPS[i]
    zy[i] ~ dnorm(mu[i], 1/sigma^2)
  }

  sigma   ~ dexp(0.1)
#  nu <- nuMinusOne+1  # used to test with student residuals
#  nuMinusOne ~ dexp(0.1)
  alpha0   ~ dnorm(0, 1/10^2)
  alpha1   ~ dnorm(0, 1/10^2)
  #alphaGFI ~ dnorm(0, 1/10^2)
  alphaSIZE~ dnorm(0, 1/10^2)
  alphaGPS ~ dnorm(0, 1/10^2)
  
  # back to original scale
  beta0   <- alpha0   * ysd + ym - alpha1 * xm / xsd  - alphaGPS * GPSm / GPSsd - alphaSIZE * SIZEm /SIZEsd 
  # - alphaGFI * GFIm / GFIsd
  
  beta1   <- alpha1   * ysd / xsd
  #betaGFI <- alphaGFI * ysd / GFIsd
  betaGPS <- alphaGPS * ysd / GPSsd
  betaSIZE<- alphaSIZE* ysd / SIZEsd
  
}
