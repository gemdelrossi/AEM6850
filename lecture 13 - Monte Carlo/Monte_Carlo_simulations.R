#===============================================================================
# AEM 6850
# Monte Carlo (MC) simulations
#===============================================================================

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 1). Preliminary -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Clean up workspace and load or install necessary packages if necessary
  rm(list=ls())
  want <- c("RColorBrewer","lmtest","sandwich","multiwayvcov")
  need <- want[!(want %in% installed.packages()[,"Package"])]
  if (length(need)) install.packages(need)
  lapply(want, function(i) require(i, character.only=TRUE))
  rm(want, need)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 2). Main code -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# 1. Simple MC simulation -----

# Here we illustrate a simple Monte Carlo (MC) simulation (or experiment) to
# illustrate the result of an important theorem in statistics and econometrics
# called the Central Limit Theorem. Broadly speaking, the theorem says that
# the sum of independent draws from a particular distribution has a normal 
# distribution, no matter what the underlying distribution is.

# For instance, if you throw a dice 100 times, and sum the number you get,
# that total will be normally distributed. Let's check it out to see if this
# holds with a simulation.
  
# Simulate the random draw from a dice
  
  sample(1:6,1) # 1 draw
  
  sample(1:6,100, replace=T) # 100 draws
 
# Settings
  r <- 10^3 # number of experiments, parameter
  n <- 100 #number of draws (per experiment)
  
# Run one simulation
  set.seed(123) #remember this is important for reproducibility
  x <- sample(1:6,n, replace=T) # 100 draws
  sum(x)  

# Repeat this r times
  set.seed(123)
  out1 <- sapply(1:r, function(i) {
    x <- sample(1:6,n, replace=T) # 100 draws
    sum(x)  
  })
# out1 is vector of sums
  
# Plot the distribution (called the sampling distribution)
  hist(out1, col="grey", border=NA, breaks=30, freq=F) #Normal distribution! 
  lines(density(out1)) 
  
  
# 2. Similar application with a Poisson distribution ------
  
# Settings
  r <- 10^3 # MC simulations
  n <- 100 # sample size
  lambda <- 2 # value of Poisson parameter
  
# Randomly generate data (1 experiment)
  x <- rpois(n, lambda)
  hist(x, col="grey", border=NA)

# Check min and max
  sum(x)
  mean(x)
  var(x)

# Repeat this many r times
  out2 <- sapply(1:r, function(i) {
    sum(rpois(n, lambda))
  })
  
# View distribution
  hist(out2, breaks=30, col="grey", border=NA, freq=F) #still normal distribution!
  lines(density(out2))
  
# generated new data from an underlying distribution we're assuming 
# and then run simulations/regressions
  
# 3. Approximate pi -----
  
# What is the area of a circle ? A = pi * r^2
# If circle has radius of 1, then area is equal to pi

# How could we approximate the area of a circle (thus approximate pi)?
  
# One strategy is to:
# 1. draw random points in a 2x2 square (area is 4)
# 2. select the points that fall within the "interior" circle
# 3. count the proportion of dots that fell within the circle
# 4. take the ratio of these dots in the circle to the total
# 5. multiply by 4 to obtain pi
  
# Settings
  n <- 10^3 # number of random points
  x1 <- runif(n,-1,1) # random x coordinate, uniform distribution
  x2 <- runif(n,-1,1) # random y coordinate, uniform distribution

# Plot all poitns
  plot(x1,x2, col="grey", xlim=c(-1,1), ylim=c(-1,1))
  abline(v=0)
  abline(h=0)
  
# Compute distance to origin
  d <- sqrt(x1^2 + x2^2) # Pythagoras theorem

# Plot dots with d less than 1 in red, other in grey  
  plot(x1,x2, col=ifelse(d<=1,"red","grey"))
  abline(v=0)
  abline(h=0)
  
# Share of red points?
  sum(d<=1) / length(d) #about 80% of points fall within unit circle

# Area of the circle? Multiply by 4 (the area of the full square, a 2 by 2)
  
  4 * sum(d<=1) / length(d) #first estimate of pi
  
# Recall that pi is area of circle (with radius equal 1)
  

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# EXERCISE 1: Develop a MC simulation to approximate the value of pi with
# r = 1000 and n = 1000. Plot the distribution of all estimates, as well
# the mean of all simulations.
  
  # settings
  r <- 10^3
  n <- 10^3
  set.seed(123)
  
  # experiment 
  
  ex1 <- sapply(1:r, function(i) {
    x1 <- runif(n,-1,1) # random x coordinate, uniform distribution
    x2 <- runif(n,-1,1) # random y coordinate, uniform distribution
    d <- sqrt(x1^2 + x2^2) #distance 
    4 * sum(d<=1) / length(d) #estimate
  })
  
  
  # histogram 
  
  hist(ex1, breaks = 20, xlim = c(3,3.3))
  abline(v=mean(ex1), col = "red", lwd =2)
  mean(ex1)
  
  # as sample size goes up (increase n), the variance goes down. Consistency increases. 
  # change the number of repetitions, things don't change much 
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
  
  
# 4. MC in a regression context -----
  
  # Generate simulated data
  n <- 500 # number of observations in dataset
  b0 <- 1 # coefficient for intercept
  b1 <- -1 # coefficient for x1
  b2 <- 1 # coefficient for x2
  data <- data.frame(x1=runif(n), x2=runif(n)*2) # data set

  # Suppose the data generating process (DGP) is:
  # y = b0 + b1*x1 + b2*x2 + e 
  # error term generates randomness
  
  # Generate y based on this DGP:
  set.seed(123) # set seed so we al get the same result
  sig <- 2 # variance of error term
  e <- rnorm(n, sd=sig) # normally distributed and i.i.d
  data$y <- b0 + b1*data$x1 + b2*data$x2 + e
  
  # What is the theoretical variance of beta hat? "Beta" is true population parameter, "beta hat" is estimate
  # beta_hat = (X' %*% X)^-1 %*% X' %*% y 
    # y is random variable, therefore beta hat is a random variable and has a variance
    # data (x1 x2) is fixed because it's data you collect
  # var( beta_hat ) = ?
  
  X <- cbind(1,as.matrix(data[,c("x1","x2")]))

  t(X) %*% X # t(X) is transpose

  solve( t(X) %*% X) # invert matrix
  
  vc_beta <- solve( t(X) %*% X) * sig^2 # variance of beta hat 
  
  se <- sqrt(diag(vc_beta)) # standard errors of true value (not estimate)
  
  # Run regression... EVERYTHING BELOW ARE ESTIMATES
  
  # ...by "hand"
  solve( t(X) %*% X ) %*% t(X) %*% as.matrix(data$y)
  
  # ...with canned code
  reg <- lm(y~x1+x2, data)
  summary(reg)
  
  # Extract coefficients from canned regression
  coef(reg)
 
  # Get canned estimates of standard errors
  vcov(reg)
  sqrt(diag(vcov(reg)))
  
  # Confirm this is right
  summary(reg) # Look at Std. Error column 
  
  # Run a MC simulation and extract coefficient estimates 
  # as well as the canned estimates of the SEs at each run
  r <- 500
  set.seed(123)
  out4 <- sapply(1:r, function(i) {
    e <- rnorm(n, sd=sig) # normally distributed and i.i.d
    data$y <- b0 + b1*data$x1 + b2*data$x2 + e
    reg <- lm(y~x1+x2, data)
    b <- coef(reg)
    se <- sqrt(diag(vcov(reg)))
    c(b,se)
  })  
  out4 <- t(out4) # t is transpose
  
  # Plot coefficients
  hist(out4[,2], breaks=seq(-2,2,.05), border=NA, col="lightblue", freq=F, xlim=c(-2,2), ylim=c(0,3))
  hist(out4[,3], breaks=seq(-2,2,.05), border=NA, col="lightgreen", freq=F, xlim=c(-2,2), add=T)
  abline(v=colMeans(out4[,2:3]), lty=2)  
  abline(v=c(b1,b2), col="red") # these are the true values  

  # Standard error of betas
  # relates to the variance of distributions
  
    # Theoretical - we imposed i.i.d errors
    se # true values

    # SD of estimated coefficients across all simulations
    apply(out4[,1:3],2,sd) # close but not same as true se
    
    # Average of SE from canned regression routine
    # Important: makes assumption of i.i.d errors
    colMeans(out4[,4:6]) 
    
    # Conclusions:
    # 1- You can recover beta + Std Err from MC simulations
    # 2- The lm() routine provides correct SEs *when* errors are i.i.d - important
  
# 5. Collinearity in regressors -----

# I am sure you have heard that multicollinearity is a problem in regression 
# analysis. Here we will explore exactly what happens when regressors are
# correlated. The goal here is to illustrate how you can easily set up your own
# MC simulation to explore a particular econometric problem. You don't necessarily
# need to "blindly" follow the suggestions of a theorist. :)
    
# Note that the variables in the previous example were uncorrelated
  plot(data[,1:2])

# Let's create a new dataset    
  n <- 1000
  k <- 2 # this is a parameter that regulates how correlated Xs are
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n)*k # increasing k reduces correlation between Xs
  x2 <- x2/sd(x2) # standardize variance of X2
  data <- data.frame(x1=x1,x2=x2)

# View scatterplot of variables again
  plot(data[,1:2]) # much more correlated

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# JOINT EXERCISE: Develop a MC simulation showing how rising correlation
# between x1 and x2 affects whether b1 and b2 are biased, and 2- whether
# it affects the SE of b1 and b2. Explore k=seq(.2,3,.2), r=500, n=1000.
    
  # Settings
  r <- 500
  n <- 1000
  sig <- 2
  klist <- seq(.2,3,.2)
    
  # Runs simulations
  out5 <- lapply(klist, function(k) {
    
      print(k)
    
      # Generate data
      set.seed(123)
      x1 <- rnorm(n)
      x2 <- x1 + rnorm(n) * k 
      x2 <- x2 / sd(x2) 
      data <- data.frame(x1=x1,x2=x2)
      plot(data[,1:2])
      
      # Loop over simulations
      o <- sapply(1:r, function(i) {
        e <- rnorm(n,sd=sig) # normally distributted and i.i.d
        data$y <- b0 + b1*data$x1 + b2*data$x2 + e
        reg <- lm(y~x1+x2, data)
        b <- coef(reg)
        se <- sqrt(diag(vcov(reg)))
        c(b,se)
      })  
      o <- t(o)
    })
  names(out5) <- klist
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
  
# What does collinearity cause?
    
  # Empty plot
  plot(0, type="n", xlim=c(-2,2), ylim=c(0,7), axes=F, xlab="",ylab="Density")
  axis(1)
  axis(2)
  
  # Loop over ks
  colors <- colorRampPalette(brewer.pal(11,"Spectral"))(length(klist))
  lapply(klist, function(k) {
    lines(density(out5[[paste(k)]][,2]), col=colors[match(k,klist)], lwd=1)
    lines(density(out5[[paste(k)]][,3]), col=colors[match(k,klist)], lwd=1)
  })
  abline(v=c(b1,b2), lty=2)
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# EXERCISE 2: Explore what occurs when you drop x2. Develop a MC simulation 
# showing how rising correlation between x1 and x2 affects whether b1 is biased, 
# and 2- whether it affects the SE of b1. k= seq(.2,3,.2), r=500, n=1000.
# Store results in list called ex2.
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
  
# What occurs with SE of b1 when k rises when you include or drop x2
  plot(klist, sapply(out5, function(x) sd(x[,2])), pch=1, type="o",ylim=c(0,.35), main="SE(b1)", ylab="Standard Error")
  lines(klist, sapply(ex2, function(x) sd(x[,2])), pch=2, type="o")
  legend("topright",c("x1 and x2 included","only x1 included"), pch=c(1,2), lwd=1, inset=.02) 
  
# 6. Consistency of an estimator -----

# I'm sure you are familiar with the term consistency to refer to an estimator
# The idea is that the variance of the estimator decreases and converges to 0
# as the sample increases in size.

# How can we capture with a simulation that OLS is a consistent estimator?  
  
# Generate y based on the DGP explored above:
  
  # Generate simulated data
  n <- 1000 # number of observations in dataset
  b0 <- 1 # coefficient for intercept
  b1 <- 5 # coefficient for x1
  data <- data.frame(x1=runif(n)) # data set
  
  #set.seed(123) # set seed so we all get the same result
  sig <- 10 # variance of error term
  e <- rnorm(n, sd=sig) # normally distributed and i.i.d
  data$y <- b0 + b1*data$x1 + e
  
# Run the regression
  reg <- lm(y~x1, data)
  reg 

# Now repeat the simulation many times
  r <- 200
  out6 <- sapply(1:r, function(i) {
    e <- rnorm(n, sd=sig) # normally distributed and i.i.d
    data$y <- b0 + b1*data$x1 + e
    coef(lm(y~x1, data))
  })


# Visualize the distribution of b1
  hist(out6["x1",], border=NA, col="lightblue", freq=F, xlim=c(-15,20), ylim=c(0,.4), main=paste0("n = ",n), xlab="")
  lines(density(out6["x1",]), col="blue")
  abline(v=mean(out6["x1",]), lty=1, col="blue")
  abline(v=b1, lty=2, lwd=2)
  abline(v=0, lty=1)
  
  
# Now return go back to the beginning and increase the sample size (n=500, then 1000)
# What do you notice?


  
# The end