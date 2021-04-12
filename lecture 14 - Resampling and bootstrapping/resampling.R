#===============================================================================
# AEM 6850
# Resampling techniques
#===============================================================================

# bootstrap is good for capturing the uncertainty in data
# cross validation is good for prediction (model selection)
# 5. is for robustness tests

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 1). Preliminary -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Clean up workspace and load or install necessary packages if necessary
  rm(list=ls())
  want <- c("RColorBrewer","splines", "boot")
  need <- want[!(want %in% installed.packages()[,"Package"])]
  if (length(need)) install.packages(need)
  lapply(want, function(i) require(i, character.only=TRUE))
  rm(want, need)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 2). Main code -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# 1. Bootstrapping confidence interval -----

# "Pull yourself up by your bootstraps" is an idiomatic expression meaning that
# you will have to undertake a seemingly impossible challenge without outside
# assistance. In statistics "bootstrapping" has a similar connotation: you are
# basically going to learn about the population from which you obtained a sample
# without actually getting your hands on the population, just dealing with the
# sample you have in your hands.

# The idea behind a bootstrap is to treat the sample like the population
# and to generate "resampled" samples from your sample many times (bootstrapped
# samples) and analyze the distribution of those new sample statistics.

# Bootstrapping is the process of obtaining (via random sampling with replacement)
# multiple sub-samples from the original sample (same size), and then calculating
# sampling distributions of specific sample statistics using this new collection
# of resampled (bootstrapped) samples.

# Reshuffle the sample **WITH** replacement (some values may be repeated)

# Boostrapping is particularly useful when you have to compute the SEs of a complicated
# statistic and you don't have a tractable way of computing it analytically.

# You can use bootstrapping techniques in a regression context. Similarly,
# the idea is to think your dataset as a sample of a broader population. So the
# simplest bootstrap consists on resampling observations in your data. There are
# many other types of bootstraps for regression purposes that allow for dependence
# in the data (e.g. clustering), heteroscedasticity, etc. We will cover just the
# basics here.

# Settings
  n <- 500
  b0 <- 1
  b1 <- 1
  data <- data.frame(x1=runif(n,-1,1))
  X <- cbind(1,data$x1)
  sig <- 2
  vc_beta <- sig^2 * solve( t(X) %*% X) #variance-covariance matrix
  # assume errors are IID

  se <- sqrt(diag(vc_beta)) #population estimates "True" (not estimates)

# Create fake dataset
  e <- rnorm(n, sd = sig)
  data$y <- b0 + b1 * data$x1 + e

# Regression analysis with full data
  beta <- solve( t(X) %*% X ) %*% t(X) %*% as.matrix(data$y) # by hand
  reg <- lm(y~x1, data) # canned code
  summary(reg)
  coef(reg)
  se.est <- sqrt(diag(vcov(reg))) # SE estimate

# Reshuffle the data with replacement
  newid <- sample(1:nrow(data), nrow(data), replace=T)
  newdata <- data[newid,] # first bootstrap
  dim(newdata)
  reg <- lm(y~x1, newdata)
  coef(reg) # different than first estimate

# Now repeat this many times
  B <- 200 # number of bootstraps
  set.seed(123)
  out2 <- sapply(1:B, function(b) {
    newid <- sample(1:nrow(data), nrow(data), replace=T)
    newdata <- data[newid,]
    reg <- lm(y~x1, newdata)
    coef(reg)
  })
  out2 <- t(out2)

# Compare true SE, with estimated SE with OLS, and SD of the bootstrapped coefficients
  se # true
  se.est # OLS estimate
  apply(out2,2,sd) # SD of bootstrapped coefs

  hist(out2[,1])

# You can use the the Std Dev of all the boostrapped coefficients as an estimate
# of the standard error of the coefficient (the boostrapped SE)
# bootstrap only predicts the variation in the same, we can't apply to whole population


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# JOINT EXERCISE: Build an MC simulation to illustrate that boostrapping yields
# standard error estimate around the true value.

# Do 1 simulation

  # Create fake dataset
  e <- rnorm(n, sd = sig)
  data$y <- b0 + b1 * data$x1 + e

  # Do bootstrap
  B <- 200
  out2 <- sapply(1:B, function(b) {
    newid <- sample(1:nrow(data), nrow(data), replace=T)
    newdata <- data[newid,]
    reg <- lm(y~x1, newdata)
    coef(reg)
  })
  out2 <- t(out2)
  apply(out2,2,sd)

# Now repeat 100-200 simulations
# (you'll have 200 bootstraps for each simulation, so this will take time)

  nsims <- 200
  set.seed(123)
  temp <- lapply(1:nsims, function(i) {

    print(i)

    # Create fake dataset
    e <- rnorm(n, sd = sig)
    data$y <- b0 + b1 * data$x1 + e

    # Do bootstrap
    B <- 200
    out2 <- sapply(1:B, function(b) {
      newid <- sample(1:nrow(data), nrow(data), replace=T)
      newdata <- data[newid,]
      reg <- lm(y~x1, newdata)
      coef(reg)
    })
    out2 <- t(out2)
    apply(out2,2,sd)

  })
  temp <- do.call("rbind", temp)

# Compare bootsrapped SEs with the true one
  colMeans(temp) # bootstrapped
  se # true

# Plot distribution of the bootsrapped SEs
  hist(temp[,2], border=NA, breaks=25, xlim=range(temp[,2]) * c(0.9,1.1))
  abline(v=se, col="red", lwd=2)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# 2. Bootstrapping confidence bands -----

# Let's generate a fake dataset
  n <- 200
  f <- function(x,k=5,a=4) k / (1+x^(2*a))

  set.seed(123)
  data <- data.frame(x=runif(n,-2.5,2.5))
  data <- data[order(data$x),,drop=F]
  e <- rnorm(n,sd=1)
  data$y <- f(data$x) + e

  plot(data, pch=21, col="black", bg="lightgrey")
  lines(data$x,f(data$x), lty=2, lwd=2)

# Run model

  ?ns # cubic spline

  df <- 10

  reg <- lm(y~ns(x, df=df), data)

# Fitted values

  newdata <- data.frame(x=seq(min(data$x),max(data$x), length.out=n*10))

  lines(newdata$x, predict(reg, newdata), col="red", lwd=2)

# Generate a confidence band around this response function with a bootsrap
# The strategy consists on reshuffling the data points in the dataset

  newid <- sample(1:nrow(data), nrow(data), replace=T)

  reg <- lm(y~ns(x, df=df), data[newid,])

# Do this many times and plot the response functions

  plot(data, pch=21, col="black", bg="lightgrey")
  lines(data$x,f(data$x), lty=2, lwd=2)

  B <- 100
  lapply(1:B, function(b) {

    newid <- sample(1:nrow(data), nrow(data), replace=T)

    reg <- lm(y~ns(x, df=df), data[newid,])

    newdata <- data.frame(x=seq(min(data$x),max(data$x), length.out=n*10))

    lines(newdata$x, predict(reg, newdata), col=adjustcolor("red", alpha.f=.2), lwd=2)

  })

# Plot the point-wise confidence band around the estimate

  # Plot with data
  plot(data, pch=21, col="black", bg="lightgrey")
  lines(data$x,f(data$x), lty=2, lwd=2)

  # Compute bootstraped responses
  B <- 500
  responses <- t(sapply(1:B, function(b) {

    newid <- sample(1:nrow(data), nrow(data), replace=T)

    reg <- lm(y~ns(x, df=df), data[newid,])

    newdata <- data.frame(x=seq(min(data$x),max(data$x), length.out=n*10))

    predict(reg, newdata)

  }))

  # Draw the band
  x <- seq(min(data$x),max(data$x), length.out=n*10)
  ci <- apply(responses,2, function(x) quantile(x, c(.025,.975)))
  polygon(x=c(x,rev(x)),y=c(ci[1,],rev(ci[2,])), col=adjustcolor("red",alpha.f=.5), border=NA)


# 3. Block Bootstrap for dependent data -------

# One of the assumptions of the previous bootstrap is that observations are
# *independent*. However, it is possible that neighboring observations have
# some unobservable component that makes them "bunch" together. The bootstrapping
# procedure you develop should account for this type of dependence.

# New example with dependent data
  n <- 200
  g <- 40 # number of clusters
  f <- function(x,k=5,a=4) k / (1+x^(2*a))

  set.seed(123)
  data <- data.frame(x=runif(n,-2.5,2.5))
  data <- data[order(data$x),,drop=F]
  e1 <- rnorm(n,sd=.25)
  e2 <- rep(rnorm(g,sd=1), each=n/g) #two error components
  cluid <- rep(1:g, each=n/g)
  data$y <- f(data$x) + (e1+e2)

  colors <- colorRampPalette(brewer.pal(11,"Spectral"))(g)

  # Visualize the bunching of neighboring observations
  plot(data, pch=21, col="black", bg=colors[cluid]) #colors are same clusters/groups
  lines(data$x,f(data$x), lty=2, lwd=2)

# Do a bootstrap ignoring the data dependence

  B <- 500
  res3a <- t(sapply(1:B, function(b) {

    newid <- sample(1:nrow(data), nrow(data), replace=T)

    reg <- lm(y~ns(x, df=df), data[newid,])

    newdata <- data.frame(x=seq(min(data$x),max(data$x), length.out=n*10))

    predict(reg, newdata)

  }))

  # Draw the band
  x <- seq(min(data$x),max(data$x), length.out=n*10)
  ci <- apply(res3a,2, function(x) quantile(x, c(.025,.975)))
  polygon(x=c(x,rev(x)),y=c(ci[1,],rev(ci[2,])), col=adjustcolor("red",alpha.f=.5), border=NA)

# Is the confidence band too wide? narrow?

# Do a "block bootstrap" accounting for data dependence
# The "trick" is to resample *clusters* of obs, not individual obs

  B <- 500
  res3b <- t(sapply(1:B, function(b) {

    newcluid <- sample(unique(cluid), length(unique(cluid)), replace=T)

    newid <- sapply(newcluid, function(i) {
      (1:nrow(data))[cluid==i]

      # samples by groups instead of individual observations
    })

    reg <- lm(y~ns(x, df=df), data[newid,])

    newdata <- data.frame(x=seq(min(data$x),max(data$x), length.out=n*10))

    predict(reg, newdata)

  }))

  # Draw the band
  x <- seq(min(data$x),max(data$x), length.out=n*10)
  ci <- apply(res3b,2, function(x) quantile(x, c(.025,.975)))
  polygon(x=c(x,rev(x)),y=c(ci[1,],rev(ci[2,])), col=adjustcolor("blue",alpha.f=.5), border=NA)

# What do you notice now? Is the confidence band too wide? narrow?

# decreases our confidence in the samples because the band increases

  # 4. Canned bootstrapping in R -----

  # Settings
  n <- 500
  b0 <- 1
  b1 <- 1
  data <- data.frame(x1=runif(n,-1,1))
  X <- cbind(1,data$x1)
  sig <- 2

  # Create fake dataset
  e <- rnorm(n, sd = sig)
  data$y <- b0 + b1 * data$x1 + e

  # Regression analysis with full data
  reg <- lm(y~x1, data)
  summary(reg)
  coef(reg)

  # Bootstrap by hand
  set.seed(123)
  B <- 200
  out4 <- sapply(1:B, function(b) {
    newid <- sample(1:nrow(data), nrow(data), replace=T)
    newdata <- data[newid,]
    reg <- lm(y~x1, newdata)
    coef(reg)
  })
  out4 <- t(out4)
  head(out4)
  apply(out4,2,sd)
  colMeans(out4)

  # Canned bootstrap
  ?boot

  mycoefs <- function(dat, indices) {
    coefs <- coef(lm(y~x1, dat[indices,]))
    return(coefs)
  }

  set.seed(123)
  out4b <- boot(data, mycoefs, R=200)

  # Check it out
  out4b # original is the coef in the original regression
  coef(reg)
  out4b$t # look at all the bootstrapped estimates
  out4b
  apply(out4b$t, 2, sd) # this is the bootstrapped SE
  coef(reg) - colMeans(out4b$t) # this is the bias

  # Compare canned bootstrap to ours
  out4b
  apply(out4,2,sd) # not exactly the same cause the observations are resampled differently
  colMeans(out4)

  # 5. Canned bootstrapping with dependent data in R -----

  # New example with dependent data
  n <- 200
  g <- 40 # number of clusters
  f <- function(x,k=5,a=4) k / (1+x^(2*a))

  set.seed(123)
  data <- data.frame(x=runif(n,-2.5,2.5))
  data <- data[order(data$x),,drop=F]
  e1 <- rnorm(n,sd=.25)
  e2 <- rep(rnorm(g,sd=1), each=n/g)
  cluid <- rep(1:g, each=n/g)
  data$y <- f(data$x) + (e1+e2)

  # Regression function
  mycoefs2 <- function(dat, indices) {
    df <- 10
    coefs <- coef(lm(y~ns(x, df=df), dat[indices,]))
  }

  # Normal bootstrap (ignoring dependence)
  set.seed(123)
  out5 <- boot(data, mycoefs2, R=200)
  out5

  # Bootstrap accounting for dependence
  ?boot
  out5b <- boot(data, mycoefs2, R=200, strata=cluid)
  out5b

  # Now compare the SEs

  plot(apply(out5$t, 2, sd), type = "l")





