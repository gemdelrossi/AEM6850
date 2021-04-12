# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# EXERCISE 1: Develop a MC simulation to approximate the value of pi with
# r = 1000 and n = 1000. Plot the distribution of all estimates, as well
# the mean of all simulations.
r <- 10^3
n <- 10^3
ex1 <- sapply(1:r, function(i) {
  x1 <- runif(n,-1,1)
  x2 <- runif(n,-1,1)
  d <- sqrt(x1^2 + x2^2)
  sum(d<=1) / length(d) * 4
})

hist(ex1, col="grey", border=NA, breaks=30)
abline(v=mean(ex1), lty=2)
abline(v=pi, col="red")
mtext(mean(ex1), side=3)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# EXERCISE 2: Explore what occurs when you drop x2. Develop a MC simulation 
# showing how rising correlation between x1 and x2 affects whether b1 is biased, 
# and 2- wether it affects the SE of b1. k= seq(.2,3,.2), r=500, n=1000.
# Store results in list called ex2.
# Settings
r <- 500
n <- 1000
klist <- seq(.2,3,.2)
# Runs simulations
ex2 <- lapply(klist, function(k) {
  print(k)
  # Generate data
  set.seed(123)
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n)*k # increasing k reduces correlation between Xs
  x2 <- x2/sd(x2) # standardize variance
  data <- data.frame(x1=x1,x2=x2)
  plot(data[,1:2])
  # Loop over simulations
  o <- sapply(1:r, function(i) {
    e <- rnorm(n)*2 # normally distributted and i.i.d
    data$y <- b0 + b1*data$x1 + b2*data$x2 + e
    reg <- lm(y~x1, data)
    b <- coef(reg)
    se <- sqrt(diag(vcov(reg)))
    c(b,se)
  })  
  o <- t(o)
})
names(ex2) <- klist
# View results: 2 things happen
# Empty plot
plot(0, type="n", xlim=c(-2,2), ylim=c(0,7), axes=F, xlab="",ylab="Density")
axis(1)
axis(2)
# Loop over ks
colors <- colorRampPalette(brewer.pal(11,"Spectral"))(length(klist))
lapply(klist, function(k) {
  lines(density(ex2[[paste(k)]][,2]), col=colors[match(k,klist)], lwd=1)
})
abline(v=c(b1,b2), lty=2)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
