# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# AEM 6850
# 2. Loops, vectorization and parallel computing
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# To measure how much time efficient a piece of code is you can time it ---

?system.time

system.time( sqrt(1:10^8) ) #elapsed is total time it took

?proc.time

ptm <- proc.time()
sqrt(1:10^8)
proc.time() - ptm

# user: actual time it took the CPU to compute things
# system : time spent opening/closing/reading files, starting process, etc.

# For loops -----

for (i in 1:10) {
  print("hey")
}
#brackets collapse the code

replicate(10, print("hey")) # can also use but limited compared to "for" loops

for (i in 1:10) print(rep(i,5))


# Sum elements of a row with for loops ----
set.seed(123)
n <- 10^6
m <- matrix(round(runif(n*5,1,n)),ncol=5)
dim(m)

system.time({
a <- NA
for (i in 1:nrow(m)) a[i] <- m[i,1] + m[i,2] + m[i,3] + m[i,4] + m[i,5]
})

# pre-allocation of empty sells helps a bit...
system.time({
a <- rep(NA,nrow(m))
for (i in 1:nrow(m)) a[i] <- m[i,1] + m[i,2] + m[i,3] + m[i,4] + m[i,5]
})

# This type of looping does not take into advantage the "vectorization" of R


# apply -----
?apply #x is matrix, margin is row/column, fun is function

system.time(b <- apply(m, MARGIN=1, sum))

system.time(apply(m, MARGIN=2, sum))


# Built-in commands for basic operations -----

system.time(c <- rowSums(m)) #better than apply function for sums, means, very speedy

colMeans(m)


# apply() more useful for more complicated operations -----

myfunction <- function(x) sd(x)/mean(x)
apply(m, MARGIN=2, myfunction ) #computes variation for each column in m (useful)

apply(m, MARGIN=2, function(x) sd(x)/mean(x) )
#can write directly inside, but sometimes want to save as a separate object

# Useful way to determine largest element of a vector
set.seed(123)
x <- runif(10)
which.max(x)
which.min(x)
x[which.max(x)] #putting x in front shows the exact value
x[which.min(x)]

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 1: Find largest element of each row of m. Find smallest element of
# each column.

dim(m)
apply(m, 1, max)

apply(m, 2, min) #outputs position
apply(m,2,which.min) #outputs number

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# lapply(): apply function over a list ------

m1 <- matrix(round(runif(100,1,100)),ncol=5)
m2 <- matrix(round(runif(200,1,100)),ncol=5)
m3 <- matrix(round(runif(300,1,100)),ncol=5)

mlist <- list(m1,m2,m3)

mlist

lapply(mlist, class)
lapply(mlist, dim)
lapply(mlist, colSums)

# sapply(): simplifies the output of lapply, when possible -----

sapply(mlist, colSums) #puts into a matrix w/elements of list as columns
#won't work with vectors of different length to form matrix

do.call("cbind", lapply(mlist, colSums) )

?do.call

sapply(mlist, rowSums) # matrices do not have the same number of rows

lapply(mlist, rowSums) # matrices do not have the same number of rows

# You can also perform a loop with "for" ------

for (i in 1:length(mlist)) print(class(mlist[[i]])) #longer than lapply


# Play around with lapply... -----
# Import basic data. Go to https://stats.oecd.org/index.aspx?DataSetCode=PDB_LV
# and download data as .csv file and put it in working directory

getwd()

list.files()

data <- read.csv("PDB_LV_06022021184239950.csv")

summary(data)

names(data)

#code below subsets data
data <- data[ data$Subject=="GDP per head of population" & data$Measure=="USD, constant prices, 2015 PPPs",]

countries <- unique(data$Country)

# Split data into a list -----
#slice by country
dlist <- split(data, data$Country)

length(dlist)

names(dlist)

dlist[[1]]

dlist[["United States"]]

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 2: Compute average GDP by country over the sample period and store in
# a numeric vector named "average". Hint: use lapply() in combination with dlist.

#finish after class
average <- lapply(dlist,function(x) mean(x$Value)) #it looks like you can set up the function like this, -->
                                                    #check in in OH to see why we can do that

average <-unlist(average)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# View data in a barplots ------
average <- sort(average)
barplot(average) #gives the growth rate for each country as a bar

# Get the growth rate for one country ----

d <- dlist[[1]]

head(d) #shows top of the data

reg <- lm(log(Value)~Time, data=d) #lm is linear model

coef(reg)

coef(reg)[[2]]

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 3: Compute the GDP growth rate by country over the sample period and
# store in a numeric vector named "growth".

#use lapply

growth<- lapply(dlist, function(d) {
  reg <- lm(log(Value)~Time, data=d)
  coef(reg)[[2]]
})

growth <- unlist(growth)

growth

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# One can parallelize lapply() over CPU cores -----

# Note that parallelization really works when you have relatively few but tedious
# tasks to perform ( rather than many small tasks )

library(parallel) # this is a base package


# Let's set up a tedious task, say a Monte Carlo simulation.

# First, create some fake data
n <- 1000
data <- data.frame(x1=runif(n),x2=runif(n),x3=runif(n))

dim(data)

# Derive a "dependent" variable based on a data generating process that you
# impose, but to which you add a normally distributed random error

bs <- list(b0=1,b1=2,b2=-1, b3=0) # true coefficients, or what you believe is the true parameter estimate
error <- 3 * rnorm(n)
data$y <- bs$b0 + bs$b1 * data$x1 + bs$b2 * data$x2 + error

# Run one regression
reg <- lm(y~x1+x2+x3, data)
summary(reg)
coef(reg)
unlist(bs) # what do you notice? why?

#The regression coefficients are in the neighborhood of the true parameter estimates, but not PRECISE

# A Monte Carlo experiment consists in repeating this exercise many times
nrep <- 10^4
betas1 <- matrix(NA,ncol=length(coef(reg)), nrow=nrep) # pre-allocate space
colnames(betas1) <- names(bs)

# Run experiments
t1 <- system.time({
for (i in 1:nrep) {
    print(i)
  # 1. generate new dependent variable
    data$y <- 1 + 2 * data$x1 - data$x2 + 3 * rnorm(nrow(data))
  # 2. Run model
    reg <- lm(y~x1+x2+x3, data)
  # 3. Store results
    betas1[i,] <- coef(reg)
}
})

# Plot distribution of coefficients
lapply(names(bs), function(v) {
  hist(betas1[,v], breaks=20, col="grey", main=v)
  abline(v=bs[[v]], col="red", lwd=3)
})

# Do this with lapply()
t2 <- system.time({
betas2 <- lapply(1:nrep, function(i) {
  # 1. generate new dependent variable
  data$y <- 1 + 2 * data$x1 - data$x2 + 3 * rnorm(nrow(data))
  # 2. Run model
  reg <- lm(y~x1+x2+x3, data)
  # 3. Store results
  coef(reg)
})
})

# This task is somewhat for complex models (not that bad in this case)


# Do this with mclapply (only works on Macs)
t3 <- system.time({
  betas3 <- mclapply(1:nrep, mc.cores=4, FUN=function(i) {
    # 1. generate new dependent variable
    data$y <- 1 + 2 * data$x1 - data$x2 + 3 * rnorm(nrow(data))
    # 2. Run model
    reg <- lm(y~x1+x2+x3, data)
    # 3. Store results
    coef(reg)
  })
})


# Do this with parLapply (only works on Macs)
t4 <- system.time({
  cl <- makeCluster(4)
  clusterExport(cl, varlist=c("data"))
  betas4 <- parLapply(cl, X=1:nrep, function(i) {
    # 1. generate new dependent variable
    data$y <- 1 + 2 * data$x1 - data$x2 + 3 * rnorm(nrow(data))
    # 2. Run model
    reg <- lm(y~x1+x2+x3, data)
    # 3. Store results
    coef(reg)
  })
  stopCluster(cl)
})


# Compare times
strategies <- c("for","lapply","mclapply","parLapply")
times <- list(t1,t2,t3,t4)
barplot(sapply(times, function(x) x[[3]]), names.arg=strategies, col="red3")



# The end



