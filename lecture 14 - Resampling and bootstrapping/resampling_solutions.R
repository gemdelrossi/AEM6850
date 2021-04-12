# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# EXERCISE 1: Write a loop over each of the 10 folds with lapply that
# repeats the out of sample error calculation above.
df <- 2
folds <- 1:10
ex1 <- sapply(folds, function(fold) {

  # Set up data
  testobs <- 1:nrow(data) %in% idx[[fold]]
  test  <- data[testobs , ]
  train <- data[!testobs, ]

  dim(train)
  dim(test)

  # Run model on training data
  reg <- lm(y~ns(x, df=df), train)

  # Prediction on testing data
  pred <- predict(reg, test)

  # Compute error
  error<- test$y - pred

  mse <- mean(error^2)

  mse
})
mean(ex1)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# EXERCISE 2: Perform this same exercise for all degrees of freedom
# from 1 to 25. Plot the out of sample MSE for each df level in a single
# plot (x-axis is df and mse is y-axis).

set.seed(12345)

k <- 10
idx <- sample(1:n,n,replace=F)
#idx <- 1:n
idx <- split(idx, rep(1:k,each=n/k))
dfs <- 1:30
folds <- 1:k

ex2 <- lapply(dfs, function(df) {

  ex1 <- lapply(folds, function(fold) {

    # Set up data
    testobs <- 1:nrow(data) %in% idx[[fold]]
    test  <- data[testobs , ]
    train <- data[!testobs, ]

    dim(train)
    dim(test)

    # Run model on training data
    reg <- lm(y~ns(x, df=df), train)
    r2 <- summary(reg)$r.squared
    r2.adj <- summary(reg)$adj.r.squared

    # Prediction on testing data
    pred <- predict(reg, newdata=test)

    # Compute error
    error<- test$y - pred

    mse <- mean(error^2)

    # Export
    data.frame(df=df, mse=mse, r2=r2, r2.adj=r2.adj)
  })
  ex1 <- do.call("rbind", ex1)
  colMeans(ex1)
})
ex2 <- as.data.frame(do.call("rbind",ex2))

# MSE
plot(dfs, ex2$mse, lwd=4, ylim=c(0,40), type="h", xlab="df", ylab="MSE", col=ifelse(ex2$mse==min(ex2$mse),"red","grey"))
lines(dfs,ex2$mse, lty=2)

# Add R2 and R2adj
lines(dfs,ex2$r2*40, col="blue", lty=2)
lines(dfs,ex2$r2.adj*40, col="blue")
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
