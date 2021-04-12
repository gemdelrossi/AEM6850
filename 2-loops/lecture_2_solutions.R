# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 1: Find largest element of each row of m. Find smallest element of
# each column.
a <- apply(m, 1, function(x) x[which.max(x)])
b <- apply(m, 2, function(x) x[which.min(x)])
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 2: Compute average GDP by country over the sample period and store in
# a numeric vector named "average". Hint: use lapply in combination with dlist.
average <- sapply(dlist, function(x) mean(x$Value))
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 3: Compute the GDP growth rate by country over the sample period and
# store in a numeric vector named growth.
growth <- sapply(dlist, function(d) {
  reg <- lm(log(Value)~Time, d)
  coef(reg)[[2]]
})
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
