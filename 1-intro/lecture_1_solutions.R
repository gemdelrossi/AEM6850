# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 1 ----
# Create a diagonal matrix with the alphabet letters with all other
# elements missing.
d <- matrix(NA, ncol=length(letters),nrow=length(letters))
diag(d) <- letters
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 2: Write the university name alternating small and large caps starting
# with the x object below:
x <- c("c","o","r","n","e","l","l","","u","n","i","v","e","r","s","i","t","y")
index <- 1:length(x)
y <- ifelse(index %in% seq(1,18,2),toupper(x),x)
paste(y, collapse=" ")
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 3: Plot GDP deviations that are red when negative, blue when positive
plot(us$Time, resid, type="h", pch=16, col=ifelse(resid<=0,"red","blue"))
abline(h=0)
abline(v=seq(1970,2010,10), lty=2, col="grey")
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
