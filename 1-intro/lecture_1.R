# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# AEM 6850
# 1. Getting started with R
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# Assigning data to an object ----
x <- c(1, 2, 3, 4, 5)
x = c(1, 2, 3, 4, 5)
assign("x",c(1, 2, 3, 4, 5))
c(1, 2, 3, 4, 5) -> x
c(1, 2, 3, 4, 5) = x #this way doesn't work, will return an error
class(x) #what type of data this object is


# Asking for help ----
help(assign)
?assign
??assign #does a more detailed version of help

# Sequences ----
x <- 1:10
?seq
x <- seq(from=1,to=10,by=1)
x <- seq(1,10,2) #assigns numbers of jumps to the sequence
x <- seq(to=10, by=1, from=1)
x <- seq(10, 1, 1) #returns an error
x <- seq(0,19, length.out=10)
rep(1:3,2) #repetition
rep(1:3,each=2)
?rep


# Check out objects ----
View(x)
edit(x) #Ariel doesn't recommend it


# Basic operations -----
x <- 1:10
x + 1
x * 2
min(x)
max(x)
var(x) #variance of x
x # does not modify X unless you assign it to x
x <- x + 1

#To do: Go through rest of notes, complete exercises in the rest of this file!

# Random numbers ----
?rnorm
rnorm(n=10, mean=0, sd=1)
rnorm(10)
rnorm(10)
set.seed(123)
rnorm(10)
set.seed(123)
rnorm(10)
?runif
sample(1:10, 2)
x <- sample(1:100, 100, replace=T)
unique(x)
table(x)
x <- sample(1:10, 10, replace=F)
unique(x)
length(unique(x))


# Manipulate numbers -----
x <- runif(10, min=0, max=10)
floor(x)
ceiling(x)
round(x,0)
round(x,1)
round(x,2)


# Manipulate characters-----
x <- c("c","o","r","n","e","l","l")
y <- c("u","n","i","v","e","r","s","i","t","y")
class(x)
length(x)
paste(x, collapse="")
paste(y, collapse=".")
paste(x,y)

z <- paste(paste(x,collapse=""),paste(y,collapse=""))
toupper(z)
class(z)
length(z)
nchar(z)
strsplit(z,"")
strsplit(z," ")

letters # are has some pre-loaded "hidden" data
LETTERS
month.abb
month.name
pi

# Missing (NA) and not a number (NaN) ------
x <- c(1:5, NA, 1/Inf, 1/0, 0/0)
x*2
is.na(x)
is.nan(x)

# Indexing vectors and conditional statements ----
x <- 1:5
x[3]
x[-3] #without the object it the string of integers
x[c(1,2,3)]
x[c(TRUE,TRUE,TRUE,FALSE,FALSE)]
x[c(T,T,T,F,F)]
x>3
x>=3
x==3 #is equal to
x<3
x<=3
x[x=="b" | x=="d"] #ask what this line of code is, not sure what it does


# Sort, order, rank, match ------
set.seed(123)
x <- round(runif(10,100,200))
sort(x) #smallest to largest
order(x)
rank(x) #gives largest and smallest by number
match(188,x)


# Factors -----
x <- sample(1:100,100, replace=T)
z <- factor(x)
?factor
class(z)
summary(x)
summary(z)


# Matrices ------
x <- matrix(1:4, ncol=2, nrow=2)
x <- matrix(1:4, ncol=2, nrow=2, byrow=T) #byrow=T means the matrix is filled by rows (default is column)
x <- 1:4
matrix(x, ncol=10,nrow=10)
matrix(x, ncol=10,nrow=9) #not compatible data length

x <- matrix(1:10,2,5)
y <- matrix(1:20,4,5)

dim(x)
ncol(x)
nrow(x)

rbind(x,y)
cbind(x,x)
cbind(x,y) #computes error, rows of matrices must match

x <- matrix(1:100, 10,10)
x[1, ]
x[ ,1]
x[ ,1:2]
x[ ,1, drop=F]
class(x[,1])
class(x[,1,drop=F]) #check what the difference between this line and 164 are

head(x)
head(x,4)
tail(x)

colnames(x)
colnames(x) <- letters[1:10]
x[,c("a","b")]

rownames(x) <- letters[1:10]
x["a","b"]
x[c("a","b"), c("a","b")]

diag(x)

diag(10)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 1 ----
# Create a diagonal matrix with the alphabet letters with all other
# elements missing.

x<-matrix(NA, nrow = length(letters), ncol =length(letters))

diag(x) <-letters
diag(x) <-c(1:26)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# Dataframes ------
x <- matrix(1:100, 10,10)
x
cbind(x, letters[1:10])
y <- data.frame(x)
z <- cbind(y, letters[1:10])
z <- data.frame(x, newvar=letters[1:10])
z[,1:3]
z[, "newvar"]
z$newvar
z[, "newvar", drop=F]
z[z$X1>5,]


# Lists ------
a <- list(x,y,z)
class(a)
length(a)
str(a)

a[1]
a[[1]]

names(a)
names(a) <- c("matrix", "df1", "df2")
a
a[["df2"]]

list(1:3)

as.list(1:3)

# Checking your workspace and working directory ----
ls() # you can also see them on your IDE
list.files()
"x" %in% ls() # is an object in your workspace?

# User defined-function to compute surface of a circle-----
pi
r <- 2
pi*r^2
f <- function(r) pi * r^2
f(3) #input integers into f(r) for r to compute function
f(1:5)

f <- function(r) {
  print(paste("The area of the circle is:",pi * r^2))
}
f(3)

class(f)

# Packages -----
install.packages("maps")
library("maps")
?map
map("world")
map("usa")
map("state")
map("world")
map("state", add=T)



# Simple date manipulation ------

Sys.Date()

Sys.time()

date()

d <- as.Date(c("2007-6-22", "2004-2-13"))

d[1]-d[2]     # Calculates the number of days between two dates.

today <- Sys.Date()

class(today)

seq(today, length.out=10, by="1 week")

seq(today, length.out=10, by="3 week")

seq(today, length.out=10, by="3 day")

as.Date("2010/1/1") + 1

as.Date("2010/1/1") + round(200*runif(100)) # random dates

?strftime

strftime(today, format = "%a")

strftime(today, format = "%A")

strftime(today, format = "%b")

strftime(today, format = "%B")

strftime(today, format = "%j") # day of the year

strftime(today, format = "%w") # weekday


# Logical statements ------

x <- 5

if (x>3) print("yes") else print("no")

if (x>3) {
  print("yes")
} else {
  print("no")
}

x <- c(1,5)
if (x>3) print("yes") else print("no") #warning message, need to check/understand why

x <- 0
while (x<3) {
  x <- x+1
}
x


# Logical statements for vectors -------

x <- runif(20,-10,10)
x > 0

ifelse(x>0, "+","-")

?ifelse

ifelse(x>5, "++",ifelse(x>0,"+","-"))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 2: Write the university name alternating small and large caps starting
# with the x object below:
x <- c("c","o","r","n","e","l","l","","u","n","i","v","e","r","s","i","t","y")

index <- 1:length(x)
y<- ifelse(index %in% seq(1,18,2),toupper(x), x)
paste(y, collaspe="")


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# Import basic data -------
# Go to https://stats.oecd.org/index.aspx?DataSetCode=PDB_LV
# and download data as .csv file and put it in working directory

getwd() # working directory

list.files()

?list.files

d <- read.csv("PDB_LV_09022021230752398.csv") #imported most up to date version, old one in zip.file didn't work

View(d)

summary(d)

names(d)

us <- d[ d$Subject=="GDP per head of population"
         & d$Measure=="USD, constant prices, 2015 PPPs"
         & d$LOCATION=="USA",]

#what does the d[] do for command

dim(us)

# Basic plot ------

?plot

plot(us$Time, us$Value)

plot(us$Time, us$Value, type="l") # line

plot(us$Time, us$Value, type="o") # overlay

plot(us$Time, us$Value, type="p") # points

plot(us$Time, us$Value, type="p", pch=2) # triangles

plot(us$Time, us$Value, type="o", pch=16, xlab="Year", ylab="GDP per capita (2010$)") # triangles

# Basic regression to get growth rate -----

reg <- lm(Value~Time, data=us)
summary(reg)

reg <- lm(log(Value)~Time, data=us) # about 1.8%/year
summary(reg)

plot(us$Time, log(us$Value), type="o", pch=16, xlab="Year", ylab="GDP per capita (2010$)", main="USA") # triangles
lines(us$Time,fitted(reg), col="red")

reg <- lm(log(Value)~Time+I(Time^2), data=us)
summary(reg)

plot(us$Time, log(us$Value), type="o", pch=16, xlab="Year", ylab="GDP per capita (2010$)") # triangles
lines(us$Time,fitted(reg), col="red")

resid <- residuals(reg)
plot(us$Time, resid, type="h")
plot(us$Time, resid, type="h", col=c("red","green","blue"))
abline(h=0)
abline(v=seq(1970,2010,10), lty=2, col="grey")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 3: Plot GDP deviations that are red when negative, blue when positive


plot(us$Time, resid, type ="h", pch=16, col= ifelse(resid<=0, "red","blue"), xlab = "Time", ylab = "Residuals",
     main = "Residuals of regression")
abline(h=0)
abline(v=seq(1970, 2010,10), lty =2, col = "grey")


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# The end
