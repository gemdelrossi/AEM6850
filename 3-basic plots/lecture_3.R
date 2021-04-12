# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# AEM 6850
# 3. Basic visualizations (with base R)
#
# Content:
# 0. Preliminary comments
# 1. Basic scatter and line plots
# 2. Customizing plots
# 3. Writing to the disk
# 4. Barplots
# 5. Shaded areas
# 6. Colors
# 7. Boxplots
# 8. Simple maps
# 9. Animations
# 10. Multi-panel plots
# 11. Custom plots: e.g. time series grid
# 12. Writing high-quality files to the disk
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# 0. Preliminary -----

# R is extremely powerful when it comes to visualizations.

# Here we are only touching the tip of the iceberg with plotting capabilities
# that come with R.

# There are popular packages like "ggplot2" that make it "easier" to make many
# different types of plots in R.

# Personally, I don't use "ggplot2". 1- It does not save me that many lines of
# code, 2- the syntax is a bit different than base R (which means I have to 
# learn an additional "dialect" to make figures)


# 1. Basic scatter and line plots ------

  # Let's import some data
  # Real GDP since 1947: https://fred.stlouisfed.org/series/GDPC1
  data <- read.csv("GDPC1.csv", stringsAsFactors=F) # point to where the file is
  
  # Convert dates
  head(data)
  class(data$DATE)
  data$date <- as.Date(data[,1], "%Y-%m-%d")
  class(data$date)
  data$date[1] + 1:10 # R understands you are adding days
  data$DATE[1] + 1:10 # you can't add numbers to a character
  
  ?substr
  
  substr("abcdefg",1,3)
  
  substr("abcdefg",6,7)
  
  data$year <- as.numeric(substr(data$DATE,1,4))
  data$month <- as.numeric(substr(data$DATE,6,7))
  
  # Create a continuous "time" variable for managing dates
  data$time <- data$year + (data$month-1)/12

  # Basic plot: plot()
  
  ?plot

  plot(data$date, data$GDPC1)
  
  plot(data[,c("date","GDPC1")]) # similar
  
  # Lines of different sorts
  
  plot(data[,c("date","GDPC1")], type="l") # lines

  plot(data[,c("date","GDPC1")], type="s") # steps
  
  plot(data[,c("date","GDPC1")], type="h") # bars
  
  plot(data[,c("date","GDPC1")], type="o") # overlay points and lines

  # Wider lines
  
  plot(data[,c("date","GDPC1")], type="l", lwd=4) 

  # Change color
  
  plot(data[,c("date","GDPC1")], type="l", lwd=4, col="red") 

  # Reference lines
  
  ?abline

  abline(v=1960) # vertical reference line in 1960; does not work well, why?
  
  abline(v=as.Date("1960-1-1")) # vertical reference line in 1960

  abline(v=seq(as.Date("1960-1-1"),as.Date("2020-1-1"), 365 * 20)) # every 20 years
  
  abline(h=c(5000, 10000, 15000), lty=2) # horizontal dashed line
  
  # Use numeric time variable
    
  plot(data[,c("time","GDPC1")], type="l") # lines
  
  abline(v=seq(1960,2010,10), lty=3) # vertical reference line in 1960
  
# 2. Customizing plots ------

  # Remove axes and labels...
  
  plot(data[,c("time","GDPC1")], type="l", axes=F, xlab="", ylab="") # lines
  
  # Add customized axis
  ?axis
  axis(1)
  axis(2)
  axis(3, las=2)
  axis(4, las=2)
  
  # Add axis with horizontal labels
  plot(data[,c("time","GDPC1")], type="l", axes=F, xlab="", ylab="") # lines
  axis(1, las=1)
  axis(2, las=2)
  
  # Add axis with horizontal labels
  plot(data[,c("time","GDPC1")], type="l", axes=F, xlab="", ylab="") # lines
  axis(1, las=1, at=seq(1950,2010,20))
  
  # Say we want to add little tick marks every 5 years
  plot(data[,c("time","GDPC1")], type="l", axes=F, xlab="", ylab="") # lines
  axis(1, las=1, at=seq(1950,2020,10))
  axis(1, las=1, at=seq(1955,2015,10), tck=-.01, lwd.tick=1, lwd=0, labels=F)
  axis(2, las=2)
  
  # Add labels to axis: mtext
  mtext("Year", side=1, line=0) # too close to the margin
  mtext("Year", side=1, line=2, font=2) # out a bit with bold font
  mtext("Year", side=1, line=3, font=3) # out even more with italics
  mtext("GDP" , side=2, line=3.5, cex=2) # huge letter
  
  # You can also add text with text()
  ?text
  plot(data[,c("time","GDPC1")], type="l", axes=F, xlab="", ylab="") # lines
  axis(1, las=1, at=seq(1950,2020,10))
  axis(1, las=1, at=seq(1955,2015,10), tck=-.01, lwd.tick=1, lwd=0, labels=F)
  axis(2, las=2)
  text(1980,5000, "USA")
  text(1980,seq(5,0,-1)*1000, "USA", cex=6:1) # what happens? dissapears
  
  # You cannot plot stuff outside the "box"
  box(col="red", lwd=5)
  
  # Unless... you really want to
  par(xpd=TRUE)
  text(1980,seq(5,0,-1)*1000, "USA", cex=6:1, col="blue") # what happens? dissapears
  
  par(xpd=FALSE) # Turn this off for subsequent plots
  
  # Perhaps more useful.. you may want to emphasize GDP each decade in the plot
  par(mar=c(4,5,2,2)) 
  plot(data[,c("time","GDPC1")], type="l", lwd=2, col="blue", axes=F, xlab="", ylab="") # lines
  axis(1, las=1, at=seq(1950,2020,10))
  axis(1, las=1, at=seq(1955,2015,10), tck=-.01, lwd.tick=1, lwd=0, labels=F)
  axis(2, las=2)
  
  # Add text....
  # 1. find a way to get the GDP each decade automatically
  idx <- which(data$time %in% seq(1950,2010,10))  # position in the dataset
  data$time[idx]
  data$GDPC1[idx]
  points(data$time[idx],data$GDPC1[idx], col="blue") # hollow circle
  
  ?points
  
  points(data$time[idx],data$GDPC1[idx], col="blue", pch=16) # hollow circle
  
  text(data$time[idx]+4,data$GDPC1[idx]-400, round(data$GDPC1[idx]), col="blue") # hollow circle
  
  # Add legend
  
  ?legend

  legend("topleft", c("GDP"), lwd=1, pch=16, col="blue")  

  legend("topleft", c("GDP"), lwd=1, pch=16, col="blue", inset=-.05)  

  legend("bottomright", c("GDP"), lwd=1, pch=16, col="blue", bty="n", title="Legend:")
 
  legend("bottomright", c("GDP"), lwd=1, pch=16, col="blue", title="Legend:", bg="cornsilk", inset=.2)
  
  # Margins
  ?par
  par(mar=c(5,5,5,5)) # large margin
  plot(data[,c("time","GDPC1")], type="l") 
  
  par(mar=c(0,0,0,0)) # no margin
  plot(data[,c("time","GDPC1")], type="l") 

  par(mar=c(2,3,1,1)) # tight margin
  plot(data[,c("time","GDPC1")], type="l") 

  
# 3. Writing images to disk ------
  
  # Up to now figures have been plotted in the RStudio device
  # To write it to a file on disk, you have to explicitly tell R to do so.
  
  ?png

  # Here is how you start writing a PNG file to the working directory  
  png("mysecondfig.png", width=600, height=800, pointsize=20, bg="transparent", type="windows")
   
  # Get a figure that you want
  par(mar=c(4,5,2,2)) # tight margin
  plot(data[,c("time","GDPC1")], type="l", lwd=3, col="blue", axes=F, xlab="", ylab="") # lines
  axis(1, las=1, at=seq(1950,2020,10))
  axis(1, las=1, at=seq(1955,2015,10), tck=-.01, lwd.tick=1, lwd=0, labels=F)
  axis(2, las=2) 
  abline(v=seq(1950,2020,10), lty=2, lwd=.5)
  abline(h=seq(5000,15000,5000), lty=2, lwd=.5)
  box()
  mtext("Gross Domestic Product", side=2, line=4)
  mtext("Year", side=1, line=2.5)
  
  # Once you are done you have to "close off" the device, so that R 
  # stops writing stuff to the image.
  
  dev.off()
  
  # GO check it out!
  
  # You can tweak many things...
  # Try the following:
  # - pointsize = 20
  # - bg = "transparent" or any color
  # - changing width and height
  # - type = "Xlib"
  

# Compute quarter to quarter growth rate, here is an example
  
  x <- c(1:10)^2
  
  diff(x) # note this is shorter than x (by 1)

  diff(log(x)) # growth rate in log points
  
  ?diff
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 1: Compute the quarterly growth rate (in log points) of the US economy and store it
# in the "data" dataframe as the variable "growth".
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

data$growth <- c(NA, diff(log(data$GDPC1)))

plot(data$time, data$growth, type="h")
  
# 4. Barplots ------
 
  ?barplot
  
  
  # Simple barplot 
  x <- c(1:100)^.5
  barplot(x)  
  
  # Notice axis starts at 0
  axis(1)

  # Add spaces
  barplot(x, space=c(1))  

  # Group bar by group
  xgroup <- matrix(x, ncol=5)
  dim(xgroup)
  
  # Stacked bar plot
  barplot(xgroup)

  # Side-by-side bar plot
  barplot(xgroup, beside=T)
  
  barplot(xgroup, beside=T, space=c(0,5)) # add more space between groups
  
  # To make sure labels are added
  colnames(xgroup) <- letters[1:ncol(xgroup)]
  
  barplot(xgroup, beside=T, space=c(0,5)) # add more space between groups
 
  # You can also customize these figures! 
  barplot(xgroup, beside=T, space=c(0,5), axes=F) # add more space between groups
  axis(2, las=2)
  abline(h=0)
  mtext("Height", side=2, line=2)
 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 2: Plot growth rate (y axis) over time (x axis) as a blue bar for
# positive quarter and red for negative quarters. Add a reference line for the 
# 0 growth rate. Hint: you may use either plot or barplot, and ifelse() to create
# a vector of colors.
  
barplot(data$growth, xlab = "", ylab = "", col = ifelse(data$growth>0,"blue","red"), axes=F)
abline(h=0, col="green", lwd = 3)
axis(1, at=seq(1950,2020,10)) #not sure why it didn't work
axis(2, las= 2)
mtext("Growth Rate", side = 2, line = 3)
mtext("Time", side = 1, line = 2)
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# 5. Shaded boxed areas ------
  
  # Shaded areas (rectangles)
  ?rect
  
  # Adding boxes or shaded areas to plots: rect()
  plot(data[,c("time","GDPC1")], type="l", col="blue", lwd=2)

  rect(xleft=1950, xright=1960, ybottom=2500, ytop=17000)

  rect(xleft=1970, xright=1980, ybottom=2500, ytop=17000, border="red", col="green")

  # Adding some transparency to boxes

  rect(xleft=1990, xright=2000, ybottom=2500, ytop=17000, border="cyan", col="blue", density=25)
  
  ?adjustcolor
  
  rect(xleft=2010, xright=2020, ybottom=2500, ytop=17000, col=adjustcolor("red", alpha.f=0.5), border=NA)
  
  # As many things with R, you can add many boxes at the same time by using vectors
  plot(data[,c("time","GDPC1")], type="l", col="blue", lwd=2)
  starts <- seq(1950,2010,20)
  rect(xleft=starts, xright=starts+5, ybottom=0, ytop=20000, col=adjustcolor("black", alpha.f=0.2), border=NA)
  
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 3: Add a transparent grey shaded area over the previous the figure of
# the previous exercise over quarters with negative growth.
  
  barplot(data$growth, xlab = "", ylab = "", col = ifelse(data$growth>0,"blue","red"), axes=T)
  abline(h=0, col="green", lwd = 3) 
  starts <-data$time[data$growth<0]
  step <- diff(data$time)[1]/2
  rect(xleft = starts - step, xright = starts + step, ybottom = -1, ytop = 1, 
       col=adjustcolor("black", alpha.f=0.2), border=NA)
  
  #Go over this is Ariel's office hours 
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  
  # Shaded areas (polygons)
  
  # Let's create some fake data
  nobs <- 100
  x <- runif(nobs,0,10)
  xmat <- cbind(1,x,x^2,x^3) # intercept plus x, x^2, x^3
  beta <- cbind(c(20,3,-1.5,.11)) # intercept plus coefficients
  error <- 5 * rnorm(nobs)
  y <- xmat %*% beta + error # what does %*% mean?
  
  # Scatter plot
  plot(x,y, xlim=c(0,10))
  
  # Run regression
  reg <- lm(y ~ xmat[,-1] ) # drop intercept from matrix
  reg <- lm(y ~ x + I(x^2) + I(x^3) ) # equivalent
  
  # Fitted values
  yhat <- fitted(reg)
  
  # Sort x
  idx <- order(x)
  lines(x[idx],yhat[idx], col="red", lwd=2)
  
  # To plot error band we need to compute the variaance of Yhat
  var.yhat <- xmat %*% vcov(reg) %*% t(xmat)
  se.yhat   <- sqrt(diag(var.yhat))
  
  # Upper and lower limit, 95% confidence band
  upper <- yhat + 1.96 * se.yhat
  lower <- yhat - 1.96 * se.yhat
  
  # Adding lines
  lines(x[idx],upper[idx], col="red", lty=2)
  lines(x[idx],lower[idx], col="red", lty=2)
  
  # Add shaded area using polygon
  ?polygon
  
  plot(x[idx],yhat[idx], col="red", lwd=2, type="l",ylim=range(y))
  
  polygon(x=c(x[idx],rev(x[idx])), y=c(upper[idx],rev(lower[idx])))

  plot(x[idx],yhat[idx], col="red", lwd=2, type="l",ylim=range(y), xlab="x", ylab="y")
  
  polygon(x=c(x[idx],rev(x[idx])), y=c(upper[idx],rev(lower[idx])), col=adjustcolor("red",alpha.f=.25), border=NA)

  points(x,y, pch=21, col="black", bg="lightgrey")
  
# 6. Colors ------
  
  # Let's create another fake dataset to highlight how to deal with colors
  nobs <- 10^4
  x <- runif(nobs,0,100)
  
  # Will add colors to these dots based on the level of x
  plot(x,1:nobs, pch=1)
  
  # Changing colors
  plot(x,1:nobs, pch=1, col="red")

  plot(x,1:nobs, pch=1, col=ifelse(x>50,"red","grey"))
  
  # Using pre-packaged colors
  colors()
  
  cols <- sample(colors(), 5) # 10 random colors
  
  # Let's colorize these points based on wether they fall in the interval
  # 0-20, 20-40, ... 80-100.
  class <-  ifelse(x<20,1,2) # 1 for those below 20, 2 otherwise
  class <-  ifelse(x<20,1,ifelse(x<40,2,3)) # 1 for those below 20, 2 below 40, 3 otherwise.
  class <-  ifelse(x<20,1,ifelse(x<40,2,ifelse(x<60,3,ifelse(x<80,4,5)))) # and so on
  
  # Plot
  plot(x,1:nobs, pch=1, col=cols[class])
  
  # What if you have many more classes, writing ifelse statements if a bit tedious
  # You can use a buit in function in R
  class2 <- findInterval(x, seq(0,100,20))

  plot(x,1:nobs, pch=1, col=cols[class2])

  # Use nice color schemes from R Color Brewer package
  install.packages("RColorBrewer")
  library(RColorBrewer)
  
  ?brewer.pal
  
  display.brewer.all()
  
  # Colorize with the Spectral color scale
  cols <- brewer.pal(11, "Spectral") # SPectral scale has only 11 colors max
  
  class3 <- findInterval(x, seq(0,100, length.out=length(cols)))

  plot(x,1:nobs, pch=1, col=cols[class3])
  
  # Making up your own colors: what if you want a finer color scale?
  # You can interpolate colors!
  
  ?colorRampPalette
  
  manycols <- colorRampPalette(cols)(100)
  
  class4 <- findInterval(x, seq(0,100, length.out=length(manycols)))
  
  plot(x,1:nobs, pch=1, col=manycols[class4])
  


# 7. Boxplots ------

  # Boxplots are useful to represent variation across classes
  ?boxplot
  
  # Generate some fake data
  x <- c(1:5,4:1,2:10,9:5)
  x <- matrix(x, ncol=length(x), nrow=100, byrow=T)
  x <- x + rnorm(length(c(x))) * c(x)/5
  colnames(x) <- letters[1:ncol(x)]  
  
  # View data a bit
  head(x)
  
  # Basic boxplot
  boxplot(x) 
  
  boxplot(x, width=seq(1,2,length.out=ncol(x))) 

  boxplot(x, notch=T) 
  
  boxplot(x, boxwex=1) 

  boxplot(x, horizontal=T) 
  
  abline(h=18.5) # you can use abline to add divisions
  
  # Customize a bit
  boxplot(x, axes=F, col=NA, border=NA) # empty plot
  axis(1, colnames(x), at=1:ncol(x))
  axis(2, las=2)
  abline(h=seq(0,15,5), col="grey")
  legend("topleft", "Outliers",pch="*",col="darkred", inset=.05, pt.cex=2)
  boxplot(x, axes=F, col="coral", border="darkred", lty=1, pch="*", cex=1.5, add=T)
  box()
  
  
  
# 8. Simple maps ------

  install.packages("maps")
  install.packages("mapproj")
  
  library("maps")
  library("mapproj")
  
  ?map
  
  # This package has a bunch of pre-packages maps
  
  map("world")
  
  map("usa")
  
  map("state")

  map("state", fill=T, col=sample(colors(),48))
  
  # You can combine them  
  map("world")
  map("state", fill=T, col=sample(colors(),48), add=T)
  
  map("state")
  map("county", add=T)
  
  map("state")
  data(us.cities)
  map.cities(us.cities, col="darkred", bg="coral", pch=21)
  
  # You can change the projection
  map("state")
  
  map("state", project="lambert",par=c(30,40)) # projections require parameters

  map("world")
  
  map("world",proj="orthographic",orientation=c(15,255,0)) 
  
  map("world",proj="orthographic",orientation=c(15,260,0)) 
  map("state",proj="orthographic",orientation=c(15,260,0), add=T) 
  
  
  # You can add colors by specifying a vector of color of the same length and order
  # than the location names in the map
  data(unemp) # load economic data
  head(unemp) # contains county IDs (FIPS)
  
  # define color buckets
  breaks <- c(0, 2, 4, 6, 8, 10, 12, 100)
  leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", "10-12%", ">12%")
  colors <- colorRampPalette(brewer.pal(9, "Reds"))(length(breaks)-1)
  unemp$colorBuckets <- as.numeric(cut(unemp$unemp, breaks))
  
  # Align data with map definitions by (partial) matching state,county
  # names, which include multiple polygons for some counties
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
  colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]
  
  # draw map
  map("county", col = colors[colorsmatched], fill = TRUE,  lty = 0, projection = "polyconic")
  map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 1, projection="polyconic")
  title("Unemployment by county, 2009")
  legend("topright", leg.txt, horiz = TRUE, fill = colors)
  

# 9. Animations ------
  
  # Basic map
  o <- c(15,260,0)
  map("world", proj="orthographic",orientation=o) 
  map("county",proj="orthographic",orientation=o, col = colors[colorsmatched], fill = TRUE, resolution = 0, lty = 0, add=T)
  map("state",proj="orthographic",orientation=o, add=T, lwd=.5) 
  
  # Make it turn by changing orientation
  o <- c(15,260,0) + c(0,-5,0) # run the code above again

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 4: Create a series of figures that make the planet rotate a bit. Write 
# these figures to the disk. Hint: 1- write a loop where the central parameter
# runs from 200 to 300.
  
  rotate.map <- function(angles, write=F) {
    for (i in angles) {
      print(i)
      number <- substr(match(i,rev(angles))+1000,2,5) # this ensures the planet rotates in the right direction
      if (write) png(paste("map_",number,".png",sep=""),600,600) 
      par(mar=c(0,0,0,0))
      o <- c(15,i,0) 
      map("world", proj="orthographic",orientation=o) 
      map("county",proj="orthographic",orientation=o, col = colors[colorsmatched], fill = TRUE, resolution = 0, lty = 0, add=T)
      map("state",proj="orthographic",orientation=o, add=T, lwd=.5) 
      if (write) dev.off()
    }
  }
  rotate.map(200:300)  
  
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  
  # More generally, you can load some packages that can do this as well
  install.packages("magick")
  install.packages("animation") 
  library(magick)
  library(animation)

  # Animation package seems fairly useful
  ?animation
  
  ani.options(interval = 0.1, nmax = 100) # set options
  
  # Write to disk (rotate.map is a function created in the solutions script file for Exercise #4)
  saveGIF(rotate.map(300:200), movie.name = "my_rotating_world2.gif", convert = "magick", clean = TRUE)
  

  
# 10. Multi-panel plots ----
  
  # Let's create a function that generates a graph
  n <- 10^4
  f <- function(n)  plot(runif(n), col=colorRampPalette(brewer.pal(11,"Spectral"))(n), pch=16)

  # Check it out
  f(n)
  
  # The ability to create multi-panel plots can be controlled with par()
  ?par
  
  # Multi panel plots 1: par() 
  par(mfrow=c(3,2)) # fill by row
  lapply(1:6, function(x) {
    f(n)
    text(n/2,.5,x,cex=8)
  })

  par(mfcol=c(3,2)) # fill by column
  lapply(1:6, function(x) {
    f(n)
    text(n/2,.5,x,cex=8)
  })
  
  par(mfcol=c(3,2), mar=c(0,0,0,0)) # no margins !
  lapply(1:6, function(x) {
    f(n)
    text(n/2,.5,x,cex=8)
  })
  
  par(mfcol=c(3,2), mar=c(0,0,0,0), oma=c(6,6,6,6)) # add some outer margins
  lapply(1:6, function(x) {
    f(n)
    text(n/2,.5,x,cex=8)
  })
  
  #  Add some text outside
  mtext("X label for the entire figure", side=1, outer=T, line=2.5)
  mtext("Y label for the entire figure", side=2, outer=T, line=2.5)
  mtext("Overall Title", side=3, outer=T, line=2.5, cex=2)
  
  
  # Multi panel plots 2: layout()
  # Provides a bit more flexibility
  
  # Create a "layout" matrix
  par(mfcol=c(3,2), mar=c(0,0,0,0), oma=c(6,6,6,6)) # add some outer margins
  lmat <- matrix(c(1,2,3,4,5,5), ncol=2, byrow=T)
  layout(lmat, widths=c(1,1), heights=c(1,1,2))  
  lapply(1:5, function(x) {
    f(n)
    text(n/2,.5,x,cex=8)
  })
  

  
# 11. Custom plots: e.g. time series grid ------
  
  # Download state-level GDP data for the USA
  # 1. go to https://apps.bea.gov/regional/downloadzip.cfm
  # 2. Select "Gross Domestic Product (GDP)" then "Annual GDP by State"
  # 3. Place files in "SAGDP" folder 

  # Select files for 1997-2017 only for simplicity
  data <- lapply(state.abb, function(state) {
    
    print(state)
    # File name
    fname <- paste("SAGDP/SAGDP9N_",state,"_1997_2017.csv",sep="")
    # Import file
    d <- read.csv(fname, stringsAsFactors=F)
    # Select desired variables
    d2 <- as.numeric(d[1,paste("X",1997:2017,sep="")])
    d2 <- data.frame(state,data.frame(t(d2)))
    names(d2)[-1] <- 1997:2017
    # Export
    d2
  })
  data <- do.call("rbind", data)
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 5: Compute annual growth rate for each state in 1 line of code
  
  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  
  # Heat map
  image(t(growth))
  
  #readjust the parameters 
  
  par(mfcol=c(1,1), oma=c(2,2,2,2))
  
  # Customize heat map
  years <- 1998:2017
  breaks <- c(-2,seq(-.1,.1,.02),2)
  colors <- colorRampPalette(brewer.pal(10,"RdBu"))(length(breaks)-1)
  image(t(growth), breaks=breaks, col=colors, axes=F)
  axis(1, at=seq(0,1,length.out=length(years))    , labels=seq(years[1],years[length(years)],1))
  mtext(state.abb, side=2, at=seq(0,1,length.out=length(state.abb)), las=2, cex=.8, line=.5)
  mtext("GDP growth rates by US state", side=3, font=2, line=1)
  box()
  
  # Confirm Alaska starts the time series with negative growth
  growth["AK",]
  
  # Sort growth data post 2008
  meang <- apply(growth[,paste(2010:2017)], 1, function(x) mean(x))
  
  # Customize heat map
  image(t(growth[order(meang),]), breaks=breaks, col=colors, axes=F)
  axis(1, at=seq(0,1,length.out=length(years))    , labels=seq(years[1],years[length(years)],1))
  mtext(state.abb[order(meang)], side=2, at=seq(0,1,length.out=length(state.abb)), las=2, cex=.8, line=.5)
  mtext("GDP growth rates by US state", side=3, font=2, line=1, cex=1)
  mtext("(sorted by post-recession growth rates)", side=3, font=3, line=0)
  box()
  
  # Make it a plot with dots
  bucketmat <- growth
  bucketmat[] <- findInterval(growth, breaks) 
  
  # Plot
  plot(1, ylim=c(1,length(state.abb)), xlim=range(years), axes=F, xlab="", ylab="")
  axis(1)  
  axis(2, at=1:length(state.abb), labels=state.abb, las=2)  
  lapply(years, function(y) {
    lapply(state.abb, function(s) {
      points(y,match(s,state.abb), col=colors[bucketmat[s,paste(y)]])
    })
  })  
  
  # Change symbol and improve colors
  plot(1, ylim=c(1,length(state.abb)), xlim=range(years), axes=F, xlab="", ylab="")
  axis(1)  
  axis(2, at=1:length(state.abb), labels=state.abb, las=2)  
  lapply(years, function(y) {
    lapply(state.abb, function(s) {
      points(y,match(s,state.abb), bg=colors[bucketmat[s,paste(y)]], pch=ifelse(growth[s,paste(y)]>0,24,25), cex=1.5, lwd=1, col="darkgrey")
    })
  })  
  
  
# 12. Writing high-quality files to the disk------
  
# You'll soon realize that the figures you first generate in your
# papers and projects may not have the required resolution or format
# for certain journals.

# There are two main types of figure formats: bitmap and vector
  
# Bitmap are stored as "pixels" on a regular grid. The resolution
# of a bitmap image reflects the number of pixels per inch. Low
# resolution allows the human eye to see "pixels" or pixellation. 
# This is obviously undesirable so journals may ask you to create 
# higher resolution figures.
  
# In some cases journals do not accept bitmap images for figures that
# can be written as a vector file. A vector file is stored as a series
# of vectors and polygons. This representation does not have a
# "resolution" so vector files can be printed in larger sizes without
# creating the pixellation.
  
# When do you decided which one to do? Depends at what stage in your 
# project you are. Early on, it might be easier to create bitmap
# images that can be easily included in your word processor.
 
# However, once your paper is accepted for publication, the journal may
# ask you to either increase the resolution or change the format.
# This can be time-consuming, so make sure you think about this potential
# change ahead of time.
  
# Bitmap example with png()
  
  ?png

  plot(1,1,pch="A", cex=15)
  
  # Write to disk with a low resolution
  png("bitmap_1.png", width=500, height = 500, res=50)
  plot(1,1,pch="A", cex=15)
  dev.off()
  
  # Increase resolution
  png("bitmap_2.png", width=500, height = 500, res=50*4)
  plot(1,1,pch="A", cex=15)
  dev.off()
  
  # Increase resolution + increase image size
  png("bitmap_3.png", width=500*4, height = 500*4, res=50*4)
  plot(1,1,pch="A", cex=15)
  dev.off()
  
  # Other bitmap formats: jpeg, tiff, bmp
  jpeg("bitmap_4.jpg", width=500*4, height = 500*4, res=50*4)
  plot(1,1,pch="A", cex=15)
  dev.off()
  
  # If you zoom in you still see pixelation 
  # Because of this, some journals may ask for a vector file
  
# Vector example with:  PDF with cairo_pdf()

  ?cairo_pdf
  
  # Create a 7 x 7 inches PDF
  cairo_pdf("vector_1.pdf", width=7, height = 7)
  plot(1,1,pch="A", cex=15)
  dev.off()
  
  # Now zoom in... Can you see pixels?
  
  # Other vector formats: postscript with cairo_ps()
  cairo_ps("vector_2.ps", width=7, height = 7)
  plot(1,1,pch="A", cex=15)
  dev.off()
  
  
# The end ----
  