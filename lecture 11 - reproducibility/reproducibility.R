#===============================================================================
# AEM 6850 - Reproducibility
# Explores different options to ensure reproducibility of your code.
#===============================================================================

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Approach #1: guaranteed to fail (what newbies do) -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Load this package (you likely already have it)

library("raster")

# Try to do it differently

x <- "raster"

library(x) # does not work

library(x, character.only = TRUE) # this works

# Check the package version

packageVersion(x) # one way to do ot

sessionInfo() # another way

# What if you now uninstall the package?

remove.packages(x) # Notice where that pacakge was stored in the notification

# Now try to load

library(x, character.only = T)

?raster

# It is no longer installed, so install it again

install.packages(x)

# Now load it

library(x, character.only = T)

# Get help (it should work now)

?raster

# Check version

packageVersion(x) # one way to do ot

# One of the main issues here is that if you code in this way:

library("raster") 

# ... it will only work in computer that have installed the raster package

# as a result simply loading libraries at the beginning of a project is
# not ideal for reproducibility, cause it is common for people not to have the 
# necessary pacakges installed.

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Approach #2: load/install necessary packages (a bit better) -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# A better strategy is to *CHECK* whether the person has a particular package
# installed.

# If the package is not installed, then you install it

# You then load all packages.

# Remove the "sp" package

remove.packages("sp")

# So you first have to determine which packages you WANT to load

want <- c("RColorBrewer","raster","sp","maps")

# You then have to check whether these packages are actually installed in the
# computer running the code:

installed.packages()

class(installed.packages())

head(installed.packages())

# You can see the pacakge names, where they are located in the disk, the version, etc.

installed <- installed.packages()[,"Package"]

# Now test whether packages are available

want %in% installed

# Subset the packages that you WANT but are NOT installed

need <- want[ !(want %in% installed) ]

need

# Load or install necessary packages IF necessary

if (length(need)>0) install.packages(need)

# Now double check all packages are installed

installed2 <- installed.packages()[,"Package"]

want[ !(want %in% installed2) ] 

# Now you can load ALL packages you wanted

lapply(want, function(i) library(i, character.only=TRUE))

# You can also use the require() function

lapply(want, function(i) require(i, character.only=TRUE))

# The main difference between require() and library() is that library() will
# return an error if the package is not installed (which is a good idea in general)

require("random") # warning (code continues to run)

library("random") # error (code stops)

# So be careful. It is preferrable to get an error so you can fix your code.

# One key differences is what these functions return when executed

out <- library("plm")

out # list of packages already loaded

out <- require("sp")

out # TRUE because you have already installed the "sp" packages

out <- require("random")

out # FALSE if you have not installed the "random" package

# So this technique offers a simple way to have any user load the packages that
# you used to run your code.

want <- c("RColorBrewer","raster","sp","maps") # packages you want
installed <- installed.packages()[,"Package"] # list those that are installed
need <- want[ !(want %in% installed) ] # select those that are not installed
if (length(need)>0) install.packages(need) # install those
lapply(want, function(i) library(i, character.only=TRUE)) # load all packages
rm(want, need, installed) # clean up the workspace


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Approach #3: installing packages in local repositories -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# The main limitation with approach #2 is that packages change. You may be
# using a package of a certain version, but then when someone else wants to
# execute your code, you can run into trouble because they installed a different
# version.

# An easy way out of this is to simply share your code along with the exact
# package versions that you used in your project.

# How do we do this? 

# When you install a new package, R defaults to your default directory in your
# computer

install.packages("sp")

?.libPaths

.libPaths() # thi sis where you packages are installed

list.files(.libPaths()) # these are all the packages

# Well, you could install your package in a local folder (i.e. library) within 
# your project

getwd()

dir <- "Rlibrary" # the local directory where you will install the pacakge

dir.create(dir) # create the local library (folder) for your pacakges

list.files() # now you have a "packages folder" (i.e. a local library)

# Now install the package from the web (your default repository) into your local library

install.packages("sas7bdat", lib = dir)

# Check it out in your file finder. Notice that not only plm was isntalled, but
# all the dependencies as well!

list.files(dir) # or list the files in that folder

# Congrats! The package is now installed "locally".

# Now, load it from that local library:

require("sas7bdat", lib.loc = dir)

?sas7bdat.sources

# Now unload the package

detach("package:sas7bdat", unload=TRUE, force=TRUE)

?sas7bdat.sources # should not work cause package is not loaded

# Now try to load from your normal library (where you have NOT installed this package)

library("sas7bdat") # error

require("sas7bdat") # warning; package is not there

?sas7bdat.sources # still cannot load help or functions!

# Now do it from local library

require("sas7bdat", lib.loc = dir)

?sas7bdat.sources # this should work

# Again, this is a way for people to rely exactly on the same packages you used
# while you were writing your project code

# Now, can you think of a limitation of this approach?

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Approach #4: pacman and remotes (requires devtools) -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# pacman is a package that will help you load the packages
# Here is the vignette: 
# http://trinker.github.io/pacman/vignettes/Introduction_to_pacman.html

install.packages("pacman")

library(pacman)

?p_load

?sas7bdat.sources # should give you an error

p_load("sas7bdat")

?sas7bdat.sources # should work!

# In essence, pacman does what Approach #2 does.

# However, you can also use a function from the devtools pacakges to do what 
# approach #3 does and ensures compatibility between Macs and Windows systems.

# Read description of devtools: https://www.r-project.org/nosvn/pandoc/devtools.html

# To install older versions of a package, first install devtools and remotes

p_load("devtools") 
p_load("remotes")

# Note we are using "pacman" here, but that not necessary
# We coculd have done:
# install.packages("devtools")
# library("devtools")
# library("remotes")

# If the devtools installation does not work, you might have to install:
# - Xcode from the Mac App Store (if you are using a Mac): https://apps.apple.com/us/app/xcode/id497799835
# - Rtools (if you are using Windows): https://cran.r-project.org/bin/windows/Rtools/
# Read the description of devtools above.

# Install the latest version of the ggplot2 package in the traditional way

install.packages("ggplot2")

packageVersion("ggplot2") 

# devtools (and its "remotes" dependencis) has a nice fucntion to allow you to install older packages

?install_version

# Try an older version of ggplot2

install_version("ggplot2", version = "0.9.1", repos = "http://cran.us.r-project.org")

packageVersion("ggplot2") # much older version

# Now, what if you just want to make sure that you have a "recent" enough version

?p_install_version

p_install_version("ggplot2","0.9.0") # cause this is older

p_install_version("ggplot2","2.0.0") # cause this is more recent

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Approach #5: renv -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Introduction to renv()
# https://cran.r-project.org/web/packages/renv/vignettes/renv.html

p_load("renv")

# The general workflow when working with renv is:

# 1. Call renv::init() to initialize a new project-local environment with a 
#    private R library,
# 2. Work in the project as normal, installing and removing new R packages as 
#    they are needed in the project,
# 3. Call renv::snapshot() to save the state of the project library to the 
#    lockfile (called renv.lock),
# 4. Continue working on your project, installing and updating R packages as needed.
# 5. Call renv::snapshot() again to save the state of your project library if 
#    your attempts to update R packages were successful, or call renv::restore() 
#    to revert to the previous state as encoded in the lockfile if your attempts 
#    to update packages introduced some new problems.

# Step 1:
renv::init() # step 1

# Now check the working directory folders

list.files() # you now have an "renv" folder

list.files("renv") # check what is inside with your finder

# Step 2: Here you can install new packages as you need them, etc.

p_load("raster")

p_load("fixest")

# Step 3:

renv::snapshot()

# type "yes", now you have an "renv.lock" file in the working directory

# Step 4: keep working... say you want to load the "calendR" package
# first go to /renv/library... and check that the folder for "calendR" is not there

p_load("calendR")

# Step 5: do it again to lock the state of the library again

renv::snapshot() 

# Now, close this RStudio project and come back to this exact line below

library("renv") # only works if this is already installed! It is in YOUR computer now, but not necessarily that of your reviewer
renv::restore()  # run this line and wait.

# It should load all pacakges that you had loaded before.

# There are limitations to renv. Check the webpage above.
# Main limitation, is that it only deals with package versions. NOT R versions.
# As with other strategies, older package versions might not be available,
# so renv might not be able to find them on the main R repository (CRAN)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Approach #6: checkpoint -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Before doing this, delete all packages installed by previous strategies 
list.files()
unlink("Rlibrary", recursive = T)
unlink("renv", recursive = T)
unlink("renv.lock")

# Vignette for checkpoint: 
# https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html

# The key strength of checkpoint is that the team behind checkpoint download a daily
# snapshot of ALL the pacakges and keeps them in a spearate repository for future
# use. That measn there is a centralized place where all the other packages are
# located and you cacn use a "time machine" to go back to a given point in time
# an load the pacakges from that period.

# How does it work? First install it

install.packages("checkpoint")
library("checkpoint")

?checkpoint

checkpoint("2014-07-01") # too old, only stars on 2014-09-17

checkpoint("2020-07-01", project=getwd()) 

# Download the reproduction package for this paper. It uses checkpoint.

# https://doi.org/10.6077/f26v-xz15

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Approach #7: docker  -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# quick introduction: https://colinfay.me/docker-r-reproducibility/

# "Docker is a program that allows to manipulate (launch and stop) multiple 
# operating systems (called containers) on your machine (your machine will be 
# called the host). "

# Tutotial in R: https://ropenscilabs.github.io/r-docker-tutorial/

# The end ----