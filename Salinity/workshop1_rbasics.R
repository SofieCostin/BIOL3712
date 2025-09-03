# ******************************************************************************
###                  BIOL3712,  Flinders University, SA                      ###
###                     Introduction to R programming                        ###
###           By Sofie Costin, based on material from Rex Mitchell           ###
### https://www.youtube.com/playlist?list=PL42DD6-0IeVOc4rnMKwAcx6zCww5vzjV_ ###
###                     Last updated 04/09/2024                             ###
# ******************************************************************************

# Introduction to R -------------------------------------------------------

# First thing's first.The hash symbol '#' is known as the 'commenting symbol'.
# It is used to make notes that are not recognized as code by the software. 

# You are reading this code in Rstudio. Rstudio is NOT R! Rstudio is an editing
# software that sends code you've written to R for executing. You can see the R 
# console below. Using Rstudio, you can slowly build your analyses and re-run 
# them as often as you like. If something does not work, you can fiddle with 
# it in Rstudio and send it back to R.

# selecting one/many line/s and hitting ctrl/cmd + enter/return will run the selected code
# (but it won't run anything that has a #-symbol at the start).

# For every analysis, you should begin a new R script and save it to a file that
# contains your data. This means that when you import and save files and images,
# they will all be contained within the same folder.



### Basic syntax ###

# R can be used as a calculator, similar to excel.
# '+' is plus
# '-' is minus
# '/' is division
# '*' is multiplication
# '^' is exponent
# 'sqrt' is the square root
# There's others too, some of which we'll get to in these sessions.

# Example:
1+1
2^2
sqrt(4)
# Notice the solution is presented in the Console panel below.

# Workbook question 1a: --------
# please write and run three lines of code to answer three simple maths 
# problems (like the examples above)

# The arrow '<-' is called the 'Assignment Operator' in R
# It is used to assign information to an object.

# You can name the object anything you like. For example, let''s create an object called 'dirty socks', 
# and store a simple math problem inside.
dirty_socks <- 2+2
# Notice that the object appears in the Environment window (top right).

# If you type the object alone, the information it contains is presented in the 
# console, in this case a vector with a single value.
dirty_socks

# You can then call upon that allocated information in further 
# calculations/analyses
dirty_socks*15
# Notice that an answer of 60 is printed in the console.
# In theory, this could continue ad infinitum. I could allocate the above line 
# to a new object:
clean_shoes <- dirty_socks*15
# And now the environment contains this object as well for future use.

# This can dramatically shorten coding for later usage

### TWO NOTES ON THIS ###
# 1) R is case-sensitive! Be careful!
# 2) No spaces allowed in object names between words. Instead use '_' or '.'
#### In our lab, we use '_' for objects and '.' for functions!!!


### Functions ###

# A function is a piece of code written to carry out a specified task.
# They are made for our convenience by other researchers. 
# Bless their cotton socks :D

# For example, if you want to print something in your console, you can use the cat function. 
cat("I would rather wear no socks than dirty socks")


# Workbook question 1b: ---------------------------------------------------
# tell me what your name is!

# create an object for your name:
myname <- "Sofie Costin" # enter your name here instead of my name
cat("************ My name is", myname, "************")



### Data Formats (see reference image) ###

## A vector ##

# A vector: 1 or more numbers in a 1-dimensional row of all the same data type
# Lets make a vector of numbers 1 through 18. We are going to call it 'vec' - short for vector.

vec <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
# The 'c' stands for concatenate or combine. It creates a temporary list for
# assignment.
# Notice that 'vec' has appeared in the environment to the right.
# If you run the object as code, the vector will be presented in the console.
vec
# Instead of writing all numbers, you can shorten this by just separating the 
# maximum and minimum values by a colon ':'
vec2 <- c(1:18)
vec2
# (Note: the difference in the objects in the environment. One is class integer,
# the other is class numeric. This doesn't matter, as R automatically converts 
# between numeric classes.)


# Workbook question 1c: ----------------------------------------------------
# create a vector using 16 numbers of your choice. you can call it anything you like




## A matrix ##

# A matrix consists of rows and columns. A 2-dimensional array. 
# Requires the same length of rows and same data type.
# Let's make a matrix this time, using the same numbers, but divided into 3 rows.
# This time we use the 'matrix' function
?matrix
# We'll name this one 'mat'- short for matrix

#Here, we distribute the vec vector (1:18) across three rows (i.e. 3 rows with 6 columns each = 18)
mat <- matrix(c(1:18), nrow=3)
mat
# Because the 'vec' vector is a vector from 1:18, the above is the same as:
mat2 <- matrix(vec, nrow=3)
mat2
# Notice how the numbers have been distributed by columns? 


# Workbook question 1d: ---------------------------------------------------
# turn your vector into a matrix with four rows




## A dataframe ##

# A dataframe is similar to a matrix, in that it consists of rows and columns. 
# The key difference is, that a dataframe can include data of different types. 
# we can use the as.data.frame function to turn our 'mat' matrix into a dataframe
mat_df <- as.data.frame(mat)
# if we compare our mat matrix with our mat data frame, we can notice a few differences.
mat
mat_df
# we can then add a column of characters (called "new") to our mat_df
mat_df$new <- c('one', 'two', 'three')
mat_df
# and we can rename one of our column headers (here, we use column 1 by typing [1], 
#but it could be column 2 if you typed [2])
names(mat_df)[1] <- 'case'
mat_df

# check out our new dataframe:
str(mat_df) # shows us our column labels, our data types, and the first few data points.



# Workbook question 1e:  --------------------------------------------------
# turn your matrix into a dataframe. add a column called 'pie' of characters (words) that relate to pies.
# (this is equivalent to the addition of the 'mat_df$new' column)
# rename another column to be called 'pizza'



# You can clear the environment with the following function:
rm(list = ls())
# Alternatively, click on the broom icon above the environment window. 
# The same icon is present in the console and plots windows.


### Now lets look at some data!!! ###

# R has many stored datasets
# To view the list:
data()

# You can view the details of each dataset.
?iris # for example

# We can allocate this data to an object, so that it appears in the environment
data <- iris

# The data can be viewed either by clicking on the small table icon to the right
# of the object in the environment, or by run the object as code.
data

# The function 'head' produces the first six lines of the dataset.
head(iris) 

# The 'summary' function presents the main parameters of the data.
summary(iris) 
# prints number of individuals, variable means, sds, IQR, etc
# NOTE: the data is essentially a matrix by coordinates. SO we can extract any
# data via their coordinates, just like before!
# for exammple, data[1,2] will give us the first value of the second column (sepal width)
data[1,2]


### Now lets do some extracting! ###

# The dollar sign '$' is used to extract named elements from a named list. 
# You can use this to extract columns from a dataframe, which is what we will do 
# later down the track.

# For example, we can allocate the variable of sepal length to its own object:
sepalL <- data$Sepal.Length
sepalL 
# Sepal length is now a vector with 150 entries - check this in the environment

# Lets do this for all variables. It's not necessary, but it shortens lines of code.
sepalW <- data$Sepal.Width
petalL <- data$Petal.Length
petalW <- data$Petal.Width
species <- data$Species
# Note that species is qualitative
species
# So it is registered as a Factor with 150 observations across three levels.

# if you save your workspace at the end of your session, it will save all of the 
# information in your global environment, which is especially handy when your code 
# takes a long time to run.



print("congratulations, now it's time to start your salinity analysis!")



######~~~~~~~Some additional info for the nerds among you :-D ~~~~~~######

# These functions are stored in packages

### Packages ###

# Some are automatically loaded when you start R. For example, 'stats'
# To view a package's contents (this will open a new window, but 
#this code is on the tab to the left):
library(help = "stats")
# This will open a new window listing all of the details of the package.
# This should include the authors for citations and all functions to search.
# When you find a function that you wish to use, you can search that function
# in the help box (bottom right window, 4th tab) and see exactly how to use it.
# Alternatively, type '?' followed by the function.
# For example, lets look at the function for a linear model, 'lm'
?lm

# Two other types of packages:
# 1) Base packages: installed with R but are not automatically loaded to save 
# memory. (see list in bottom right window, 3rd tab)
# 2) Contributed packages: created by external parties. Must be installed and 
# loaded. For example, 'geomorph' is specifically tailored for Geometric 
# Morphometrics. This must be installed.

# Three places to find them:
# 1)	Search in the packages window (click install and type the package name)
# 2)	CRAN: comprehensive R archive network. Go to https://cran.r-project.org/
# 3)	GitHub: https://github.com/trending/r
