# ******************************************************************************

###                     Rex and Sof's Salinity Workshop                      ###

# ******************************************************************************

# ******************************************************************************

# Introduction to R -------------------------------------------------------

# ******************************************************************************

# First thing's first. The hash symbol '#' is known as the 'commenting symbol'.
# It is used to make notes that are not recognized as code by the software.


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


### The arrow '<-' is called the 'Assignment Operator' in R ###
# It is used to assign information to an object.

# You can name the object anything you like.
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
#### In out lab, we use '_' for objects and '.' for functions!!!

### Functions ###

# A function is a piece of code written to carry out a specified task.
# They are made for our convenience by other researchers. 
# Bless their cotton socks :D

# These functions are stored in packages


### Packages ###

# Some are automatically loaded when you start R. For example, 'stats'
# To view a package's contents:
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


### Data Formats (see reference image) ###

## A vector ##

# A vector: 1 or more numbers in a 1-dimensional row of all the same data type
# Lets make a vector of numbers 1 through 18. 
# We'll name it after the Vesper martini - a delicious cocktail that was created 
# by Ian Fleming for James Bond in the original Casino Royale novel ;)
vesper <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
# The 'c' stands for concatenate or combine. It creates a temporary list for
# assignment.
# Notice that 'vesper' has appeared in the environment to the right.
# If you run the object as code, the vector will be presented in the console.
vesper
# Instead of writing all numbers, you can shorten this by just separating the 
# maximum and minimum values by a colon ':'
vesper2 <- c(1:18)
vesper2
# (Note: the difference in the objects in the environment. One is class integer,
# the other is class numeric. This doesn't matter, as R automatically converts 
# between numeric classes.)
# In addition, if we wished to produce a vector with only numbers one through 
# six and ten through eighteen, we can do this easily:
vesper3 <- c(1:6,10:18)
vesper3
# This can be important for selecting specific data from a larger dataset.

## A matrix ##

# A matrix consists of rows and columns. A 2-dimensional array. 
# Requires the same length of rows and same data type.
# Let's make a matrix this time, using the same numbers, but divided into 3 rows.
# This time we use the 'matrix' function
?matrix
# We'll name this one after the Manhattan cocktail. 
# I had a Manhattan in Manhattan once - very nice :D
manhattan <- matrix(c(1:18), nrow=3)
manhattan
# Note that we can also do this with our vesper vector
manhattan2 <- matrix(vesper, nrow=3)
manhattan2
# Notice how the numbers have been distributed by columns? 
# We can switch this to distribute the numbers by rows if desired.
manhattan3 <- matrix(vesper, nrow=3, byrow=TRUE)
manhattan3
# 'TRUE' and 'FALSE' arguments in R are called 'logical' arguments.
# Both can usually be presented as 'T' and 'F' as well and almost never need 
# quotation marks unlike other arguments.

## A dataframe ##

# A dataframe is similar to a matrix, in that it consists of rows and columns. 
# The key difference is, that a dataframe can included data of different types. 
# we can use the as.data.frame function to turn our manhattan matrix into a dataframe
manhattan_df <- as.data.frame(manhattan)
# if we compare our manhattan matrix with our manhattan data frame, we can notice a few differences.
manhattan
manhattan_df
# we can then add a column of characters (called "new") to our manhattan_df
manhattan_df$new <- c('one', 'two', 'three')
# and we can rename one of our column headers
names(manhattan_df)[1] <- 'icePlease'
# check out our new dataframe:
str(manhattan_df) # shows us our column labels, our data types, and the first few data points.


## An array ##

# An array is a stack of matrices (layers), making it three-dimensional. 
# Now lets make an array using the same numbers.
# A stack of three matrices, each with two rows and three columns.
# This requires the 'array' function
?array
# We'll name this one after the delicious Aviation cocktail (nomnom).
# Another cocktail of New York origins - they certainly knew what they were doing 
# in the early 20th century :P
aviation <- array(c(1:18), c(2,3,3))
# Note that this could also be generated from the vesper object. It is possible
# to convert matrices to arrays and vice versa as well but we won't do that here.
aviation
# Notice in the console each matrix from the array is presented one at a time..

# We can call upon any individual row, column, or matrix by identifying the 
# coordinates of interest.
# Notice how the aviation is presented in the Environment as [1:2,1:3,1:3]?
# This represents [rows,columns,layers], like a coordinate system!
# Some examples:
aviation[1,1,1] # will present the value from the first column of the first row
# from the first layer.
# If a section is left empty, all from that dimension are included
aviation[,1,] # will present the first columns of every matrix
aviation[,,1] # will present every row and column from the first matrix only.
# Its a bit weird at first, but it's a super easy way to partition your data for
# isolated analyses.

# You can clear the environment with the following function:
rm(list = ls())
# Alternatively, click on the broom icon above the environment window. 
# The same icon is present in the console and Plots windows.


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
# Data[1,2] will give us the first value of the second column (sepal width)
data[1,2]


### Now lets do some extracting! ###

# The dollar sign '$' is used to extract named elements from a named list.

# For example, we can allocate the variable of sepal length to its own object:
sepalL <- data$Sepal.Length
sepalL 
# Sepal length is now a vector with 150 entries.

# Lets do this for all variables. It's not necessary, but it shortens lines of code.
sepalW <- data$Sepal.Width
petalL <- data$Petal.Length
petalW <- data$Petal.Width
species <- data$Species
# Note that species is qualitative
species
# So it is registered as a Factor with 150 observations across three levels.

# ******************************************************************************

# Salinity Data Analysis ----------------

# ******************************************************************************

# first, we're going to clear the data from our global environment, so we have a 
# fresh start

rm(list = ls())


# Let's import our data

# the folder we are working in is called our "working directory". It is good 
# convention to keep everything you're working on in this folder. You can check 
# you're using the correct working directory by using 
getwd()
# if the working directory isn't the folder you want to use, you can use 
# setwd("xxxx enter filepath here xxx") # to set your working directory. 
# your data should be saved in your working directory folder. You can then easily
# access this data:

all_data <- na.omit(data.frame(read.csv("2022_data.csv")))  
# read.csv will import the data from our working directory
head(all_data) 
# note that we now have all_data in our global environment, which is ##### observations 
# of #### variables
# the name of our species column is a little weird, so lets sort that out:
names(all_data)[1] <- 'species'
# and we can check that everything looks right
str(all_data)
summary(all_data)

# To standardise our results, we have calculated each variable as 
# a ratio of the mean of the control. 
# We are going to analyse the response variables leaf_no_ratio, shoot_height_ratio, 
# root_length_ratio, fw_shoot_root_ratio, dw_shoot_root_ratio, shoot_moisture, and root_moisture.


# Before we get into it, let's make sure our data is formatted so that we can work 
# with it easily. This can be tedious, but saves time in the long run

# Let's assign all of the variables we're going to use to their own object. 

species <- all_data$species
saltConc <- as.character(all_data$salt_conc) 
# this would automatically be numerical, we want it to be character, because we're 
# using it as a categorical variable.
leafNoRatio <- all_data$leaf_no_ratio
shootHtRatio <- all_data$shoot_height_ratio
rootLnRatio <- all_data$root_length_ratio
fwShootRoot <- all_data$fw_shoot_root_ratio
dwShootRoot <- all_data$dw_shoot_root_ratio
shootMoist <- all_data$shoot_moisture
rootMoist <- all_data$root_moisture
totMoist <- all_data$total_moisture

# While we do this, we're going to exclude the controls for the "leaf number-", 
# "shoot height-" and "root length (ratio to control mean)" variables. We're going 
# to combine them into dataframes with the species and salt concentration
# because we want to make sure we have the same number of observations for each of these variables.

asratio_df1 <- cbind.data.frame(species, saltConc, leafNoRatio, shootHtRatio, rootLnRatio)
asratio_df <- asratio_df1[asratio_df1$saltConc>0,] 
# this will ensure that all salt concentrations in this dataframe are greater than 0
                             
# we can get the summary for any of these variables and see the datatypes, etc
summary(asratio_df)
str(asratio_df)

# and we'll just make a dataframe of the rest of the variables, because it makes 
# plotting easier down the track:
alldata_df <- cbind.data.frame(species, saltConc, fwShootRoot, dwShootRoot, 
                               shootMoist, rootMoist, totMoist)

# we'll save this as a .csv for later:
getwd() # find the path for your working directory then paste it as your output path
# in the line below:
path_out = " ## enter your filepath here, make sure it ends with /"
write.csv(alldata_df,paste(path_out,'alldata_df.csv'))


# Next, we need to perform some assumption testing. This is like checking in the 
# pantry to see what ingredients you have. If you have eggs, flour, milk, and sugar, 
# you can make a cake. If you have eggs, milk, bacon and cheese
# you can make an omelette. We want to do a two-way ANOVA, but we need to check 
# that we have all of the ingredients, first. 
# The assumptions (ingredients) of an ANOVA are:
#     > normally distributed data
#     > homogeneity of variances
#     > data are independent (we know our data are independent because the plants 
# were grown in their own pots, and don't influence each other either way.)


# Leaf Number as a Ratio of Control Mean ----------------------------------

# first, we're going to analyse the plant's leaf number as a ratio to the mean control in 
# response to salinity level.

# Let's check our assumptions:

### Assumption 1: data are normally distributed
# The central limit theorem tells us that When we have a sample size large enough 
# (n > 30), the sampling distribution will be normal, regardless of the distribution 
# of the data. However, it's good practice to report your assumption testing in 
# your statistical methods section. 
# We are going to use the Shapiro-Wilk method to test normality. 
# the null hypothesis is that sample distribution is normal. If the test is significant, 
# the distribution is non-normal. 
# If our p-value is > 0.05, we can assume that our data is normally distributed.

# We just extract the column needed from our relevant dataset, and we can do this 
# within the shapiro.test function
shapiro.test(asratio_df$leafNoRatio)

# we can visually assess distribution using histograms 
hist(asratio_df$leafNoRatio)


# if we compare the results from our normality tests with our histograms, we can 
# see whether our data are normally distributed.

# Sometimes, to bring our data closer to normality, we can remove some outliers. 
# We need to explore this visually using boxplots, for both our species and our salt concentration. 

# we're going to use the package 'ggpubr' to make some plots to visualise our group differences

# install.packages("ggpubr")
library(ggpubr)
library(dplyr)

#plot leaf number as ratio to control mean by groups "salt concentration"
# colour box plot by second group "species"

ggboxplot(asratio_df, x = "saltConc", y = "leafNoRatio", color = "species")

# In this case, we can see that there are some outliers present. Generally, outliers
# are classed as numbers outside of 1.5 * the interquartile range. 
# we are going to remove these outliers. Make sure to give justification for this removal in your methods section.

# first, we work out what our first and third quartiles are for our data:
quartiles <- quantile(asratio_df$leafNoRatio, probs=c(.25, .75), na.rm = FALSE) 
# and determine the interquartile range (IQR)
IQR <- IQR(asratio_df$leafNoRatio)

# Then we find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# and reassign any data above the lower threshold and below the upper threshold to a new dataframe, 
# which we will use for the analysis.
leafNoRatio_noOutliers <- filter(asratio_df, leafNoRatio > Lower & leafNoRatio < Upper )

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(leafNoRatio_noOutliers$leafNoRatio)

# we still have a significant p-value, but let's check out our histogram:
hist(leafNoRatio_noOutliers$leafNoRatio)

ggboxplot(leafNoRatio_noOutliers, x = "saltConc", y = "leafNoRatio", color = "species")

# looks pretty good. Let's proceed.

### Assumption 2: variances are homogeneous

# the Levene's test for Homogeneity of Variance (in the 'car' package) will tell us if our variances are homogeneous - 
# that is, that all population variances are equal. A p - value < 0.05 will suggest that variances are significantly 
# different and that this assumption has been violated
# install.packages("car")
library(car) # access the car package

leveneTest(leafNoRatio ~ species*saltConc, data = asratio_df)

# we're going to check how many replicates we have for each of our factors
table(species, saltConc)

### two-way ANOVA

# now that we have the ingredients, let's bake our lovely ANOVA cake!
# the null hypotheses for a two-way ANOVA are:
#       > there is no difference in the means of factor A
#       > there is no difference in the means of factor B
#       > there is no interaction between factor A and B. 
# if the p-value for our ANOVA is >0.05, we reject the null, and find that there 
# is a significant difference or interaction.

leafno.aov <- aov(leafNoRatio ~ species * saltConc, data = asratio_df)
summary(leafno.aov)

 # we can generate some summary statistics for our response data, depending on our two factors:
require("dplyr")
group_by(asratio_df, species, saltConc) %>%
  summarise(count = n(),mean = mean(leafNoRatio, na.rm = TRUE),
            sd = sd(leafNoRatio, na.rm = TRUE))

# the ANOVA tells us that there are differences group means, but we don't know 
# which pairs of groups are different. We can use a Tukey multiple pairwise comparisons 
# test to see where these differences lie. Since we only have two species, 
# only need to do this for our salt concentrations:

TukeyHSD(leafno.aov, which = "saltConc")
# when interpreting the output, diff: difference between means of the two groups, 
# lwr & upr: the lower and the upper end point of the confidence interval at 95%, 
# p adj: p-value after adjustment for the multiple comparisons.


### let's graph it!

# To visualise our data (and for our report), we need to generate a means plot.
# we are going to plot our response variable by groups "saltConc", and use the colours to show the species.
# we add error bars using mean_se
# we can explore the options for our plot using the help for ggline, 
# have a play and see what you think shows your results the best!
?ggline


ggline(asratio_df, x = "saltConc", y = "leafNoRatio", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "number of leaves (ratio to control mean)",
       legend = "right")

# once we're happy with our plot, we click "export" and save it in our working folder.


# Root length as a Ratio of Control Mean ----------------------------------

# Let's check our assumptions:

### Assumption 1: data are normally distributed
# let's do a shapiro-wilk test of normality on our rootLnRatio variable
shapiro.test(asratio_df$rootLnRatio)

# If the Sig. value of the Shapiro-Wilk Test is greater than 0.05, the data is normal. 
# If it is below 0.05, the data significantly deviate from a normal distribution.

# we can visually assess distribution using histograms 
hist(asratio_df$rootLnRatio)

# if we compare the results from our normality tests with our histograms, we can 
# see whether our data are normally distributed.

# Sometimes, to bring our data closer to normality, we can remove some outliers. 
# We need to explore this visually using boxplots, for both our species and our salt concentration. 

# plot root length as a ratio to control mean by groups "salt concentration"
# colour box plot by second group "species"

ggboxplot(asratio_df, x = "saltConc", y = "rootLnRatio", color = "species")

# We can then make note of where outliers occur, and remove them from our data set if we 
# have a valid reason to do so.

# In this case, we can see that there are some outliers present. Generally, outliers
# are classed as numbers outside of 1.5 * the interquartile range. 
# we are going to remove these outliers. Make sure to give justification for this removal in your methods section.

# first, we work out what our first and third quartiles are for our data:
quartiles <- quantile(asratio_df$rootLnRatio, probs=c(.25, .75), na.rm = FALSE) 
# and determine the interquartile range (IQR)
IQR <- IQR(asratio_df$rootLnRatio)

# Then we find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# and reassign any data above the lower threshold and below the upper threshold to a new dataframe, 
# which we will use for the analysis.
rootLnRatio_noOutliers <- filter(asratio_df, rootLnRatio > Lower & rootLnRatio < Upper )

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(rootLnRatio_noOutliers$rootLnRatio)

# And double check using a histogram:
# we can visually assess distribution using histograms 
hist(rootLnRatio_noOutliers$rootLnRatio)

# and have another look at our new boxplots:
ggboxplot(rootLnRatio_noOutliers, x = "saltConc", y = "rootLnRatio", color = "species")

### Assumption 2: variances are homogeneous

# Let's do our levene's test

leveneTest(rootLnRatio ~ species*saltConc, data =rootLnRatio_noOutliers)

# and we're good to go!

### two-way ANOVA

rootLn.aov <- aov(rootLnRatio ~ species * saltConc, data = rootLnRatio_noOutliers)
summary(rootLn.aov)


# we can generate some summary statistics for our response data, depending on our two factors:

group_by(rootLnRatio_noOutliers, species, saltConc) %>%
  summarise(count = n(),mean = mean(rootLnRatio, na.rm = TRUE),
            sd = sd(rootLnRatio, na.rm = TRUE))

# Tukey's HSD will tell is where the differences between groups are

TukeyHSD(rootLn.aov, which = "saltConc")


### let's graph it!

ggline(rootLnRatio_noOutliers, x = "saltConc", y = "rootLnRatio", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "root length (ratio to control mean)",
       legend = "right")

# don't forget to export your plot

# Shoot height as a Ratio of Control Mean ----------------------------------

# Let's check our assumptions:

### Assumption 1: data are normally distributed
# let's do a shapiro-wilk test of normality on our rootLnRatio variable
shapiro.test(asratio_df$shootHtRatio)

# If the Sig. value of the Shapiro-Wilk Test is greater than 0.05, the data is normal. 
# If it is below 0.05, the data significantly deviate from a normal distribution.

# we can visually assess distribution using histograms 
hist(asratio_df$shootHtRatio)

# hmm, there's quite a bit of skew there. Let's look a little closer using boxplots:

ggboxplot(asratio_df, x = "saltConc", y = "shootHtRatio", color = "species")

# and remove our outliers:

quartiles <- quantile(asratio_df$shootHtRatio, probs=c(.25, .75), na.rm = FALSE) 
IQR <- IQR(asratio_df$shootHtRatio)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

shootHtRatio_noOutliers <- filter(asratio_df, shootHtRatio > Lower & shootHtRatio < Upper )

# now we check again:
shapiro.test(shootHtRatio_noOutliers$shootHtRatio)
hist(shootHtRatio_noOutliers$shootHtRatio)
ggboxplot(shootHtRatio_noOutliers, x = "saltConc", y = "shootHtRatio", color = "species")

# we still have some skew, and our shapiro-wilk test tells us that our data is not normal.
# however, since ANOVA are robust to deviations from normality, we will proceed and check 
# our next assumption:

### Assumption 2: variances are homogeneous
# Let's do our levene's test
leveneTest(shootHtRatio ~ species*saltConc, data =shootHtRatio_noOutliers)

# and we're good to go!

### two-way ANOVA

shootHt.aov <- aov(shootHtRatio ~ species * saltConc, data = shootHtRatio_noOutliers)
summary(shootHt.aov)

# summary statistics:
group_by(shootHtRatio_noOutliers, species, saltConc) %>%
  summarise(count = n(),mean = mean(shootHtRatio, na.rm = TRUE),
            sd = sd(shootHtRatio, na.rm = TRUE))

# Tukey's HSD will tell is where the differences between groups are

TukeyHSD(shootHt.aov, which = "saltConc")


### let's graph it!

ggline(shootHtRatio_noOutliers, x = "saltConc", y = "shootHtRatio", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "shoot height (ratio to control mean)",
       legend = "right")

# don't forget to export your plot
