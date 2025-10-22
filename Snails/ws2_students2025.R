# effect of temperature on the oxygen consumption of freshwater snails
# Sofie Costin & *your name here* October 2025

# MAKE SURE TO SET YOUR WORKING DIRECTORY FIRST: Session - Set Working Directory - To Source File

# install (if needed) and call packages
# install.packages(c("dplyr", "car"))

library(dplyr)
library(car)

# clear environment 
rm(list = ls())

# import data -------------------------------------------------------------

snaildata <- data.frame(read.csv("snaildata2025.csv"))

# read.csv will import the data from our working directory
head(snaildata) 
# note that we now have all_data in our global environment, which is ### observations of 2 variables
# and we can check out our data types and variables
summary(snaildata)
str(snaildata)

# we need to set our temperature variable as a character, because we want it to behave as categorical data, not numeric.
snaildata$temp <- as.factor(snaildata$temp) 
# run the below and look at the output compared to the previous time you ran str. Can you see how $temp has changed from integer to factor?
str(snaildata) 

# From the summary, we can see that our minimum DO consumed is a negative value. 
# Why is this not realistic?

# let's remove data points below 0
data_rm1 <- data.frame(dplyr::filter(snaildata, do_consumed > 0 ))
# this creates a new data frame containing all data that has a do consumed value greater than 0

# How many data points are left? 


# assumption testing ------------------------------------------------------
# Because we have one predictor variable and a numerical response, we are thinking we might use an ANOVA.
# The assumptions (ingredients) of an ANOVA are:
#     > normally distributed data 
#     > homogeneity of variances
#     > data are independent (snails are in their own containers and don't affect each other)

# assumption 1: data are normally distributed -----------------------------

# SHAPIRO WILK TEST OF NORMALITY
# the null hypothesis is that sample distribution is normal. 
# If the test is significant (P<0.05), the distribution is non-normal. 
# If our p-value is > 0.05, we can assume that our data is normally distributed.

# how do we do this test? If you've forgotten have a look at the salinity code. Half a coder's life is copy-pasting code that has worked before!





# visually assess distribution using histograms 
hist(
  data_rm1$do_consumed,
  main = "Histogram: DO consumed (all data > 0)",
  xlab = "Dissolved oxygen consumption (mg/kg/hour)"
)

# if our data are not normally distributed, check for outliers using a boxplot:
boxplot(
  do_consumed ~ temp, data = data_rm1,
  main = "Boxplot: DO consumed by temperature",
  xlab = "Temperature (°C)", ylab = "DO consumed (mg/kg/hour)"
)

# if data are not normally distributed, and we can see that there are outliers, 
# remove data outside of 1.5*IQR

# set first and third quartiles
quartiles <- quantile(data_rm1$do_consumed, probs = c(.25, .75), na.rm = FALSE) 
# determine the interquartile range (IQR)
IQR_val <- IQR(data_rm1$do_consumed)

# find our lower and upper limits:
Lower <- quartiles[1] - 1.5 * IQR_val
Upper <- quartiles[2] + 1.5 * IQR_val 

# reassign data above lower and below upper to new dataframe called data_rm
data_rm <- data.frame(dplyr::filter(data_rm1, do_consumed > Lower & do_consumed < Upper ))

# Have a look in the global environment, are there less observations? has anything changed?

# RETEST NORMALITY USING SHAPIRO WILK & HISTOGRAM



# how do we do this test? Check the salinity code if you need to



# visually assess distribution using histograms 
hist(
  data_rm$do_consumed,
  main = "Histogram: DO consumed (all data > 0)",
  xlab = "Dissolved oxygen consumption (mg/kg/hour)"
)

# if our data are not normally distributed, check for outliers using a boxplot:
boxplot(
  do_consumed ~ temp, data = data_rm,
  main = "Boxplot: DO consumed by temperature",
  xlab = "Temperature (°C)", ylab = "DO consumed (mg/kg/hour)"
)





# are we happy with this? are your data normally distributed? did anything change?



# when reporting this in our methods section, we need to include the outcomes of the test, and how many outliers were removed. 
# We can check what outliers were removed using:
removed_data_points <- which(!snaildata$do_consumed %in% data_rm$do_consumed)
snaildata[removed_data_points,]


# assumption 2: variances are homogenous ----------------------------------

# How do we do this again, and how is it reported? Check the salinity code if you don't remember. 




# you can also visually assess homogeneity / normality by group with QQ-plots (explanation below)

par(mfrow = c(2, ceiling(nlevels(data_rm$temp)/2)), mar = c(4,4,2,1)) # sets multiple panels for graph
by(data_rm$do_consumed, data_rm$temp, function(x) {
  qqnorm(x, main = paste("QQ-plot:", unique(data_rm$temp[data_rm$do_consumed %in% x])))
  qqline(x)
})

par(mfrow = c(1, 1)) # reset to 1 x 1 plotting

# interpreting Q-Q plots:

# A Q-Q (quantile–quantile) plot compares your sample’s distribution to a theoretical normal distribution:
# X-axis: theoretical quantiles (what we’d expect under perfect normality)
# Y-axis: actual quantiles from your sample data
# If your data are normally distributed, the points should fall roughly along the straight line.
#
# How to interpret the plots in your image:
# Each panel represents one temperature treatment (e.g., 15°C, 18°C, 21°C, etc.).
# Deviation from the line = deviation from normality.
# If points hug the line closely, that group’s data are approximately normal.
# If points curve away (S-shape), that indicates skewness:
#   - Upward curve at the ends → right-skewed (tail to the right)
#   - Downward curve → left-skewed
# If the ends deviate sharply upward or downward → heavy tails or outliers.
# 
# Sample size matters.
# You have small group sizes (e.g., n ≈ 8–10).
# In small samples, QQ plots can look messy even when data are roughly normal.
# That’s why you should always combine visual inspection with the Shapiro–Wilk test

#What do these results tell you about normality in your various temperature treatements? Are there any that look particularly dodgy? That deserves a brief mention in your poster, regardless of your decision whether to go parametric or non-parametric


###################################################DECISION TIME ~~~~~ Do you think your data are sufficiently normally distributed and sufficiently homogenous in variance to use a parametric ANOVA? Or are you better off using a parametric test? Make a decision NOW and then continue below for an ANOVA or skip to line 271 for non-parametric testing. You need to only do one. And there's no  right or wrong answer, just well or not-so-well explain reasons. Sof & Vera would both be totally on the fence as to what to use. #################################################

# PARAMETRIC TEST & PLOTTING------------------------------------------------
# because we have a single predictor and a numerical response, we'll use an ANOVA

# the null hypothesis for ANOVA is that there is no difference in means.
# if the p-value for our ANOVA is <0.05, we reject the null, and find that there 
# is a significant difference between the means
# or that there is less than 5% chance that the relationship that we see is due to random chance

# you will need to edit this code to make y your response variable and x your predictor variable
snailanova <- aov(RESPONSE ~ PREDICTOR, data = data_rm)    
    
summary(snailanova)

# summary statistics ------------------------------------------------------

# this will tell us the means and standard deviation of our data, plus the number of replicates (n) for each level
group_by(data_rm, temp) %>%
  summarise(
    count = n(),
    mean = mean(do_consumed, na.rm = TRUE),
    sd   = sd(do_consumed, na.rm = TRUE)
  )

# post-hoc analysis -------------------------------------------------------
# ONLY NEEDED IF ANOVA RESULT IS SIGNIFICANT

# the ANOVA tells us that there are differences in group means, but we don't know 
# which pairs of groups are different. We can use a Tukey multiple pairwise comparisons 
# test to see where these differences lie. 
TukeyHSD(snailanova, which = "temp")




#  create means plot ---------------------------------------------
#This is using a slightly different technique to last time, allowing for even more customisation of what values are used in plotting. 

# first, we're going to create a dataframe including the means and standard deviation for our data
# we are calling this dataframe data_means_sd, and we are grouping our data by temperature levels.
data_means_sd <- data_rm %>%  
  group_by(temp) %>% 
  summarize(
    mean_do     = mean(do_consumed),
    sd_do       = sd(do_consumed),
    upper_limit = mean_do + sd_do, 
    lower_limit = mean_do - sd_do,
    .groups = "drop"
  )

# this is the output of that data frame. We will use these values to put into the plot later.
data_means_sd

# x positions for factor levels (it just tells it there are six x values -  i.e. 6 temperatures)
x <- seq_len(nrow(data_means_sd))

#This sets the highest y value to be represented on the chart, taken from the largest upper limit standard deviation value of all temp treatments. 
ylim_max <- max(150, max(data_means_sd$upper_limit, na.rm = TRUE))


# empty plot with desired limits and labels

#This plot has nothing in it - completely blank! This is useful if you want to then add tons of elements exactly how you like it.
plot(
  x, 
  data_means_sd$mean_do, 
  type = "n",
  xaxt = "n",
  ylim = c(0, ylim_max),
  xlab = "temperature (degrees celsius)",
  ylab = "dissolved oxygen consumption rate (mg/kg/hour)",
  main = ""
)

#Now we add in all the things we want

# add x-axis with factor labels
axis(1, at = x, labels = as.character(data_means_sd$temp))

# add lines and points between each level of temperature treatment and the y values for the means
lines(x, data_means_sd$mean_do)
points(x, data_means_sd$mean_do, pch = 16)

# add error bars using arrows(). This is super nifty - you use an arrow with a blunt tip (90deg angle) either side going from the upper to the lower standard deviation around the mean. 
arrows(
  x0 = x, y0 = data_means_sd$lower_limit,
  x1 = x, y1 = data_means_sd$upper_limit,
  angle = 90, code = 3, length = 0.05
)

# add grouping letters (EDIT THESE to match your Tukey groups)
letters_vec <- c("a","a","a","a","a","a")  # must match length(x)

#add text to at each temperature (x) just above the upper limit of each temperature (+2 here, but any value is fine, you can play with that)
text(x, data_means_sd$upper_limit + 2, labels = letters_vec, font = 2)


# in order to get a HD for this assignment, you will need to introduce at a change to at least four elements of the plot - and they have to be substantially different (i.e. don't just change the colour of arrows, fonts, lines and points!)



# NON-PARAMETRIC TEST & PLOTTING --------------------------------------------------

# If normality or homogeneity of variances do not hold, use a Kruskal–Wallis test.
# The null hypothesis is that the distributions are the same across groups (medians are equal).

# Kruskal–Wallis rank-sum test
kw <- kruskal.test(RESPONSE ~ PREDICTOR, data = data_rm)

kw

# Simple effect size for Kruskal–Wallis (epsilon-squared ε², 0–1 scale)
# ε² ~ proportion of variability in ranks explained by group


H  <- as.numeric(kw$statistic)                # Kruskal–Wallis H
k  <- nlevels(data_rm$temp)                 # number of groups
N  <- nrow(data_rm)                         # total N
epsilon2 <- (H - k + 1) / (N - k)
epsilon2   # report this alongside the KW test in methods/results

# Interpretation:
# ε² value:
# < 0.01	Very small / negligible:	Temperature explains almost none of the variability
# 0.01 – 0.08	Small effect:	Weak but noticeable difference among temperatures
# 0.08 – 0.26	Medium effect:	Moderate differences among group medians
# > 0.26	Large effect:	Strong group differences; temperature explains much of the variation
# (These cut-offs are based on common guidelines from Tomczak & Tomczak, 2014.)

# compare group medians:

aggregate(do_consumed ~ temp, data = data_rm, median)


# summary statistics ------------------------------------------------------

group_by(data_rm, temp) %>%
  summarise(
    n        = n(),
    median   = median(do_consumed, na.rm = TRUE),
    Q1       = quantile(do_consumed, 0.25, na.rm = TRUE),
    Q3       = quantile(do_consumed, 0.75, na.rm = TRUE)
  )


# post-hoc analysis -------------------------------------------------------
# ONLY IF KW IS SIGNIFICANT
# Pairwise Wilcoxon rank-sum tests with Holm correction for multiple comparisons
# Because many comparisons are made, we use a Holm correction (p.adjust.method = "holm") to control the risk of false positives (Type I error).


pw <- pairwise.wilcox.test(
  x = data_rm$do_consumed,
  g = data_rm$temp,
  p.adjust.method = "holm",
  exact = FALSE
)
pw  # Use this table to decide which groups differ; update letters on your plot accordingly

# Interpretation:
# The pairwise Wilcoxon rank-sum test (sometimes called the Mann–Whitney U test) compares each pair of temperature groups to see where the significant differences lie, following a significant Kruskal–Wallis test.
# While the Kruskal–Wallis test only tells us that “at least one group differs”, this pairwise test helps identify which pairs differ.
# Each test compares the distribution (median ranks) between two temperature 
# levels, e.g.:  15°C vs 18°C
# Each comparison returns a p-value indicating whether those two groups differ significantly.


# create median IQR plot --------------------------------------------------

# Create a small table with medians and IQR limits per temperature
data_medians_iqr <- data_rm %>%
  group_by(temp) %>%
  summarise(
    med_do   = median(do_consumed),
    q1_do    = quantile(do_consumed, 0.25),
    q3_do    = quantile(do_consumed, 0.75),
    .groups = "drop"
  )
# check
data_medians_iqr

# x positions for factor levels (it just tells it there are six x values -  i.e. 6 temperatures)
x_med <- seq_len(nrow(data_medians_iqr))

#This sets the highest y value to be represented on the chart, taken from the largest upper limit IQR of all temp treatments.
ylim_max_med <- max(150, max(data_medians_iqr$q3_do, na.rm = TRUE))

# Empty plot with labels and limits
#This plot has nothing in it - completely blank! This is useful if you want to then add tons of elements exactly how you like it.

plot(
  x_med, 
  data_medians_iqr$med_do, 
  type = "n",
  xaxt = "n",
  ylim = c(0, ylim_max_med),
  xlab = "temperature (degrees celsius)",
  ylab = "dissolved oxygen consumption rate (mg/kg/hour)",
)

# Add x-axis with factor labels

axis(1, at = x_med, labels = as.character(data_medians_iqr$temp))

# add lines and points between each level of temperature treatment and the y values for the means
lines(x_med, data_medians_iqr$med_do)
points(x_med, data_medians_iqr$med_do, pch = 16)

# add error bars using arrows(). This is super nifty - you use an arrow with a blunt tip (90deg angle) either side going from the upper to the lower standard deviation around the mean. 
arrows(
  x0 = x_med, y0 = data_medians_iqr$q1_do,
  x1 = x_med, y1 = data_medians_iqr$q3_do,
  angle = 90, code = 3, length = 0.05
)

# Add group letters above the IQR bars (**EDIT these based on 'pw' table)
letters_vec_np <- c("a", "a", "a", "a", "a", "a")

#add text to at each temperature (x) just above the upper limit of each temperature (+2 here, but any value is fine, you can play with that)
text(x_med, data_medians_iqr$q3_do + 2, labels = letters_vec_np, font = 2)

# in order to get a HD for this assignment, you will need to introduce at a change to at least four elements of the plot - and they have to be substantially different (i.e. don't just change the colour of arrows, fonts, lines and points!)

