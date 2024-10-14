
# effect of temperature on the oxygen consumption of freshwater snails
# Sofie Costin & *your name here* October 2024

# MAKE SURE TO SET YOUR WORKING DIRECTORY FIRST

# install and call packages
# 
# 
# install.packages("ggpubr", "dplyr", "car", "plotly")

library(ggpubr)
library(dplyr)
library(car)
library(plotly)


# clear environment 
rm(list = ls())

# import data -------------------------------------------------------------

snaildata <- data.frame(read.csv("snaildata2024.csv"))

# read.csv will import the data from our working directory
head(snaildata) 
# note that we now have all_data in our global environment, which is 72 observations of 2 variables
# and we can check out our data types and variables
summary(snaildata)
str(snaildata)

# we need to set our temperature variable as a character, because we want it to behave as categorical data, not numeric.

snaildata$temp <- as.factor(snaildata$temp) 
# run the below and look at the output compared to the previous time you ran str. Can you see how $temp has changed from integer to factor?
str(snaildata) 

# From the summary, we can see that our minimum DO consumed is a negative value (-22.026!). This is not realistic, because a snail cannot produce oxygen

# let's remove data points below 0

data_rm1 <- data.frame(filter(snaildata, do_consumed > 0 ))


# this creates a new data frame containing all data that has a do consumed value greater than 0


# assumption testing ------------------------------------------------------
# Because we have more than two predictor variables, we are thinking we might use an ANOVA.
# The assumptions (ingredients) of an ANOVA are:
#     > normally distributed data 
#     > homogeneity of variances
#     > data are independent (snails are in their own containers and don't affect each other)

# assumption 1: data are normally distributed -----------------------------

# SHAPIRO WILK TEST OF NORMALITY
# the null hypothesis is that sample distribution is normal. 
# If the test is significant (P<0.05), the distribution is non-normal. 
# If our p-value is > 0.05, we can assume that our data is normally distributed.

# how do we do this test?



# visually assess distribution using histograms 



# if our data are not normally distributed, check for outliers using a boxplot:

ggboxplot(data_rm1, x = "temp", y = "do_consumed")

# if data are not normally distributed, and we can see that there are outliers, 
# remove data outside of 1.5*IQR
# this doesn't look like there are outliers but we can try anyway

# set first and third quartiles
quartiles <- quantile(data_rm1$do_consumed, probs=c(.25, .75), na.rm = FALSE) 
# determine the interquartile range (IQR)
IQR <- IQR(data_rm1$do_consumed)

# find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# reassign data above lower and below upper to new dataframe called data_rm
data_rm <- data.frame(filter(data_rm1, do_consumed > Lower & do_consumed < Upper ))

# Have a look in the global environment, are there less observations? has anything changed?

# RETEST NORMALITY USING SHAPIRO WILK & HISTOGRAM


# are we happy with this? are your data normally distributed? did anything change?

# when inspecting our data frame, we can see some problematic values in all of our treatments.
# let's remove all values under 9 to get rid of these. 
# why might these values be an issue? make sure to include justification

data_rm <- data.frame(filter(data_rm, do_consumed > 9 )) # this will overwrite data_rm with a new data frame

# RETEST NORMALITY USING SHAPIRO WILK & HISTOGRAM (one last time...)



# when reporting this in our methods section, we need to include the outcomes of the test, and how many outliers were removed. 

# We can check what outliers were removed using:
removed_data_points <- which(!snaildata$do_consumed %in% data_rm$do_consumed)
snaildata[removed_data_points,]


# assumption 2: variances are homogenous ----------------------------------

# LEVENE'S TEST FOR HOMOGENEITY OF VARIANCE
# A p-value < 0.05 will suggest that variances are significantly 
# different and that this assumption has been violated


# if your Levene's test has errors, please see one of the demonstrators to fix it

# you must report the outcome of this in your methods section

# visually assess homogeneity of variance using Q-Q plots by group

qqPlot(data_rm$do_consumed)



# ANOVA -------------------------------------------------------------------

# the null hypothesis for ANOVA is that there is no difference in means.
# if the p-value for our ANOVA is <0.05, we reject the null, and find that there 
# is a significant difference between the means
# or that there is less than 5% chance that the relationship that we see is due to random chance

# you will need to edit this code to make y your response variable and x your predictor variable

snailanova <- aov(y ~ x, data = data_rm)     
summary(snailanova)

# summary statistics ------------------------------------------------------

# this will tell us the means and standard deviation of our data, plus the number of replicates (n) for each level
group_by(data_rm, temp) %>%
  summarise(count = n(),mean = mean(do_consumed, na.rm = TRUE),
            sd = sd(do_consumed, na.rm = TRUE))


# post-hoc analysis -------------------------------------------------------
# ONLY NEEDED IF ANOVA RESULT IS SIGNIFICANT

# the ANOVA tells us that there are differences in group means, but we don't know 
# which pairs of groups are different. We can use a Tukey multiple pairwise comparisons 
# test to see where these differences lie. 

# TUKEY MULTIPLE COMPARISONS PAIRWISE TEST

TukeyHSD(snailanova, which = "temp")

# we can visually represent out tukey's output using this plot: 

plot(TukeyHSD(snailanova, conf.level=.95), las = 2)


#  create means plot ------------------------------------------------------

# we are going to create our final plots using a different method this time. 

# first, we're going to create a dataframe including the means and standard deviation for our data
# we are calling this dataframe data_means_sd, and we are grouping our data by temperature levels.

data_means_sd <- data_rm %>%  
  group_by(temp) %>% 
  summarize(mean_do=mean(do_consumed), 
            sd_do=sd(do_consumed), 
            upper_limit=mean_do+sd_do, 
            lower_limit=mean_do-sd_do) 

# this is the output of that data frame:
data_means_sd

# now we will plot this using ggplot. ggplot works by adding each element as a layer to the plot, using the + symbol.
# you can see more about ggplot by using the help functions:
?ggplot
?geom_point
?geom_line
?geom_errorbar

#also, the cheat sheet might be useful for you https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf

# you can change the aesthetics of each element, check this out using
vignette("ggplot2-specs")

# I have added code to add your labels into the plot itself, using "geom_text". 
# You will need to edit the letters here to reflect your groupings.

ggplot(data_means_sd, aes(x=temp, y=mean_do)) + # creates the blank plot
  geom_point() + # adds the points
  geom_line(aes(y=mean_do), color = "black", group=1) + # adds the line
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit, width=0.25)) + # adds error bars
  xlab("temperature (degrees celsius)") + # adds y axis label
  ylab("dissolved oxygen consumption rate (mg/kg/hour)") + # adds x axis label
  scale_y_continuous(limits = c(0,150), breaks = seq(0,150, by = 10)) + # changes y axis scale
  geom_text(aes(label = c("a", "b", "c", "d", "e", "f"), fontface = "bold"), nudge_y = 50) + # adds labels
  theme_bw()  # I like this theme because it's simple. you can also use theme_classic(), 
              # theme_light(), theme_dark(), theme_grey(), etc


# you can save the last plot you made into your working directory using:

ggsave("do_cons_plot.png", width = 8, height = 5)

# in order to get a HD for this assignment, you will need to edit the aesthetics of this plot. 

