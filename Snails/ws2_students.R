
# effect of temperature on the oxygen consumption of freshwater snails
# Sofie Costin & *your name here*

# MAKE SURE TO SET YOUR WORKING DIRECTORY FIRST

# install.packages
library(ggpubr)
library(dplyr)
library(car)
library(plotly)

# clear environment 
rm(list = ls())

# import data -------------------------------------------------------------

data <- data.frame(read.csv("SnailData.csv"))
# read.csv will import the data from our working directory
head(data) 
# note that we now have all_data in our global environment, which is ##### observations 
# of #### variables
# the name of our species column is a little weird, so lets sort that out:
names(data)[1] <- 'group_name'
# and we can checkout our data types and variables
summary(data)

# we need to set our temperature variable as a character, because we want it to behave as 
# categorical data, not numeric.

data$temp <- as.character(data$temp) 

# We can see here that our minimum DO consumed is -8.886. This is not realistic,
# because a snail cannot produce oxygen

# let's remove data points below 0

data_rm1 <- data.frame(filter(data, do_consumed > 0 ))


# assumption testing ------------------------------------------------------

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

ggboxplot(data, x = "temp", y = "do_consumed")

# if data are not normally distributed, and we can see that there are outliers, 
# remove data outside of 1.5*IQR

# set first and third quartiles
quartiles <- quantile(data_rm1$do_consumed, probs=c(.25, .75), na.rm = FALSE) 
# determine the interquartile range (IQR)
IQR <- IQR(data_rm1$do_consumed)

# find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# reassign data above lower and below upper to new dataframe
data_rm <- data.frame(filter(data_rm1, do_consumed > Lower & do_consumed < Upper ))


# RETEST NORMALITY USING SHAPIRO WILK & HISTOGRAM





# are we happy with this? are your data normally distributed? did anything change?

# when inspecting our data frame, we can see some problematic values in both the 15 and 18 degree treatments.
# let's remove all values under 10 to get rid of these. 
# why might these values be an issue? make sure to include justification

data_rm <- data.frame(filter(data_rm, do_consumed > 10 )) # this will overwrite data_rm with a new data frame

# RETEST NORMALITY USING SHAPIRO WILK & HISTOGRAM (one last time...)





# when reporting this in our methods section, we need to include the outcomes of the test, 
# and how many outliers were removed. 

# assumption 2: variances are homogenous ----------------------------------

# LEVENE'S TEST FOR HOMOGENEITY OF VARIANCE
# A p-value < 0.05 will suggest that variances are significantly 
# different and that this assumption has been violated






# you must report the outcome of this in your methods section


# ANOVA -------------------------------------------------------------------

# the null hypothesis for ANOVA is that there is no difference in means.
# if the p-value for our ANOVA is >0.05, we reject the null, and find that there 
# is a significant difference between the means

# you will need to edit this code to make y your response variable and x your predictor variable

DOConsRate_aov <- aov(y ~ x, data = data_rm)
summary(DOConsRate_aov)

# summary statistics ------------------------------------------------------

# this will tell us the means and standard deviation of our data, plus the n for each level
group_by(data_rm, temp) %>%
  summarise(count = n(),mean = mean(do_consumed, na.rm = TRUE),
            sd = sd(do_consumed, na.rm = TRUE))


# post-hoc analysis -------------------------------------------------------

# the ANOVA tells us that there are differences in group means, but we don't know 
# which pairs of groups are different. We can use a Tukey multiple pairwise comparisons 
# test to see where these differences lie. 

# TUKEY MULTIPLE COMPARISONS PAIRWISE TEST

TukeyHSD(DOConsRate_aov, which = "temp")

# we can visually represent out tukey's output using this plot: 

plot(TukeyHSD(DOConsRate_aov, conf.level=.95), las = 2)


#  create means plot ------------------------------------------------------

# we are going to create our plots using a different method this time. 

# first, we're going to create a dataframe including the means and standard deviation for our data
# we are calling this datafeame data_means_sd, and we are grouping our data by temperature levels.

data_means_sd <- data_rm %>%  
  group_by(temp) %>% 
  summarize(mean_do=mean(do_consumed), 
            sd_do=sd(do_consumed), 
            upper_limit=mean_do+sd_do, 
            lower_limit=mean_do-sd_do 
  ) 

# this is the output of that data frame:
data_means_sd

# now we will plot this using ggplot. ggplot works by adding each element as a layer to the plot, 
# using the + symbol.
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
  xlab("temperature (degrees celcius)") + # adds y axis label
  ylab("dissolved oxygen consumption rate (mg/kg/hour)") + # adds x axis label
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + # changes y axis scale
  geom_text(aes(label = c("a", "b", "c", "d", "e", "f"), fontface = "bold"), nudge_y = 25) + # adds labels
  theme_bw()  # I like this theme because it's simple. you can also use theme_classic(), 
              # theme_light(), theme_dark(), theme_grey(), etc


# you can save the last plot you made into your working directory using:

ggsave("do_cons_plot.png", width = 8, height = 5)

# in order to get a HD for this assignment, you will need to edit the aesthetics of this plot. 
