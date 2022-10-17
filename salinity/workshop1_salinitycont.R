# ******************************************************************************

###                     Rex and Sof's Salinity Workshop                      ###

# ******************************************************************************

# ******************************************************************************

# Salinity Data Analysis continued ----------------

# ******************************************************************************

### PLEASE NOTE THAT USING THIS CODE WILL VOID YOUR CHANCE AT GETTING AN HD FOR 
### THIS PART OF THE ASSIGNMENT

# first, we're going to clear the data from our global environment, so we have a 
# fresh start

rm(list = ls())

# and load our libraries
library(ggpubr)
library(dplyr)
library(car)

# Let's import our data

all_data <- na.omit(data.frame(read.csv("2022_data.csv")))  
# read.csv will import the data from our working directory
head(all_data) 
# note that we now have all_data in our global environment, which is 108 observations 
# of 20 variables
# the name of our species column is a little weird, so lets sort that out:
names(all_data)[1] <- 'species'
# and we can check that everything looks as we expect it to:
str(all_data)


# Let's assign all of the variables we're going to use to their own object. 
species <- all_data$species
saltConc <- as.character(all_data$salt_conc) 
fwShootRoot <- all_data$fw_shoot_root_ratio
dwShootRoot <- all_data$dw_shoot_root_ratio
shootMoist <- all_data$shoot_moisture
rootMoist <- all_data$root_moisture
totMoist <- all_data$total_moisture

alldata_df <- cbind.data.frame(species, saltConc, fwShootRoot, dwShootRoot, 
                               shootMoist, rootMoist, totMoist)

# check all is as we expect:
str(alldata_df)

# Fresh weight root to shoot ratio ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$fwShootRoot)

hist(alldata_df$fwShootRoot, breaks = 100)

# woah, something whacky is happening here. Let's open our data and have a look
# what can you see is wrong? we need to remove some outliers

# first, we work out what our first and third quartiles are for our data:
quartiles <- quantile(alldata_df$fwShootRoot, probs=c(.25, .75), na.rm = FALSE) 
# and determine the interquartile range (IQR)
IQR <- IQR(alldata_df$fwShootRoot)

# Then we find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# and reassign any data above the lower threshold and below the upper threshold to a new dataframe, 
# which we will use for the analysis.
fwShootRoot_noOutliers <- filter(alldata_df, fwShootRoot > Lower & fwShootRoot < Upper )

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(fwShootRoot_noOutliers$fwShootRoot)

# much better
hist(fwShootRoot_noOutliers$fwShootRoot)

ggboxplot(fwShootRoot_noOutliers, x = "saltConc", y = "fwShootRoot", color = "species")

table(fwShootRoot_noOutliers$species, fwShootRoot_noOutliers$saltConc)

# looks pretty good. Let's proceed.

### Assumption 2: variances are homogeneous

library(car) # access the car package

leveneTest(fwShootRoot ~ species*saltConc, data = fwShootRoot_noOutliers)

### two-way ANOVA

fwShootRoot.aov <- aov(fwShootRoot ~ species * saltConc, data = fwShootRoot_noOutliers)
summary(fwShootRoot.aov)

# we can generate some summary statistics for our response data, depending on our two factors:
require("dplyr")
group_by(fwShootRoot_noOutliers, species, saltConc) %>%
  summarise(count = n(),mean = mean(fwShootRoot, na.rm = TRUE),
            sd = sd(fwShootRoot, na.rm = TRUE))

# post-hoc test

TukeyHSD(fwShootRoot.aov, which = "saltConc")
# when interpreting the output, diff: difference between means of the two groups, 
# lwr & upr: the lower and the upper end point of the confidence interval at 95%, 
# p adj: p-value after adjustment for the multiple comparisons.


### let's graph it!

ggline(fwShootRoot_noOutliers, x = "saltConc", y = "fwShootRoot", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "fresh weight shoot to root ratio (%)",
       legend = "right")

# once we're happy with our plot, we click "export" and save it in our working folder.


# Dry weight root to shoot ratio ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$dwShootRoot)

hist(alldata_df$dwShootRoot)

ggboxplot(alldata_df, x = "saltConc", y = "dwShootRoot", color = "species")

# remove outliers

# first, we work out what our first and third quartiles are for our data:
quartiles <- quantile(alldata_df$dwShootRoot, probs=c(.25, .75), na.rm = FALSE) 
# and determine the interquartile range (IQR)
IQR <- IQR(alldata_df$dwShootRoot)

# Then we find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# and reassign any data above the lower threshold and below the upper threshold to a new dataframe, 
# which we will use for the analysis.
dwShootRoot_noOutliers <- filter(alldata_df, dwShootRoot > Lower & dwShootRoot < Upper )

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(dwShootRoot_noOutliers$dwShootRoot)

# much better
hist(dwShootRoot_noOutliers$dwShootRoot)

ggboxplot(dwShootRoot_noOutliers, x = "saltConc", y = "dwShootRoot", color = "species")

table(dwShootRoot_noOutliers$species, dwShootRoot_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

leveneTest(dwShootRoot ~ species*saltConc, data = dwShootRoot_noOutliers)

### two-way ANOVA

dwShootRoot.aov <- aov(dwShootRoot ~ species * saltConc, data = dwShootRoot_noOutliers)
summary(dwShootRoot.aov)

# we can generate some summary statistics for our response data, depending on our two factors:

group_by(dwShootRoot_noOutliers, species, saltConc) %>%
  summarise(count = n(),mean = mean(dwShootRoot, na.rm = TRUE),
            sd = sd(dwShootRoot, na.rm = TRUE))

# post-hoc test

TukeyHSD(dwShootRoot.aov, which = "saltConc")
# when interpreting the output, diff: difference between means of the two groups, 
# lwr & upr: the lower and the upper end point of the confidence interval at 95%, 
# p adj: p-value after adjustment for the multiple comparisons.


### let's graph it!

ggline(dwShootRoot_noOutliers, x = "saltConc", y = "dwShootRoot", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "dry weight shoot to root ratio (%)",
       legend = "right")

# once we're happy with our plot, we click "export" and save it in our working folder.


# Shoot Moisture ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$shootMoist)

hist(alldata_df$shootMoist)

ggboxplot(alldata_df, x = "saltConc", y = "shootMoist", color = "species")

# remove outliers

# first, we work out what our first and third quartiles are for our data:
quartiles <- quantile(alldata_df$shootMoist, probs=c(.25, .75), na.rm = FALSE) 
# and determine the interquartile range (IQR)
IQR <- IQR(alldata_df$shootMoist)

# Then we find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# and reassign any data above the lower threshold and below the upper threshold to a new dataframe, 
# which we will use for the analysis.
shootMoist_noOutliers <- filter(alldata_df, shootMoist > Lower & shootMoist < Upper )

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(shootMoist_noOutliers$shootMoist)

# much better
hist(shootMoist_noOutliers$shootMoist)

ggboxplot(shootMoist_noOutliers, x = "saltConc", y = "shootMoist", color = "species")

table(shootMoist_noOutliers$species, shootMoist_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

library(car) # access the car package

leveneTest(shootMoist ~ species*saltConc, data = shootMoist_noOutliers)


### two-way ANOVA

shootMoist.aov <- aov(shootMoist ~ species * saltConc, data = shootMoist_noOutliers)
summary(shootMoist.aov)

# summary statistics
group_by(shootMoist_noOutliers, species, saltConc) %>%
  summarise(count = n(),mean = mean(shootMoist, na.rm = TRUE),
            sd = sd(shootMoist, na.rm = TRUE))

# post-hoc test

TukeyHSD(shootMoist.aov, which = "saltConc")
# when interpreting the output, diff: difference between means of the two groups, 
# lwr & upr: the lower and the upper end point of the confidence interval at 95%, 
# p adj: p-value after adjustment for the multiple comparisons.


### let's graph it!

ggline(shootMoist_noOutliers, x = "saltConc", y = "shootMoist", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "dry weight shoot to root ratio (%)",
       legend = "right")

# once we're happy with our plot, we click "export" and save it in our working folder.

# Root Moisture ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$rootMoist)

hist(alldata_df$rootMoist)

ggboxplot(alldata_df, x = "saltConc", y = "rootMoist", color = "species")

# remove outliers

# first, we work out what our first and third quartiles are for our data:
quartiles <- quantile(alldata_df$rootMoist, probs=c(.25, .75), na.rm = FALSE) 
# and determine the interquartile range (IQR)
IQR <- IQR(alldata_df$rootMoist)

# Then we find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# and reassign any data above the lower threshold and below the upper threshold to a new dataframe, 
# which we will use for the analysis.
rootMoist_noOutliers <- filter(alldata_df, rootMoist > Lower & rootMoist < Upper )

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(rootMoist_noOutliers$rootMoist)

# much better
hist(rootMoist_noOutliers$rootMoist)

ggboxplot(rootMoist_noOutliers, x = "saltConc", y = "rootMoist", color = "species")

table(rootMoist_noOutliers$species, rootMoist_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

leveneTest(rootMoist ~ species*saltConc, data = rootMoist_noOutliers)


### two-way ANOVA

rootMoist.aov <- aov(rootMoist ~ species * saltConc, data = rootMoist_noOutliers)
summary(rootMoist.aov)

# we can generate some summary statistics for our response data, depending on our two factors:

group_by(rootMoist_noOutliers, species, saltConc) %>%
  summarise(count = n(),mean = mean(rootMoist, na.rm = TRUE),
            sd = sd(rootMoist, na.rm = TRUE))

# post-hoc test

TukeyHSD(rootMoist.aov, which = "saltConc")
# when interpreting the output, diff: difference between means of the two groups, 
# lwr & upr: the lower and the upper end point of the confidence interval at 95%, 
# p adj: p-value after adjustment for the multiple comparisons.


### let's graph it!

ggline(rootMoist_noOutliers, x = "saltConc", y = "rootMoist", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "dry weight shoot to root ratio (%)",
       legend = "right")

# once we're happy with our plot, we click "export" and save it in our working folder.

# Total Moisture ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$totMoist)

hist(alldata_df$totMoist)

ggboxplot(alldata_df, x = "saltConc", y = "totMoist", color = "species")

# remove outliers

# first, we work out what our first and third quartiles are for our data:
quartiles <- quantile(alldata_df$totMoist, probs=c(.25, .75), na.rm = FALSE) 
# and determine the interquartile range (IQR)
IQR <- IQR(alldata_df$totMoist)

# Then we find our lower and upper limits:
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

# and reassign any data above the lower threshold and below the upper threshold to a new dataframe, 
# which we will use for the analysis.
totMoist_noOutliers <- filter(alldata_df, totMoist > Lower & totMoist < Upper )

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(totMoist_noOutliers$totMoist)

# much better
hist(totMoist_noOutliers$totMoist)

ggboxplot(totMoist_noOutliers, x = "saltConc", y = "totMoist", color = "species")

table(totMoist_noOutliers$species, totMoist_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

leveneTest(totMoist ~ species*saltConc, data = totMoist_noOutliers)


### two-way ANOVA

totMoist.aov <- aov(totMoist ~ species * saltConc, data = totMoist_noOutliers)
summary(totMoist.aov)

# we can generate some summary statistics for our response data, depending on our two factors:
require("dplyr")
group_by(totMoist_noOutliers, species, saltConc) %>%
  summarise(count = n(),mean = mean(totMoist, na.rm = TRUE),
            sd = sd(totMoist, na.rm = TRUE))

# post-hoc test

TukeyHSD(totMoist.aov, which = "saltConc")
# when interpreting the output, diff: difference between means of the two groups, 
# lwr & upr: the lower and the upper end point of the confidence interval at 95%, 
# p adj: p-value after adjustment for the multiple comparisons.


### let's graph it!

ggline(totMoist_noOutliers, x = "saltConc", y = "totMoist", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "dry weight shoot to root ratio (%)",
       legend = "right")

# once we're happy with our plot, we click "export" and save it in our working folder.

### well done!
