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
library(RcmdrMisc)

# Let's import our data

all_data <- na.omit(data.frame(read.csv("Salinity_data.csv")))  

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

#And tie it all together into a nice data frame
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

#There are still some clearly horrible data in here. Let's take them out. 

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(fwShootRoot_noOutliers$fwShootRoot)

# much better
hist(fwShootRoot_noOutliers$fwShootRoot)

desired_salt_order <- c("1.5", "2.5", "5", "10", "15")
fwShootRoot_noOutliers$saltConc <- factor(fwShootRoot_noOutliers$saltConc, levels = desired_salt_order)
levels(fwShootRoot_noOutliers$saltConc)

boxplot(fwShootRoot ~ saltConc, data = fwShootRoot_noOutliers, 
        xlab = "saltConc",
        ylab = "fwShootRoot"
)


#This is still not great. Normally, we would go thorugh and remove the offending data in the dataset, but this will be too confusing. But again, some of the plants have been shooting atypically fast, so it's OK to take out any values that are very high, e.g. higher than 1.5:
fwShootRoot_noOutliers <- filter(alldata_df, fwShootRoot < 1.6 )

# Re-test for normality using our Shapiro-Wilk tests.
shapiro.test(fwShootRoot_noOutliers$fwShootRoot)

# much better
hist(fwShootRoot_noOutliers$fwShootRoot)

desired_salt_order <- c("1.5", "2.5", "5", "10", "15")
fwShootRoot_noOutliers$saltConc <- factor(fwShootRoot_noOutliers$saltConc, levels = desired_salt_order)
levels(fwShootRoot_noOutliers$saltConc)

boxplot(fwShootRoot ~ saltConc, data = fwShootRoot_noOutliers, 
        xlab = "saltConc",
        ylab = "fwShootRoot"
)


table(fwShootRoot_noOutliers$species, fwShootRoot_noOutliers$saltConc)

# as good as it gets. Let's proceed.

### Assumption 2: variances are homogeneous

library(car) # access the car package

leveneTest(fwShootRoot ~ species*saltConc, data = fwShootRoot_noOutliers)

### two-way ANOVA

fwShootRoot.aov <- aov(fwShootRoot ~ saltConc, data = fwShootRoot_noOutliers)
summary(fwShootRoot.aov)
#have a good look at this. What is this telling us - what follow-on tests are appropriate, which are not?

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

plotMeans(fwShootRoot_noOutliers$fwShootRoot,
          fwShootRoot_noOutliers$saltConc,
          fwShootRoot_noOutliers$species,
          error.bars = "se",
          xlab = ("Salt Concentration (g/L)"),
          ylab = ("fresh weight shoot to root ratio (%)"),
          main=(""),
          legend.pos = "bottomleft",
          legend.lab = "Species"
)

# once we're happy with our plot, we click "export" and save it in our working folder. 


# Dry weight root to shoot ratio ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$dwShootRoot)

hist(alldata_df$dwShootRoot)

desired_salt_order <- c("1.5", "2.5", "5", "10", "15")
alldata_df$saltConc <- factor(alldata_df$saltConc, levels = desired_salt_order)
levels(alldata_df$saltConc)

boxplot(dwShootRoot ~ saltConc, data = alldata_df, 
        xlab = "saltConc",
        ylab = "dwShootRoot"
)

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


boxplot(dwShootRoot ~ saltConc, data = dwShootRoot_noOutliers, 
        xlab = "saltConc",
        ylab = "dwShootRoot"
)

#again, we have some interesting differences possibly due to anomalous plants, but we will assume that the ANOVA is robust to thes few outliers.

table(dwShootRoot_noOutliers$species, dwShootRoot_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

leveneTest(dwShootRoot ~ species*saltConc, data = dwShootRoot_noOutliers)

### two-way ANOVA

dwShootRoot.aov <- aov(dwShootRoot ~ saltConc, data = dwShootRoot_noOutliers)
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

plotMeans(dwShootRoot_noOutliers$dwShootRoot,
          dwShootRoot_noOutliers$saltConc,
          dwShootRoot_noOutliers$species,
          error.bars = "se",
          xlab = ("Salt Concentration (g/L)"),
          ylab = ("Dry wieght Shoot Root ratio (%)"),
          main=(""),
          legend.pos = "bottomleft",
          legend.lab = "Species"
)

# once we're happy with our plot, we click "export" and save it in our working folder.


# Shoot Moisture ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$shootMoist)

hist(alldata_df$shootMoist)

boxplot(shootMoist ~ saltConc, data = alldata_df, 
        xlab = "saltConc",
        ylab = "shootMoist"
)

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

boxplot(shootMoist ~ saltConc, data = shootMoist_noOutliers, 
        xlab = "saltConc",
        ylab = "shootMoist"
)

table(shootMoist_noOutliers$species, shootMoist_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

library(car) # access the car package

#What does the result mean? We will continue here and assume the ANOVA is robust, but you will be able to see why this test is significant when you look at the boxplot above.
leveneTest(shootMoist ~ species*saltConc, data = shootMoist_noOutliers)


### two-way ANOVA

shootMoist.aov <- aov(shootMoist ~ saltConc, data = shootMoist_noOutliers)
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

plotMeans(shootMoist_noOutliers$shootMoist,
          shootMoist_noOutliers$saltConc,
          shootMoist_noOutliers$species,
          error.bars = "se",
          xlab = ("Salt Concentration (g/L)"),
          ylab = ("shoot Moisture (%)"),
          main=(""),
          legend.pos = "bottomleft",
          legend.lab = "Species"
)

# once we're happy with our plot, we click "export" and save it in our working folder.

# Root Moisture ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$rootMoist)

hist(alldata_df$rootMoist)

boxplot(rootMoist ~ saltConc, data = alldata_df, 
        xlab = "saltConc",
        ylab = "rootMoist"
)

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

boxplot(rootMoist ~ saltConc, data = rootMoist_noOutliers, 
        xlab = "saltConc",
        ylab = "rootMoist"
)

table(rootMoist_noOutliers$species, rootMoist_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

leveneTest(rootMoist ~ species * saltConc, data = rootMoist_noOutliers)

#Both Shapiro or Levene test are significant - so again, this needs flagging in your report. Can you think of why the variance in the lower concentrations is so much higher than in the upper ones?

### two-way ANOVA

rootMoist.aov <- aov(rootMoist ~ saltConc, data = rootMoist_noOutliers)
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

plotMeans(rootMoist_noOutliers$rootMoist,
          rootMoist_noOutliers$saltConc,
          rootMoist_noOutliers$species,
          error.bars = "se",
          xlab = ("Salt Concentration (g/L)"),
          ylab = ("dry weight shoot to root ratio (%)"),
          main=(""),
          legend.pos = "bottomleft",
          legend.lab = "Species"
)

# once we're happy with our plot, we click "export" and save it in our working folder.

# Total Moisture ----------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$totMoist)

hist(alldata_df$totMoist)

boxplot(totMoist ~ saltConc, data = alldata_df, 
        xlab = "saltConc",
        ylab = "totMoist"
)

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

boxplot(totMoist ~ saltConc, data = totMoist_noOutliers, 
        xlab = "saltConc",
        ylab = "totMoist"
)

table(totMoist_noOutliers$species, totMoist_noOutliers$saltConc)


### Assumption 2: variances are homogeneous

leveneTest(totMoist ~ species*saltConc, data = totMoist_noOutliers)


### two-way ANOVA

totMoist.aov <- aov(totMoist ~ saltConc, data = totMoist_noOutliers)
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

plotMeans(totMoist_noOutliers$totMoist,
          totMoist_noOutliers$saltConc,
          totMoist_noOutliers$species,
          error.bars = "se",
          xlab = ("Salt Concentration (g/L)"),
          ylab = ("Total moisture"),
          main=(""),
          legend.pos = "bottomleft",
          legend.lab = "Species"
)

# once we're happy with our plot, we click "export" and save it in our working folder.

### well done!
