# ******************************************************************************

###                     Rex and Sof's Salinity Workshop                      ###

# ******************************************************************************

# ******************************************************************************

# Salinity Data Analysis ----------------

# ******************************************************************************

# first, we're going to clear the data from our global environment, so we have a 
# fresh start

rm(list = ls())


# Let's import our data. If working on a Flinders Uni computer, put it on your U drive or you WILL lose all your work!

# the folder we are working in is called our "working directory". It is good 
# convention to keep everything you're working on in this folder. You can check 
# you're using the correct working directory by using 
getwd()

# if the working directory isn't the folder you want to use, you can use 
# setwd("xxxx enter filepath here xxx") # to set your working directory. 
# your data should be saved in your working directory folder. You can then easily
# access this data:

all_data <- na.omit(data.frame(read.csv("Salinity_data.csv")))  
# read.csv will import the data from our working directory
head(all_data) 
# note that we now have all_data in our global environment, which is ##### observations 
# of #### variables

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
# salt concentration would automatically be numerical, we want it to be character, because we're 
# using it as a categorical variable.
leafNoRatio <- all_data$leaf_no_ratio_to_control_mean
shootHtRatio <- all_data$shoot_height_ratio_to_control_mean
rootLnRatio <- all_data$root_length_ratio_to_control_mean
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

# and we can check how many replicates we have for each treatment
table(asratio_df$species, asratio_df$saltConc)

# and we'll just make a dataframe of the rest of the variables, because it makes 
# plotting easier down the track:
alldata_df <- cbind.data.frame(species, saltConc, fwShootRoot, dwShootRoot, 
                               shootMoist, rootMoist, totMoist)

### remember, we have TWO DATA FRAMES  - one with the ratios, one with the remainder of the variables - which means that you don't find fwShootRoot in asratio_df, for example!

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
#Is this normal? 

# we can visually assess distribution using histograms 
hist(asratio_df$leafNoRatio)


# if we compare the results from our normality tests with our histograms, we can 
# see whether our data are normally distributed or how badly the data are not normal, in this case.

# Sometimes, to bring our data closer to normality, we can remove some outliers. In this year's experiments, it's particularly relevant because some plants grew so quick that they developed far more leaves than would be expected. You can see this in the histogram already.

# We need to explore this visually using boxplots, for both our species and our salt concentration. 

# we're going to use the package 'ggpubr' to make some plots to visualise our group differences. This first needs
#installing like so:

install.packages("ggpubr")
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


# let's check how many replicates we have for each, now

table(leafNoRatio_noOutliers$species, leafNoRatio_noOutliers$saltConc)

# After relevant outliers are removed, we can then re-test for normality using our Shapiro-Wilk tests.
shapiro.test(leafNoRatio_noOutliers$leafNoRatio)

# we still have a significant p-value, but let's check out our histogram and plot data points
hist(leafNoRatio_noOutliers$leafNoRatio)

ggboxplot(leafNoRatio_noOutliers, x = "saltConc", y = "leafNoRatio", color = "species")

# looks pa bit better but the histogram shows us that there are clearly some huge outliers above 1.4 that are probably still plants that underwent a growth spurt that made their leaf growth totally different to the othe plants. These are not comparable to all the other plants, which is why we here decide to remove them. 

#NB: we don't remove the outliers because they are inconvenient - we have a good biological reason to exclude them. It is extremely important to be transparent and have a good reason for outlier removal, otherwise you are just massaging your data! So make sure you include the reason for the removal in your write-up.

leafNoRatio_noOutliers <- filter(asratio_df, leafNoRatio < 1.4 )

# Re-test for normality using our Shapiro-Wilk tests. It's still not great
shapiro.test(leafNoRatio_noOutliers$leafNoRatio)

# But our histogram now shows that the values are all more or less part of the same distribution. Because ANOVAs are relatively robust to violations of non-normality, we will choose to continue here but just make sure you mention that the normality criterion was violated.
hist(leafNoRatio_noOutliers$leafNoRatio)

ggboxplot(leafNoRatio_noOutliers, x = "saltConc", y = "leafNoRatio", color = "species")


### Assumption 2: variances are homogeneous

# the Levene's test for Homogeneity of Variance (in the 'car' package) will tell us if our variances are homogeneous - 
# that is, that all population variances are equal. A p - value < 0.05 will suggest that variances are significantly 
# different and that this assumption has been violated
# install.packages("car")
library(car) # access the car package

#run the test. Again, the test is highly significant but there is not much we can do about this. We will report these data as having violated both assumptions and keep going with that precaution.
leveneTest(leafNoRatio ~ species*saltConc, data = leafNoRatio_noOutliers)

### two-way ANOVA

# now that we have the ingredients (kind of), let's bake our lovely ANOVA cake!
# the null hypotheses for a two-way ANOVA are:
#       > there is no _difference_ in the means of factor A (barley and what don't have different mean leaf numbers)
#       > there is no _difference_ in the means of factor B (plants don't have different mean leav numbers across salt concentrations)
#       > there is no _interaction_ between factor A and B. (i.e. the difference in leaf numbers in a salt concentration does not depend on whether we are looking at barley or wheat)
# if the p-value for our ANOVA is <0.05, we reject the null, and find that there 
# is a significant difference or interaction.

leafno.aov <- aov(leafNoRatio ~ species * saltConc, data = leafNoRatio_noOutliers)
summary(leafno.aov)

 # But what does this mean? To get a feeling for this, we can generate some summary statistics for our response data, depending on our two factors. This has nothing to do with the ANOVA but it allows you to look at the magnitudes of difference between means.
require("dplyr")
group_by(leafNoRatio_noOutliers, species, saltConc) %>%
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


ggline(leafNoRatio_noOutliers, x = "saltConc", y = "leafNoRatio", color = "species",
       add = c("mean_se"),
       xlab = "salt concentration (g/L)",
       ylab = "number of leaves (ratio to control mean)",
       legend = "right")

# once we're happy with our plot, we click "export" and save it in our working folder.


# Congratulations! You've now gone through ALL code you will need to use for this prac!
# The next analyses use exactly the same functions as for the leafNoRatio variable. The
#code will now have fewer and fewer comments as you get more used to repeating the same
#analyses.

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
# see whether our data are normally distributed. In this case, the histogram looks pretty normal even though the normality test doesn't. 

# So it is worth testing for outliers again for both our species and our salt concentration. 

# plot root length as a ratio to control mean by groups "salt concentration"
# colour box plot by second group "species"

ggboxplot(asratio_df, x = "saltConc", y = "rootLnRatio", color = "species")

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

# And double check using a histogram - this is now looking OK and we'll again note that we expect the ANOVA to be robust to the violation of normality because the data are at least clearly part of a coherent distribution.
hist(rootLnRatio_noOutliers$rootLnRatio)

# and have another look at our new boxplots:
ggboxplot(rootLnRatio_noOutliers, x = "saltConc", y = "rootLnRatio", color = "species")

#We still clearly have some very high and very low outliers. But it would be a bit intransparent to just take them out, since there is no good reason to do so. We will therefore leave them here and continut.

table(rootLnRatio_noOutliers$species, rootLnRatio_noOutliers$saltConc)

### Assumption 2: variances are homogeneous

# Let's do our levene's test

leveneTest(rootLnRatio ~ species*saltConc, data =rootLnRatio_noOutliers)

# The variance is homogenous - phew - we're good to go!

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

#Here you can also see that the outliers are not looking like they are obscuring a very clear picture. So, all good.

# don't forget to export your plot

# Shoot height as a Ratio of Control Mean ----------------------------------

# Let's check our assumptions:

### Assumption 1: data are normally distributed
# let's do a shapiro-wilk test of normality on our shoot height ratio variable
shapiro.test(asratio_df$shootHtRatio)

#What does this result mean?

# we can visually assess distribution using histograms 
hist(asratio_df$shootHtRatio)

# Let's look a little closer using boxplots:

ggboxplot(asratio_df, x = "saltConc", y = "shootHtRatio", color = "species")

# Clearly some outliers, let's see if any of them are outside the IQR:

quartiles <- quantile(asratio_df$shootHtRatio, probs=c(.25, .75), na.rm = FALSE) 
IQR <- IQR(asratio_df$shootHtRatio)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

shootHtRatio_noOutliers <- filter(asratio_df, shootHtRatio > Lower & shootHtRatio < Upper )

# now we check again:
shapiro.test(shootHtRatio_noOutliers$shootHtRatio)
hist(shootHtRatio_noOutliers$shootHtRatio)
ggboxplot(shootHtRatio_noOutliers, x = "saltConc", y = "shootHtRatio", color = "species")

#What do you notice here? Make sure you put this into your write-up!
table(shootHtRatio_noOutliers$species, shootHtRatio_noOutliers$saltConc)

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

### from here, you can either use your newfound knowledge to analyse the remaining five
### variables (this will put you in the running for an HD) or open and run the file 
### "workshop1_salinitycont.R" to work through our code.

###DO NOT CREATE A NEW FILE OR READ IN NEW DATA. JUST KEEP WORKING FROM THE BOTTOM OF THIS CODE ONWARDS. 
###REMEMBER THAT THE RATIOS AND THE REMAINING VARIABLES ARE IN DIFFERENT DATA FRAMES. IF YOU GET ERRORS, IT MIGHT BE THAT YOU ARE USING THE WRONG DATA FRAME.

### good luck!
