# ******************************************************************************
###                     Sof, Maddi and Rex' Salinity Workshop               ###
# ******************************************************************************

# ******************************************************************************
# Salinity Data Analysis -------------------------------------------------------
# ******************************************************************************

# First, we're going to clear the data from the global environment so we have a
# fresh start.
rm(list = ls())


# Data import and preparation --------------------------------------------------

# Let's import our data. If you're working on a Flinders Uni computer,
# put it on your U: drive or you WILL lose your work.

# The folder we are working in is called the "working directory." It's good
# practice to keep everything you're working on in this folder. You can check
# that you're using the correct working directory with:
getwd()

# If the working directory isn't the folder you want to use, set it with:
# setwd("<enter filepath here>")
# Your data should be saved in your working directory folder. You can then easily
# access this data.

# read.csv() imports the data from the working directory; na.omit() removes rows
# with missing values (helps avoid errors later in tests/plots).
all_data <- na.omit(read.csv("Salinity_data.csv"))

# Quick look at the data
head(all_data)

# Check that everything looks right — for example, that variables have the
# correct data types.
str(all_data)
summary(all_data)


# We are going to analyse the response variables: shoot.height, root.length,
# fw.root.shoot.ratio, dw.root.shoot.ratio, shoot.moisture, root.moisture,
# total.moisture, and leaf.no.


# Before we get into it, let's make sure our data are formatted so we can work
# with them easily. This can be tedious, but it saves time in the long run.

# Assign the variables we're going to use to their own objects.

species     <- all_data$species
saltconc    <- as.character(all_data$salt.conc)  
# Salt concentration would automatically be numeric; we want it as character,
# because we'll use it as a categorical variable. We'll set a factor order below.
shootht     <- all_data$shoot.height
rootln      <- all_data$root.length
leafno      <- all_data$leaf.no
fwrootshoot <- all_data$fw.root.shoot.ratio
dwrootshoot <- all_data$dw.root.shoot.ratio
shootmoist  <- all_data$shoot.moisture
rootmoist   <- all_data$root.moisture
totmoist    <- all_data$total.moisture

# Build a tidy data frame for plotting and analysis.
alldata_df <- cbind.data.frame(
  species, saltconc, shootht, rootln, leafno,
  fwrootshoot, dwrootshoot, shootmoist, rootmoist, totmoist
)

# Define the desired order for salt concentrations and convert to a factor.
desired_salt_order <- c("0", "1.5", "2.5", "5", "10", "15")
alldata_df$saltconc <- factor(alldata_df$saltconc, levels = desired_salt_order)
levels(alldata_df$saltconc)


# Next, we need to perform some assumption testing. This is like checking the
# pantry to see which ingredients you have. If you have eggs, flour, milk, and
# sugar, you can make a cake. If you have eggs, milk, bacon, and cheese, you can
# make an omelette. We want to do a one‑way ANOVA, but we need to check that we
# have all the ingredients first.
# The assumptions (ingredients) of an ANOVA are:
#   > Normally distributed data
#   > Homogeneity of variances
#   > Independence (our data are independent because the plants were grown in
#     separate pots and do not influence each other).



# Shoot height -----------------------------------------------------------------

# We'll use dplyr for tidy summaries.
# install.packages("dplyr")  # run once if needed
library(dplyr)

# First, we're going to analyse the plants' shoot height in response to salinity.
# Let's see how many replicates we have for each treatment.

table(alldata_df$saltconc)


# Assumption 1: data are approximately normal
# The central limit theorem tells us that when n > 30, the sampling distribution
# of the mean is approximately normal, regardless of the population distribution.
# It's still good practice to report assumption checks.
# We'll use the Shapiro–Wilk test. The null hypothesis is that the sample comes
# from a normal distribution. If p > 0.05, we treat the data as approximately
# normal.
shapiro.test(alldata_df$shootht)

# Visual check of distribution
hist(alldata_df$shootht, main = "Histogram: Shoot height", xlab = "Shoot height (cm)")

# Compare the statistical test with the histogram. Do they agree?

# Quick check for extreme outliers with a boxplot.
# We'll use the RcmdrMisc package for a convenient means plot later as well.
# install.packages("RcmdrMisc")  # run once if needed
library(RcmdrMisc)

boxplot(shootht ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "Shoot height (cm)")



# In this case, we don’t see extreme outliers. Let's proceed to the next
# assumption.

# Assumption 2: homogeneity of variances
# Levene’s test (in the car package) tests whether group variances are equal.
# A p‑value < 0.05 suggests unequal variances (assumption violated).


leveneTest(shootht ~ saltconc, data = alldata_df)

# One‑way ANOVA
# The null hypothesis is that mean shoot height is equal across salt
# concentrations. If the ANOVA p‑value < 0.05, we reject the null and conclude
# that at least one group mean differs.
shootht.aov <- aov(shootht ~ saltconc, data = alldata_df)
summary(shootht.aov)

# Helpful group summaries (not part of the ANOVA)
alldata_df %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(shootht, na.rm = TRUE),
            sd    = sd(shootht,   na.rm = TRUE),
            .groups = "drop")

# Post hoc comparisons: Tukey’s HSD tests which pairs of groups differ.
shootht.tukey <- TukeyHSD(shootht.aov, which = "saltconc")
shootht.tukey

# Interpreting output:
#   diff  = difference in means between the two groups
#   lwr,upr = 95% confidence interval
#   p adj = p‑value adjusted for multiple comparisons

# Let’s graph it.
# We’ll plot mean ± SE by salt concentration with plotMeans().
plotMeans(alldata_df$shootht,
          alldata_df$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "Shoot height (cm)",
          main = "")


# Once you're happy with the plot, click "Export" and save it in your working
# folder.

# Great job — you've now covered most of the code you’ll need for this prac!
# The next analyses reuse the same functions you used for shootht. The code below
# gradually reduces comments as you repeat the workflow.



# Root length ------------------------------------------------------------------

# Assumption 1: data are approximately normal
# Shapiro–Wilk test
shapiro.test(alldata_df$rootln)
# If p > 0.05, treat as approximately normal; if p < 0.05, it deviates from
# normality.

# Visual check
hist(alldata_df$rootln, main = "Histogram: Root length", xlab = "Root length (cm)")

# Check for outliers
boxplot(rootln ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "Root length (cm)")

# Assumption 2: homogeneity of variances
leveneTest(rootln ~ saltconc, data = alldata_df)

# One‑way ANOVA
rootln.aov <- aov(rootln ~ saltconc, data = alldata_df)
summary(rootln.aov)

# Group summaries
alldata_df %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(rootln, na.rm = TRUE),
            sd    = sd(rootln,   na.rm = TRUE),
            .groups = "drop")

# Tukey’s HSD
rootln.tukey <- TukeyHSD(rootln.aov, which = "saltconc")
rootln.tukey

# Plot mean ± SE and add letters
plotMeans(alldata_df$rootln, alldata_df$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "Root length (cm)",
          main = "")


# Don’t forget to export your plot.



# Leaf number ------------------------------------------------------------------

# Assumption 1: data are approximately normal
# Shapiro–Wilk test
shapiro.test(alldata_df$leafno)
# What does this result mean for normality?

# Visual check
hist(alldata_df$leafno, main = "Histogram: Leaf number", xlab = "Leaf number")

# The data are counts. There is little variation in leaf number for 0–5 g/L salt
# concentration, so the Shapiro–Wilk test may indicate non‑normality. ANOVA is
# reasonably robust to deviations from normality, but note this in your write‑up.

# Boxplot by treatment
boxplot(leafno ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "Number of leaves")

# Assumption 2: homogeneity of variances
leveneTest(leafno ~ saltconc, data = alldata_df)

# One‑way ANOVA
leafno.aov <- aov(leafno ~ saltconc, data = alldata_df)
summary(leafno.aov)

# Group summaries
alldata_df %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(leafno, na.rm = TRUE),
            sd    = sd(leafno,   na.rm = TRUE),
            .groups = "drop")

# Tukey’s HSD
leafno.tukey <- TukeyHSD(leafno.aov, which = "saltconc")
leafno.tukey

# Plot mean ± SE and add letters
plotMeans(alldata_df$leafno, alldata_df$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "Number of leaves",
          main = "")
# Don’t forget to export your plot.



# Fresh Weight root:shoot ratio - the case with outliers ------------------------------------------------------------

# In the previous examples we didn't need to remove outliers, but you might need
# to for some variables. A good example is the FW root:shoot ratio, which we'll do next. 

# Test normality - this is highly significant!
shapiro.test(alldata_df$fwrootshoot)

#The distribution to the left looks OK, but there are some real outlying values.
hist(alldata_df$fwrootshoot,
     main = "Histogram: FW root:shoot (no outliers)",
     xlab = "FW root:shoot ratio")


# Another visual check to see where the outliers are in the groups
boxplot(alldata_df$fwrootshoot ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "FW root:shoot ratio")

# It looks like something is happening in the 15 g/L treatment, where the roots are ~5.5* longer than the shoots. #Maybe someone didn't put the decimals right? Regardless, this is probably a situation where a mistake has occurred and we have to remove some of the extreme outliers.

# Check replicates per treatment - remember the full number of replicates
table(alldata_df$saltconc)

# Define outliers as points outside 1.5 × IQR.
# If there are extreme outliers, we will remove them. Be sure to justify any
# removals in your methods section.
quartiles <- quantile(alldata_df$fwrootshoot, probs = c(.25, .75), na.rm = TRUE)
iqr_val   <- IQR(alldata_df$fwrootshoot, na.rm = TRUE)
Lower     <- quartiles[1] - 1.5 * iqr_val
Upper     <- quartiles[2] + 1.5 * iqr_val

# Keep values within the limits in a new data frame for analysis.
fwrootshoot_nooutliers <- dplyr::filter(alldata_df,
                                        fwrootshoot >= Lower & fwrootshoot <= Upper)

# Check replicate counts after filtering. Which treatments had outliers removed, which didn't?
table(fwrootshoot_nooutliers$saltconc)

# Re‑test normality after removal. This is less highly significant but still not amazing
shapiro.test(fwrootshoot_nooutliers$fwrootshoot)

#But the histogram suggests taht we are now looking at one continuous distribution, which should be fine for ANOVA testing.
hist(fwrootshoot_nooutliers$fwrootshoot,
     main = "Histogram: FW root:shoot (no outliers)",
     xlab = "FW root:shoot ratio")

fwrootshoot_nooutliers$saltconc <- factor(fwrootshoot_nooutliers$saltconc,
                                          levels = desired_salt_order)
levels(fwrootshoot_nooutliers$saltconc)

#The boxplot comparisons also look like they are not too affected by the outliers
boxplot(fwrootshoot ~ saltconc, data = fwrootshoot_nooutliers,
        xlab = "Salt concentration (g/L)",
        ylab = "FW root:shoot ratio")

# NB: Don’t remove outliers just because they’re inconvenient — you need a sound
# biological reason. Be transparent and justify any removals; otherwise, you’re
# simply massaging the data.

# Our histogram now suggests a more coherent distribution. Because ANOVA is
# relatively robust to non‑normality, we will continue — just note that the
# normality assumption was violated and how you addressed it.

##############Go forth and conquer the stats!!########################

# From here, you can either use your new skills to analyse the remaining
# variables (this will put you in the running for an HD!) or open and run the
# file "workshop1_salinitycont.R" to work through our code.

# IF YOU CHOOSE TO PROCEED YOURSELF, DO NOT CREATE A NEW FILE OR READ NEW DATA.
# KEEP WORKING FROM THE BOTTOM OF THIS SCRIPT ONWARDS.

# Good luck!
