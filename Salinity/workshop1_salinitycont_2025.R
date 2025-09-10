# ******************************************************************************
###                     Rex and Sof's Salinity Workshop                      ###
# ******************************************************************************

# ******************************************************************************
# Salinity Data Analysis (continued) ------------------------------------------
# ******************************************************************************

### PLEASE NOTE: USING THIS CODE WILL VOID YOUR CHANCE AT AN HD FOR
### THIS PART OF THE ASSIGNMENT.

# Start fresh ------------------------------------------------------------------
rm(list = ls())

# Libraries --------------------------------------------------------------------
# (Install first if needed, e.g., install.packages("dplyr"))
library(dplyr)
library(car)          # for leveneTest()


# Data import & preparation -----------------------------------------------------
# Read from working directory and drop rows with any NA values
all_data <- na.omit(read.csv("Salinity_data.csv"))

# Quick checks
head(all_data)
str(all_data)

# Variables we will use
saltconc    <- as.character(all_data$salt.conc) # treat as categorical later
fwrootshoot <- all_data$fw.root.shoot.ratio
dwrootshoot <- all_data$dw.root.shoot.ratio
shootmoist  <- all_data$shoot.moisture
rootmoist   <- all_data$root.moisture
totmoist    <- all_data$total.moisture

# Single tidy data frame for analysis/plotting
alldata_df <- cbind.data.frame(saltconc, fwrootshoot, dwrootshoot,
                               shootmoist, rootmoist, totmoist)
str(alldata_df)

# Order the salt concentration factor for sensible x-axis ordering
desired_salt_order <- c("0", "1.5", "2.5", "5", "10", "15")
alldata_df$saltconc <- factor(alldata_df$saltconc, levels = desired_salt_order)
levels(alldata_df$saltconc)



# Fresh weight root:shoot ratio - re-capped from previous code ------------------------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$fwrootshoot)

hist(alldata_df$fwrootshoot, breaks = 100,
     main = "Histogram: FW root:shoot",
     xlab = "FW root:shoot ratio")

# As per last code, something wacky is happening here. Let's open our data and have a look.
# What can you see is wrong? We need to remove some outliers
quartiles <- quantile(alldata_df$fwrootshoot, probs = c(.25, .75), na.rm = TRUE)
iqr_val   <- IQR(alldata_df$fwrootshoot, na.rm = TRUE)
Lower     <- quartiles[1] - 1.5 * iqr_val
Upper     <- quartiles[2] + 1.5 * iqr_val

fwrootshoot_nooutliers <- dplyr::filter(alldata_df,
                                        fwrootshoot >= Lower & fwrootshoot <= Upper)

# Replicates per treatment after filtering
table(fwrootshoot_nooutliers$saltconc)

# re-test for normality-as per previous code, this is now a bit better
shapiro.test(fwrootshoot_nooutliers$fwrootshoot)

hist(fwrootshoot_nooutliers$fwrootshoot,
     main = "Histogram: FW root:shoot (no outliers)",
     xlab = "FW root:shoot ratio")

# and let's see how the boxplot has changed
boxplot(fwrootshoot ~ saltconc, data = fwrootshoot_nooutliers,
        xlab = "Salt concentration (g/L)",
        ylab = "FW root:shoot ratio")

# Assumption 2: homogeneity of variances
leveneTest(fwrootshoot ~ saltconc, data = fwrootshoot_nooutliers)

# One-way ANOVA
fwrootshoot.aov <- aov(fwrootshoot ~ saltconc, data = fwrootshoot_nooutliers)
summary(fwrootshoot.aov)

# Group summaries
fwrootshoot_nooutliers %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(fwrootshoot, na.rm = TRUE),
            sd    = sd(fwrootshoot,   na.rm = TRUE),
            .groups = "drop")

### let's graph it!
plotMeans(fwrootshoot_nooutliers$fwrootshoot,
          fwrootshoot_nooutliers$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "FW root:shoot ratio",
          main = "")

# once we're happy with our plot, we click "export" and save it in our working folder.


# Dry weight root:shoot ratio --------------------------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$dwrootshoot)

hist(alldata_df$dwrootshoot,
     main = "Histogram: DW root:shoot",
     xlab = "DW root:shoot ratio")

boxplot(dwrootshoot ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "DW root:shoot ratio")

# remove outliers
quartiles <- quantile(alldata_df$dwrootshoot, probs = c(.25, .75), na.rm = TRUE)
iqr_val   <- IQR(alldata_df$dwrootshoot, na.rm = TRUE)
Lower     <- quartiles[1] - 1.5 * iqr_val
Upper     <- quartiles[2] + 1.5 * iqr_val

dwrootshoot_nooutliers <- dplyr::filter(alldata_df,
                                        dwrootshoot >= Lower & dwrootshoot <= Upper)

# re-test for normality
shapiro.test(dwrootshoot_nooutliers$dwrootshoot)

hist(dwrootshoot_nooutliers$dwrootshoot,
     main = "Histogram: DW root:shoot (no outliers)",
     xlab = "DW root:shoot ratio")

boxplot(dwrootshoot ~ saltconc, data = dwrootshoot_nooutliers,
        xlab = "Salt concentration (g/L)",
        ylab = "DW root:shoot ratio")

table(dwrootshoot_nooutliers$saltconc)

# Assumption 2: homogeneity of variances
leveneTest(dwrootshoot ~ saltconc, data = dwrootshoot_nooutliers)

# One-way ANOVA
dwrootshoot.aov <- aov(dwrootshoot ~ saltconc, data = dwrootshoot_nooutliers)
summary(dwrootshoot.aov)

# Summaries
dwrootshoot_nooutliers %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(dwrootshoot, na.rm = TRUE),
            sd    = sd(dwrootshoot,   na.rm = TRUE),
            .groups = "drop")

# Post-hoc (Tukey)
dwrootshoot.tukey <- TukeyHSD(dwrootshoot.aov, which = "saltconc")
dwrootshoot.tukey

### let's graph it! and letters
plotMeans(dwrootshoot_nooutliers$dwrootshoot,
          dwrootshoot_nooutliers$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "DW root:shoot ratio",
          main = "")

# Shoot moisture ---------------------------------------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$shootmoist)

hist(alldata_df$shootmoist,
     main = "Histogram: Shoot moisture",
     xlab = "Shoot moisture (%)")

boxplot(shootmoist ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "Shoot moisture (%)")

# remove outliers
quartiles <- quantile(alldata_df$shootmoist, probs = c(.25, .75), na.rm = TRUE)
iqr_val   <- IQR(alldata_df$shootmoist, na.rm = TRUE)
Lower     <- quartiles[1] - 1.5 * iqr_val
Upper     <- quartiles[2] + 1.5 * iqr_val

shootmoist_nooutliers <- dplyr::filter(alldata_df,
                                       shootmoist >= Lower & shootmoist <= Upper)

# re-test for normality
shapiro.test(shootmoist_nooutliers$shootmoist)

hist(shootmoist_nooutliers$shootmoist,
     main = "Histogram: Shoot moisture (no outliers)",
     xlab = "Shoot moisture (%)")

boxplot(shootmoist ~ saltconc, data = shootmoist_nooutliers,
        xlab = "Salt concentration (g/L)",
        ylab = "Shoot moisture (%)")

table(shootmoist_nooutliers$saltconc)

# Assumption 2: homogeneity of variances
leveneTest(shootmoist ~ saltconc, data = shootmoist_nooutliers)

# One-way ANOVA
shootmoist.aov <- aov(shootmoist ~ saltconc, data = shootmoist_nooutliers)
summary(shootmoist.aov)

# Summaries
shootmoist_nooutliers %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(shootmoist, na.rm = TRUE),
            sd    = sd(shootmoist,   na.rm = TRUE),
            .groups = "drop")

# Post-hoc (Tukey)
shootmoist.tukey <- TukeyHSD(shootmoist.aov, which = "saltconc")
shootmoist.tukey

### let's graph it! and letters
plotMeans(shootmoist_nooutliers$shootmoist,
          shootmoist_nooutliers$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "Shoot moisture (%)",
          main = "")

# Root moisture ----------------------------------------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$rootmoist)

hist(alldata_df$rootmoist,
     main = "Histogram: Root moisture",
     xlab = "Root moisture (%)")

boxplot(rootmoist ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "Root moisture (%)")

# remove outliers
quartiles <- quantile(alldata_df$rootmoist, probs = c(.25, .75), na.rm = TRUE)
iqr_val   <- IQR(alldata_df$rootmoist, na.rm = TRUE)
Lower     <- quartiles[1] - 1.5 * iqr_val
Upper     <- quartiles[2] + 1.5 * iqr_val

rootmoist_nooutliers <- dplyr::filter(alldata_df,
                                      rootmoist >= Lower & rootmoist <= Upper)

# re-test for normality
shapiro.test(rootmoist_nooutliers$rootmoist)

hist(rootmoist_nooutliers$rootmoist,
     main = "Histogram: Root moisture (no outliers)",
     xlab = "Root moisture (%)")

boxplot(rootmoist ~ saltconc, data = rootmoist_nooutliers,
        xlab = "Salt concentration (g/L)",
        ylab = "Root moisture (%)")

table(rootmoist_nooutliers$saltconc)

# Assumption 2: homogeneity of variances
leveneTest(rootmoist ~ saltconc, data = rootmoist_nooutliers)

# If either Shapiro–Wilk or Levene’s test is significant, flag this in your report
# and discuss potential reasons (e.g., greater variability at low salt levels).

# One-way ANOVA
rootmoist.aov <- aov(rootmoist ~ saltconc, data = rootmoist_nooutliers)
summary(rootmoist.aov)

# Summaries
rootmoist_nooutliers %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(rootmoist, na.rm = TRUE),
            sd    = sd(rootmoist,   na.rm = TRUE),
            .groups = "drop")

# Post-hoc (Tukey)
rootmoist.tukey <- TukeyHSD(rootmoist.aov, which = "saltconc")
rootmoist.tukey

### let's graph it! and letters
plotMeans(rootmoist_nooutliers$rootmoist,
          rootmoist_nooutliers$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "Root moisture (%)",
          main = "")


# Total moisture ----------------------------------------------------------------

### Assumption 1: data are normally distributed
shapiro.test(alldata_df$totmoist)

hist(alldata_df$totmoist,
     main = "Histogram: Total moisture",
     xlab = "Total moisture (%)")

boxplot(totmoist ~ saltconc, data = alldata_df,
        xlab = "Salt concentration (g/L)",
        ylab = "Total moisture (%)")

# remove outliers
quartiles <- quantile(alldata_df$totmoist, probs = c(.25, .75), na.rm = TRUE)
iqr_val   <- IQR(alldata_df$totmoist, na.rm = TRUE)
Lower     <- quartiles[1] - 1.5 * iqr_val
Upper     <- quartiles[2] + 1.5 * iqr_val

totmoist_nooutliers <- dplyr::filter(alldata_df,
                                     totmoist >= Lower & totmoist <= Upper)

# re-test for normality
shapiro.test(totmoist_nooutliers$totmoist)

hist(totmoist_nooutliers$totmoist,
     main = "Histogram: Total moisture (no outliers)",
     xlab = "Total moisture (%)")

boxplot(totmoist ~ saltconc, data = totmoist_nooutliers,
        xlab = "Salt concentration (g/L)",
        ylab = "Total moisture (%)")

table(totmoist_nooutliers$saltconc)

# Assumption 2: homogeneity of variances
leveneTest(totmoist ~ saltconc, data = totmoist_nooutliers)

# One-way ANOVA
totmoist.aov <- aov(totmoist ~ saltconc, data = totmoist_nooutliers)
summary(totmoist.aov)

# Summaries
totmoist_nooutliers %>%
  group_by(saltconc) %>%
  summarise(count = n(),
            mean  = mean(totmoist, na.rm = TRUE),
            sd    = sd(totmoist,   na.rm = TRUE),
            .groups = "drop")

# Post-hoc (Tukey)
totmoist.tukey <- TukeyHSD(totmoist.aov, which = "saltconc")
totmoist.tukey

### let's graph it! and letters
plotMeans(totmoist_nooutliers$totmoist,
          totmoist_nooutliers$saltconc,
          error.bars = "se",
          xlab = "Salt concentration (g/L)",
          ylab = "Total moisture (%)",
          main = "")

# Well done!
