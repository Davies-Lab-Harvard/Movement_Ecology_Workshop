## ---------------------------
##
## Script name: fit_iSSA.R
##
## Authors: Rhemi Toth and Nick Russo
##
## Date Created: 2025-10-24
##
##
## ---------------------------
##
## This script:
## 1) Loads an RData file containing used and available steps with covariates already extracted
## 2) Shows how to fit separate iSSA models to individuals to understand differences in direction of selection between individuals
## 3) Shows how visualize and compare magnitude of selection across individuals using Relative Selection Strength
## 4) Shows how to fit an iSSA with mixed effects to derive population level coefficients and compare magnitude of selection across individuals
## 5) Demonstrates how to compete hypotheses regarding drivers of animal movement by building off of a "core" iSSA model.
##
## NOTE:
## we show you how to do both model fitting and plotting in this script for ease of presentation.
## However, it is good practice to perform model fitting and plotting in separate scripts for organizational purposes. 

## ---------------------------

rm(list = ls()) # clean your environment
working_directory <- "/Users/rhemitoth/Documents/PhD/Movement_Ecology_Workshop" # SET THIS TO WHERE THE MOVEMENT ECOLOGY WORKSHOP REPOSITORY IS LOCATED ON YOUR COMPUTER PLZ !!!!!
setwd(working_directory) # sets the working directory to the movement ecology workshop repository

# Load Packages -----------------------------------------------------------

library(amt)
library(tidyverse)
library(glmmTMB)


# Load the data -----------------------------------------------------------

load("data/hornbill.extracted.RData") # used and available steps with covariates extracted (hornbill.extracted)

# Unnest the data and extract the steps for each individual

steps <- hornbill.extracted %>%
  select(id, steps) %>%
  unnest(steps)


# Movement parameters -----------------------------------------------------
# When fitting an iSSA, the coefficient estimates for step length (sl_), log of the step length (log_sl_), and cosine of the turn angle (ta_)
# serve as updated parameters for the gamma and von mises distributions used to generate the random steps.
# Before fitting the iSSA, we need to calculate the log of the step length and the cosine of the turn angle for every step.

steps <- steps %>%
  mutate(log_sl_ = log(sl_),
         cos_ta_ = cos(ta_))

# Scale and center variables ----------------------------------------------
# Before running the iSSA models, it is good practice to scale and center your continuous covariates
# Doing so improves model convergence and allows direct comparison of effect sizes among variables measured on different scales
# (e.g. scaling allows you to compare effect size of NDVI which is measured on a scale from -1 to 1 vs elevation which can be measured on a scale of tens to hundreds of meters above sea level)
# Our continous variables are "CanopyHeight10m" "dist2gap50"      "dist2gap500"     "VCI"  

steps$CanopyHeight10m_scaled <- scale(steps$CanopyHeight10m)
steps$dist2gap50_scaled <- scale(steps$dist2gap50)
steps$dist2gap500_scaled <- scale(steps$dist2gap500)
steps$VCI_scaled <- scale(steps$VCI)


# Categorical variables ---------------------------------------------------
# In our dataset we have swamp as a categorical variable
# Right now, it is coded as a 0 for no swamp and 1 for swamp
# We need to make sure that R treats swamp as a category instead of a number by converting swamp from numeric to factor
steps$Swamp <- factor(steps$Swamp)

# Fitting separate iSSA models to each individual -------------------------

# Step 1: Subset used and available steps for each individual

steps_8970 <- steps %>%
  filter(id == "8970") 

steps_9894 <- steps %>%
  filter(id == "9894") 

steps_9919 <- steps %>%
  filter(id == "9919") 

steps_11850 <- steps %>%
  filter(id == "11850") 

steps_11852 <- steps %>%
  filter(id == "11852") 

# Step 2: Fit iSSA models for each individual

# iSSA model for 8970
fit8970 <- fit_issf(# Used and available steps
                    steps_8970, 
                    # Response
                    case_ ~ 
                      # Habitat
                      CanopyHeight10m_scaled + dist2gap50_scaled + dist2gap500_scaled + VCI_scaled + Swamp +
                      # Movement
                      sl_ + log_sl_ + cos_ta_ + 
                      # Stratum
                      strata(step_id_),
                    # Need this later for model predictions
                    model = TRUE)

# iSSA model for 9894
fit9894 <- fit_issf(# Used and available steps
  steps_9894, 
  # Response
  case_ ~ 
    # Habitat
    CanopyHeight10m_scaled + dist2gap50_scaled + dist2gap500_scaled + VCI_scaled + Swamp +
    # Movement
    sl_ + log_sl_ + cos_ta_ + 
    # Stratum
    strata(step_id_),
  # Need this later for model predictions
  model = TRUE)

# iSSA model for 9919
fit9919 <- fit_issf(# Used and available steps
  steps_9919, 
  # Response
  case_ ~ 
    # Habitat
    CanopyHeight10m_scaled + dist2gap50_scaled + dist2gap500_scaled + VCI_scaled + Swamp +
    # Movement
    sl_ + log_sl_ + cos_ta_ + 
    # Stratum
    strata(step_id_),
  # Need this later for model predictions
  model = TRUE)

# iSSA model for 11850
fit11850 <- fit_issf(# Used and available steps
  steps_11850, 
  # Response
  case_ ~ 
    # Habitat
    CanopyHeight10m_scaled + dist2gap50_scaled + dist2gap500_scaled + VCI_scaled + Swamp +
    # Movement
    sl_ + log_sl_ + cos_ta_ + 
    # Stratum
    strata(step_id_),
  # Need this later for model predictions
  model = TRUE)

# iSSA model for 11852
fit11852 <- fit_issf(# Used and available steps
  steps_11852, 
  # Response
  case_ ~ 
    # Habitat
    CanopyHeight10m_scaled + dist2gap50_scaled + dist2gap500_scaled + VCI_scaled + Swamp +
    # Movement
    sl_ + log_sl_ + cos_ta_ + 
    # Stratum
    strata(step_id_),
  # Need this later for model predictions
  model = TRUE)


# Step 3: Extract the model summaries for each individual

sum8970 <- summary(fit8970)
sum9894 <- summary(fit9894)
sum9919 <- summary(fit9919)
sum11850 <- summary(fit11850)
sum11852 <- summary(fit11852)

# How to interperet the model summary (Example Using 8970):

# Coef tells you the magnitude and direction of selection. Negative value indicates avoidance, positive value indicates selection.
# exp(coef) tells gives you the odds ratio (relative odds of selecting a step when the covariate value increases by 1). Values < 1 indicate avoidance and values >1 indicate selection.
# se(coef) is the standard error on the coefficient estimate
# z is the wald test statistic (coef/se(coef)). Larger z provides stronger evidence that the coefficient is statistically different from 0.
# (Pr(>|z|) gives you the p value

# CanopyHeight10m_scaled: 8970 shows significant, positive selection
# dist2gap50_scaled: 8970 shows significant selection for areas near 50m gaps (Remember we are dealing with distance here Negative coefficient indicates significant avoidance of areas that are far away)
# dist2gap500_scaled: 8970 shows siginifanct avoidance of areas near 500m gaps (Remember we are dealing with distance here. Positive coefficient indicates significant selection of areas that are far away)
# VCI_Scaled: 8970 shows significant, positive selection for areas with high vertical complexity

# Swamp: Because the summary says "Swamp1", this means that the coefficient for swamp is reported with respect to the reference value 0.
# In other words, the coefficient Swamp1 tells us how much more (or less if the value was negative) 8970 is to select swamps relative to non-swamp areas. 
# The coef for Swamp1 is negative and the p value is < 0.05 so 8970 shows significant avoidance of swamps relative to non-swamp areas.

# The coefficients fro sl_, log_sl_, and cos_ta_ can be used to update the step length and turning angle distributions for each animal. We will talk about this later on.

sum8970 

# Step 4: Extract confidience intervals for coefficient estimates for each indivdual

# Extract the confidence intervals
# Add column for ID
# Add column for variable (from rownames)
confints8970 <- as.data.frame(sum8970$conf.int) %>% mutate(ID = "8970", var = rownames(sum8970$conf.int))
confints9894 <- as.data.frame(sum9894$conf.int) %>% mutate(ID = "9894", var = rownames(sum9894$conf.int))
confints9919 <- as.data.frame(sum9919$conf.int) %>% mutate(ID = "9919", var = rownames(sum9919$conf.int))
confints11850 <- as.data.frame(sum11850$conf.int) %>% mutate(ID = "11850", var = rownames(sum11850$conf.int))
confints11852 <- as.data.frame(sum11852$conf.int) %>% mutate(ID = "11852", var = rownames(sum11852$conf.int))

# Combine into one dataframe (makes plotting simpler)
# and add a column for variable
confints <- rbind(confints8970,
                   confints9894,
                   confints9919,
                   confints11850,
                   confints11852) 

# Step 6: Odds ratio plot

# Filter out movement parameters
confints_for_plotting <- confints %>%
  filter(!(var %in% c("sl_", "log_sl_", "cos_ta_")))

# Odds ratio plot
ggplot(data = confints_for_plotting, aes(color = ID)) + # global aesthetics can go up top
  geom_point(aes(x = `exp(coef)`, y = ID)) + # when variables have special characters or spaces you need to surround them with ``
  geom_errorbar(aes(y = ID,xmin=`lower .95`,xmax = `upper .95`)) +
  geom_vline(aes(xintercept = 1),color = "grey", linetype = "dashed")+ # Line to help distinguish selection from avoidance. Because we are plotting the odds ratio, the cutoff is 1
  theme_bw()+
  facet_wrap(~var)

# How to interperet plot:
# - values > 1 indicate seleciton
# - values < 1 indicate avoidance
# - When you fit separate iSSA models per individual, each model’s coefficients (and exp(coef) odds ratios) 
#   are conditional on that individual’s steps and random steps. This means that you can't directly compare magnitude of selection between individuals. 
#.  However, it is okay to say that different individuals have the same sign or different direction of selection (i.e. positive or negative)


# Relative Selection Strength ---------------------------------------------
# If all individuals have the same model structure and  continuous covariates are centered and scaled across all individuals,
# then Relative Selection Strength (RSS) can be used to compare magnitude of selection across individuals.
# RSS tells you how much more likely an animal is to select a location with a given change in the covariate relative to a baseline.
# The formula for RSS is RSS=exp(βΔX) where β = coefficient from the iSSA/SSF and ΔX = change in the covariate (e.g., 1 SD if scaled)

# How much more likely is a hornbill to step into habitat of a given VCI value relative to the mean VCI?
# To answer this using an RSS plot, we need to construct two dataframes, x1 and x2. Both x1 and x2 are defined using data.frames 
# and require all of the covariates in the fitted model. x1 can be any number of rows you wish,
# but to avoid unintended issues with R's recycling rules, we limit
# x2 to be exactly 1 row.

x1 <- data.frame(VCI_scaled = seq(min(steps$VCI_scaled,na.rm=TRUE),max(steps$VCI_scaled,na.rm=TRUE), length.out = 100),  # Defining a range of values for VCI_scaled across the observed range
                 CanopyHeight10m_scaled = 0, # The values for the other variables can be anything you want AS LONG AS THEY ARE THE SAME BETWEEN x1 and x2 !!!!
                 dist2gap50_scaled = 0,
                 dist2gap500_scaled = 0,
                 Swamp = factor("0", levels = levels(steps$Swamp)),
                 sl_ = 50, log_sl_ = log(50),
                 cos_ta_ = 0)

x2 <- data.frame(VCI_scaled = mean(steps$VCI_scaled,na.rm=TRUE),  # Defining a range of values for VCI_scaled across the observed range
                 CanopyHeight10m_scaled = 0, # The values for the other variables can be anything you want AS LONG AS THEY ARE THE SAME BETWEEN x1 and x2 !!!!
                 dist2gap50_scaled = 0,
                 dist2gap500_scaled = 0,
                 Swamp = factor("0", levels = levels(steps$Swamp)),
                 sl_ = 50, log_sl_ = log(50),
                 cos_ta_ = 0)

# Compute log RSS for each individual
logRSS_8970 <- log_rss(fit8970, x1, x2, ci = "se", ci_level = 0.95) # Use the issa model for each individual (fitXXXXX)
logRSS_9894 <- log_rss(fit9894, x1, x2, ci = "se", ci_level = 0.95) # Use the issa model for each individual (fitXXXXX)
logRSS_9919 <- log_rss(fit9919, x1, x2, ci = "se", ci_level = 0.95) # Use the issa model for each individual (fitXXXXX)
logRSS_11850 <- log_rss(fit11850, x1, x2, ci = "se", ci_level = 0.95) # Use the issa model for each individual (fitXXXXX)
logRSS_11852 <- log_rss(fit11852, x1, x2, ci = "se", ci_level = 0.95) # Use the issa model for each individual (fitXXXXX)

# Add ID column for each individual to nested DF
logRSS_8970$df$ID <- '8970'
logRSS_9894$df$ID <- '9894'
logRSS_9919$df$ID <- '9919'
logRSS_11850$df$ID <- '11850'
logRSS_11852$df$ID <- '11852'

# Then combine into one DF for plotting
logRSS_df <- rbind(
  logRSS_8970$df,
  logRSS_9894$df,
  logRSS_9919$df,
  logRSS_11850$df,
  logRSS_11852$df
)

# Make the plot
# Because VCI is in scaled units, a value of 0 on the x axis corresponds to mean VCI across all used and available points across all individuals
ggplot(logRSS_df, aes(x = VCI_scaled_x1, group = ID, color = ID, fill = ID)) +
  geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr)), alpha = 0.2) +
  geom_line(aes(y = exp(log_rss)), size = 1)+
  geom_hline(yintercept = 1, color = "grey40", linetype = "dashed") +
  theme_bw()

# Effect of VCI on habitat selection appears to be stronger in 8970 and 9984 compared to the others


# Mixed Effects iSSA with glmmTMB -----------------------------------------

# Next we will show you how to use glmmTMB to:
# 1) compute population level coefficients
# 2) compete hypotheses 

# Sometimes you would like to use iSSA to compete different hypotheses about the drivers of animal movement and habitat selection
# To do this, you can start by constructing a "core model", which explains movement and habitat selection without respect to the variables of interest for your study

# For example, say  that you would like to examine whether swamps are an important driver of hornbill habitat selection. To test this using iSSA, we would start by constructing a "core model" that captures hornbill movement and habitat selection
# without respect to swamps. Here is how to do this using glmmTMB:

# m_core <- glmmTMB(
#   case_ ~
#      # Habitat
#     CanopyHeight10m_scaled + VCI_scaled + dist2gap500_scaled +
#     # Movement
#     sl_ + log_sl_ + cos_ta_ +
#     # Random effects
#     (1 | step_id_) +                   # random intercept per step (Poisson trick)
#     (0 + dist2gap500_scaled | id) +  # Allows selection for dist2gap500_scaled to vary across individuals
#    (0 + sl_ + log_sl_ + cos_ta_ | id), # random slopes for movement parameters
#  family = poisson(),
#    data = steps
#   )
# 
# save(m_core, file = "output/m_core.RData")

load("output/m_core.RData") # glmmTMB models take a long time to run so instead of running it live (using the above code), we are just loading the fitted model

# Now that the core model has been specified, we will fit a model that includes Swamp, using the core model as a foundation:

# m_swamp <- glmmTMB(
#   case_ ~
#     # Habitat
#     CanopyHeight10m_scaled + VCI_scaled + dist2gap500_scaled + Swamp +
#     # Movement
#     sl_ + log_sl_ + cos_ta_ +
#     # Random effects
#     (1 | step_id_) +                   # random intercept per step (Poisson trick)
#     (0 + dist2gap500_scaled | id) +  # Allows selection for dist2gap500_scaled to vary across individuals
#     (0 + sl_ + log_sl_ + cos_ta_ | id), # random slopes for movement parameters
#   family = poisson(),
#   data = steps
# )
# 
# save(m_swamp, file = "output/m_swamp.RData")

load("output/m_swamp.RData") # glmmTMB models take a long time to run so instead of running it live (using the above code), we are just loading the fitted model

# First, we can ask if including dist2gap500 in the model improves model fit by competing m_core and m_dist2gap500 
# by comparing their Akaike Information Criterion (AIC) scores:

AIC(m_core,m_swamp)

# Based on the AIC scores, including Swamp in the model improves model fit, indicating that swamps an important driver of hornbill habitat selection.

# Now that we have identified a top model, we can look at the summary to see how hornbill
# habitat selection varies at the population level:

summary(m_swamp)

# We can also examine how individual selection with respect to dist2gap500 varies, since we included dist2gap500_scaled as a random effect in our model:

re <- ranef(m_swamp) # Extract all random effects
re_id <- re$cond$id # Random effects for 'id'

# The random slopes for from re_id show how each individual deviates from that population slope:
re_id

# To get the true slopes for each individual, you need to add the population level fixed effect (from the model summary):
# Step 1: Extract the population-level fixed effect from the model
fixed_effect_dist2gap <- fixef(m_swamp)$cond["dist2gap500_scaled"]

# Step 2: Extract the random effects (deviations) for each individual
re <- ranef(m_swamp)        # all random effects
re_id <- re$cond$id          # random effects for 'id'

# Step 3: Compute the true slope for each individual
# The random slope is a deviation from the population mean, so add the fixed effect
dist2gap_individual_slopes <- data.frame(
  id = rownames(re_id),
  slope = re_id$dist2gap500_scaled + fixed_effect_dist2gap
)

# Step 4: View the result
dist2gap_individual_slopes
