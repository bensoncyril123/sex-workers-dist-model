# Load required libraries for data manipulation, visualization, and modeling
library(tidyverse)     # Core data manipulation and visualization
library(easyalluvial)  # Alluvial plots for categorical data relationships
library(effects)       # Visualization of effects in regression models
library(ggmosaic)      # Mosaic plots for categorical data
library(car)           # Companion for applied regression
library(lmerTest)      # Linear mixed-effects models
library(mosaic)        # Simplified statistics functions
library(ggResidpanel)  # Residual diagnostics for regression models
library(emmeans)       # Estimated marginal means (post-hoc comparisons)
library(GGally)        # Pair plots and correlation diagnostics
library(MASS)          # Support for generalized linear models (e.g., negative binomial)
library(performance)   # Model performance metrics
library(glmnet)        # Regularization paths for GLMs
library(nlme)          # Nonlinear mixed-effects models
library(glmmTMB)       # Generalized linear mixed-effects models
library(DHARMa)        # Diagnostics for hierarchical models
library(pscl)          # For additional model diagnostics

# Set global options for cleaner output in reports
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

# Apply a consistent theme to all ggplot2 visualizations
theme_set(theme_bw())

# Preprocess the dataset: clean and prepare variables
# Convert categorical variables to factors and handle missing data
training_data <- load.data %>%
  mutate(
    country = factor(country),
    region = factor(region),
    month = factor(month),
    dataYear = factor(dataYear),
    popDensity = factor(popDensity),
    hivRate = factor(hivRate),
    hivFear = factor(hivFear),
    highCrime = factor(highCrime)
  ) %>%
  drop_na() # Remove rows with missing data

# Create a testing dataset by filtering rows where FSWCount is missing
testing_data <- load.data %>%
  mutate(
    country = factor(country),
    region = factor(region),
    month = factor(month),
    dataYear = factor(dataYear),
    popDensity = factor(popDensity),
    hivRate = factor(hivRate),
    hivFear = factor(hivFear),
    highCrime = factor(highCrime)
  ) %>%
  filter(is.na(FSWCount))

# Visualize pairwise relationships and correlations
# Continuous variables are selected for ggpairs plots
training_data %>%
  ggpairs(
    columns = 5:13, 
    upper = list(continuous = GGally::wrap(ggally_cor, stars = FALSE))
  ) +
  theme_bw()

training_data %>%
  ggpairs(
    columns = c(5, 14:20), 
    upper = list(continuous = GGally::wrap(ggally_cor, stars = FALSE))
  ) +
  theme_bw()

# Plot the distribution of FSWCount using a histogram
ggplot(training_data, aes(x = FSWCount)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(
    x = "Female Sex Workers Count",
    y = "Frequency",
    title = "Histogram of FSW Counts"
  ) +
  theme_minimal()

# Fit initial regression models and evaluate residuals
# Example: Residual analysis for model 'fit1'
training_data$residuals <- resid(fit1)
training_data$fitted <- predict(fit1)

# Residual vs Fitted plot
par(mfrow = c(1, 2)) # Set up 2-panel plotting area
plot(
  training_data$fitted, training_data$residuals,
  ylab = "Residuals", xlab = "Fitted",
  main = "Residuals vs Fitted Values"
)
abline(h = 0, col = "red")

# Normality check for residuals (Q-Q plot)
qqnorm(training_data$residuals)
qqline(training_data$residuals, col = "steelblue")

# Residual analysis for model 'fit2'
training_data$residuals_fit2 <- resid(fit2)
training_data$fitted_fit2 <- predict(fit2)

# Residual vs Fitted plot for fit2
par(mfrow = c(1, 2))
plot(
  training_data$fitted_fit2, training_data$residuals_fit2,
  ylab = "Residuals", xlab = "Fitted",
  main = "Residuals vs Fitted Values for fit2"
)
abline(h = 0, col = "red")

# Normality check for residuals of fit2 (Q-Q plot)
qqnorm(training_data$residuals_fit2)
qqline(training_data$residuals_fit2, col = "steelblue")

# MODEL SELECTION FOR ENVIRONMENTAL VARIABLES
fit1 <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                  temperature + nightlight + cleanWater + 
                  (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                offset = log(surveyArea), 
                data = training_data, family = poisson) # Poisson

fit2 <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                  temperature + nightlight + cleanWater + 
                  (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                offset = log(surveyArea), 
                data = training_data, family = nbinom2) # Negative Binomial

fit3 <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                  temperature + nightlight + cleanWater + 
                  (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                offset = log(surveyArea), zi = ~1, 
                family = truncated_poisson, data = training_data) # Hurdle Poisson

fit4 <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                  temperature + nightlight + cleanWater + 
                  (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                offset = log(surveyArea), zi = ~1, 
                family = truncated_nbinom2, data = training_data) # Hurdle Negative Binomial

fit5 <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                  temperature + nightlight + cleanWater + 
                  (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                offset = log(surveyArea), zi = ~1, 
                family = nbinom2, data = training_data) # Zero-Inflated Negative Binomial

fit6 <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                  temperature + nightlight + cleanWater + 
                  (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                offset = log(surveyArea), zi = ~1, 
                family = poisson, data = training_data) # Zero-Inflated Poisson

# Compare models using AIC
AIC(fit1, fit2, fit3, fit4, fit5, fit6)

# RANDOM EFFECT SELECTION FOR ENVIRONMENTAL VARIABLES
fit2A <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater + 
                   (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # Full model

fit2B <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater + 
                   (1 | country) + (1 | region) + (1 | month), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # Excluding dataYear

fit2C <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater + 
                   (1 | country) + (1 | region) + (1 | dataYear), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # Excluding month (BEST)

fit2D <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater + 
                   (1 | country) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # Excluding region

fit2E <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater + 
                   (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # Excluding country

fit2p <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater, 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # No random effects

fit2q <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater + 
                   (1 | country) + (1 | region), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # Only country and region

fit2r <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater + 
                   (1 | country), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = TRUE) # Only country

# Compare models using AIC
AIC(fit2A, fit2B, fit2C, fit2D, fit2E)

# Perform ANOVA tests for nested model comparisons
anova(fit2A, fit2B)
anova(fit2A, fit2C)
anova(fit2A, fit2p)
anova(fit2A, fit2q)
anova(fit2A, fit2r)

# FIXED VARIABLE SELECTION FOR ENVIRONMENTAL VARIABLES
# Full model with all fixed effects
fit2i <- glmmTMB(FSWCount ~ popDensity + built + growingSeason + rain +
                   temperature + nightlight + cleanWater +
                   (1 | country) + (1 | region), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = FALSE) # Negative Binomial

# Reduced model with selected fixed effects
fit2t <- glmmTMB(FSWCount ~ popDensity + built + temperature +
                   (1 | country) + (1 | region), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = FALSE) # Negative Binomial

# Intercept-only model
fit2u <- glmmTMB(FSWCount ~ 1 + 
                   (1 | country) + (1 | region), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2, REML = FALSE) # Negative Binomial

# Model comparison
AIC(fit2i)   # AIC for full model
AIC(fit2t)   # AIC for reduced model
AIC(fit2u)   # AIC for intercept-only model

# Perform ANOVA tests for nested model comparisons
anova(fit2t, fit2i) # Compare reduced model with full model
anova(fit2u, fit2t) # Compare intercept-only model with reduced model

# MODEL SELECTION FOR SOCIOLOGICAL VARIABLES
fits1 <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                   ageFirstSex + wealthIndex + 
                   (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), 
                 data = training_data, family = poisson) # Poisson

fits2 <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                   ageFirstSex + wealthIndex + 
                   (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), 
                 data = training_data, family = nbinom2) # Negative Binomial

fits3 <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                   ageFirstSex + wealthIndex + 
                   (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), zi = ~1, 
                 family = truncated_poisson, data = training_data) # Hurdle Poisson

fits4 <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                   ageFirstSex + wealthIndex + 
                   (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), zi = ~1, 
                 family = truncated_nbinom2, data = training_data) # Hurdle Negative Binomial

fits5 <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                   ageFirstSex + wealthIndex + 
                   (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), zi = ~1, 
                 family = nbinom2, data = training_data) # Zero-Inflated Negative Binomial

fits6 <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                   ageFirstSex + wealthIndex + 
                   (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                 offset = log(surveyArea), zi = ~1, 
                 family = poisson, data = training_data) # Zero-Inflated Poisson

# Compare models using AIC
AIC(fits1, fits2, fits3, fits4, fits5, fits6)

# RANDOM EFFECT SELECTION FOR SOCIOLOGICAL VARIABLES
fits2A <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex + 
                    (1 | country) + (1 | region) + (1 | month) + (1 | dataYear), 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Full model

fits2B <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex + 
                    (1 | country) + (1 | region) + (1 | month), 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Excluding dataYear

fits2C <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex + 
                    (1 | country) + (1 | region) + (1 | dataYear), 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Excluding month (BEST)

fits2D <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex + 
                    (1 | country) + (1 | month) + (1 | dataYear), 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Excluding region

fits2E <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex + 
                    (1 | region) + (1 | month) + (1 | dataYear), 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Excluding country

# Model with no random effects
fits2p <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex, 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Negative Binomial

# Model with only country and region as random effects
fits2q <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex + 
                    (1 | country) + (1 | region), 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Negative Binomial

# Model with only country as a random effect
fits2r <- glmmTMB(FSWCount ~ hivRate + protected + hivFear + highCrime + insectNet +
                    ageFirstSex + wealthIndex + 
                    (1 | country), 
                  offset = log(surveyArea), 
                  data = training_data, family = nbinom2, REML = TRUE) # Negative Binomial

# Compare models using AIC
AIC(fits2A, fits2B, fits2C, fits2D, fits2E)

# Perform ANOVA tests for nested model comparisons
anova(fits2A, fits2B)
anova(fits2A, fits2C)
anova(fits2A, fits2D)
anova(fits2A, fits2p)
anova(fits2A, fits2q)
anova(fits2C, fits2p)

