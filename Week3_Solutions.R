# clear environment
rm(list = ls())

# turn off scientific notation
options(scipen = 999)

#set wd
setwd("C:/Users/vinug/OneDrive - University of Reading/mm333/Week 3")

#libraries
library(tidyverse)
library(broom)
library(effectsize)
library(rstatix)
library(car)
library(lmtest)

# load csv file for regression
stores <- read_csv("MM333_week3_regression.csv", show_col_types = FALSE) %>%
  mutate(region = factor(region, levels = c("South", "North", "East", "West")))


# Activity 1 --------------------------------------------------------------

# Examine the structure of the dataset
glimpse(stores)

# Obtain descriptive statistics
summary(stores)

# Count the number of observations
nrow(stores)

# Count the number of stores in each region
table(stores$region)

# Check for missing values
colSums(is.na(stores))

# Check for duplicate store IDs
sum(duplicated(stores$store_id))

# Activity 2 --------------------------------------------------------------

# scatterplot
ggplot(stores, aes(x = advertising_spend_k,
                   y = sales_revenue_k)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Advertising Expenditure and Sales Revenue",
    x = "Advertising expenditure (£1,000s)",
    y = "Sales revenue (£1,000s)"
  ) +
  theme_minimal()

# Fit the simple linear regression model
simple <- lm(
  sales_revenue_k ~ advertising_spend_k,
  data = stores
)

# Examine the regression results
summary(simple)

# Display the regression results
summary(simple)

# Obtain 95% confidence intervals
confint(simple, level = 0.95)

# Create a new observation
new_store <- data.frame(
  advertising_spend_k = 15
)

# Predict sales revenue
predict(simple, newdata = new_store)

# Obtain a 95% confidence interval
predict(
  simple,
  newdata = new_store,
  interval = "confidence",
  level = 0.95
)

# Obtain a 95% prediction interval
predict(
  simple,
  newdata = new_store,
  interval = "prediction",
  level = 0.95
)


# Activity 3 --------------------------------------------------------------

# Examine the residuals versus fitted values plot
plot(simple, which = 1)

# Perform the Durbin-Watson test
durbinWatsonTest(simple)

# Examine the Q-Q plot
plot(simple, which = 2)

# Option with 95%CI
qqPlot(simple$residuals)

# Perform the Shapiro-Wilk test
shapiro.test(simple$residuals)

# Examine the Scale-Location plot
plot(simple, which = 3)

# Perform the Breusch-Pagan test
bptest(simple)

# Examine Cook's distance
plot(simple, which = 4)

# Calculate Cook's distance for each observation
cooks_d <- cooks.distance(simple)

# Calculate the screening threshold
threshold <- 4 / nobs(simple)

# Display the threshold
threshold

# Identify observations exceeding the threshold
which(cooks_d > threshold)


# Activity 4 --------------------------------------------------------------

# Fit the multiple regression model
multiple <- lm(
  sales_revenue_k ~ advertising_spend_k + region,
  data = stores
)

# Examine the regression results
summary(multiple)

# Obtain 95% confidence intervals
confint(multiple)

# Compare R-squared and adjusted R-squared
summary(simple)$r.squared
summary(multiple)$r.squared

summary(simple)$adj.r.squared
summary(multiple)$adj.r.squared

# Compare AIC
AIC(simple, multiple)

# Check Linearity
plot(multiple, which = 1)

# Check Independence of Errors
durbinWatsonTest(multiple)

# Check Normality of Residuals [QQ Plot]
qqPlot(multiple$residuals)

# Check Normality of Residuals [Shapiro-Wilk]
shapiro.test(multiple$residuals)

# Check Homoscedasticity [Plot]
plot(multiple, which = 3)

# Check Homoscedasticity [Breusch-Pagan test]
bptest(multiple)

# Check for Influential Observations [Cook's distance]
plot(multiple, which = 4)

# Calculate Cook's distance for each observation
cooks_d <- cooks.distance(multiple)

# Calculate the screening threshold
threshold <- 4 / nobs(multiple)

# Display the threshold
threshold

# Identify observations exceeding the threshold
which(cooks_d > threshold)

# Check Multicollinearity
vif(multiple)


# Activity 5 --------------------------------------------------------------

# Fit the full multiple regression model
full <- lm(
  sales_revenue_k ~ advertising_spend_k +
    region +
    store_size_sq_m +
    training_hours +
    wait_time_minutes,
  data = stores
)

# Examine the regression results
summary(full)

# Obtain the AIC
AIC(full)

# Perform bidirectional stepwise regression
selected <- step(
  full,
  direction = "both",
  trace = 1
)

# Display the final model
formula(selected)

# Examine the regression results
summary(selected)

# Compare AIC values
AIC(simple, multiple, full, selected)

# Create a summary table
model_comparison <- tibble(
  Model = c("Simple", "Multiple", "Full", "Stepwise"),
  R_squared = c(
    summary(simple)$r.squared,
    summary(multiple)$r.squared,
    summary(full)$r.squared,
    summary(selected)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(simple)$adj.r.squared,
    summary(multiple)$adj.r.squared,
    summary(full)$adj.r.squared,
    summary(selected)$adj.r.squared
  ),
  AIC = c(
    AIC(simple),
    AIC(multiple),
    AIC(full),
    AIC(selected)
  )
)

# Display the comparison
model_comparison

# Check the number of observations
nobs(simple)
nobs(multiple)
nobs(full)
nobs(selected)

# Calculate BIC for both models
BIC(full)
BIC(selected)

# Calculate the approximate Bayes Factor
BF <- exp((BIC(full) - BIC(selected)) / 2)

# Display the result
BF

# Check Linearity
plot(selected, which = 1)

# Check Independence of Errors
durbinWatsonTest(selected)

# Check Normality of Residuals [QQ Plot]
qqPlot(selected$residuals)

# Check Normality of Residuals [Shapiro-Wilk]
shapiro.test(selected$residuals)

# Check Homoscedasticity [Plot]
plot(selected, which = 3)

# Check Homoscedasticity [Breusch-Pagan test]
bptest(selected)

# Check for Influential Observations [Cook's distance]
plot(selected, which = 4)

# Calculate Cook's distance for each observation
cooks_d <- cooks.distance(selected)

# Calculate the screening threshold
threshold <- 4 / nobs(selected)

# Display the threshold
threshold

# Identify observations exceeding the threshold
which(cooks_d > threshold)

# Check the number of observations
nobs(simple)
nobs(multiple)
nobs(full)
nobs(selected)

# Examine correlations between numeric predictors
stores %>% 
  select(
    advertising_spend_k,
    store_size_sq_m,
    training_hours,
    wait_time_minutes
  ) %>% 
  cor()

# Check multicollinearity in the full model
vif(full)

# Check multicollinearity in the selected model
vif(selected)


# Activity 6 --------------------------------------------------------------

# Create a summary table of the regression models
model_summary <- tibble(
  Model = c("Simple", "Multiple", "Full", "Stepwise"),
  R_squared = c(
    summary(simple)$r.squared,
    summary(multiple)$r.squared,
    summary(full)$r.squared,
    summary(selected)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(simple)$adj.r.squared,
    summary(multiple)$adj.r.squared,
    summary(full)$adj.r.squared,
    summary(selected)$adj.r.squared
  ),
  AIC = c(
    AIC(simple),
    AIC(multiple),
    AIC(full),
    AIC(selected)
  )
)

# Display the results
model_summary

# Display regression coefficients and test statistics
summary(simple)

# Obtain 95% confidence intervals
confint(simple)

# Create new observations
new_stores <- data.frame(
  advertising_spend_k = c(15, 15, 15, 15),
  region = factor(
    c("South", "North", "East", "West"),
    levels = c("South", "North", "East", "West")
  )
)

# Predict sales revenue
new_stores$predicted_sales <- predict(
  multiple,
  newdata = new_stores
)

# Display predictions
new_stores

