# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)
library(lme4)
library(MASS)

# Read the pre-processed CSV file
hourly_averages <- read_csv('./hourly_averages.csv')

# Convert relevant columns to factors (categorical variables)
hourly_averages <- hourly_averages %>%
  mutate(
    hour = as.factor(hour),
    day = as.factor(day),
    week = as.factor(week),
    site_id = as.factor(site_id),
    year = as.factor(year),
    directory_pair = as.factor(directory_pair)
  )

# Ensure that the response variable is also correctly formatted
hourly_averages$people_counts <- as.integer(hourly_averages$people_counts)

# Define the formula for the mixed-effects model
# Fixed effects: year, hour, day, week, site_id
# Random effect: directory_pair
formula <- people_counts ~ year + hour + day + week + site_id + (1 | directory_pair)

# Fit the Negative Binomial mixed-effects model using glmer.nb
model_fit <- glmer.nb(formula, data = hourly_averages)

# Print the summary of the model
summary(model_fit)

