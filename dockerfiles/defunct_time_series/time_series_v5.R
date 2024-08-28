# Load necessary libraries
library(dplyr)
library(readr)
library(glmmTMB)

# Read the pre-processed CSV file
hourly_averages <- read_csv('./hourly_averages.csv')

# Filter the data for the specific site ('ASH')
site <- 'ASH'
filtered_data <- hourly_averages %>% filter(site_id == site)

# Convert relevant columns to factors (categorical variables)
filtered_data <- filtered_data %>%
  mutate(
    hour = as.factor(hour),
    day = as.factor(day),
    week = as.factor(week),
    year = as.factor(year),
    directory_pair = as.factor(directory_pair)
  )

# Define the super category to model (e.g., 'people')
super_category <- 'people'
response_variable <- paste0(super_category, '_counts')

# Define the formula for the zero-inflated negative binomial mixed-effects model
# Fixed effects: year, hour, day, week
# Random effect: directory_pair
formula <- as.formula(paste(response_variable, "~ year + hour + day + week + (1 | directory_pair)"))

# Fit the zero-inflated negative binomial mixed-effects model using glmmTMB
model_fit <- glmmTMB(formula, 
                     data = filtered_data, 
                     ziformula = ~1,         # Zero-inflation formula
                     family = nbinom2())     # Negative binomial family

# Print the summary of the model
summary(model_fit)

