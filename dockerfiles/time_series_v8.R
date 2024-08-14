# Load necessary libraries
library(dplyr)
library(readr)
library(glmmTMB)
library(broom.mixed) # For tidying glmmTMB output

# Read the pre-processed CSV file
hourly_averages <- read_csv('./hourly_averages.csv')

# Filter the data for the specific site ('ASH')
site <- 'EL'
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
                     family = nbinom2(),     # Negative binomial family
                     control = glmmTMBControl(
                        optCtrl = list(iter.max = 100, eval.max = 100)
                     ), verbose = TRUE)

# Print the summary of the model
summary(model_fit)

# Extract coefficients, confidence intervals, and exponentiate them
tidy_result <- tidy(model_fit, conf.int = TRUE, exponentiate = TRUE)

# Filter out zero-inflation coefficients, keep only the conditional model
tidy_result <- tidy_result %>%
  filter(effect == "fixed") %>%
  select(term, estimate, conf.low, conf.high)

# Define the output directory path
output_dir <- "/outputs"

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the results to a CSV file in the /outputs directory
output_file <- file.path(output_dir, paste0(site, '_model_coefficients.csv'))
write_csv(tidy_result, output_file)

# Calculate and print pseudo-R² (McFadden's R²)
null_model <- glmmTMB(formula, data = filtered_data, ziformula = ~1, family = nbinom2(), doFit = FALSE)
null_logLik <- null_model$logLik # Log-likelihood of the null model
model_logLik <- logLik(model_fit) # Log-likelihood of the fitted model
pseudo_r2 <- 1 - as.numeric(model_logLik) / as.numeric(null_logLik)

cat("Pseudo-R² (McFadden's):", pseudo_r2, "\n")

