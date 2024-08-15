# Load necessary libraries
library(dplyr)
library(readr)
library(glmmTMB)
library(broom.mixed) # For tidying glmmTMB output

# Read the pre-processed CSV file
hourly_averages <- read_csv('./hourly_averages.csv')

# Get the unique site IDs
site_ids <- unique(hourly_averages$site_id)

# Define the super category to model (e.g., 'people')
super_category <- 'people'
response_variable <- paste0(super_category, '_counts')

# Prepare a data frame to store the R-squared values for each site
r2_results <- data.frame(site = character(), 
                         marginal_R2 = numeric(), 
                         conditional_R2 = numeric(), 
                         stringsAsFactors = FALSE)

# Loop over each site and fit the model
for (site in site_ids) {
  
  # Print the site ID to associate with the following model output
  cat("Processing site:", site, "\n")
  
  # Filter the data for the specific site
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
  print(summary(model_fit))
  
 # Compute the pseudo-R² values
  r2_values <- performance::r2(model_fit)
  
  # Print the R² values to the console
  print(r2_values)
  
  # Store the R² values in the results data frame
  r2_results <- r2_results %>%
    add_row(site = site, 
            marginal_R2 = r2_values$R2_marginal, 
            conditional_R2 = r2_values$R2_conditional)

  # Extract coefficients, confidence intervals, and exponentiate them
  # 1. Tidy all coefficients (fixed, random, zero-inflation)
  tidy_all <- tidy(model_fit, conf.int = TRUE, exponentiate = TRUE)
  
  # 2. Tidy only the fixed effects
  tidy_conditional <- tidy(model_fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
  
  # Define the output directory path
  output_dir <- "/outputs"
  
  # Create the directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save the results to CSV files
  output_all <- file.path(output_dir, paste0(site, '_all_model_coefficients.csv'))
  write_csv(tidy_all, output_all)
  
  output_conditional <- file.path(output_dir, paste0(site, '_conditional_model_coefficients.csv'))
  write_csv(tidy_conditional, output_conditional)
}

# Save the R² results to a CSV file
r2_output_path <- file.path(output_dir, 'pseudo_r2_results.csv')
write_csv(r2_results, r2_output_path)

# Print the R² results to the console
print(r2_results)
