# Load necessary libraries
library(dplyr)
library(readr)
library(glmmTMB)
library(broom.mixed) # For tidying glmmTMB output
library(performance) # For calculating R²

# Read the pre-processed CSV file
hourly_averages <- read_csv('./hourly_averages_v3.csv')

# Get the unique site IDs
site_ids <- unique(hourly_averages$site_id)

# Define the super categories to model
super_categories <- c('people', 'small_vehicles', 'large_vehicles', 'market', 'two_wheelers', 'refuse', 'animal')

# Prepare a data frame to store the R-squared values for each site and super category
r2_results <- data.frame(site = character(), 
                         super_category = character(),
                         marginal_R2 = numeric(), 
                         conditional_R2 = numeric(), 
                         stringsAsFactors = FALSE)

# Loop over each site and super category to fit the model on the full dataset
for (site in site_ids) {
  for (super_category in super_categories) {
    
    # Print the site ID and super category to associate with the following model output
    cat("Processing site:", site, "with super category:", super_category, "\n")
    
    # Filter the data for the specific site
    site_data <- hourly_averages %>% filter(site_id == site)
    
    # Convert relevant columns to factors (categorical variables)
    site_data <- site_data %>%
      mutate(
        hour = as.factor(hour),
        day = as.factor(day),
        week = as.factor(week),
        year = as.factor(year),
        directory_pair = as.factor(directory_pair)
      )
    
    # Diagnostic Print: Check unique levels for each factor
    cat("Unique levels for 'hour':", length(unique(site_data$hour)), "\n")
    cat("Unique levels for 'day':", length(unique(site_data$day)), "\n")
    cat("Unique levels for 'week':", length(unique(site_data$week)), "\n")
    cat("Unique levels for 'year':", length(unique(site_data$year)), "\n")
    cat("Unique levels for 'directory_pair':", length(unique(site_data$directory_pair)), "\n")
    
    # Check if any factor has less than two levels
    if (length(unique(site_data$hour)) < 2 || length(unique(site_data$day)) < 2 || 
        length(unique(site_data$week)) < 2 || length(unique(site_data$year)) < 2 || 
        length(unique(site_data$directory_pair)) < 2) {
      cat("Skipping due to a factor with less than two levels.\n")
      next
    }
    
    # Define the response variable based on the current super category
    response_variable <- paste0(super_category, '_counts')
    
    # Define the formula for the zero-inflated negative binomial mixed-effects model
    formula <- as.formula(paste(response_variable, "~ year + hour * day + week + (1 | directory_pair)"))
    
    # Fit the zero-inflated negative binomial mixed-effects model using glmmTMB
    model_fit <- glmmTMB(formula, 
                         data = site_data, 
                         ziformula = ~1,         # Zero-inflation formula
                         family = nbinom2(),     # Negative binomial family
                         control = glmmTMBControl(
                            optCtrl = list(iter.max = 100, eval.max = 100)
                         ), verbose = TRUE)
    
    # Print the summary of the model
    print(summary(model_fit))
    
    # Calculate the pseudo-R² values on the full dataset
    r2_values <- performance::r2(model_fit)
    
    # Print the R² values to the console
    print(r2_values)
    
    # Store the R² values in the results data frame
    r2_results <- r2_results %>%
      add_row(site = site, 
              super_category = super_category,
              marginal_R2 = r2_values$R2_marginal, 
              conditional_R2 = r2_values$R2_conditional)
    
    # Extract coefficients, confidence intervals, and exponentiate them
    # 1. Tidy all coefficients (fixed, random, zero-inflation)
    tidy_all <- tidy(model_fit, conf.int = TRUE, exponentiate = TRUE)
    
    # 2. Tidy only the fixed effects
    tidy_conditional <- tidy(model_fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
    
    # Define the output directory path
    output_dir <- "/outputs/interaction_full_data"
    
    # Create the directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Save the results to CSV files, labeled by site and super category
    output_all <- file.path(output_dir, paste0(site, '_', super_category, '_all_model_coefficients.csv'))
    write_csv(tidy_all, output_all)
    
    output_conditional <- file.path(output_dir, paste0(site, '_', super_category, '_conditional_model_coefficients.csv'))
    write_csv(tidy_conditional, output_conditional)

    # Generate predictions for unique combinations in the dataset
    unique_combinations_fixed <- site_data %>%
      select(hour, day, week, year) %>%
      distinct()
    
    unique_combinations_random <- site_data %>%
      select(hour, day, week, year, directory_pair) %>%
      distinct()

    # Predict using the model for these unique combinations with fixed effects only
    predictions_fixed_only <- predict(model_fit, newdata = unique_combinations_fixed, type = "response", re.form = NA)
    
    # Predict using the model for these unique combinations including random effects
    predictions_with_random <- predict(model_fit, newdata = unique_combinations_random, type = "response")
    
    # Add the predictions and actual counts from the dataset to the prediction dataframes
    prediction_results_fixed <- unique_combinations_fixed %>%
      mutate(predicted_counts_fixed_only = predictions_fixed_only)
    
    prediction_results_random <- unique_combinations_random %>%
      mutate(predicted_counts_with_random = predictions_with_random)
    
    # Join predictions with actual dataset values
    dataset_actual <- site_data %>%
      select(hour, day, week, year, directory_pair, !!response_variable)
    
    prediction_results <- prediction_results_fixed %>%
      left_join(prediction_results_random, by = c("hour", "day", "week", "year")) %>%
      left_join(dataset_actual, by = c("hour", "day", "week", "year", "directory_pair"))

    # Save the prediction results to a CSV file, labeled by site and super category
    output_predictions <- file.path(output_dir, paste0(site, '_', super_category, '_model_predictions.csv'))
    write_csv(prediction_results, output_predictions)
  }
}

# Save the R² results to a CSV file
r2_output_path <- file.path("/outputs", 'pseudo_r2_results_full_data.csv')
write_csv(r2_results, r2_output_path)

# Print the R² results to the console
print(r2_results)

