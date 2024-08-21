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

# Prepare a data frame to store the R-squared values for each site, super category, and fold
r2_results <- data.frame(site = character(), 
                         super_category = character(),
                         fold = integer(),
                         marginal_R2 = numeric(), 
                         conditional_R2 = numeric(), 
                         stringsAsFactors = FALSE)

# Loop over each site, super category, and fold to fit the model
for (site in site_ids) {
  for (super_category in super_categories) {
    for (fold in 1:5) {
      
      # Print the site ID, super category, and fold to associate with the following model output
      cat("Processing site:", site, "with super category:", super_category, "on fold:", fold, "\n")
      
      # Filter the data for the specific site and fold
      training_data <- hourly_averages %>% filter(site_id == site, fold != fold)
      test_data <- hourly_averages %>% filter(site_id == site, fold == fold)
      
      # Convert relevant columns to factors (categorical variables)
      training_data <- training_data %>%
        mutate(
          hour = as.factor(hour),
          day = as.factor(day),
          week = as.factor(week),
          year = as.factor(year),
          directory_pair = as.factor(directory_pair)
        )
      
      # Diagnostic Print: Check unique levels for each factor
      cat("Unique levels for 'hour':", length(unique(training_data$hour)), "\n")
      cat("Unique levels for 'day':", length(unique(training_data$day)), "\n")
      cat("Unique levels for 'week':", length(unique(training_data$week)), "\n")
      cat("Unique levels for 'year':", length(unique(training_data$year)), "\n")
      cat("Unique levels for 'directory_pair':", length(unique(training_data$directory_pair)), "\n")
      
      # Check if any factor has less than two levels
      if (length(unique(training_data$hour)) < 2 || length(unique(training_data$day)) < 2 || 
          length(unique(training_data$week)) < 2 || length(unique(training_data$year)) < 2 || 
          length(unique(training_data$directory_pair)) < 2) {
        cat("Skipping fold due to a factor with less than two levels.\n")
        next
      }
      
      # Define the response variable based on the current super category
      response_variable <- paste0(super_category, '_counts')
      
      # Define the formula for the zero-inflated negative binomial mixed-effects model
      formula <- as.formula(paste(response_variable, "~ year + hour + day + week + (1 | directory_pair)"))
      
      # Fit the zero-inflated negative binomial mixed-effects model using glmmTMB
      model_fit <- glmmTMB(formula, 
                           data = training_data, 
                           ziformula = ~1,         # Zero-inflation formula
                           family = nbinom2(),     # Negative binomial family
                           control = glmmTMBControl(
                              optCtrl = list(iter.max = 100, eval.max = 100)
                           ), verbose = TRUE)
      
      # Print the summary of the model
      print(summary(model_fit))
      
      # Calculate the pseudo-R² values on the test set
      r2_values <- performance::r2(model_fit, newdata = test_data)
      
      # Print the R² values to the console
      print(r2_values)
      
      # Store the R² values in the results data frame
      r2_results <- r2_results %>%
        add_row(site = site, 
                super_category = super_category,
                fold = fold,
                marginal_R2 = r2_values$R2_marginal, 
                conditional_R2 = r2_values$R2_conditional)
      
      # Extract coefficients, confidence intervals, and exponentiate them
      # 1. Tidy all coefficients (fixed, random, zero-inflation)
      tidy_all <- tidy(model_fit, conf.int = TRUE, exponentiate = TRUE)
      
      # 2. Tidy only the fixed effects
      tidy_conditional <- tidy(model_fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
      
      # Define the output directory path
      output_dir <- paste0("/outputs/fold_", fold)
      
      # Create the directory if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      # Save the results to CSV files, labeled by site, super category, and fold
      output_all <- file.path(output_dir, paste0(site, '_', super_category, '_fold_', fold, '_all_model_coefficients.csv'))
      write_csv(tidy_all, output_all)
      
      output_conditional <- file.path(output_dir, paste0(site, '_', super_category, '_fold_', fold, '_conditional_model_coefficients.csv'))
      write_csv(tidy_conditional, output_conditional)

      # Generate predictions for unique combinations in the test set
      unique_combinations_fixed <- test_data %>%
        select(hour, day, week, year) %>%
        distinct()
      
      unique_combinations_random <- test_data %>%
        select(hour, day, week, year, directory_pair) %>%
        distinct()

      # Predict using the model for these unique combinations with fixed effects only
      predictions_fixed_only <- predict(model_fit, newdata = unique_combinations_fixed, type = "response", re.form = NA)
      
      # Predict using the model for these unique combinations including random effects
      predictions_with_random <- predict(model_fit, newdata = unique_combinations_random, type = "response")
      
      # Add the predictions and actual counts from the test set to the prediction dataframes
      prediction_results_fixed <- unique_combinations_fixed %>%
        mutate(predicted_counts_fixed_only = predictions_fixed_only)
      
      prediction_results_random <- unique_combinations_random %>%
        mutate(predicted_counts_with_random = predictions_with_random)
      
      # Join predictions with actual test set values
      test_set_actual <- test_data %>%
        select(hour, day, week, year, directory_pair, !!response_variable)
      
      prediction_results <- prediction_results_fixed %>%
        left_join(prediction_results_random, by = c("hour", "day", "week", "year")) %>%
        left_join(test_set_actual, by = c("hour", "day", "week", "year", "directory_pair"))

      # Save the prediction results to a CSV file, labeled by site, super category, and fold
      output_predictions <- file.path(output_dir, paste0(site, '_', super_category, '_fold_', fold, '_model_predictions.csv'))
      write_csv(prediction_results, output_predictions)
    }
  }
}

# Save the R² results to a CSV file, labeled by fold
r2_output_path <- file.path("/outputs", 'pseudo_r2_results.csv')
write_csv(r2_results, r2_output_path)

# Print the R² results to the console
print(r2_results)

