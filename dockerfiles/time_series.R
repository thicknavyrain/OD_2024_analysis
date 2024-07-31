# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(lme4)
library(MASS)

# Read the CSV file
object_data <- read_csv('./2024_Jul_ob_count.csv')

# Create category and count columns
categories <- c('car', 'person', 'trotro', 'stall', 'truck', 'stove', 'motorcycle', 'vendor', 'lorry', 'umbrella', 
                'bus', 'trash', 'taxi', 'van', 'debris', 'loudspeaker', 'bowl', 'food', 'animal', 'bicycle')

count_cols <- paste0(categories, '_counts')

super_count_cols <- c('people_counts', 'small_vehicles_counts', 'two_wheelers_counts', 
                      'large_vehicles_counts', 'refuse_counts', 'market_counts', 'animal_counts')

all_count_cols <- c(count_cols, super_count_cols)

vehicle_categories <- c('car', 'trotro', 'truck', 'motorcycle', 'lorry', 'bus', 'taxi', 'van', 'bicycle')

# Define super categories
super_categories <- list(
  people = c('person', 'vendor'),
  small_vehicles = c('car', 'taxi', 'truck'),
  two_wheelers = c('bicycle', 'motorcycle'),
  large_vehicles = c('trotro', 'van', 'lorry', 'bus'),
  refuse = c('trash', 'debris'),
  market = c('umbrella', 'stall', 'bowl', 'food'),
  animal = c('animal')
)

# Ensure datetime is in datetime format
object_data$datetime <- ymd_hms(object_data$datetime_rectified)

# Create additional time-related columns
object_data <- object_data %>%
  mutate(
    hour = hour(datetime),
    day = wday(datetime, week_start = 1),  # Monday as the start of the week
    week = isoweek(datetime),
    year = year(datetime)
  )

# Split 'site_id_cam_angle' into 'site_id' and 'camera' columns
object_data <- object_data %>%
  separate(site_id_cam_angle, into = c('site_id', 'camera'), sep = '_', extra = 'merge', fill = 'right')

# Fill missing values in 'camera' with 'single'
object_data$camera[is.na(object_data$camera)] <- 'single'

# Filter data between specified dates
start_date <- ymd('2019-04-01')
end_date <- ymd('2024-04-01')
fixed_object_data <- object_data %>%
  filter(datetime >= start_date & datetime <= end_date, view == 'clear')

# Sum counts for each super category
for (super_cat in names(super_categories)) {
  fixed_object_data <- fixed_object_data %>%
    mutate(!!paste0(super_cat, '_counts') := rowSums(across(all_of(paste0(super_categories[[super_cat]], '_counts)))))
}

# Round 'datetime' to the nearest hour
fixed_object_data <- fixed_object_data %>%
  mutate(datetime_hour = floor_date(datetime, "hour"))

# Sum the counts within each hour for each camera at each site
hourly_counts <- fixed_object_data %>%
  group_by(site_id, camera, datetime_hour) %>%
  summarise(across(all_of(all_count_cols), sum, na.rm = TRUE)) %>%
  ungroup()

# Calculate the mean for each object category across cameras
hourly_averages <- hourly_counts %>%
  group_by(site_id, datetime_hour) %>%
  summarise(across(all_of(all_count_cols), mean, na.rm = TRUE)) %>%
  ungroup()

# Add additional time-related columns
hourly_averages <- hourly_averages %>%
  mutate(
    date = as.Date(datetime_hour),
    hour = as.factor(hour(datetime_hour)),
    day = as.factor(wday(datetime_hour, week_start = 1)),
    week = as.factor(isoweek(datetime_hour)),
    year = as.factor(year(datetime_hour)),
    site_id = as.factor(site_id)
  )

# Define the counts column to model (e.g., 'people_counts')
counts_column <- 'people_counts'

# Construct the formula for the mixed-effects model with a negative binomial family
formula <- as.formula(paste0(counts_column, " ~ year + (1|hour) + (1|day) + (1|week) + (1|site_id)"))

# Fit the model using glmer.nb (generalized linear mixed-effects model with Negative Binomial family)
model_fit <- glmer.nb(formula, data = hourly_averages)

# Print the summary of the model
summary(model_fit)

