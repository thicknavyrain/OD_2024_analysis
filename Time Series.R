{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 1,
   "id": "c6819d34-9108-4237-ab3e-efa25b1f1ac0",
   "metadata": {},
   "outputs": [
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "Warning message in install.packages(\"nloptr\"):\n",
      "“installation of package ‘nloptr’ had non-zero exit status”\n",
      "Updating HTML index of packages in '.Library'\n",
      "\n",
      "Making 'packages.html' ...\n",
      " done\n",
      "\n",
      "also installing the dependency ‘nloptr’\n",
      "\n",
      "\n",
      "Warning message in install.packages(\"lme4\"):\n",
      "“installation of package ‘nloptr’ had non-zero exit status”\n",
      "Warning message in install.packages(\"lme4\"):\n",
      "“installation of package ‘lme4’ had non-zero exit status”\n",
      "Updating HTML index of packages in '.Library'\n",
      "\n",
      "Making 'packages.html' ...\n",
      " done\n",
      "\n"
     ]
    }
   ],
   "source": [
    "install.packages(\"nloptr\")\n",
    "install.packages(\"lme4\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "61f7e73d-f84a-40bf-afba-a466cb4dcceb",
   "metadata": {},
   "outputs": [],
   "source": [
    "library(lme4)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "9bed8d10-29f8-4d48-8e4d-763344f2568a",
   "metadata": {
    "scrolled": true
   },
   "outputs": [],
   "source": [
    "# Load necessary libraries\n",
    "library(dplyr)\n",
    "library(lubridate)\n",
    "library(readr)\n",
    "library(tidyr)  \n",
    "\n",
    "# Read the CSV file\n",
    "object_data <- read_csv('./2024_Jul_ob_count.csv')\n",
    "\n",
    "# Create category and count columns\n",
    "categories <- c('car', 'person', 'trotro', 'stall', 'truck', 'stove', 'motorcycle', 'vendor', 'lorry', 'umbrella', \n",
    "                'bus', 'trash', 'taxi', 'van', 'debris', 'loudspeaker', 'bowl', 'food', 'animal', 'bicycle')\n",
    "\n",
    "count_cols <- paste0(categories, '_counts')\n",
    "\n",
    "super_count_cols <- c('people_counts', 'small_vehicles_counts', 'two_wheelers_counts', \n",
    "                      'large_vehicles_counts', 'refuse_counts', 'market_counts', 'animal_counts')\n",
    "\n",
    "all_count_cols <- c(count_cols, super_count_cols)\n",
    "\n",
    "vehicle_categories <- c('car', 'trotro', 'truck', 'motorcycle', 'lorry', 'bus', 'taxi', 'van', 'bicycle')\n",
    "\n",
    "# Define super categories\n",
    "super_categories <- list(\n",
    "  people = c('person', 'vendor'),\n",
    "  small_vehicles = c('car', 'taxi', 'truck'),\n",
    "  two_wheelers = c('bicycle', 'motorcycle'),\n",
    "  large_vehicles = c('trotro', 'van', 'lorry', 'bus'),\n",
    "  refuse = c('trash', 'debris'),\n",
    "  market = c('umbrella', 'stall', 'bowl', 'food'),\n",
    "  animal = c('animal')\n",
    ")\n",
    "\n",
    "# Ensure datetime is in datetime format\n",
    "object_data$datetime <- ymd_hms(object_data$datetime_rectified)\n",
    "\n",
    "# Create additional time-related columns\n",
    "object_data <- object_data %>%\n",
    "  mutate(\n",
    "    hour = hour(datetime),\n",
    "    day = wday(datetime, week_start = 1),  # Monday as the start of the week\n",
    "    week = isoweek(datetime),\n",
    "    year = year(datetime)\n",
    "  )\n",
    "\n",
    "# Split 'site_id_cam_angle' into 'site_id' and 'camera' columns\n",
    "object_data <- object_data %>%\n",
    "  separate(site_id_cam_angle, into = c('site_id', 'camera'), sep = '_', extra = 'merge', fill = 'right')\n",
    "\n",
    "# Fill missing values in 'camera' with 'single'\n",
    "object_data$camera[is.na(object_data$camera)] <- 'single'\n",
    "\n",
    "# Filter data between specified dates\n",
    "start_date <- ymd('2019-04-01')\n",
    "end_date <- ymd('2024-04-01')\n",
    "fixed_object_data <- object_data %>%\n",
    "  filter(datetime >= start_date & datetime <= end_date, view == 'clear')\n",
    "\n",
    "# Sum counts for each super category\n",
    "for (super_cat in names(super_categories)) {\n",
    "  fixed_object_data <- fixed_object_data %>%\n",
    "    mutate(!!paste0(super_cat, '_counts') := rowSums(select(., all_of(paste0(super_categories[[super_cat]], '_counts'))), na.rm = TRUE))\n",
    "}\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "30f67488-3021-4b49-8f66-ed9d15881db8",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Load necessary libraries for the mixed-effects model\n",
    "library(lme4)\n",
    "\n",
    "# Define the counts column to model (e.g., 'car_counts')\n",
    "counts_column <- 'people_counts'\n",
    "\n",
    "# Construct the formula for the mixed-effects model\n",
    "formula <- as.formula(paste0(counts_column, \" ~ (1|hour) + (1|day) + (1|week) + year + (1|site_id)\"))\n",
    "\n",
    "# Fit the model using glmer (generalized linear mixed-effects model with Poisson family)\n",
    "model_fit <- glmer(formula, data = fixed_object_data, family = poisson(link = \"log\"))\n",
    "\n",
    "# Print the summary of the model\n",
    "summary(model_fit)\n"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "4.3.1"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
