FROM rocker/tidyverse:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    cmake \
    libnlopt-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('dplyr', 'nloptr', 'lme4', 'lubridate', 'readr', 'tidyr', 'glmmTMB','broom.mixed', 'performance'), repos='http://cran.rstudio.com/')"

# Copy your R script into the Docker image
COPY time_series.R /local/time_series.R
COPY time_series_all_cat.R /local/time_series_all_cat.R
COPY time_series_predictions.R /local/time_series_predictions.R
# COPY time_series_AD.R /local/time_series_AD.R
COPY hourly_averages_v3.csv /local/hourly_averages_v3.csv

