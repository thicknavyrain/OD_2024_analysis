import os
import shutil
import re

# Define the source and target directories
source_dir = './time_series'  # Replace with the path to your 'time_series' directory
target_dir = './time_series_by_category'  # Replace with the path to your 'time_series_by_category' directory

# Define the regex patterns to match the specific files
patterns = {
    'hour': re.compile(r'Aggregated_Effect_of_Hour_of_Day_on_[\w_]+_Counts_aggregated\.png'),
    'day': re.compile(r'Aggregated_Effect_of_Day_of_Week_on_[\w_]+_Counts_aggregated\.png'),
    'week': re.compile(r'Aggregated_Effect_of_Week_on_[\w_]+_Counts_aggregated\.png'),
    'year': re.compile(r'Aggregated_Effect_of_Year_on_[\w_]+_Counts_aggregated\.png')
}

# Iterate through the directories
for site in os.listdir(source_dir):
    site_path = os.path.join(source_dir, site)
    if os.path.isdir(site_path):
        for obj_category in os.listdir(site_path):
            obj_path = os.path.join(site_path, obj_category)
            if os.path.isdir(obj_path):
                for file in os.listdir(obj_path):
                    for period, pattern in patterns.items():
                        if pattern.match(file):
                            # Create the target directory if it doesn't exist
                            target_period_dir = os.path.join(target_dir, obj_category, period)
                            os.makedirs(target_period_dir, exist_ok=True)
                            
                            # Construct the source file path and the new filename with the site prefix
                            source_file = os.path.join(obj_path, file)
                            new_filename = f"{site}_{file}"
                            target_file = os.path.join(target_period_dir, new_filename)
                            
                            # Copy the file
                            shutil.copy2(source_file, target_file)
                            print(f"Copied {source_file} to {target_file}")
