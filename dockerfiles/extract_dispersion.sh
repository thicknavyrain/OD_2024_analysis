#!/bin/bash

# Initialize output CSV
output_csv="dispersion_parameters.csv"
echo "site,dispersion_parameter" > "$output_csv"

# Parse the log file and extract site and dispersion parameters
while read -r line; do
    if [[ $line == "Processing site:"* ]]; then
        site=$(echo $line | awk '{print $3}')
    fi
    if [[ $line == "Dispersion parameter for nbinom2 family (): "* ]]; then
        dispersion=$(echo $line | awk '{print $7}')
        echo "$site,$dispersion" >> "$output_csv"
    fi
done < model_output.log

echo "Dispersion parameters saved to $output_csv"

