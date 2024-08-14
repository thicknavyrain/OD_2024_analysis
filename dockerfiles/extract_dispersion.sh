#!/bin/bash

# Initialize output CSV
output_csv="/outputs/dispersion_parameters.csv"
echo "site,dispersion_parameter" > "$output_csv"

# Parse the log file and extract site and dispersion parameters
while read -r line; do
    if [[ $line == "Processing site:"* ]]; then
        site=$(echo $line | awk '{print $3}')
    fi

    if [[ $line == "Dispersion parameter for nbinom2 family ():"* ]]; then
        dispersion=$(echo $line | awk '{print $6}')
        echo "$site,$dispersion" >> "$output_csv"
    fi
done < /outputs/model_output.log

echo "Dispersion parameters saved to $output_csv"

