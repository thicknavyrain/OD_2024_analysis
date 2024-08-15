import csv

# Open the input file
with open('hourly_averages.csv', 'r') as infile:
    reader = csv.reader(infile)
    header = next(reader)  # Read the header row

    # Track the indices of the columns to keep
    keep_indices = []
    seen_animal_counts = False

    # Determine which columns to keep
    for i, column_name in enumerate(header):
        if column_name == 'animal_counts':
            if not seen_animal_counts:
                # Keep the first 'animal_counts' column
                keep_indices.append(i)
                seen_animal_counts = True
        else:
            # Keep all other columns
            keep_indices.append(i)

    # Prepare the new header based on columns to keep
    new_header = [header[i] for i in keep_indices]

    # Write the updated CSV
    with open('hourly_averages_cleaned.csv', 'w', newline='') as outfile:
        writer = csv.writer(outfile)
        writer.writerow(new_header)  # Write the new header

        # Write the rows, keeping only the desired columns
        for row in reader:
            new_row = [row[i] for i in keep_indices]
            writer.writerow(new_row)

print("Duplicate 'animal_counts' columns removed, except for the first one. Output saved to 'hourly_averages_cleaned.csv'.")

