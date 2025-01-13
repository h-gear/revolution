import pandas as pd
from collections import defaultdict
from datetime import timedelta

path = ['1696','51','564','2031','51','564','696','564','696','564','51','4948']
path = ['1696','51','564','2031','51','564','1022']


# Original data - assuming it's a DataFrame with columns ['sender_id', 'receiver_id', 'date']
df = pd.read_csv('C:/Projects/AmericanRevolution/revolution/data/processed/founders/letters_with_topic_info.csv')
df = df[df['topic'] == "02_Republican politics"] 
df.info()

# Ensure sender_id and receiver_id are strings
df['sender_id'] = df['sender_id'].astype(str)
df['receiver_id'] = df['receiver_id'].astype(str)
# Convert sending_date column to datetime for time interval comparison
df['sending_date'] = pd.to_datetime(df['sending_date'])

# Create a lookup dictionary from your original dataframe
original_data_lookup = defaultdict(list)
for _, row in df.iterrows():
    original_data_lookup[(row['sender_id'], row['receiver_id'])].append(row)

# Function to extract consecutive pairs from the path
def extract_pairs(path):
    return [(path[i], path[i + 1]) for i in range(len(path) - 1)]

# Function to connect the path to the original data and check the date constraints
def connect_path_to_data(path, original_data_lookup, max_days=50):
    pairs = extract_pairs(path)  # Extract pairs from the path
    path_rows = []                # Store valid rows in order
    previous_row = None           # Track the last selected row

    for sender, receiver in pairs:
        # Look up the original rows for the current pair
        rows = original_data_lookup.get((sender, receiver), [])
        
        print(f"Checking pair: ({sender}, {receiver}) - Available rows: {len(rows)}")  # Debugging line
        
        if not rows:
            print(f"No data found for pair: ({sender}, {receiver})")
            return None
        
        # Sort the rows by sending_date to maintain chronological order
        rows = sorted(rows, key=lambda x: x['sending_date'])
        
        if previous_row is not None:
            # Filter rows that are valid based on the date constraints
            valid_rows = [
                row for row in rows
                if row['sending_date'] > previous_row['sending_date'] and
                (row['sending_date'] - previous_row['sending_date']) <= timedelta(days=max_days)
            ]
        else:
            valid_rows = rows  # If no previous row, take all
        
        print(f"Valid rows for ({sender}, {receiver}): {[row['sending_date'] for row in valid_rows]}")  # Debugging line
        
        if valid_rows:
            # Select the first valid row and append to the result
            chosen_row = valid_rows[0]
            path_rows.append(chosen_row)  # Append chosen row
            previous_row = chosen_row       # Update previous_row for next iteration
        else:
            print(f"No valid consecutive date for pair: ({sender}, {receiver})")
            return None

    return path_rows  # Return the list of valid rows

path_rows = connect_path_to_data(path, original_data_lookup)

# Convert the result into a dataframe if path_rows are found
if path_rows:
    relevant_df = pd.DataFrame(path_rows)
    print(relevant_df)
else:
    print("No valid path found.")
        
# Create a new DataFrame from the collected rows
new_df = pd.DataFrame(path_rows)

# Save the new DataFrame to a CSV file
new_df.to_csv('C:/Projects/AmericanRevolution/revolution/data/processed/founders/path_rows6.csv', index=False)
new_df.info()