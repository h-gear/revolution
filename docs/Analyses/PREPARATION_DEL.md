# Preprocessing raw data on Letters of Delegates to Congress

## 0. GOAL
The script aims to preprocess raw data on Letters of Delegates to Congress (1774-1789) for future temporal network and SHICO analysis.

## 1. LOAD IN LIBRARIES
```r
library(tidyverse) # For data wrangling
library(polyglotr) # For text translation and manipulation
```

## 2. DATA PREPROCESSING

### A) Volume and Year Variables
**Objective:** Extract and clean the volume number, period, and year from the dataset.

**Steps:**
1. Split the text column to separate Volume and Content.
2. Extract volume number and period from the Volume column.
3. Extract the year from the Content column.
4. Handle cases where the title is not present and clean it.

### B) Improve Information on Year
**Objective:** Correct and fill in missing or incorrect year information.

**Steps:**
1. Define a function to extract year indices from text.
2. Loop through each row to verify and update the Year column based on extracted year indices.

### C) Title, From, and To Variables
**Objective:** Clean and standardize title, sender, and recipient information.

**Steps:**
1. Use predefined separators to handle titles without proper delimiters.
2. Clean the title by removing specific words, symbols, and correcting anomalies.
3. Separate titles into From and To columns based on the presence of " to ".
4. Remove or update unknown sender or recipient information.

### D) Letter Contents Variable
**Objective:** Clean and format the content of letters.

**Steps:**
1. Remove editor notes and special characters.
2. Use a function to display letters with author and recipient details.
3. Clean Content2 by removing unwanted text and special characters.

### E) Date Variable
**Objective:** Extract and standardize date information from letter content.

**Steps:**
1. Define a function to clean and process date information.
2. Standardize date formats and handle manual corrections for specific IDs.
3. Convert the cleaned dates to Date format and filter data based on the specified date range.

## 3. SAVING PREPROCESSED DATA

### A1) All Data in CSV File
**Objective:** Save the preprocessed dataset to a CSV file.

**Steps:**
1. Select relevant columns and rename them.
2. Convert column names to lowercase and write the data to a CSV file.

### A2) All Data in RDS File
**Objective:** Save the preprocessed dataset to an RDS file.

**Steps:**
1. Save the data to an RDS file for further use.

### B) For Making Word2Vec Models (SHICO)
**Objective:** Prepare a dataset for Word2Vec model building.

**Steps:**
1. Select relevant columns and save them to a CSV file.

### C) Letters as Separate Text Files
**Objective:** Save each letter as a separate text file for further processing.

**Steps:**
1. Loop through volume numbers, read RDS files, clean text, and save each letter as a text file in a corresponding folder.

### D) Letters Per Year
**Objective:** Split data by year and save to CSV files.

**Steps:**
1. Define a time period and filter data by year (commented out in the script).

**Note:** Standard cleaning of letter texts for Word2Vec models is performed in a separate Python script (`make_word2vec_models_for_delegates.py`), found in `w2v_models/delegates`. This script preprocesses the data to develop Word2Vec models for SHICO analysis.
