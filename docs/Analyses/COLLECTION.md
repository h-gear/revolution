# Letters of Delegates to Congress: Data scraping

This documentation outlines the process of scraping, cleaning, and storing data from the "Letters of Delegates to Congress (1774-1789)". The goal is to extract all relevant text data for further analysis, such as temporal network analysis and SHiCo analysis.

## Overview

### Objective

The main objective is to scrape and save raw data from the "Letters of Delegates to Congress (1774-1789)" website hosted by the Library of Congress. This dataset will be used for various analytical purposes.

### Workflow

1. **Loading Required Libraries**: Begin by loading the necessary R packages to facilitate web scraping and data manipulation.
2. **Scraping the Target Website**: Extract URLs and corresponding text data from the Library of Congress website.
3. **Data Cleaning**: Process the scraped text to remove unnecessary content and standardize the format.
4. **Matching ID to Text**: Organize and structure the text data by linking it with relevant identifiers.
5. **Saving Raw Data**: Store the cleaned data into files, either by individual volumes or as a combined dataset for further analysis.

## Step-by-Step Guide

### 1. Loading Required Libraries

To start, load the essential libraries needed for web scraping and data manipulation:

- **`rvest`**: For web scraping.
- **`xml2`**: To parse HTML content.
- **`tidyverse`**: For data wrangling and manipulation.

These libraries enable the scraping of HTML content from the web, parsing the data, and organizing it for further analysis.

### 2. Scraping the Target Website

The main task here is to scrape the text data from the "Letters of Delegates to Congress" available on the Library of Congress website. The process starts by accessing the base URL and extracting the relevant HTML nodes that contain the URLs to each volume of the letters.

These URLs are then compiled into a list, which will be iterated over to scrape the text data. Each volume contains multiple documents, and the code captures the links to these documents, scrapes the text, and stores it for later use.

### 3. Data Cleaning

Once the text data has been scraped, it requires cleaning to remove any extraneous content that is not relevant to the analysis. The cleaning process involves:

- **Removing Unwanted Text**: Stripping out repeated headers, footers, and navigation text that appear throughout the documents.
- **Filtering Content**: Excluding empty rows or rows with irrelevant data (e.g., "Page" headers).
- **Standardizing Format**: Ensuring that the remaining text is formatted consistently, which involves removing excess whitespace and duplicate rows.

This cleaned text will then be structured for further analysis.

### 4. Matching ID to Text

After cleaning, the data is further processed to match each piece of text with its corresponding identifier. This step involves organizing the text by assigning IDs to ensure each entry is correctly associated with its source. The cleaned and structured data will be stored in a tabular format, with the text content linked to its identifier.

This structured format makes it easier to analyze and query the data, ensuring that each letter or document is correctly identified and accessible.

### 5. Saving Raw Data

Finally, the cleaned and organized data is saved for future use. Depending on your needs, the data can be stored in individual files corresponding to each volume or combined into a single dataset:

- **Individual Volume Files**: If you need to analyze or review specific volumes, you can save each volume as a separate file.
- **Combined Dataset**: For broader analyses, all volumes can be merged into a single dataset and saved accordingly.

The data is saved in `.rds` format, which preserves the structure and content of the data while being efficient in storage. This final step ensures that the raw data is ready for any further analysis you plan to perform.

## Conclusion

This process allows you to efficiently scrape, clean, and store a large dataset from the "Letters of Delegates to Congress" collection. By following these steps, you ensure that the data is well-organized, consistent, and ready for complex analyses, such as temporal network analysis or SHiCo analysis.
