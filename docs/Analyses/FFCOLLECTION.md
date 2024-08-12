# Founding Fathers Letters: Data scraping

This documentation outlines the process for scraping, cleaning, and saving data from the "Founding Fathers" website, available at [https://founders.archives.gov/](https://founders.archives.gov/). This dataset is utilized for historical research and analysis. The modified code provided by Jay Timm, which can be found [here](https://github.com/jaytimm/founders-online-corpus), has been adapted for this purpose.

## Overview

### Objective

The goal is to scrape and save text data from the "Founding Fathers" website, extracting metadata and content from XML and JSON sources. The data will be cleaned and organized into a structured format suitable for further analysis.

### Workflow

1. **Load Required Libraries**: Import the necessary R packages for data manipulation and scraping.
2. **Scrape Data**: Extract metadata and text content from XML and JSON files.
3. **Save Data**: Organize and save the cleaned data into files categorized by historical periods.

## Step-by-Step Guide

### 1. Load Required Libraries

First, load the `tidyverse` library, which includes essential tools for data manipulation and scraping. This library facilitates various data wrangling tasks.

```r
library(tidyverse)
```

Set the working directory to where your XML metadata file is located and specify the file name.

## 2. Scrape Data

### Extract Metadata

To obtain metadata for each letter, read the XML file using the `xml2::read_xml()` function. This function parses the XML file, allowing you to extract key metadata fields such as title, permalink, project, authors, recipients, and dates. These fields are then compiled into a data frame.

- **Variable**: `xml2`
- **Function**: `xml2::as_list()`

### Extract Text Content

The full-text content of each letter is retrieved through the provided API. Use the `jsonlite::fromJSON()` function to parse the JSON responses from the API. A custom function, `clean_text()`, is applied to process the raw text, removing unnecessary whitespace and empty lines.

- **Variable**: `x2$api`
- **Function**: `jsonlite::fromJSON()`
- **Function**: `clean_text()`

### Categorize Texts by Historical Period

Assign each letter to a historical period based on its date using the `mutate()` function along with the `case_when()` helper from the `dplyr` package. Historical periods include Colonial, Revolutionary War, Confederation Period, and various presidencies.

- **Function**: `mutate()`
- **Function**: `case_when()`

## 3. Save Data

The cleaned and categorized data is saved into separate RDS files, one for each historical period. Use the `saveRDS()` function to serialize the data frames into RDS files, which are suitable for storage and further analysis.

- **Function**: `saveRDS()`

Set the working directory to the desired location for saving files. Utilize `lapply()` to iterate through each period’s data and save it as individual RDS files.

## Conclusion

By following these steps, you can efficiently scrape, clean, and save text data from the "Founding Fathers" website. This process ensures that the data is well-organized and ready for further historical analysis.
