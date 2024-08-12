# Geospatial distribution of letters

## Overview

This document outlines the workflow for mapping the geospatial distribution of letters from the Founders Online and ecco datasets. The aim is to visualize and analyze the geographic locations of letters, providing insights into their distribution across different regions.

It is based on the retrieval of the place where the letter was written and its geographical
coordinates. This geodata allows to find central hubs of writings. Also, with this information we can make subgroups of letters based on geographical location, and examine whether there are different ideas mentioned in the letters. For example, we can
compare the letters written in the US with those in Europe, or those written
in the same city but in different years.

There are a lot of cities with the same name in different states or/ and in different countries. If the place of writing is missing in the letter or cannot be clearly specified, we tried to extract the place of writing from other letters written by the same person within a specified time frame (30 days) to obtain the most likely place of writing.
 

## Workflow

### 1. Setup and Libraries

Begin by loading the necessary libraries:

- **`tidyverse`**: For data manipulation and visualization.
- **`sf`**: For handling spatial data and converting data frames into spatial objects.
- **`tidygeocoder`**: For geocoding addresses to obtain latitude and longitude.
- **`mapview`**: For interactive visualization of spatial data.
- **`tigris`**: For accessing US shapefiles to map geographic regions.

### 2. Data Preparation

#### Load Datasets

- Import the `founders` dataset, which contains coordinates of letters.
- Import the `ecco` dataset, adjusting column names to match expectations.

#### Address Combination

- For both datasets, create a combined address string that includes city, state, and country. This combined address helps in accurate geocoding by incorporating as much location detail as possible.

### 3. Geocoding

#### Obtain Coordinates

- Use the `tidygeocoder` package to geocode the combined addresses. This process converts addresses into geographic coordinates (latitude and longitude).

#### Update Coordinates

- If geocoding fails to provide coordinates, retain existing coordinates from the dataset where available. Ensure no information is lost during this process.

#### Filter Valid Coordinates

- Remove entries with missing latitude or longitude to ensure only valid geographic points are used for analysis.

#### Convert to Spatial Objects

- Convert the filtered data into `sf` objects, which represent spatial features with coordinates in a specified coordinate reference system (CRS).

#### Summarize Data

- Group the data by city and count the number of letters per city. This step helps in understanding the distribution and density of letters in different locations.

### 4. Visualization

#### Interactive World Map

- Use the `mapview` package to create an interactive map showing the distribution of letters for both datasets. Different colors are used to distinguish between datasets.

#### US State-Level Visualization

- Obtain US state shapefiles using `tigris` and join these with the letter data to visualize the number of letters by state. This visualization helps in understanding regional patterns within the US.

#### Spatial Clustering

- Apply K-means clustering to categorize the geographic points into clusters. This helps in identifying distinct geographic regions where letters are concentrated.

#### Cluster Visualization

- Visualize the clusters on a map to analyze the spatial distribution of letters across different clusters.

## Conclusion

This workflow effectively combines data preparation, geocoding, and visualization techniques to map the distribution of letters across geographic locations. By following this process, you can gain insights into the spatial patterns and concentration of letters, which can inform further analysis and research. The use of interactive maps and spatial clustering enhances the understanding of geographic distributions and reveals underlying patterns within the datasets.
