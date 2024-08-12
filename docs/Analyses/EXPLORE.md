# Aggregated network visualization

## Overview

This document details the workflow for plotting an aggregated network using data from the Letters of Delegates to Congress (1774-1789). The goal is to visualize the network of interactions among delegates, highlighting their communication patterns and connections.

## Workflow

### 1. Load Libraries

Start by loading the necessary libraries:

- **`tidyverse`**: For data manipulation and visualization.
- **`igraph`**: For network analysis and plotting.

### 2. Load Preprocessed Data

- Import the preprocessed dataset using `readRDS`. This dataset, `combined_df2`, contains the data required for plotting the network.

### 3. Plot Aggregated Network

#### Select and Prepare Data

- Extract relevant columns (`sender_id` and `receiver_id`) from the dataset.
- Filter out rows with missing `sender_id` or `receiver_id`.
- Group the data by `sender_id` and `receiver_id` and calculate the weight of each edge by counting occurrences.

#### Create and Simplify Network

- Convert the prepared data into a network object using `igraph::graph_from_data_frame`.
- Simplify the network by removing loops and multiple edges between the same nodes to streamline the visualization.

#### Visualize Full Network

- Plot the entire network to observe the overall structure and connectivity.

#### Focus on Subgraphs

- Create a subset of the network with nodes having a degree greater than a specified threshold (e.g., degree > 20).
- Define neighbors for main vertices and include them in an induced subgraph for focused analysis.

#### Cluster Analysis

- Use the Louvain method to detect communities in the network. Apply modularity-based clustering to identify significant groups within the network.
- Assign community membership to vertices and plot the network with these clusters highlighted.

#### Customize Visualization

- Create a custom color palette for different clusters.
- Calculate node strength, which reflects the sum of edge weights connected to each node, and adjust node sizes accordingly.
- Plot the network with customized vertex colors, sizes, and labels, and highlight specific groups if needed.

### 4. Save Network Plot

- Save the final visualization of the network plot to a file (implementation details to be added).

## Conclusion

This workflow provides a comprehensive approach to visualizing and analyzing the network of interactions among delegates. By focusing on network structure, subgraphs, and community detection, it allows for a detailed exploration of communication patterns and relationships. The use of customized visual elements and clustering techniques enhances the clarity and interpretability of the network analysis, providing valuable insights into the connections between historical figures.

# Descriptive Analysis of Founders Online Dataset

## Overview

This document provides a detailed workflow for conducting a descriptive analysis of the Founders Online dataset. The analysis focuses on understanding the corpus of letters, including author and recipient interactions, document counts, and temporal patterns.

## Workflow

### 1. Load Libraries

Begin by loading the necessary library:

- **`tidyverse`**: For data manipulation and visualization.

### 2. Read Data

- Import the preprocessed datasets using `readRDS`:
  - `ffc5`: Contains the primary dataset for analysis.
  - `ffc6`: Contains additional data, possibly including more comprehensive records or extended periods.

### 3. Descriptive Analysis

#### Background Information

- Create a reference table (`Period_table`) outlining the periods of interest, including:
  - Period names (e.g., Colonial, Revolutionary War).
  - General date ranges and specific start and end dates for each period.

#### Unique Authors and Recipients

- Calculate the number of unique authors and recipients in the `ffc5` dataset using `n_distinct`.

#### Documents and Word Counts by Period

- Aggregate the number of documents and total word count by period using `data.table::setDT` and summarizing with `by_period`.
- Join the results with `Period_table` to enrich the data and format it for readability using `knitr::kable`.

#### Temporal Analysis of Writings

- **Frequency of Writings by Month**:
  - Filter and aggregate the number of documents written by notable founders each month.
  - Plot the monthly frequency of writings, highlighting periods using vertical lines and faceting by author.

- **Frequency of Received Writings by Month**:
  - Similarly, analyze and plot the frequency of received documents by founders over time.
  - Adjust the visual details to highlight periods and facet by recipient.

#### Interaction Between Founders

- **Number of Messages Sent**:
  - Summarize and visualize the number of messages sent by each founder.

- **Number of Messages Received**:
  - Summarize and visualize the number of messages received by each founder.

### 4. Save Plots and Results

- Save the generated plots and summary results (implementation details to be added).

## Conclusion

This workflow provides a thorough descriptive analysis of the Founders Online dataset, focusing on document counts, author-recipient interactions, and temporal patterns. By following this process, you can gain valuable insights into the corpus of writings, track communication trends over time, and understand the volume and distribution of written exchanges among key historical figures.

