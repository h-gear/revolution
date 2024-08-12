# Network analysis and community detection

## Workflow Overview

This document outlines the steps for performing network analysis, community detection, and calculating centralities using R. The analysis involves several key stages, including data preparation, network construction, community detection, dynamic network analysis, and centrality calculations.

## Step-by-Step Guide

### 1. Libraries

Load the necessary R libraries for network analysis and visualization:
- **`visNetwork`** for interactive network visualizations.
- **`tidyverse`** for data manipulation and cleaning.
- **`ndtv`** for dynamic network visualizations.
- **`tsna`** for time series network analysis.
- **`sna`** for network analysis metrics.
- **`networkDynamic`** for handling dynamic networks.
- **`igraph`** for community detection and additional network analysis.

### 2. Read Data

Import and prepare the data:
- **Letters Data**: Load a CSV file containing information about letters and their associated topics.
- **Preprocessed Data**: Load an RDS file containing preprocessed information about the letters. Ensure the data types are correctly set for subsequent analysis.

### 3. Create Edge List

Generate an edge list for the network:
- Filter and select relevant data based on a specific topic of interest.
- Compute the weight of interactions (i.e., number of letters exchanged) between sender and receiver.
- Prepare the edge list by adding attributes such as onset and terminus years, edge ID, and undirected weight (if applicable).

### 4. Create Node List

Construct a node list:
- Identify unique nodes (authors and recipients) and their attributes.
- Include distinguishing features like specific colors for prominent figures (e.g., founding fathers).

### 5. Aggregated Network

Build and summarize the static network:
- Create a network object using the edge list and node list.
- Plot the network to visualize relationships, using attributes such as node color and edge width.

### 6. Community Detection

Detect communities within the network:
- Convert the network into an igraph object.
- Apply the Louvain algorithm for community detection to identify clusters within the network.
- Visualize the network with community-based color coding and node sizes proportional to centrality measures.

### 7. Dynamic Network Analysis

Analyze the network dynamics over time:
- Create a dynamic network object using edge and vertex spells.
- Reconcile vertex activity with edge activity to ensure correct temporal alignment.
- Visualize the network dynamics using timeline plots and network slices.

### 8. Centrality Calculations

Calculate and visualize centralities:
- Analyze graph-level statistics such as density, reciprocity, and dyad census over time.
- Compute and plot node-level centralities, including degree and other metrics.

### 9. Animation

Create animations to visualize changes in the network over time:
- Compute and render an animated version of the network to reveal structural patterns and transitions.

## Conclusion

This workflow provides a comprehensive approach to network analysis, from data preparation to dynamic analysis and visualization. By following these steps, users can gain insights into community structures, network dynamics, and centrality measures, allowing for a deeper understanding of the network's evolution and key actors.
