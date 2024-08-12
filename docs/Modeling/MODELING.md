# Time-respecting path-based network analysis of letter correspondences

## Overview
The purpose of this script is to analyse time-respecting paths of a letter correspondence
network of writers in the era of the American Revolution. We aim to identify key persons/
writers and patterns of communication in the transmission of political ideas through 
letters. We distinguish between two types of political thinking: liberal ideology and 
republican ideology. By employing a temporal network approach, the chronological order
of the edges within the network are preserved. Unlike traditional aggregated networks
that disregard the temporal information that is implicit in the letter data, this 
approach significantly mitigates the bias in topological network measures.

## Workflow
The workflow for analyzing time-respecting paths in a letter correspondence network involves several systematic steps. This approach integrates temporal network analysis with betweenness centrality measures to uncover key individuals and communication patterns associated with different political ideologies.

## Step-by-Step Guide

### Load Libraries

Begin by loading essential libraries for data manipulation, temporal network handling, and visualization. Key libraries include datetime for date management, pathpy for temporal network analysis, pandas for data manipulation, and igraph for network analysis.

### Import and Prepare Data

- Define File Paths: Specify the paths for data and results storage to ensure that data is correctly read and processed.
- Select Dataset: Choose the dataset based on the ideological focus (liberal or republican). This determines the subset of data used for analysis.
- Load Data: Depending on the chosen dataset, load the relevant CSV files containing the correspondence links or letter information. If conducting SHICO analysis, use specific link files; for topic-based analysis, filter data based on the topic.
- Clean Data: Convert sending dates into time differences to facilitate temporal analysis. Rename columns for consistency and remove unnecessary columns. Prepare and save the cleaned data for temporal network analysis.

### Compute Time-Respecting Paths

- Create Temporal Network: Read the cleaned data to construct a temporal network. This network preserves the chronological order of correspondence, which is crucial for analyzing time-respecting paths.
- Extract Paths: Compute paths within the temporal network for various time intervals (delta_t). Save these paths to files for further analysis.

### Compute Betweenness Centrality

- Betweenness for Time-Respecting Paths: Calculate the betweenness centrality for the extracted time-respecting paths. This measure identifies key nodes (writers) that act as intermediaries in the network.
- Betweenness for Aggregated Network: Construct an aggregated network and compute betweenness centrality for this network to compare with time-respecting paths. This step involves building a weighted graph and calculating centrality metrics.

### Robustness Check: Betweenness for Different delta_t

- Compute Betweenness for Various delta_t: Analyze the betweenness centrality for different time intervals to assess the consistency of key individuals' importance across different temporal scales. Aggregate the results and save them for comparison.

### Visualize Betweenness Scores

- Plot Betweenness Scores: Create visualizations to compare betweenness centrality scores between time-respecting paths and the aggregated network. This helps in understanding the relative importance of nodes.
- Bump Chart for Betweenness Across delta_t: Use a bump chart to visualize how the ranks of writers change with different time intervals, providing insights into the stability of key nodes' roles over time.

### Robustness Check: Path Length Frequencies

- Compute Path Length Frequencies: Analyze the distribution of path lengths for different delta_t values to understand the frequency of various path lengths in the network. This helps in evaluating the robustness of the path analysis.

## Conclusion
This workflow offers a comprehensive approach to analyzing historical letter correspondence networks by focusing on time-respecting paths and centrality measures. By preserving the chronological order of correspondence, the analysis mitigates biases found in traditional network measures. The combination of temporal network analysis and betweenness centrality provides valuable insights into key individuals and their roles within different ideological contexts. The robustness checks and visualizations further enhance the understanding of the network's structure and the stability of central nodes over time. This methodology can be instrumental in uncovering historical communication patterns and understanding the influence of individuals in the context of ideological movements.
 