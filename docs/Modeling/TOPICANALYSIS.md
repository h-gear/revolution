# Semi-supervised topic modeling

## Description

This script performs a comprehensive topic analysis on the Founders Online texts. It categorizes letters based on their dominant topics, creating a framework to analyze subsets of letters according to specific themes. This method complements the results from the Shico tool, which selects homogeneous letters based on word occurrences related to particular concepts. The combined approach of topic analysis and Shico's results provides a robust method to validate findings and refine topic-based subsets. Ultimately, this processed data supports the creation of animated word clouds and network analyses for deeper insights into the topics of interest.

## Purpose

- **Assess Topic Diversity:** To understand the range of topics present in the letters.
- **Create Homogeneous Subsets:** To generate subsets of letters categorized by different political ideologies as identified by the lead applicant.
- **Validate Shico Results:** To cross-check the results from Shico with the topic analysis to ensure consistency and accuracy.
- **Generate Input for Visualization:** To prepare datasets suitable for creating animated word clouds, enhancing topic visualization.

## Input Dataset

- **Letters:** The preprocessed dataset from Founders Online, including letter IDs and text.

## Output

- **Data Frame:** A data frame that includes an additional column indicating the dominant topic for each letter and topic probabilities for all topics.

## Workflow

1. **Import Libraries**
   Begin by loading the necessary libraries for text processing and topic modeling. This includes tools for parallel computation and specialized packages such as `quanteda`, `seededlda`, and `furrr`.

2. **Read and Prepare Data**
   Load the preprocessed dataset of letters. Ensure the letters are sorted by sending date, and verify that each letter is correctly formatted for subsequent analysis.

3. **Text Preprocessing**
   - **Part-of-Speech Tagging:** Segment the text into chunks and extract nouns. This process may be divided into multiple files to handle large datasets efficiently.
   - **Tokenization:** Convert text to lowercase, remove abbreviations and stopwords, and create tokens. Identify and integrate multi-word expressions into tokens.
   - **Lemmatization:** Use a predefined lemma table to reduce words to their base forms, ensuring consistency in the text analysis.

4. **Combine and Clean Data**
   Combine the original and cleaned versions of the letters. Create a document-feature matrix (DFM) from the lemmatized tokens, and refine it by removing infrequent and overly common terms.

5. **Topic Modeling**
   - **Initial Topic Count:** Experiment with different numbers of topics using measures like Kullback-Leibler divergence to find the optimal count.
   - **Model Testing:** Utilize the `seededlda` approach to guide topic formation with specific keywords. Test multiple models with varying numbers of topics to identify the most meaningful configuration.

6. **Evaluate Models**
   Assess the candidate models for coherence and exclusivity. These evaluations help determine the quality and distinctiveness of the topics, guiding the selection of the most representative models.

7. **Save Results**
   Save the processed data and model results in RDS format for future use. This includes the final document-feature matrix, topic models, and any intermediate outputs.

8. **Topic Probabilities per Letter**
   Calculate the topic probabilities for each letter. This involves extracting the topic probabilities from the model results and transforming them into a format suitable for further analysis.

9. **Most-likely Topic for Each Letter**
   Determine the dominant topic for each letter based on the highest probability and add this information to the original texts dataframe. This allows for more focused analyses and visualizations.

10. **Distribution and Visualization**
    - **Distribution of Letters per Main Topic:** Analyze and visualize the distribution of letters across different topics. Generate a bar plot to show the proportion of letters associated with each topic.
    - **Most Prevalent Topics:** Identify and visualize the most prevalent topics overall. Create a horizontal bar plot to display the prevalence of each topic.
    - **Topic Prevalence by Year:** Examine how topic prevalence changes over time. Calculate the mean topic prevalence by year and visualize temporal trends in topic distribution.

11. **Visualize Word Probabilities per Topic**
    Visualize the top words associated with each topic. Plot the top terms for each topic to understand the key themes and their significance.

12. **LDAvis**
    Utilize LDAvis for interactive visualization of the LDA topic model. This tool helps in exploring topic-term relationships and provides a graphical representation of the topics and their associated terms.

13. **Save Data for Further Analysis**
    Prepare and save dataframes for additional analyses, such as animated word clouds and network analyses. This includes merging topic probabilities with text data and creating datasets for detailed exploration.

## Conclusion

This workflow efficiently manages large text datasets by integrating text preprocessing, topic modeling, and validation techniques. The combination of topic analysis with Shico's results offers robust insights into letter themes and supports further exploration through visualizations such as animated word clouds and network analyses. This comprehensive approach facilitates a deeper understanding of the political ideologies and themes in historical letters, aiding both qualitative and quantitative research.
