# Sentiment analysis by topic per year

## Description

This script performs sentiment analysis on historical texts, categorized by topic and year. It evaluates the sentiment associated with each topic over time and visualizes sentiment trends through various plots. By integrating historical events and topic analysis results, the script provides insights into how sentiment evolves within specific topics across different periods. This analysis supports a deeper understanding of the thematic shifts and emotional tone in historical letters.

## Purpose

- **Analyze Sentiment by Topic:** To assess the sentiment associated with different topics over time.
- **Visualize Sentiment Trends:** To create visual representations of sentiment changes, including ridge plots and time series.
- **Compare Sentiments:** To generate word clouds contrasting positive and negative sentiments for additional insights.

## Input Dataset

- **Historical Events:** A CSV file containing significant historical events with start and end dates.
- **Topic-Analytical Results:** A CSV file with preprocessed texts including topic information and clean text.

## Output

- **Figures:** TIFF files containing visualizations of sentiment trends, including sentiment over time for specific topics, overall topic sentiment trends, and word clouds.

## Workflow

1. **Import Libraries**
   Load necessary R libraries for text processing, sentiment analysis, and visualization. This includes packages like `sentopics`, `tidytext`, `ggridges`, `tidyverse`, `reshape2`, `wordcloud`, `zoo`, and `lubridate`.

2. **Read Data**
   - **Historical Events:** Load historical events data and parse dates.
   - **Topic-Analytical Results:** Load text data with topic information and preprocess dates for analysis.

3. **Prepare Data**
   - **Text Data:** Ensure each word is in a separate row and format the dataset for sentiment analysis by year and topic.
   - **Historical Time Periods:** Define key historical periods with start and end dates for contextual analysis.

4. **Calculate Sentiment Per Topic Per Year**
   - **Sentiment Analysis:** Join text data with sentiment lexicons (AFINN) to calculate average sentiment scores by topic and year.

5. **Visualize Sentiment for Individual Topics**
   - **Plot Sentiment for Specific Topics:** Generate and save TIFF files showing sentiment trends over time for specific topics, e.g., "Republican politics" and "Liberal politics."

6. **Visualize Sentiment Development for All Topics**
   - **Ridge Graph:** Create a ridge plot displaying sentiment development across all topics over time. Use a dynamic color palette to represent different topics.

7. **Generate Word Clouds**
   - **Positive vs. Negative Sentiments:** Create word clouds to visualize the most frequent words associated with positive and negative sentiments, providing insights into the emotional tone of the texts.

8. **Save Results**
   Save the generated plots and word clouds as TIFF files for further analysis and reporting.

## Conclusion

This workflow provides a detailed sentiment analysis of historical texts, categorized by topic and year. By leveraging various visualizations, including time series plots, ridge graphs, and word clouds, the analysis offers a comprehensive view of how sentiment evolves within different themes over time. This approach enhances the understanding of historical texts, revealing shifts in emotional tone and thematic focus, and supports both qualitative and quantitative research efforts.
