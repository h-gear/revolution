# Dynamic topic modeling

## Workflow Overview

This document describes the process for performing dynamic topic analysis on the Founders Online texts using the `keyATM` package in R. The analysis focuses on understanding how the prevalence of topics evolves over time. The workflow involves loading necessary libraries, preparing data, creating a dynamic topic model, and evaluating the results.

## Step-by-Step Guide

### 1. Script Header

**Description:**
This script performs a dynamic topic analysis on the Founders Online texts, analyzing how the prevalence of topics changes over time. It uses the `keyATM` package to fit a dynamic keyATM model to the cleaned text data.

**Purpose:**

- Understand the evolution of topics over time.
- Provide an alternative to the shifting concepts in time (shico) approach.

**References:**

- [keyATM Documentation](https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_dynamic.html)
- [Seeded Topic Models](https://towardsdatascience.com/why-to-use-seeded-topic-models-in-your-next-project-and-how-to-implement-them-in-r-8502d15d6e8d)

### 2. Libraries

Load the necessary R libraries:

- **`keyATM`** for dynamic topic modeling.
- **`parallel`** for parallel processing.
- **`quanteda`** for text analysis.
- **`tidyverse`** for data manipulation.
- **`future`** for handling parallel computing.

### 3. Data

Prepare the text data:

- Load the cleaned text data from `semi_supervised_topic_modleing.R`.
- Filter out empty texts and focus on the time period between 1750 and 1825.

### 4. Create keyATM Docs

Transform data into a format suitable for the `keyATM` model:

- Convert the text data into a `keyATM`-readable format.
- Save the transformed documents for later use.

### 5. Keywords

Define and visualize keywords:

- Load keyword definitions from an external script.
- Process keywords for analysis, including tokenization and lemmatization.
- Visualize the frequency of keywords by topic and save the results.

### 6. Create Decade Time Index Variable

Generate a time index variable:

- Create a period variable that represents 10-year intervals starting from 1720.

### 7. Dynamic keyATM

Fit the dynamic keyATM model:

- Set up parallel processing.
- Initialize and fit the dynamic keyATM model with specified settings.
- Save the model and its output for future reference.

### 8. Model Evaluation

Evaluate the fitted model:

- Inspect topic-term associations.
- Diagnose model performance through log-likelihood and perplexity trends.
- Visualize topic proportions and keyword importance over time.

### 9. Theta (Document-Topic Distribution)

Analyze the document-topic distribution:

- Preprocess and format the theta matrix for plotting.
- Create and save plots showing topic distributions over time and their historical context.


## Conclusion

This workflow provides a structured approach to dynamic topic analysis, focusing on how topics evolve over time using the `keyATM` package. By following these steps, users can gain insights into topic trends, visualize changes, and evaluate the effectiveness of the topic modeling approach.
