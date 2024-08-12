# Text processing and word2vec model training

## Overview

This script performs various text processing tasks on a dataset of letters of the Delegates of Congres, including:

- Extracting letter content from raw files.
- Tokenizing and cleaning text data.
- Training Word2Vec models on the processed text.

## 1. Libraries

The script requires several libraries for its operations:

- **`argparse`**: For command-line argument parsing.
- **`pandas`**: For data manipulation and reading CSV files.
- **`tqdm`**: For progress bars.
- **`numpy`**: For numerical operations.
- **`nltk`**: For natural language processing (tokenization and stopword removal).
- **`string`**: For string operations.
- **`gensim`**: For topic modeling and Word2Vec model training.
- **`os`**: For file and directory operations.
- **`csv`**: For CSV file operations.

## 2. Check System Architecture

The script adjusts the CSV field size limit based on whether the system is 32-bit or 64-bit.

## 3. Functions

### `get_content(args)`

**Purpose**: Extracts the content of letters from text files and saves them into CSV files by year.

- **Parameters**: `args` (command-line arguments, though not used in the function).
- **Process**:
  - Reads metadata from `Letters.csv`.
  - Retrieves the content from text files based on the 'TCP' field.
  - Saves each year's content to a separate CSV file.

### `split_into_sentences(text)`

**Purpose**: Splits text into sentences.

- **Parameters**: `text` (string).
- **Returns**: List of sentences.

### `split_into_words(text)`

**Purpose**: Tokenizes sentences into words.

- **Parameters**: `text` (list of sentences).
- **Returns**: List of lists of words.

### `remove_stopwords_and_punctuation(text, stopwords)`

**Purpose**: Removes stopwords and punctuation from tokenized text.

- **Parameters**: 
  - `text` (list of lists of words).
  - `stopwords` (list of stopwords).
- **Returns**: Cleaned list of lists of words.

### `join_words(text)`

**Purpose**: Joins lists of words into a single string document.

- **Parameters**: `text` (list of lists of words).
- **Returns**: String document.

### `tokenize(args)`

**Purpose**: Processes raw text files into tokenized and cleaned text, and saves them in CSV files.

- **Parameters**: `args` (command-line arguments, though not used in the function).
- **Process**:
  - Reads text content from CSV files.
  - Tokenizes, removes stopwords, and joins words.
  - Saves the processed text to new CSV files.

### `get_min_max_year()`

**Purpose**: Retrieves the minimum and maximum year from tokenized data.

- **Returns**: Tuple of (min_year, max_year).

### `get_sentences_for_year(year)`

**Purpose**: Retrieves tokenized sentences from a CSV file for a specific year.

- **Parameters**: `year` (int).
- **Returns**: List of lists of sentences.

### `get_sentences_in_range(start_y, end_y)`

**Purpose**: Retrieves tokenized sentences for a range of years.

- **Parameters**:
  - `start_y` (int): Start year.
  - `end_y` (int): End year.
- **Returns**: List of lists of sentences.

### `train(args)`

**Purpose**: Trains Word2Vec models on text data from specified year ranges.

- **Parameters**: `args` (command-line arguments, includes optional window size for model training).
- **Process**:
  - Constructs and trains a Word2Vec model for each year range.
  - Saves the trained model to disk.

## 4. Main Function

**Purpose**: Parses command-line arguments and executes the corresponding function (`get_content`, `tokenize`, or `train`).

- **Command-line Arguments**:
  - `content`: Extracts content from raw data.
  - `tokenize`: Tokenizes and cleans text data.
  - `train`: Trains Word2Vec models, with an optional window size argument.

## Conclusion

This script provides a comprehensive solution for processing and analyzing historical letter data. It extracts and tokenizes text content, prepares it for modeling, and trains Word2Vec models to capture semantic relationships between words.
