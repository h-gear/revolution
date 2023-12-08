# 0. GOAL ----
# Preprocessing the raw data from the Evans Archive and save it.
# The PhiloLogic EVANS-TCP database is comprised of 5012 works in English
# published in the United States during between 1640 and 1800.
# The full text provided by the Text Creation Partnership.
# This preprocessing will consist of retrieving year and place of
# publication.

# Evans Early American Imprints collection
# https://quod.lib.umich.edu/e/evans/

# 1. LOAD LIBRARIES ----
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(tidyverse)

t_folder <- "C:/Projects/AmericanRevolution/revolution/data/raw/evans/raw_data/"

# 2. LOAD DATA ----
read_folder <- function(infolder) {
    tibble(file = dir(infolder, full.names = TRUE)) %>%
        mutate(text = map(file, read_lines)) %>%
        transmute(id = basename(file), text) %>%
        unnest(text)
}

# takes about 2-3 minutes to read all data
raw_text <- tibble(folder = dir(t_folder, full.names = TRUE)) %>%
    mutate(folder_out = map(folder, read_folder)) %>%
    unnest(cols = c(folder_out)) %>%
    transmute(Ngroup = basename(folder), id, text)

head(raw_text)

# concatenate all text per id into one row
rawdata <- raw_text %>%
    group_by(Ngroup,id) %>%
    mutate(text = paste0(text, collapse = " ")) %>%
    distinct(Ngroup,id,text) %>%
    mutate(text = tolower(text)) %>%
    #mutate(text = str_squish(tolower(text))) %>%
    mutate(TCP = glue::trim(str_remove(as.character(id), ".headed.txt")))

# remove dataframe
rm(raw_text)

# 3. LOAD AND MERGE EVANS META DATA ----
evans <- read.csv("data/raw/evans/EvansTCP.csv")

# merge evans metadata with complete text ----
evans <- evans %>% right_join(rawdata, by = "TCP")

# Grouping by date, we can count how many documents are
# available for each year, and plot these numbers
evans %>%
    group_by(Date) %>%
      summarize(count = n()) %>%
    ungroup() %>%
    ggplot(aes(x = Date, y = count,group = 1)) +
    geom_line() +
    theme_minimal()

evans <- evans %>%
  select(-EEBO,-VID,-id,-STC,-Status,-Ngroup)

glimpse(evans)

evans <- evans %>% mutate(text = tolower(text))

# Save data
saveRDS(evans, file = "data/raw/evans/evans_5012.rds")

# 4. EXTRACT SENTENCE WITH PLACE OF PUBLICATION ----

# see also Preprocessing Evans Corpus.ipynb for:
# - extracting the contents to separate files per year
# - preprocessing the data
# - training the word2vec models
# - these word2vec models are in the w2vModels folder as part of the docker shico files

# extract location of publication ----

# 1. We define the target words "print" and "printed"
# 2. We create a custom function extract_surrounding_words that takes a text string
#    and the target words as input
# 3. Within the function, we use str_locate to find the position of the first
#    occurrence of any target word in the text
# 4. If a match is found, we extract the surrounding words (5 words before and 5
#    words after the match) using str_split and array indexing
# 5. We combine the surrounding words into a single string
# 6. Finally, we apply this function to the text column of our data frame using
#    rowwise() from dplyr

# search terms in text used to locate the place of publication
target_words <- c("print", "printed")



# Function to extract surrounding words around target words
extract_surrounding_words <- function(text, target_words) {

  # Initialize a vector to store the results
  result <- character(0)

  # Split the text into words
  words <- unlist(str_split(text, "\\s+"))

  # Find the positions of target words
  target_positions <- which(words %in% target_words)

  if (length(target_positions) > 0) {
    # Find the position of the first occurrence of any target word
    first_occurrence_pos <- min(target_positions)

    # Extract surrounding words (10 words before and after)
    start_pos         <- max(1, first_occurrence_pos - 13)
    end_pos           <- min(length(words), first_occurrence_pos + 13)
    surrounding_words <- words[start_pos:end_pos]

    # Combine surrounding words into a single string
    result <- paste(surrounding_words, collapse = " ")
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Apply the extract_surrounding_words function to the text column
evans <- evans %>%
  rowwise() %>%
  mutate(surrounding_words_print = extract_surrounding_words(text, target_words))

# 5. DATE OF PUBLICATION ----

sum(is.na(evans$Date))  #Check after: 0 dates missing

# 6. PLACE OF PUBLICATION ----

# Function to extract place of publication mentioned before ': printed'
extract_word_with_colon <- function(text) {
  # Find words with a colon
  words_with_colon <- str_extract_all(text, "\\b\\w+\\: printed")

  if (length(words_with_colon[[1]]) > 0) {
    result <- str_replace(words_with_colon[[1]][1], ": printed", "")  # Remove ':'
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Function to extract place of publication mentioned directly after 'printed at'
extract_printed_at  <- function(text) {
  words_with_printed_at <- str_extract_all(text, "(?<=printed at\\s)\\w+")

  if (length(words_with_printed_at[[1]]) > 0) {
    result <- str_replace(words_with_printed_at[[1]][1], "printed at", "")
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Function to extract place of publication mentioned before ', printed'
extract_word_with_comma <- function(text) {
  # Find words with a colon
  words_with_comma <- str_extract_all(text, "\\b\\w+\\, printed")

  if (length(words_with_comma[[1]]) > 0) {
    result <- str_replace(words_with_comma[[1]][1], ", printed", "")
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Function to extract place of publication mentioned before '; printed'
extract_word_with_semicolon <- function(text) {
  # Find words with a colon
  words_with_semicolon <- str_extract_all(text, "\\b\\w+\\; printed")

  if (length(words_with_semicolon[[1]]) > 0) {
    result <- str_replace(words_with_semicolon[[1]][1], "; printed", "")
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Function to extract place of publication mentioned before '. printed'
extract_word_with_dot <- function(text) {
  # Find words with a colon
  words_with_dot <- str_extract_all(text, "\\b\\w+\\. printed")

  if (length(words_with_dot[[1]]) > 0) {
    result <- str_replace(words_with_dot[[1]][1], ". printed", "")
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Function to extract place of publication mentioned before 'printed'
extract_word_with_space <- function(text) {
  # Find words with a colon
  words_with_space <- str_extract_all(text, "\\b\\w+\\ printed")

  if (length(words_with_space[[1]]) > 0) {
    result <- str_replace(words_with_space[[1]][1], " printed", "")
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Integrated function that applies the functions in consecutive order
extract_combined <- function(text) {
  result <- extract_word_with_colon(text)
  if (is.na(result)) {
    result <- extract_printed_at(text)
  }
  if (is.na(result)) {
    result <- extract_word_with_comma(text)
  }
  if (is.na(result)) {
    result <- extract_word_with_semicolon(text)
  }
  if (is.na(result)) {
    result <- extract_word_with_dot(text)
  }
  if (is.na(result)) {
    result <- extract_word_with_space(text)
  }
  return(result)
}

glimpse(evans)

evans2 <- evans %>%
  rowwise() %>%
  mutate(place_of_publication = extract_combined(surrounding_words_print)) %>%
  ungroup() %>%
  select(text,surrounding_words_print, Date, place_of_publication)

# Count the number of NA values in 'place of publication'
sum(is.na(evans2$place_of_publication)) # 889

table(evans2$place_of_publication)

# 7. CLEANING PLACE OF PUBLICATION ----
# extract cities from https://query.wikidata.org/

uscities <- read.csv("data/external/us_cities_1640_1800.csv") %>% as.data.frame() %>%
  rename(city = sLabel,
         state = label) %>%
  mutate(city = tolower(city),
         state = tolower(state),
         coordinates = coordinates %>%
           str_remove_all(paste(c("Point", "\\(", "\\)"), collapse = "|"))) %>%
  separate("coordinates", c("longitude","latitude"), sep = " ",remove = T) %>%
  mutate(longitude = as.numeric(longitude),
         latitude  = as.numeric(latitude)) %>%
  distinct(city,state, .keep_all = TRUE)

pattern_cities <- paste0("(?i)(", paste0(uscities$city, collapse = "\\b|\\b"), "\\b)")

## some manual corrections ----
evans2 <- evans2 %>%
  mutate(place_of_publication = ifelse(str_detect(surrounding_words_print, "new york")                   & place_of_publication == "york", "new york", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "charleston, south carolina") & place_of_publication == "carolina", "charleston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, in new england")     & place_of_publication == "england", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston in new england")      & place_of_publication == "england", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston new england")         & place_of_publication == "england", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, new england")        & place_of_publication == "england", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston; new england")        & place_of_publication == "england", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston: new england")        & place_of_publication == "england", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "providence, in new england") & place_of_publication == "england", "providence", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "salem, new england")         & place_of_publication == "england", "salem", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "new bury-port, new england") & place_of_publication == "england", "new bury-port", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston in n.e.")             & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, n.e.")               & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston: n.e.")               & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, in n. e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston: n. e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, n. e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston n.e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston. n.e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, in n.e. ")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston: in n. e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston n. e.")           & place_of_publication == "e", "boston", place_of_publication),

         # check and compare with cities information ----
         place_of_publication = ifelse(is.na(place_of_publication), str_extract(evans2$surrounding_words_print, pattern_cities), place_of_publication),

         # remove irrelevant words
         place_of_publication = ifelse(place_of_publication %in% c("administration","fire","goldyng","newry","notes","our","author",
                                                                   "poetry","prayers","g","pth","the","xcii","ii","treatise",
                                                                   "large","journal","invasion","virgil","callico","quarto",
                                                                   "a","account","accounts","actually","and","any","are",
                                                                   "as","be","been","being","bible","books"," contributors",
                                                                   "elegantly","ever","fairly","first","fore",
                                                                   "former","formerly", "had","have", "having",
                                                                   "house","is","lately","lick","my","neatly",
                                                                   "not","o","or","originally","pamphlet","part",
                                                                   "partly", "plainly","poem","publick","s",
                                                                   "schools","separately","some","t021112cw33100685950054802800books",
                                                                   "their","them","they","two","was","wearing",
                                                                   "were","who","by","de","great","high",
                                                                   "in","new","of","on","royal","the","tow",
                                                                   "under","upon","with","worth"), NA, place_of_publication))

sum(is.na(evans2$place_of_publication)) #940

table(evans2$place_of_publication)

# 8. SAVE PROCESSED DATA ----
saveRDS(evans2, file = "data/processed/evans/evans_preprocessed.rds")

