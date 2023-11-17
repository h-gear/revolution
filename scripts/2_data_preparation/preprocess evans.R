# --------------------------------------------------------------------------------------
# Reading data from the EvansTCP dataset 
# --------------------------------------------------------------------------------------
  
# see "Preprocessing Evans Corpus.ipynb" for similar procedures

# libraries ----
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)   
library(tidyverse)

t_folder <- "C:/Projects/AmericanRevolution/Data/EvansTCP/EvansTCP/"

# reading raw data in N0,N1,N2 and N3 folders ----
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

# save data ----
# saveRDS(raw_text, file = "raw_text.RDS") # 9306009 obs, 3 variables

# read dataset ----
raw_text <- readRDS("raw_text.RDS")

# concatenate all text per id into one row
n1 <- raw_text %>% 
    group_by(newsgroup,id) %>%
    mutate(text = paste0(text, collapse = " ")) %>%
    distinct(newsgroup,id,text) %>% 
    mutate(text = tolower(text)) %>% 
    #mutate(text = str_squish(tolower(text))) %>% 
    mutate(TCP = glue::trim(str_remove(as.character(id), ".headed.txt")))

# reading meta data in csv file ----
evans <- read.csv("EvansTCP.csv")

# merge evans metadata with complete text ----
evans <- evans %>% left_join(n1, by = "TCP")

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
  select(-EEBO,-VID,-id,-STC,-Status,-newsgroup)

glimpse(evans)

# A function for displaying a letter
display_letter <- function(x) {
  a1 <- strsplit(x$text, '\n')[[1]]
  a2 <- trimws(a1)
  a3 <- subset(a2, a2 != '')
  a4 <- paste(a3, collapse = '\n')
  # a4a <- paste0('Author: '   , x$From, '\n>', 
  #               'Recipient: ', x$To, '\n>', 
  #               'Period: '   , x$Period, '\n>\n', 
  #               a4)
  a5 <- gsub(' *\n', '  \n', a4)
  paste('>', gsub(' *(\n*) *$', '\\1', a5))
}
display_letter(n1[3538,])

#evans$Author3 <- stringr::str_extract(evans$Author, "\\d{4}(?=-)")
#evans$Author2 <- stringr::str_extract(evans$Author, "(?<=-)\\d{4}")

# save data ----
 saveRDS(evans, file = "evans_5012_unprocessed.RDS") 

# Next, see Preprocessing Evans Corpus.ipynb for: 
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
# 
# The result will be a new column surrounding_words in our data frame that 
# contains the surrounding words for the first occurrence of "print" or "printed"
# in each text entry

target_words <- c("print", "printed")

# Function to extract surrounding words
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
    
    # Extract surrounding words (4 words before and after)
    start_pos <- max(1, first_occurrence_pos - 10)
    end_pos <- min(length(words), first_occurrence_pos + 10)
    surrounding_words <- words[start_pos:end_pos]
    
    # Combine surrounding words into a single string
    result <- paste(surrounding_words, collapse = " ")
  } else {
    result <- NA  # No match found
  }
  
  return(result)
}

# Apply the extract_surrounding_words function to the text column
evans <- evans %>% rowwise() %>%
  mutate(surrounding_words_print = extract_surrounding_words(text, target_words))

# Function to extract place of publication mentioned before : 'printed'
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

# Function to extract place of publication mentioned before , 'printed'
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

# Function to extract place of publication mentioned before ; 'printed'
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

# Function to extract place of publication mentioned before . 'printed'
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
  return(result)
}

# Integrated function
# extract_combined <- function(text) {
#   word_with_colon <- extract_word_with_colon(text)
#   if (!is.na(word_with_colon)) {
#     return(word_with_colon)
#   } else {
#     word_with_printed_at <- extract_printed_at(text)
#     if (!is.na(word_with_printed_at)) {
#       return(word_with_printed_at)
#     } else {
#       return(extract_word_with_comma(text))
#     }
#   }
# }

evans2 <- evans %>% 
  rowwise() %>%
  mutate(place_of_publication = extract_combined(surrounding_words_print)) %>% 
  ungroup() %>% 
  select(surrounding_words_print, place_of_publication)

# Count the number of NA values in the vector or column
sum(is.na(evans2$place_of_publication))


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
         
         
         boston in n e printed
          )        
                                                             
cities <- read.csv("1_uscities_till1783.csv", sep = ',') %>%
  as.data.frame() %>% 
  rename(city = sLabel,
         state = label) %>% 
  mutate(city = tolower(city),
         coordinates = coordinates %>% 
           str_remove_all(paste(c("Point", "\\(", "\\)"), collapse = "|"))) %>% 
  separate("coordinates", c("longitude","latitude"), sep = " ",remove = T) %>% 
  mutate(longitude = as.numeric(longitude),
         latitude  = as.numeric(latitude)) %>% 
  distinct(city,state, .keep_all = TRUE)

pattern  <- paste0("(?i)(", paste0(cities$city,   collapse = "\\b|\\b"), "\\b)")

evans2 <- evans2 %>%
  mutate(place_of_publication = ifelse(is.na(place_of_publication), str_extract(evans2$surrounding_words_print, pattern), place_of_publication))


evans2 <- evans2 %>%
mutate(place_of_publication = case_when(str_detect(surrounding_words_print, "new york") & place_of_publication == "york" ~ "new york",
                                              .default = stringr::str_extract(evans2$surrounding_words_print, pattern)))




# # Apply the extract_word_with_colon function to the text column
# evans2 <- evans %>% rowwise() %>%
#   mutate(place_of_publication = extract_word_with_colon(surrounding_words_print)) %>% 
#   ungroup() %>% select(surrounding_words_print, place_of_publication)
# 
# evans2 <- evans %>% rowwise() %>%
#   mutate(place_of_publication = extract_printed_at(surrounding_words_print)) %>% 
#   ungroup() %>% select(surrounding_words_print, place_of_publication)
