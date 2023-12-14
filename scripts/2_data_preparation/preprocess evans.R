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
evans <- readRDS("data/raw/evans/evans_5012.rds")

# A function for displaying a letter & metadata inline
display_letter <- function(x) {
  a1 <- strsplit(x$text, '\n')[[1]]
  a2 <- trimws(a1)
  a3 <- subset(a2, a2 != '')
  a4 <- paste(a3, collapse = '\n')
  a5 <- stringi::stri_extract_first_regex(a4, "\\s*(\\S+\\s*){1,1000}")

  paste('>', gsub(' *(\n*) *$', '\\1', a5))
}

# example of displaying letter
display_letter(evans[10,])

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

publication <- c("place of publication")

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
  mutate(surrounding_words_print = extract_surrounding_words(text, target_words),
         surrounding_words_publication = extract_surrounding_words(text, publication))

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

# Function to extract place of publication mentioned before ': n.e. printed'
extract_word_with_ne <- function(text) {
  # Find words with a colon
  words_with_ne <- str_extract_all(text, "\\b\\w+\\: n.e. printed")

  if (length(words_with_ne[[1]]) > 0) {
    result <- str_replace(words_with_ne[[1]][1], ": n.e. printed", "")  # Remove ':'
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Function to extract place of publication mentioned before ', n.e. printed'
extract_word_with_cne <- function(text) {
  # Find words with a colon
  words_with_cne <- str_extract_all(text, "\\b\\w+\\, n.e. printed")

  if (length(words_with_cne[[1]]) > 0) {
    result <- str_replace(words_with_cne[[1]][1], ", n.e. printed", "")  # Remove ':'
  } else {
    result <- NA  # No match found
  }

  return(result)
}

# Function to extract place of publication mentioned before ', n.e. printed'
extract_word_with_spne <- function(text) {
  # Find words with a colon
  words_with_spne <- str_extract_all(text, "\\b\\w+\\ n.e. printed")

  if (length(words_with_spne[[1]]) > 0) {
    result <- str_replace(words_with_spne[[1]][1], " n.e. printed", "")  # Remove ':'
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
    result <- extract_word_with_ne(text)
  }
  if (is.na(result)) {
    result <- extract_word_with_cne(text)
  }
  if (is.na(result)) {
    result <- extract_word_with_spne(text)
  }
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

# Vector of words to keep
city <- c("cambridge","cambridg","boston","london","philadelphia","maryland","monmouth","york","massachvset","annapolis","davis","massachusetts","williamsbvrgh",
"williamsburg","dublin","charlestown","newport","londdon","glasgow","philandelphia","germantown","milton","haven","groton","jersey","hampshire","portsmouth","providence",
"piladelphia","hartford","iersey","georgia","carolina","salem","newbern","philadelahia","burlington","savannah","norwich","lancaster","watertown","worcester",
"wilmington","chelmsford","baltimore","danvers","philadelhia","dresden","rochefoucault","chatham","newbury","exeter","westminster","philladephia","bennington",
"trenton","poughkeepsie","philadeliphia","windsor","charleston","richmond","carlisle","hudson","litchfield","northampton","petersburg","vermont","medford","newton","oxford",
"portland","stockbridge","windham","catskill","newburyport","lexington","amsterdam","dover","cork","elizabethtown","brunswick","rutland","cooperstown","goshen","hanover",
"walpole","andover","newfield","newburgh","danbury","leipsick","newhampshire","pennsylvania","washington","hallowell","suffield","schenectady","middletown","springfield",
"lansingburgh","dedham","whitestown","greenfield","whitehall","amherst","blatchford","emerson","fryeburg","harrisburgh","byfield","newark","vergennes","leipsie","bayley",
"nashville","wiscasset","haverhill","bedford","virginia","reading","bancroft","wrentham","honeywood","easton","y0rk","columbia","stancliff","newcastle","brooklyn","brookfield",
"davenport","burling","keach","connecticot","bristol","paris","pittsburgh","geneva","salisbury","norristown","gilmanton")

# Update the Country column based on the condition
evans2 <- evans2 %>%
  mutate(place_of_publication = ifelse(place_of_publication %in% city, place_of_publication, NA))

# 7. CLEANING PLACE OF PUBLICATION ----

# us staes
usstates <- read.csv("data/external/usstates.csv", sep = ";")

# cities extract from https://query.wikidata.org/
uscities <- read.csv("data/external/us_cities_1640_1800.csv") %>%
  as.data.frame() %>%
  rename(city = sLabel,
         state = label) %>%
  mutate(city = tolower(city),
         state = tolower(state),
         coordinates = coordinates %>%
           str_remove_all(paste(c("Point", "\\(", "\\)"), collapse = "|"))) %>%
  separate("coordinates", c("longitude","latitude"), sep = " ",remove = T) %>%
  mutate(longitude = as.numeric(longitude),
         latitude  = as.numeric(latitude)) %>%
  distinct(city,state, .keep_all = TRUE)  %>%
  left_join(usstates, by = c("state" = "state"))

pattern_cities <- paste0("(?i)(", paste0(unique(uscities$city), collapse = "\\b|\\b"), "\\b)")

pattern_states <- paste0("(?i)(", paste0(uscities$fullname, collapse = "\\b|\\b"), "\\b)")
pattern_short <- paste0("(?i)(", paste0(unique(uscities$short), collapse = "\\b|\\b"), "\\b)")
pattern_enclosed <- paste0("(?i)(", paste0(unique(uscities$enclosed), collapse = "\\b|\\b"), "\\b)")

pattern_states <- paste(pattern_states, pattern_short, pattern_enclosed, sep = "|")

phili <- c("philadelahia","philadelhia","philadeliphia","philadelphia","philandelphia","philladephia","piladelphia")

## some manual corrections ----
evans2 <- evans2 %>%
  mutate(place_of_publication = ifelse(place_of_publication == "cambridg", "cambridge", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "connecticot", "connecticut", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "williamsbvrgh", "williamsburg", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "southwick", "new london", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "stonington", "stonington port", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "y0rk", "york", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "iersey", "jersey", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "londdon", "london", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "leipsick", "leipzig", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "fryeburg", "freiburg", place_of_publication),
         place_of_publication = ifelse(place_of_publication == "leipsie", "leipzig", place_of_publication),
         place_of_publication = ifelse(place_of_publication %in% phili, "philadelphia", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "new york")                   & place_of_publication == "york", "new york", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "new london")                   & place_of_publication == "london", "new london", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "new jersey")                   & place_of_publication == "jersey", "new jersey", place_of_publication),
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
         place_of_publication = ifelse(str_detect(surrounding_words_print, "providence, rhode island")   & is.na(place_of_publication), "providence", place_of_publication),

         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston in n.e.")             & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, n.e.")               & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston: n.e.")               & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, in n. e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston: n. e.")              & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, n. e.")              & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston n.e.")                & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston. n.e.")               & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston, in n.e. ")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston: in n. e.")           & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston n. e.")               & place_of_publication == "e", "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "new haven")                  & place_of_publication == "haven", "new haven", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "boston new england")        , "boston", place_of_publication),
         place_of_publication = ifelse(str_detect(surrounding_words_print, "london, printed. philadelphia, reprinted")   & place_of_publication == "london", "philadelphia", place_of_publication),

         # check and compare with cities information ----
         place_of_publication = ifelse(is.na(place_of_publication), str_extract(evans2$surrounding_words_print, pattern_cities), place_of_publication))

# extract states information ----
evans2 <- evans2 %>%
mutate(state_of_publication = str_extract(surrounding_words_print, pattern_states))

sum(is.na(evans2$place_of_publication)) #1469

table(evans2$place_of_publication)

# 8. SAVE PROCESSED DATA ----
saveRDS(evans2, file = "data/processed/evans/evans_preprocessed.rds")

