# 0. GOAL ----
# Preprocessing the raw data from the ECCO Archive:
# https://textcreationpartnership.org/tcp-texts/ecco-tcp-eighteenth-century-collections-online/
# and save it. This preprocessing will consist of retrieving year and place of
# publication. See also # https://github.com/Text-Creation-Partnership/ECCO-TCP

# With the support of more than 35 libraries, the TCP keyed, encoded, edited,
# and released 2,473 ECCO-TCP texts. A further tranche of 628 texts was keyed
# and encoded but never fully proofed or edited. The texts in this group remain
# useful for many purposes, however, and bring the total of ECCO-TCP texts to
# over 3,000. The publications span the eighteenth century period (i.e.1701-1803)

# 1. LOAD LIBRARIES ----
library(tidyverse)
library(XML)
library(data.table)

# 2. LOAD DATA ----

# finished XML ecco files (see ecco documentation)
fin_folder <- "C:/Projects/AmericanRevolution/revolution/data/raw/ecco/XML/finished/"

# unfinished XML ecco files (see ecco documentation)
unf_folder <- "C:/Projects/AmericanRevolution/revolution/data/raw/ecco/XML/unfinished/"

read_xml_files <- function(file) {
    xml_data <- xml2::read_xml(file)
    text_content <- xml2::xml_text(xml_data)
    df <- data.frame(id = rep(basename(file), length(text_content)), text = text_content, stringsAsFactors = FALSE)
    return(df)
}

# function to read all files from a folder into a data frame
read_folder <- function(infolder) {
    tibble(file = dir(infolder, full.names = TRUE)) %>%
        mutate(text = map(file, read_xml_files)) %>%
        transmute(new_id = basename(file), text) %>%
        unnest(text)
}

# finished texts
# Get a list of all subfolder names within the parent directory
fin_text <- tibble(folder = dir(fin_folder, full.names = TRUE)) %>%
    # Use map() to apply read_folder to each subfolder
    mutate(folder_out = map(folder, read_folder)) %>%
    unnest(cols = c(folder_out)) %>%
    transmute(newsgroup = basename(folder), id, text)

# unfinished texts
unf_text <- tibble(folder = dir(unf_folder, full.names = TRUE)) %>%
    mutate(folder_out = map(folder, read_folder)) %>%
    unnest(cols = c(folder_out)) %>%
    transmute(newsgroup = basename(folder), id, text)

# merge finished and unfinished texts ----
rawdata <- rbind(fin_text, unf_text) %>%
    rename(TCP = id) %>%
    mutate(TCP = stringr::str_remove(TCP,".xml"))

# remove dataframes
rm(fin_text, unf_text)

# 3. LOAD AND MERGE ECCO META DATA ----
ecco <- read.csv("data/raw/ecco/ECCOTCP.csv")

## merge ecco metadata with complete text ----
ecco <- ecco %>% right_join(rawdata, by = "TCP")

# Grouping by date, we can count how many documents are available for each year,
# and plot these numbers
ecco %>%
    group_by(Date) %>%
    filter(Date > 1700 & Date < 1810) %>%
        summarize(count = n(),na.rm = TRUE) %>%
    ungroup() %>%
    ggplot(aes(x = Date, y = count, group = 1)) +
    geom_line() +
    theme_minimal()

ecco <- ecco %>% mutate(text = tolower(text))

# Save data
saveRDS(ecco, file = "data/raw/ecco/ecco_3097.rds")
#ecco <- readRDS(file = "data/raw/ecco/ecco_3097.rds")

# 4. EXTRACT SENTENCE WITH PLACE OF PUBLICATION ----

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

# Apply the extract_surrounding_words function to the ecco text column
ecco <- ecco %>%
    rowwise() %>%
    mutate(surrounding_words_print = extract_surrounding_words(text, target_words))

# 5. DATE OF PUBLICATION ----
# extract date (i.e. year) for those publications who don't have any year
# indication yet. To do so, we look for Roman numerals used to inform on
# publication year: M: 1000 D: 500 C: 100 L: 50 X: 10 V:5 I:1
# For example: 1778 would be MDCCLXXVIII in Roman numerals

# Define a regular expression pattern to match Roman years
pattern <- "m\\.?d\\.?c\\.?(.{11})"

# Use str_extract to extract occurrences that start with mdc (Roman years) from 'text'
ecco$Date2 <- str_extract(str_squish(ecco$text), pattern)

# Remove everything after "i." in the text column
ecco$Date2 <- sub("i\\..*", "i.", ecco$Date2)

# Remove punctuation from the text column
ecco$Date2 <- gsub("[[:punct:]]", "", ecco$Date2)

# Remove everything after the first occurrence of a letter that is not
# "m", "d", "c", "l", "x", "v", or "i"
ecco$Date2 <- sub("[^mdclxivMDCLXIV](.*)", "", ecco$Date2)

# Define a function to convert Roman numerals to years
convertRomanToYear <- function(text) {
    # Convert the text to uppercase
    text <- toupper(text)

    roman_numerals  <- c("M",  "D", "C", "L", "X", "V", "I")
    values          <- c(1000, 500, 100, 50,   10,  5,   1)
    roman_to_arabic <- setNames(values, roman_numerals)

    result <- 0
    previous_value <- 0

    for (char in strsplit(text, "")[[1]]) {
        value <- roman_to_arabic[char]

        if (is.na(value)) {
            # Invalid character, ignore
            next
        }

        if (value > previous_value) {
            # Subtract the previous value (e.g., IV = 4)
            result <- result - 2 * previous_value
        }

        result <- result + value
        previous_value <- value
    }

    return(result)
}

# Convert Roman numerals to years in the text
ecco$Date3 <- sapply(ecco$Date2, convertRomanToYear)

# Replace 0 with NA
ecco$Date3 <- ifelse(ecco$Date3 == 0, NA, ecco$Date3)

# if date was empty but we obtained new information on date via the Roman numerals,
# insert the year in date

sum(is.na(ecco$Date)) # Check before: 624 dates missing

# in case of added information on publication date, add to date column
ecco <- ecco %>%
    mutate(Date = ifelse(is.na(Date) & !is.na(Date3), Date3, Date))

sum(is.na(ecco$Date))  #Check after: 320 dates missing

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

glimpse(ecco)

ecco2 <- ecco %>%
    rowwise() %>%
    mutate(place_of_publication = extract_combined(surrounding_words_print)) %>%
    ungroup() %>%
    select(text,surrounding_words_print, Date, place_of_publication)

# Count the number of NA values in 'place of publication'
sum(is.na(ecco2$place_of_publication)) #367

# Count the number of NA values in date
sum(is.na(ecco2$Date)) #320

table(ecco2$place_of_publication)


# 7. CLEANING PLACE OF PUBLICATION ----
ukcities <- readLines("data/external/uk-towns-and-cities-a.txt") %>%
  as.data.frame() %>%
  rename(city = '.') %>%
  mutate(city = tolower(city))

pattern_cities <- paste0("(?i)(", paste0(ukcities$city, collapse = "\\b|\\b"), "\\b)")

## some manual corrections ----
ecco2 <- ecco2 %>%
    mutate(place_of_publication = ifelse(str_detect(surrounding_words_print, "new york") & place_of_publication == "york", "new york", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "new castle") & place_of_publication == "castle", "newcastle", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "strawberry-hill") & place_of_publication == "hill", "strawberry-hill", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "london") & str_detect(place_of_publication,"london"), "london", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "dvblin"), "dublin", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "dʋblin"), "dublin", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "tatedublin"), "dublin", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "lonon"), "london", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "leedes"), "leeds", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "worcsster"), "worcesster", place_of_publication),
           place_of_publication = ifelse(str_detect(surrounding_words_print, "street"), " ", place_of_publication),
           place_of_publication = ifelse(grepl("london", place_of_publication, ignore.case = TRUE), "london", place_of_publication),
           place_of_publication = ifelse(grepl("dublin", place_of_publication, ignore.case = TRUE), "dublin", place_of_publication),
           place_of_publication = ifelse(grepl("edinburgh", place_of_publication, ignore.case = TRUE), "edinburgh", place_of_publication),
           place_of_publication = ifelse(grepl("york", place_of_publication, ignore.case = TRUE), "york", place_of_publication),
           place_of_publication = ifelse(grepl("adigonchester", place_of_publication, ignore.case = TRUE), "chester", place_of_publication),
           place_of_publication = ifelse(grepl("cambridge", place_of_publication, ignore.case = TRUE), "cambridge", place_of_publication),
           place_of_publication = ifelse(grepl("glasgow", place_of_publication, ignore.case = TRUE), "glasgow", place_of_publication),
           place_of_publication = ifelse(grepl("newcastle", place_of_publication, ignore.case = TRUE), "newcastle", place_of_publication),
           place_of_publication = ifelse(grepl("castlecarlisle", place_of_publication, ignore.case = TRUE), "carlisle", place_of_publication),
           place_of_publication = ifelse(place_of_publication == "edin", "edinburgh", place_of_publication),
           place_of_publication = ifelse(place_of_publication == "edmunds", "st. edmunds", place_of_publication),
           place_of_publication = ifelse(place_of_publication == "shields", "north shields", place_of_publication),
           place_of_publication = ifelse(place_of_publication == "wight", "isle of wight", place_of_publication),

           # check and compare with cities information ----
           place_of_publication = ifelse(is.na(place_of_publication), str_extract(surrounding_words_print, pattern_cities), place_of_publication),

           # remove irrelevant words
           place_of_publication = ifelse(place_of_publication %in%
          c("administration","fire","goldyng","newry","notes","our","author",
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
          "under","upon","with","worth","eye",
          "contributors"), NA, place_of_publication))


ecco2 <- ecco2 %>%
  mutate(place_of_publication = str_replace(place_of_publication, "perth", "london"),
         place_of_publication = str_replace(place_of_publication, "tyne", "newcastle"),
         place_of_publication = str_replace(place_of_publication, "new castle", "newcastle"),
         place_of_publication = str_replace(place_of_publication, "vienna", "london")) %>%
  mutate(place_of_publication = ifelse(place_of_publication == "bury", NA, place_of_publication),
         place_of_publication = ifelse(place_of_publication == "windsor", NA, place_of_publication)) %>%
  mutate(place_of_publication = str_replace(place_of_publication, "newcastle", "newcastle upon tyne"))


sum(is.na(ecco2$place_of_publication)) #489

table(ecco2$place_of_publication)

# 8. SAVE PROCESSED DATA ----
saveRDS(ecco2, file = "data/processed/ecco/ecco_preprocessed.rds")

# 9. ADD COORDINATES ----
world_cities <- read.csv("data/external/3_allcities.csv", sep = ';') %>%
  as.data.frame() %>%
  rename(city       = Name,
         state      = Admin1.Code,
         country    = Country.name.EN,
         citynames  = Alternate.Names,
         population = Population) %>%

  mutate(city       = tolower(city),
         state      = tolower(state),
         country    = tolower(country),
         citynames  = tolower(citynames)) %>%

  # we'll focus on cities in Europe and North America
  filter(str_detect(country, "united kingdom|ireland|france")) %>%
  separate("Coordinates", c("latitude","longitude"), sep = " ",remove = F) %>%
  select(city,state, country, latitude,longitude) %>%
  filter(country != "") %>%
  mutate(state = if_else(str_detect(state, "^[0-9]"), NA_character_, state)) %>%
  rowwise() %>%
  mutate(latitude  = as.numeric(gsub(",","",latitude)),
         longitude = as.numeric(longitude)) %>%
  distinct(city,state,country,.keep_all = TRUE)

## merge with longitude and latitude information
ecco2 <- ecco2 %>%
  left_join(world_cities,
            by = c("place_of_publication"    = "city")) %>%
  mutate(state   = ifelse(place_of_publication == "philadelphia", "pennsylvania",state),
         country = ifelse(place_of_publication == "philadelphia", "united states",country),
         place_of_publication = ifelse(place_of_publication == " ", NA, place_of_publication),
         place_of_publication = ifelse(place_of_publication == "tonquin", NA, place_of_publication),
         place_of_publication = ifelse(place_of_publication == "westminster", "london", place_of_publication))

# save results
saveRDS(ecco2, file = "data/processed/ecco/ecco_geo_ref.rds")
