# 0. GOAL ----
# Preprocessing the raw data from the Founders Online Archive and save it. 
# the raw data was saved in the folder 'datanew' and is the result of running
# the script 'scrape founders online.R'. This script integrates and builds further
# upon the previous data exploration on the founders online by Thomas Hoekstra
# (github link:)

# 1. LOAD IN LIBRARIES ----
library(tidyverse)
library(data.table)
library(polyglotr) # for translation of texts

local <- 'C:/Projects/AmericanRevolution/Syntax/02_cleaning/datanew'
setwd(local)

# 2. LOAD IN DATA ----
gfiles <- list.files(path = local, 
                     pattern = "rds", 
                     recursive = TRUE) 

ffc <- lapply(gfiles, readRDS) %>% rbindlist()

# OR

# f1 <- read_rds("datanew/Adams_Presidency.rds")
# f2 <- read_rds("datanew/Colonial.rds")
# f3 <- read_rds("datanew/Confederation_Period.rds")
# f4 <- read_rds("datanew/Jefferson_Presidency.rds")
# f5 <- read_rds("datanew/Madison_Presidency.rds")
# f6 <- read_rds("datanew/post-Madison_Presidency.rds")
# f7 <- read_rds("datanew/Revolutionary_War.rds")
# f8 <- read_rds("datanew/Washington_Presidency.rds")
# 
# ffc <- rbind(f1,f2,f3,f4,f5,f6,f7,f8)

dim(ffc) #183715 obs
setDT(ffc)

# count the amount of words in each letter
ffc[, doc_length := lengths(gregexpr("\\W+", text))]


# 3. SAVE COMBINED DATA AS CSV ----
# use this dataset in Hoekstra's code to apply its preprocessing steps
write.csv(ffc, "founders_online_archive_raw.csv", row.names = FALSE)

# 4. PREPROCESS DATA ----

## A) INCLUDE HOEKSTRA CODE ----
#TODO: use reticulate and incorporate python code here


## B) ADD METADATA ----

# retrieve the data after having run Thomas Hoekstra's preprocessing script
ffc <- read.csv("/Hoekstra/founders_online_data_exploration_data.csv")
setDT(ffc)

ffc$Month_Yr <- sub('-[0-9]*$', '', ffc$date_from)
ffc$Month_Yr <- as.Date(paste0(ffc$Month_Yr, '-01'), format = '%Y-%m-%d')

# A function for displaying a letter & metadata inline
display_letter <- function(x) {
    a1 <- strsplit(x$og_text, '\n')[[1]]
    a2 <- trimws(a1)
    a3 <- subset(a2, a2 != '')
    a4 <- paste(a3, collapse = '\n')
    a4a <- paste0('Author: ', x$author, '\n>', 
                 'Recipient: ', x$recipient, '\n>', 
                 'Date: ', x$date_to, '\n>', 
                 'Period: ', x$period, '\n>\n', 
                 a4)
    a5 <- gsub(' *\n', '  \n', a4a)
    paste('>', gsub(' *(\n*) *$', '\\1', a5))
}

display_letter(ffc[150681,])

# filter out correspondence to oneself, if any
ffc2 <- ffc %>% filter(authors != recipients)

# remove row when there is no letter
ffc2 <- ffc2 %>% filter(text != "")

# remove 'empty nodes'
# This data includes only letters with known authors and recipients, and does not
# include data with no author or no recipient, such as receipts or account books 
empty <- c("","——","Unknown","UNKNOWN")

ffc2 <- subset(ffc2, !(authors %in% empty | recipients %in% empty))

## C) CREATE SOURCE-TARGET PAIRS ----
# In the case of multiple authors or multiple recipients, these were split into
# multiple records with only one author and one recipient each. For example, 
# the letter "Sarah Read to Benjamin and Deborah Franklin, 10 April 1734," 
# was split into two records: one with ‘author: Sarah Read’ and ‘recipient: 
# Benjamin Franklin,’ and the other with author: ‘Sarah Read, recipient: Deborah Franklin.’ 

ffc2$authors2    <- gsub("([&.,|])|[[:punct:]]", "\\1", ffc2$authors)
ffc2$recipients2 <- gsub("([&.,|])|[[:punct:]]", "\\1", ffc2$recipients)

# replace | with ; to have one uniform separator
ffc2$authors2    <- gsub("\\|", ";", ffc2$authors2)
ffc2$recipients2 <- gsub("\\|", ";", ffc2$recipients2)

# in case of multiple authors, these are split so that each author has its own row
ffc3 <- ffc2 %>% 
    mutate(authors2 = strsplit(as.character(authors2), ";")) %>% 
    unnest(authors2) %>% 
    mutate(authors2 = trimws(authors2))

# Next, building further on the separated authors and in case of multiple recipients, 
# these are split as well so that each recipient has its own row
ffc4 <- ffc3 %>% 
    mutate(recipients2 = strsplit(as.character(recipients2), ";")) %>% 
    unnest(recipients2) %>% 
    mutate(recipients2 = trimws(recipients2))

ffc4 <- ffc4 %>% filter(authors2 != recipients2)
ffc4 <- ffc4 %>% mutate(year = as.integer(year(date_from)),
                        end.year  =  as.integer(year(date_to)))
glimpse(ffc4)

## D) REMOVE DUPLICATE LETTERS ----
ffc5 <- ffc4 %>%
    group_by(authors2,recipients2, date_from,date_to,year,end.year) %>%
        mutate(amount = row_number()) %>%
        # amount != 1 suggests multiple letters on the same day by author A to recipient B
        # if this is indeed the case (e.g. amount = 2), then do a similarity check with the text
        # string one before (lag(content)) within the same group_by 
        mutate(diff = ifelse(amount != 1, RecordLinkage::levenshteinSim(text,lag(text)),0)) %>% 
    ungroup() %>% 
    filter(diff < 0.90) 

## E) TRANSLATE ALL NON-ENGLISH TEXTS ----

# Function to detect language
detect_language <- function(row) {
    text <- row[["text"]]
    tryCatch({
        language <- language_detect(text)
        return(language)
    }, error = function(e) {
        # Handle errors, e.g., return "Unknown" for problematic rows
        return("Unknown")
    })
}

# Apply the function to each row and create a new 'language' column
# Note; this takes quite some time!!
ffc5$language <- apply(ffc5, 1, detect_language)
table(ffc5$language)

# Function to translate text to English
translate_to_english <- function(text, source_lang) {
    if (!is.na(text) && nchar(text) > 0) {
        
        # If the source language is not English, it uses the google_translate 
        # function to translate the text to English 
        if (source_lang %in% c("it")) {
            translated <- google_translate(text, target_language = "en", source_language = source_lang)
            return(ifelse(nchar(translated) > 0, translated, "Translation Failed"))
        } else {
            return(text)
        }
    } else {
        return("No Text")
    }
}

translate_to_english <- function(text, source_lang) {
    # It takes a text string and its corresponding source language as inputs 
    
    # If the source language is not English, it uses the google_translate 
    # function to translate the text to English 
    if (source_lang != "en" && source_lang != "No Text" && source_lang != "Unknown") {
        translated <- google_translate(text, target_language = "en", source_language = source_lang)
        return(translated)
        
        #otherwise, it returns the original text 
    } else {
        return(text)
    }
}

# Add a translated column. The map2_chr function takes the text_column and 
# language column as inputs for each row and uses the translate_to_english function
# to translate the text while considering the source language from the language column 
test <- ffc5 %>% 
    mutate(translated_text = map2_chr(text, language, ~ translate_to_english(.x, .y)))

## F) CREATE UNIX-TIME VARIABLE ----
ffc6 <- ffc5 %>% 
    arrange(date_from) %>% 
    # create id
    mutate(id = rownames(ffc5)) %>% 
    select(-authors,-recipients) %>% 
    rename(authors = authors2,recipients = recipients2) %>% 
    select(id,title,date_from,date_to,Month_Yr,year,end.year,period,authors,recipients,text,doc_length) %>% 
    arrange(date_from) %>% 

    # Since several later analyses uses unix timestamps which can only work with dates
    # after 1st January 1970, we convert the sending dates of letters into time differences
    # compared to the very first date of the revolution

    mutate(date_from_dt = date(date_from),
           timestart   = min(date_from_dt), # first date in dataset
           # number of days from earliest sending date in revolution to the 
           # current sending date. Store time differences in separate column 
           # called 'time'. Note that pathpy later requires a column called 'time'
           time = as.numeric(date_from_dt - timestart)) %>% 
    arrange(time) %>% 
    select(-date_from_dt,-timestart,-date_to) %>% 
    rename(sending_date = date_from)

dim(ffc6) # 164062 letters
glimpse(ffc6)

## G) CREATE IDS ----
# create a table of all people to map to numeric ids
correspondents <- unique(c(ffc6$recipients,ffc6$authors))

# unique nodes/vertices
length(correspondents) 
head(correspondents, 10)

# ids for authors and recipients
ffc6$sender_id   <- match(ffc6$authors   , correspondents)
ffc6$receiver_id <- match(ffc6$recipients, correspondents)

# 5) SAVE PREPROCESSED DATA ----
write.csv(ffc6, "../../Data/FoundingFathers/ffc_total.csv", row.names = FALSE)

## A) SAVE DICTIONARY IDS AND NAMES ----

# create a table of all people to map to numeric ids
pp_nodes_sender   <- ffc6 %>% filter(!is.na(sender_id)) %>% select(sender_id, authors) %>% 
    rename(person_id = sender_id, last_name = authors)

pp_nodes_receiver <- ffc6 %>% filter(!is.na(receiver_id)) %>% select(receiver_id, recipients) %>% 
    rename(person_id = receiver_id, last_name = recipients)

nodes <- rbind(pp_nodes_sender,pp_nodes_receiver) %>% distinct(person_id, last_name)

# this dataset is used in the pathpy analysis
write.csv(nodes, "nodes_ff.csv", row.names = FALSE)