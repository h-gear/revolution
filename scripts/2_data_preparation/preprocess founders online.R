# 0. GOAL ----
# Preprocessing the raw data from the Founders Online Archive [Founders Online]
# (https://www.founders.archives.gov/) and save it. This preprocessing will
# consist of removing irrelevant documents, removing non-letters, and merging
# split nodes (name disambiguation).

# The raw data was saved in the folder data/raw/founders and is the result of
# running the script 'scrape founders online.R'. This script integrates the
# previous data exploration on the founders online by Thomas Hoekstra (github
# link:) https://github.com/Rohrym/letters_of_the_revolution_project

# 1. LOAD LIBRARIES ----
library(tidyverse)
library(data.table)
library(polyglotr)  # for translation of texts
library(furrr)      # for parallel processing

# 2. LOAD DATA ----
gfiles <- list.files(path      = "data/raw/founders",
                     pattern   = "rds",
                     recursive = TRUE)

path <- "data/raw/founders/"
ffc <- lapply(paste0(path, gfiles), readRDS) %>%
  rbindlist() #183715 observations

setDT(ffc)

# 3. PREPROCESSING DATA ----

# Here we will start manipulating the data to make it fit our needs.
# First of all, all duplicate texts and non-correspondence will be removed. This
# because the project is exclusively interested in correspondence, and it can
# happen that unique letters appear more then once in the Founders dataset due
# to the way it is structured. These operations will be conducted by dropping
# any data entries without recipient (as you would need at least one recipient
# to call something correspondence), and to drop any exact duplicates.

# In the rest of this section I will be dealing with name disambiguation *i.e.*
# name attribution to specific people. Due to spelling mistakes and other
# problems with the encoding of the metadata for the Founders Dataset did the
# names of certain individuals split up into multiple names. By replacing
# (parts of) names I am making the naming of these individuals consistent again
# across the database.

# The results of this section will be saved into a new csv file which will be
# used for further the data exploration.

# count the amount of words in each letter
ffc[, doc_length := lengths(gregexpr("\\W+", text))]

# dropping any content without recipients and removing duplicate texts.
# Drop rows with missing values in the 'recipients' column
ffc <- ffc[!is.na(ffc$recipients), ]

# Drop duplicates based on the 'text' column
ffc <- ffc[!duplicated(ffc$text), ]

# Replace multiple patterns in 'authors' and 'recipients' columns
ffc[, c("authors", "recipients")] <- lapply(ffc[, c("authors", "recipients")], function(x) {
        gsub(', & Fins', ' & Fins', x, fixed = TRUE) %>%
        gsub(', & Cie.', ' & Cie.', ., fixed = TRUE) %>%
        gsub(', & ', ' | ', .        , fixed = TRUE) %>%
        gsub(', Jr.', ' Jr.', .      , fixed = TRUE) %>%
        gsub(', Sr.', ' Sr.', .      , fixed = TRUE) %>%
        gsub(' \\(business\\)', '', ., fixed = TRUE)
})

# Correcting inconsistently encoded names in 'authors' and 'recipients' columns
ffc[, c("authors", "recipients")] <- lapply(ffc[, c("authors", "recipients")], function(x) {

        gsub('Adams, Abigail Smith', 'Adams, Abigail', x, fixed = TRUE) %>%
        gsub('Hamilton, Alexander \\(Lieutenant Colonel\\)', 'Hamilton, Alexander', ., fixed = TRUE) %>%
        gsub('Steuben, Major General', 'Steuben, Friedrich Wilhelm Ludolf Gerhard Augustin, Baron [von]', ., fixed = TRUE) %>%
        gsub('Steuben, Friedrich Wilhelm Ludolf Gerhard Augustin, Baron von', 'Steuben, Friedrich Wilhelm Ludolf Gerhard Augustin, Baron [von]', ., fixed = TRUE) %>%
        gsub('Steuben, Friedrich Wilhelm Ludolf Gerhard Augustin, baron von', 'Steuben, Friedrich Wilhelm Ludolf Gerhard Augustin, Baron [von]', ., fixed = TRUE) %>%
        gsub('Steuben, Baron von', 'Steuben, Friedrich Wilhelm Ludolf Gerhard Augustin, Baron [von]', ., fixed = TRUE) %>%
        gsub('Vergennes, Charles Gravier, comte de', 'Vergennes, Charles Gravier, Comte de', ., fixed = TRUE) %>%
        gsub('Dumas, Charles-Guillaume-Frédéric', 'Dumas, Charles William Frederic', ., fixed = TRUE) %>%
        gsub('Dumas, C. W. F.', 'Dumas, Charles William Frederic', ., fixed = TRUE) %>%
        gsub('Boudinot, Elias Jr.', 'Boudinot, Elias', ., fixed = TRUE) %>%
        gsub('Bowdinot, Elias', 'Boudinot, Elias', ., fixed = TRUE) %>%
        gsub('Schuyler, Philip John', 'Schuyler, Philip', ., fixed = TRUE) %>%
        gsub('Lee, Henry Jr.', 'Lee, Henry', ., fixed = TRUE) %>%
        gsub('Nelson, Thomas Jr.', 'Nelson, Thomas', ., fixed = TRUE) %>%
        gsub('Muhlenberg, John Peter Gabriel', 'Muhlenberg, Peter', ., fixed = TRUE) %>%
        gsub('Harrison, Benjamin Sr.', 'Harrison, Benjamin', ., fixed = TRUE) %>%
        gsub('Williams, Jonathan Jr.', 'Williams, Jonathan', ., fixed = TRUE) %>%
        gsub('Wharton, Thomas Jr.', 'Wharton, Thomas', ., fixed = TRUE) %>%
        gsub('Virginia Delegates in Congress', 'Virginia Delegates', ., fixed = TRUE) %>%
        gsub('Virginia Delegates, American Continental Congress', 'Virginia Delegates', ., fixed = TRUE) %>%
        gsub('Thaxter, John Jr.', 'Thaxter, John', ., fixed = TRUE) %>%
        gsub('Schweighauser, John Daniel', 'Schweighauser, Jean-Daniel', ., fixed = TRUE) %>%
        gsub('Rendon, Francisco', 'Rendón, Francisco', ., fixed = TRUE) %>%
        gsub('Neufville, Jean de', 'Neufville, Jean [de]', ., fixed = TRUE) %>%
        gsub('Neufville, Leendert de', 'Neufville, Leonard [de]', ., fixed = TRUE) %>%
        gsub('McDougall, Maj. Gen. Alexander', 'McDougall, Alexander', ., fixed = TRUE) %>%
        gsub('Malcom, Colonel William', 'Malcom, William', ., fixed = TRUE) %>%
        gsub('Létombe, Philippe-André-Joseph de', 'Létombe, Philippe André Joseph de', ., fixed = TRUE) %>%
        gsub('Johnson, Thomas Jr.', 'Johnson, Thomas', ., fixed = TRUE) %>%
        gsub('Hamilton, Alexander (Lieutenant Colonel)', 'Hamilton, Alexander', ., fixed = TRUE) %>%
        gsub('Greene, Nathaniel', 'Greene, Nathanael', ., fixed = TRUE) %>%
        gsub('Gérard, Conrad-Alexandre', 'Gérard, Conrad Alexandre', ., fixed = TRUE) %>%
        gsub('Genet, Edme-Jacques', 'Genet, Edmé Jacques', ., fixed = TRUE) %>%
        gsub('Dana, Francis M.', 'Dana, Francis', ., fixed = TRUE) %>%
        gsub('Cooke, Nicholas Sr.', 'Cooke, Nicholas', ., fixed = TRUE) %>%
        gsub('Chaumont, Jacques Donatien, Leray de', 'Chaumont, Jacques-Donatien Le Ray de', ., fixed = TRUE) %>%
        gsub('Church, William Singleton', 'Digges, Thomas', ., fixed = TRUE) %>%
        gsub('Fitzpatrick, William', 'Digges, Thomas', ., fixed = TRUE) %>%
        gsub('Dundas, T.', 'Digges, Thomas', ., fixed = TRUE) %>%
        gsub('Ross, Timothy D.', 'Digges, Thomas', ., fixed = TRUE) %>%
        gsub('Beaumarchais, Pierre-Augustin Caron de', 'Beaumarchais, Pierre Augustin Caron de', ., fixed = TRUE) %>%
        gsub('Rochambeau, Comte de', 'Rochambeau, Jean-Baptiste Donatien de Vimeur, comte de', ., fixed = TRUE) %>%
        gsub('Rochambeau, Jean-Baptiste-Donatien de Vimeur, comte de, Jean-Baptiste Donatien de Vimeur, comte de', 'Rochambeau, Jean-Baptiste Donatien de Vimeur, comte de', ., fixed = TRUE) %>%
        gsub('Barbé de Marbois, François', 'Barbé-Marbois (Barbé de Marbois), François', ., fixed = TRUE) %>%
        gsub('Barbé-Marbois, François de', 'Barbé-Marbois (Barbé de Marbois), François', ., fixed = TRUE) %>%
        gsub('Barbé-Marbois, François', 'Barbé-Marbois (Barbé de Marbois), François', ., fixed = TRUE) %>%
        gsub('Barbé-Marbois, Marquis de', 'Barbé-Marbois (Barbé de Marbois), François', ., fixed = TRUE) %>%
        gsub('Barbé-Marbois, Pierre-François', 'Barbé-Marbois (Barbé de Marbois), François', ., fixed = TRUE) %>%
        gsub('Marbois, François Barbé de', 'Barbé-Marbois (Barbé de Marbois), François', ., fixed = TRUE) %>%
        gsub('Marbois, François Marquis de Barbé-Marbois', 'Barbé-Marbois (Barbé de Marbois), François', ., fixed = TRUE) %>%
        gsub('La Vauguyon, Paul-François de Quélen de Stuer de Caussade, duc de', 'La Vauguyon, Paul François de Quélen de Stuer de Causade, Duc de', ., fixed = TRUE) %>%
        gsub('Dubuysson des Aix, Charles-François, chevalier', 'Du Buysson des Aix, Charles-François, vicomte', ., fixed = TRUE) %>%
        gsub('Destouches, —— (f. 1779–80)', 'Charles-René-Dominique Sochet', ., fixed = TRUE) %>%
        gsub('Destouches, —— (f. 1780–1782)', 'Destouches, Charles-René-Dominique Sochet', ., fixed = TRUE) %>%
        gsub('Destouches, ——', 'Destouches, Charles-René-Dominique Sochet', ., fixed = TRUE) %>%
        gsub('Destouches, Chevalier', 'Destouches, Charles-René-Dominique Sochet', ., fixed = TRUE) %>%
        gsub('Destouches, Charles-René-Dominique Sochet \\(f. 1779–80\\)', 'Destouches, Charles-René-Dominique Sochet', ., fixed = TRUE) %>%
        gsub('Destouches, Charles-René-Dominique Sochet \\(f. 1780–1782\\)', 'Destouches, Charles-René-Dominique Sochet', ., fixed = TRUE) %>%
        gsub('Chaumont, Jacques-Donatien Le Ray de', 'Chaumont, Jacques Donatien, Leray de', ., fixed = TRUE) %>%
        gsub('Sartine, Antoine-Raymond-Gualbert-Gabriel de', 'Sartine, Antoine Raymond Jean Gualbert Gabriel de', ., fixed = TRUE) %>%
        gsub('Armand, Charles', 'Armand (Armand-Charles Tuffin, marquis de La Rouërie)', ., fixed = TRUE) %>%
        gsub('Armand Tuffin, Charles, marquis de La Rouërie', 'Armand (Armand-Charles Tuffin, marquis de La Rouërie)', ., fixed = TRUE) %>%
        gsub('Estaing, Charles-Hector Theodat, comte d’', 'Estaing, Charles-Hector Théodat, comte d’', ., fixed = TRUE) %>%
        gsub('Estaing, Charles-Hector, comte d’', 'Estaing, Charles-Hector Théodat, comte d’', ., fixed = TRUE) %>%
        gsub('Estaing, Charles-Henri, comte d’', 'Estaing, Charles-Hector Théodat, comte d’', ., fixed = TRUE) %>%
        gsub('Caracciolo, Domenico, Marchese di Villamaina', 'Caracciolo, Domenico, Marchesse di Villa Marina', ., fixed = TRUE) %>%
        gsub('Arendt, Henry Leonard Philip, baron d’', 'Arendt, Henry Leonard Philip', ., fixed = TRUE) %>%
        gsub('Trumbull, Jonathan Sr.', 'Trumbull, Jonathan', ., fixed = TRUE) %>%
        gsub('Berubé de Costentin, ——', 'Costentin, Berubé de', ., fixed = TRUE) %>%
        gsub('Rocquette, Jacques, Elsevier, T. A.', 'Rocqùette, J.,Th. A. Elsevier, & P. Th. Rocqùette,', ., fixed = TRUE) %>%
        gsub('Dubbeldemuts, Adrianus', 'Dubbeldemuts, F. & A.', ., fixed = TRUE) %>%
        gsub('Dubbeldemuts, Franco', 'Dubbeldemuts, F. & A.', ., fixed = TRUE) %>%
        gsub('Parsons, Samuel Holden', 'Parsons, Samuel H.', ., fixed = TRUE) %>%
        gsub('Stirling, Lord \\(née William Alexander\\)', 'Stirling, Lord (né William Alexander)', ., fixed = TRUE) %>%
        gsub('Alexander, William Lord Stirling', 'Stirling, Lord (né William Alexander)', ., fixed = TRUE) %>%
        gsub('Stirling, Major General', 'Stirling, Lord (né William Alexander)', ., fixed = TRUE) %>%
        gsub('Alexander, William', 'Stirling, Lord (né William Alexander)', ., fixed = TRUE) %>%
        gsub('La Lande & Fynje, de', 'La Lande & Fynje', ., fixed = TRUE) %>%
        gsub('Horneca, Fizeaux & Cie.', 'Horneca, Fizeaux & Co.', ., fixed = TRUE) %>%
        gsub('Staphorst, Nicholas & Jacob van', 'Staphorst, Nicolaas & Jacob van', ., fixed = TRUE) %>%
        gsub('Ambler, Jaquelin \\(Jacquelin\\)', 'Ambler, Jacquelin', ., fixed = TRUE) %>%
        gsub('La Luzerne, Anne César, Chevalier de', 'La Luzerne, Anne-César, chevalier de', ., fixed = TRUE)
})

# some filtering will be done to remove unknown, anonymous and otherwise
# non-existent authors. Additionally entries with certain other authors or
# recipients will be removed due to various reasons.

# Defining function to remove non-existent and unwanted authors and recipients
remove_author_recipient <- function(df, names) {
    for (name in names) {
        df <- df[!(df$authors == name | df$recipients == name), ]
    }
    df <- df[!is.na(df$authors), ]
    return(df)
}

# List of names to be removed
names_to_remove <- c('Unknown', 'UNKNOWN', 'Anonymous', 'First Joint Commission at Paris',
                     'American Commissioners', 'Son', 'Son ()', 'Sons ()',
                     'Cie.', ' Cie.', 'fils','et al.', 'Zoon ()',
                     'American Peace Commissioners', 'William Bradford',
                     'Smith, William', 'Smith, John', 'Thornton, John',
                     'Rocquette, Pieter Th.')

# Applying the function to the data frame
ffc <- remove_author_recipient(ffc, names_to_remove)

# ADD METADATA TO LETTERS
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

# example of displaying letter
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

## A) CREATE SOURCE-TARGET PAIRS ----
# In the case of multiple authors or multiple recipients, these were split into
# multiple records with only one author and one recipient each. For example,
# the letter "Sarah Read to Benjamin and Deborah Franklin, 10 April 1734,"
# was split into two records: one with ‘author: Sarah Read’ and ‘recipient:
# Benjamin Franklin,’ and the other with author: ‘Sarah Read, recipient:
# Deborah Franklin.’

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

# Next, building further on the separated authors and in case of multiple
# recipients,these are split as well so that each recipient has its own row
ffc4 <- ffc3 %>%
    mutate(recipients2 = strsplit(as.character(recipients2), ";")) %>%
    unnest(recipients2) %>%
    mutate(recipients2 = trimws(recipients2))

ffc4 <- ffc4 %>% filter(authors2 != recipients2)
ffc4 <- ffc4 %>% mutate(year = as.integer(year(date_from)),
                        end.year  =  as.integer(year(date_to)))
glimpse(ffc4)

## B) REMOVE DUPLICATE LETTERS ----
ffc5 <- ffc4 %>%
    group_by(authors2, recipients2, date_from, date_to, year, end.year) %>%
        mutate(amount = row_number()) %>%

        # amount != 1 suggests multiple letters on the same day by author A to
        # recipient B if this is indeed the case (e.g. amount = 2), then do a
        # similarity check with the text string one before (lag(content)) within
        # the same group_by
        mutate(diff = ifelse(amount != 1, RecordLinkage::levenshteinSim(text,lag(text)),0)) %>%
    ungroup() %>%
    filter(diff < 0.90) %>%
  select(-amount,-diff)

saveRDS(ffc5, file = "data/interim/ffc_no_duplicates.rds")
ffc5 <- readRDS("data/interim/ffc_no_duplicates.rds")

## C) TRANSLATE NON-ENGLISH TEXTS ----

### 1. Letters already translated? ----
# first search for all letters already translated by the editors which then
# come after the original letter. We then keep only the translated part (in English).
# Such an existing translation is marked by the words "editors’ translation"
# in the text

# these are all the different ways in which the editors marked their translations
edit_translated <- c("editors’ translation ","EDITORS’ TRANSLATION ",
                     "Editors’ Translation ","Editors’ translation ",
                     "editors’ Translation ")

# Create a regular expression pattern dynamically
pattern <- paste(edit_translated, collapse = "|")

# Add new variable 'ed_trans' and set to 1 if any of the editor words occur, else 0
ffc5 <- ffc5 %>% mutate(ed_trans = as.integer(str_detect(text, pattern)))
table(ffc5$ed_trans) #733 translations by editors

ffc5 <- ffc5 %>%
  separate(text, c("original", "translation"), sep = pattern, remove = F)

# We replace the text in the text column with the corresponding translation column
# is not NA, and keep the original 'text' when 'translation' is NA, the latter
# meaning there is either no translation by the editors or the text was already
# in English)
ffc5$text <- ifelse(!is.na(ffc5$translation),ffc5$translation, ffc5$text)
ffc5 <- ffc5 %>% select(-original,-translation,-ed_trans)

### 2. Detect the language of the letter ----

detect_language <- function(text) {
    tryCatch({
      # This function detects the language of a given text using the Google Translate API
        language <- language_detect(text)
        return(language)
    }, error = function(e) {
        # Handle errors, e.g., return "Unknown" for problematic rows
        return("Unknown")
    })
}

# setup 11 threads
plan(multisession, workers = 11)

# Set seed globally
future::plan(seed = TRUE)

# detect the language of each letter
ffc5 <- ffc5 %>%
    mutate(language = furrr::future_map(text, detect_language)) %>%
    as.data.frame()

saveRDS(ffc5, file = "data/interim/ffc_with_language_information.rds")

# Read stored data with information on type of language, if necessary
ffc5 <- readRDS(file = "data/interim/ffc_with_language_information.rds")

# Save subset of letters with problematic language detection
ffc5_language_unknown <- ffc5 %>% mutate(language = unlist(language)) %>%
  filter(language == "Unknown") %>%
  mutate(texts = str_squish(text))

write.csv(ffc5_language_unknown, "data/interim/ffc_language_unknown.csv", row.names = FALSE)

ffc5 <- ffc5 %>% mutate(language = unlist(language)) %>%
  filter(language != "Unknown")
glimpse(ffc5)

# get an overview of the number of letters per language (e.g., fr = 6033 letters)
frequency_table <- ffc5 %>% count(language) %>% arrange(desc(n))
frequency_table
#Unknown: 1998 letters

# # small manual correction
# french_words <- c("Monsieur", "Messieurs", "Messrs", "c'est", "J’ai")
# french_words <- c("votre")
#
# checken <- ffc5 %>%
#   filter(language == "Unknown") %>%
#   mutate(
#     language = if (any(str_detect(text, regex(paste(french_words, collapse = "|"), ignore_case = TRUE)))) {
#       "fr"
#     } else {
#       as.character(language)
#     }
#   )
#
# checken <- ffc5 %>%
#   filter(language == "Unknown") %>%
#   mutate(
#     has_french_word = ifelse(
#       any(str_detect(text, regex(paste(french_words, collapse = "|"), ignore_case = TRUE))),
#       1,
#       0
#     )
#   )


### 3. Translate the still non-English letters with google_translate ----

# setup 11 threads
plan(multisession, workers = 11)

# Set seed globally
future::plan(seed = TRUE)

translated_texts <- ffc5 %>%

  # 6854 letters remain to be translated into English
  filter(language != "en") %>%

  mutate(translation = furrr::future_map(text,
                       ~ google_translate(.x,
                       target_language = "en",
                       source_language = "auto"),
                       .options = furrr_options(seed = 1))) %>%
  select(authors2,recipients2,text,translation) %>%
  unnest(cols = c(translation)) %>% # unnest the translation column

  mutate(
    translation = str_squish(translation),
    translation = ifelse(startsWith(translation, "body{overflow:auto!important;display:block!important;}"), NA, translation)
  )

saveRDS(translated_texts, file = "data/interim/translated_texts.rds")
#translated_texts <- readRDS(file = "data/interim/translated_texts.rds")

### 4. Handle translation problem cases ----

# Some letters (# 303) were not correctly translated in the step above using
# google_translate from the polyglotr package. We therefore 'manually' translated
# these letters using the documents import in google translate

# select the non-translate letters
not_translated <- translated_texts %>% filter(is.na(translation)) %>%
  mutate(id = row_number()) %>%
  select(id,authors2,recipients2,text)
writexl::write_xlsx(not_translated, "data/interim/problemcases_to_translate.xlsx")

# import into google translate

# Read in the excel file with the now correctly translated texts
google_translated <- readxl::read_excel("data/interim/problemcases_translated.xlsx") %>%
  rename(translation = text) %>%
  select(id,translation)

problems_translated <- not_translated %>%
  left_join(google_translated, by = c("id")) %>%
  select(authors2,recipients2,text,translation)

# join all translated letters together in one dataframe
translated_texts <- translated_texts %>%
  left_join(problems_translated, by = c("authors2","recipients2","text")) %>%
  # update the translation variable
  mutate(translation.x = ifelse(is.na(translation.x), translation.y, translation.x)) %>%
  select(-translation.y) %>%
  rename(translation = translation.x)

# Join all translated texts back into the original dataframe
ffc5_translated <- ffc5 %>%
  left_join(translated_texts, by = c("authors2","recipients2","text")) %>%

  # update the text variable with the translated text, if any
  mutate(text = ifelse(is.na(translation), text, translation))

### 4. Save data (all English now) ----
saveRDS(ffc5_translated, file = "data/interim/ffc5_translated.rds")

ffc5_translated <- readRDS(file = "data/interim/ffc5_translated.rds")

## D) CREATE UNIX-TIME VARIABLE ----
# Since several analyses in this project uses unix timestamps which works only
# with dates after 1st January 1970, we convert the sending dates of letters
# into time differences compared to the very first available date

ffc6 <- ffc5_translated %>%
    arrange(date_from) %>%

    # create id
    mutate(id = rownames(ffc5)) %>%
    select(-authors,-recipients) %>%
    rename(authors = authors2,recipients = recipients2) %>%
    select(id,title,date_from,date_to,Month_Yr,year,end.year,period,authors,recipients,text,doc_length) %>%
    arrange(date_from) %>%
    mutate(date_from_dt = date(date_from),
           timestart   = min(date_from_dt), # first date in dataset

           # number of days from earliest sending date to the current sending
           # date. Store time differences in nw column 'time'
           time = as.numeric(date_from_dt - timestart)) %>%
    arrange(time) %>%
    select(-date_from_dt,-timestart,-date_to) %>%
    rename(sending_date = date_from)

# check amount of letters and variables in the dataframe
dim(ffc6) # 163578 letters
glimpse(ffc6)

## E) CREATE IDS ----
# create a table of all people to map to numeric ids
correspondents <- unique(c(ffc6$recipients,ffc6$authors))

# unique nodes/vertices
length(correspondents)
head(correspondents, 10)

# ids for authors and recipients
ffc6$sender_id   <- match(ffc6$authors   , correspondents)
ffc6$receiver_id <- match(ffc6$recipients, correspondents)

# 4) SAVE PREPROCESSED DATA ----
# as csv
write.csv(ffc6, "data/processed/founders/ffc_preprocessed.csv", row.names = FALSE)

# as rds
saveRDS(ffc6, file = "data/processed/founders/ffc_preprocessed.rds")

## A) SAVE DICTIONARY IDS AND NAMES ----

# create a table of all people to map to numeric ids
pp_nodes_sender   <- ffc6 %>% filter(!is.na(sender_id)) %>%
    select(sender_id, authors) %>%
    rename(person_id = sender_id, last_name = authors)

pp_nodes_receiver <- ffc6 %>% filter(!is.na(receiver_id)) %>%
    select(receiver_id, recipients) %>%
    rename(person_id = receiver_id, last_name = recipients)

nodes <- rbind(pp_nodes_sender,pp_nodes_receiver) %>%
  distinct(person_id, last_name)

# this dataset is used later in the pathpy analysis
write.csv(nodes, "data/processed/founders/nodes_ff.csv", row.names = FALSE)
