# 0. GOAL ----
# Preprocess all raw data on Letters of Delegates to Congress (1774-1789)
# for later temporal network and shico analysis

# 1. LOAD IN LIBRARIES ----
library(tidyverse) # data wrangling
library(polyglotr) # provide easy methods to translate pieces of text

# 2. DATA PREPROCESSING ----

directory_path <- "./data/raw/delegates/"

# Load the raw data on the delegates of congress
combined_df <- readRDS(paste0(directory_path,"delegates_raw_data.rds"))

## A) VOLUME AND YEAR VARIABLES ----
combined_df2 <- combined_df %>%
    # split the text string at the first occurrence of the separator '--'
    # extra = "merge" to merge any additional columns created due to multiple
    # occurrences of the separator
    separate(text,    c('Volume', 'Content'), sep = '--', extra = "merge") %>%

    # Split after the first complete number and keep the first part
    mutate(Vol_nr   = sub("^(.*?\\d+)\\s.*", "\\1", Volume),
           # Save the second part as well in a different column
           Period   = sub("^.*?\\d+\\s(.*)", "\\1", Volume),
           Year     = as.numeric(str_extract(Content, "\\d{4}"))) %>%

    separate(Content, c('Title', 'Content2'), sep = "\\s{2,3}", extra = "merge", remove = F) %>%

    # in case of no title, set to NA
    mutate(Title = na_if(Title, ""))

## B) IMPROVE INFORMATION ON YEAR ----

# Function to extract year indices from the text string column
extract_year_indices <- function(text_string) {
    year_match <- regexpr("\\d{4}", text_string)
    year_indices <- as.integer(regmatches(text_string, year_match))
    return(year_indices)
}

# Define the range to check for missing years
start_year <- 1774
end_year   <- 1789

# Loop through the rows of the data.frame
for (i in 1:nrow(combined_df2)) {

    year_text  <- combined_df2$Period[i]
    year_value <- combined_df2$Year[i]

    # Check if the year column is missing or not within the specified range
    if (is.na(year_value) || year_value < start_year || year_value > end_year) {
        # Extract the year indices from the text string
        year_indices <- extract_year_indices(year_text)

        # Check if the year indices are equal and within the specified range
        if (length(unique(year_indices)) == 1 && year_indices[1] >= start_year && year_indices[1] <= end_year) {
            # Use the year value and update the year column
            combined_df2$Year[i] <- year_indices[1]
        }
    }
}

# CHECK
# check <- combined_df2 %>% filter(Year < 1774 | Year > 1789 | is.na(Year))

## C) TITLE, FROM AND TO VARIABLES ----
# For a few letters, this separating did not work because of a lacking separator.
# For these cases, we use the following vector of separator words:
separator_words <- c("Gentlemen", "Gent", "Dear", "Dr Sir", "Dr. Sr","Dr. Sir", "Sir", "Please","\\[", "Dr sr")

# Function to extract the first part
extract_first_part <- function(text, separators) {
    parts <- unlist(strsplit(text, paste(separators, collapse = "|"), perl = TRUE))
    first_part <- trimws(parts[1])
    return(first_part)
}

# Apply the function to the data.frame
combined_df2 <- combined_df2 %>%
    mutate(Title = case_when(is.na(Title) ~ sapply(Content, extract_first_part,
                                                   separators = separator_words),
                             !is.na(Title) ~ Title)) %>%

    # Delete specific words from the Title
    mutate(Title = gsub(paste(c("NEXT SECTION ..  NAVIGATOR"," et al."), collapse = "|"), "", Title),

           # Remove all instances of "(X)" including the brackets using gsub with a regular expression
           Title = gsub("\\s*\\(\\d+\\)", "", Title),

           # Delete symbols
           Title = gsub("[^[:alnum:]]", " ", Title),

           # Replace multiple spaces with one space
           Title = str_squish(Title)) %>%
    # Title = gsub("\\s+", " ", Title)) %>%

    # hard-code some anomalies
    mutate(Title = case_when(ID == "dg015572" ~ "North Carolina Delegates to Abner Nash",
                             ID == "dg018376" ~ "Virginia Delegates to Benjamin Harrison",
                             ID == "dg018505" ~ "Theodorick Bland to St. George Tucker",
                             ID == "dg018528" ~ "Virginia Delegates to Benjamin Harrison",
                             ID == "dg018535" ~ "Elias Boudinot to the Chevalier de La Luzerne",
                             ID == "dg021139" ~ "James McHenry to Margaret Caldwell",
                             ID == "dg021172" ~ "James Tilton to Gunning Bedford, Jr.",
                             ID == "dg021246" ~ "South Carolina Delegates to Benjamin Guerard",
                             ID == "dg021266" ~ "Pennsylvania Delegates to Congress",
                             ID == "dg021347" ~ "James McHenry to Margaret McHenry",
                             ID == "dg021580" ~ "Charles Thomson to Jacob Read",
                             ID == "dg021614" ~ "Charles Thomson to Benjamin Bankson",
                             ID == "dg021616" ~ "Charles Thomson to Benjamin Franklin",
                             ID == "dg02273" ~ "Lambert Cadwalader to George Mitchell",
                             ID == "dg022203" ~ "James McHenry to Daniel of St. Thomas Jenifer",
                             ID == "dg022348" ~ "Richard Henry Lee to Thomas Lee Shippen",
                             ID == "dg022363" ~ "Charles Thomson to Commissioners of Accounts",
                             ID == "dg022385" ~ "Richard Henry Lee to Thomas Lee Shippen",
                             ID == "dg022418" ~ "William Samuel Johnson to Matthew Griswold",
                             ID == "dg022524" ~ "Lambert Cadwalader to George Mitchell",
                             ID == "dg022542" ~ "Charles Thomson to the Board of Treasury",
                             ID == "dg022559" ~ "Massachusetts and New York Agents to Congress",
                             TRUE ~ Title)
    ) %>%

    # separate >>title<< if there is a separator " to "
    # if there is no separator " to ", leave the new columns 'from' and 'to' NA
    mutate(has_to = grepl(" to ", Title),
           From   = ifelse(has_to, word(Title, 1, sep = " to "), NA),
           To     = ifelse(has_to, word(Title, 2, sep = " to "), NA)) %>%

    # reorder variables
    select(ID,Vol_nr,Period,Year,Title, From,To, Content,Content2) %>%
    rename(Volume = Vol_nr)

# remove when sender or receiver is unknown
# filter(From != "Unknown"| To != "Unknown")

# CHECK: Filter strings longer than 50 characters
#filtered_df <- subset(combined_df2, nchar(Title) > 50)
#sort(unique(combined_df2$To))

# Some letters on the original website are split across two rows. let's check
# this by examining which strings end with "to"
ends_with_to <- combined_df2 %>% filter(str_detect(Title, "to$"))

# In 12 cases, the from and to are split across two rows. Here, we bring them
# back together manually.

# Flag the rows that meet the conditions of ending with to and their letter is
# (almost) empty
# check_to <- combined_df2 %>%
#  mutate(flag = endsWith(Title, " to") & nchar(Content2) < 50) %>%
#  filter(flag ==TRUE)

# manual  corrections due to original typos
combined_df2 <- combined_df2 %>%
    mutate(From = case_when(ID == "dg009545" ~ "Committee for Foreign Affairs",
                            ID == "dg010247" ~ "William Henry Draytons Draft Address",
                            ID == "dg02327"  ~ "Charles Thomson",
                            ID == "dg02349"  ~ "Massachusetts and New York Delegates",
                            ID == "dg023325" ~ "Charles Thomson",
                            ID == "dg023330" ~ "Charles Thomson",
                            ID == "dg023355" ~ "Charles Thomson",
                            ID == "dg023362" ~ "Massachusetts Delegates",
                            ID == "dg02429"  ~ "Timothy Bloodworth",
                            ID == "dg02481"  ~ "Stephen Mix Mitchell",
                            ID == "dg024405"  ~ "Charles Thomson",
                            ID == "dg025327"  ~ "James Madisons Answers",
                            TRUE ~ From)
    )
combined_df2 <- combined_df2 %>%
    mutate(To = case_when( ID == "dg009545" ~ "William Bingham",
                           ID == "dg010247" ~ "Conrad Alexandre Gerard",
                           ID == "dg02327"  ~ "Samuel Provost and John Rodgers",
                           ID == "dg02349"  ~ "Thomas Hutchins",
                           ID == "dg023325" ~ "the Court of Appeals Judges",
                           ID == "dg023330" ~ "the Indian Affairs Commissioners",
                           ID == "dg023355" ~ "Richard Butler and Samuel Holden Parsons",
                           ID == "dg023362" ~ "John Lowell, Theophilus Parsons, and James Sullivan",
                           ID == "dg02429"  ~ "the North Carolina Assembly",
                           ID == "dg02481"  ~ "the Connecticut Committee of the Pay Table",
                           ID == "dg024405"  ~ "Richard Caswell and George Mathews",
                           ID == "dg025327"  ~ "the Queries of the Comte de Moustier",
                           TRUE ~ To)
    )

## D) LETTER CONTENTS VARIABLE ----

# A function for displaying a letter
display_letter <- function(x) {
    a1 <- strsplit(x$Content2, '\n')[[1]]
    a2 <- trimws(a1)
    a3 <- subset(a2, a2 != '')
    a4 <- paste(a3, collapse = '\n')
    a4a <- paste0('Author: '   , x$From, '\n>',
                  'Recipient: ', x$To, '\n>',
                  'Period: '   , x$Period, '\n>\n',
                  a4)
    a5 <- gsub(' *\n', '  \n', a4a)
    paste('>', gsub(' *(\n*) *$', '\\1', a5))
}

display_letter(combined_df2[8,])

# Remove editor notes: We can identify these at the bottom of a letter with
# specific abbreviations: "RC ", "FC ", "MS " or "LB "
# see http://rs5.loc.gov/cgi-bin/ampage?collId=lldg&fileName=001/lldg001.db&recNum=11

combined_df2$Content2 <- sub("(RC|FC|MS|LB|Tr) \\(.*", "", combined_df2$Content2)

# Remove special characters or numbers from the first 5 positions to clean
# 'left-overs' of dates etc.
combined_df2$Content2 <- sub("^[^a-zA-Z]{0,7}", "", combined_df2$Content2)

combined_df2$Content <- str_remove_all(combined_df2$Content,"NEXT SECTION ..  NAVIGATOR")
combined_df2$Content2 <- str_remove_all(combined_df2$Content2,"NEXT SECTION ..  NAVIGATOR")

## E) DATE VARIABLE ----
process_dates <- function(data) {

    valid_months <- c("jan", "jn", "jnr", "feb", "fbr", "mar","mrc", "ap", "may", "jun",
                      "jul", "aug", "sep", "spt", "oct", "ocr","nov", "nvr", "dec", "dcr")

    unwanted_words <- c("augustin", "maryland", "maryld", "maryl", "marble", "junior",
                        "marine", "julia","marquis", "marsh", "market", "appointed")

    # get first sentence where the date is being mentioned
    data$date <- stringr::word(string = data$Content,
                               start  = 1,
                               end    = 25,
                               sep    = stringr::fixed(" "))

    # Clean and process text based on data$date
    clean_and_process <- function(text) {

        # Delete numbers between brackets, which refer to footnotes by the editors
        cleaned_text <- gsub("\\s*\\(\\d+\\)", "", text)

        # Remove brackets containing "i.e." or "i.e.," and the text within
        cleaned_text <- gsub("\\[i\\.e\\.\\s.*?\\]", "", cleaned_text)
        cleaned_text <- gsub("\\[i\\.e\\.,.*?\\]", "", cleaned_text)

        # Delete symbols except letters and numbers
        cleaned_text <- gsub("[^[:alnum:]]", " ", cleaned_text)

        # Delete the number before "oclock", "o clock", or "o'clock"
        cleaned_text <- gsub("\\b(\\d+)\\s*(oclock|o\\s*clock|o'clock|o clock)\\b", "\\2", cleaned_text, ignore.case = TRUE)

        # Delete everything up to the first occurrence of two consecutive white spaces
        cleaned_text <- sub(".*?\\s{2}", "", cleaned_text)

        # Convert to lowercase
        cleaned_text <- tolower(cleaned_text)

        # Delete everything after the last number
        cleaned_text <- sub("^(.*\\d).*", "\\1", cleaned_text)

        # Remove "no" followed by a number or a space and then a number
        cleaned_text <- str_replace_all(cleaned_text, "(no)\\s*\\d+|\\b(no)\\s+", "")

        # Remove unwanted words from the text
        cleaned_text <- str_remove_all(cleaned_text, paste(unwanted_words, collapse = "|"))

        # Replace multiple spaces with one space
        cleaned_text <- gsub("\\s+", " ", cleaned_text)

        # remove whitespace from start and end of string
        cleaned_text <- str_trim(cleaned_text)

        # Delete any letters still attached to numbers
        cleaned_text <- gsub("(\\d+)st\\b", "\\1", cleaned_text)
        cleaned_text <- gsub("(\\d+)nd\\b", "\\1", cleaned_text)
        cleaned_text <- gsub("(\\d+)th\\b", "\\1", cleaned_text)
        cleaned_text <- gsub("(\\d+)rd\\b", "\\1", cleaned_text)
        cleaned_text <- gsub("(\\d+)d\\b",  "\\1", cleaned_text)

        # Remove words consisting of 1 character (excluding single-digit numbers)
        cleaned_text <- str_replace_all(cleaned_text, "\\b[a-z]\\b", "")

        # Replace specific substrings that are attached to a number with a space
        cleaned_text <- str_replace_all(cleaned_text, "\\b(th|nd|st|rd)\\b", " ")

        # Remove a single letter attached to a number
        cleaned_text <- gsub("(?<=\\d)[a-zA-Z](?=\\s|$)", "", cleaned_text, perl = TRUE)

        # Insert a space between a number and attached letters, if any
        cleaned_text <- gsub("(\\d)([a-zA-Z]+)", "\\1 \\2", cleaned_text)

        # Change abbreviated months into full month name
        cleaned_text <- gsub("\\bjan\\w*\\b", "january", cleaned_text)
        cleaned_text <- gsub("\\bfeb\\w*\\b", "february", cleaned_text)
        cleaned_text <- gsub("\\bmar\\w*\\b", "march", cleaned_text)
        cleaned_text <- gsub("\\bmrc\\w*\\b", "march", cleaned_text)
        cleaned_text <- gsub("\\bapr\\w*\\b", "april", cleaned_text)
        cleaned_text <- gsub("\\bap\\w*\\b", "april", cleaned_text)
        cleaned_text <- gsub("\\bapl\\w*\\b", "april", cleaned_text)
        cleaned_text <- gsub("\\bmay\\w*\\b", "may", cleaned_text)
        cleaned_text <- gsub("\\bjun\\w*\\b", "june", cleaned_text)
        cleaned_text <- gsub("\\bjul\\w*\\b", "july", cleaned_text)
        cleaned_text <- gsub("\\baug\\w*\\b", "august", cleaned_text)
        cleaned_text <- gsub("\\bsep\\w*\\b", "september", cleaned_text)
        cleaned_text <- gsub("\\bspt\\w*\\b", "september", cleaned_text)
        cleaned_text <- gsub("\\boct\\w*\\b", "october", cleaned_text)
        cleaned_text <- gsub("\\bocr\\w*\\b", "october", cleaned_text)
        cleaned_text <- gsub("\\bnov\\w*\\b", "november", cleaned_text)
        cleaned_text <- gsub("\\bnvr\\w*\\b", "november", cleaned_text)
        cleaned_text <- gsub("\\bdec\\w*\\b", "december", cleaned_text)
        cleaned_text <- gsub("\\bdcr\\w*\\b", "december", cleaned_text)

        # Merge "17" and a number between 74 and 89
        cleaned_text <- gsub("17\\s+([74-89])", "17\\1", cleaned_text, perl = TRUE)

        # Merge "1", "7", and a number between 74 and 89
        cleaned_text <- gsub("1\\s+7\\s+([74-89])", "17\\1", cleaned_text, perl = TRUE)

        # Remove the second 1- or 2-digit number after the month and before the 4-digit number
        cleaned_text <- gsub("(\\b\\w+\\s+\\d+)(\\s+\\d{1,2})(\\s+\\d{4})", "\\1\\3", cleaned_text)

        return(cleaned_text)
    }

    process_text <- function(text) {
        matching_words <- unlist(str_extract_all(text, paste0("\\b(", paste(valid_months, collapse = "|"), ")\\w*\\b|\\d+")))
        return(paste(matching_words, collapse = " "))
    }

    # Attach "17" to two-digit years based on data$date
    attach_17 <- function(text) {
        if (grepl("\\d{4}", text)) {
            return(text)
        } else {
            modified_text <- gsub("\\b(8[0-9]|7[4-9])\\b", "17\\1", text)
            return(modified_text)
        }
    }

    standardize_date <- function(date_string) {
        # Try to extract day, month, and year components
        day   <- str_extract(date_string, "\\d{1,2}")
        month <- str_extract(date_string, "(?i)jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?")
        year  <- str_extract(date_string, "\\d{4}")

        # If all components are present, format them as "day month year"
        if (!is.na(day) && !is.na(month) && !is.na(year)) {
            standardized_date <- sprintf("%02d %s %s", as.numeric(day), month, year)
            return(standardized_date)
        }

        # Return original string if components are missing
        return(date_string)
    }

    # Apply cleaning and standardizing to date
    data$date <- sapply(data$date, clean_and_process)
    data$date <- sapply(data$date, process_text)
    data$date <- sapply(data$date, attach_17)
    data$FinalDate <- sapply(data$date, standardize_date)
    data$FinalDate[data$FinalDate == ''] <- NA

    # Manual corrections due to original typos not accounted for in the above code
    data <- data %>%
        mutate(FinalDate = case_when( ID == "dg0012"   ~ "29 august 1774",
                                      ID == "dg01449"  ~ "09 september 1779",
                                      ID == "dg001132" ~ "05 september 1774",
                                      ID == "dg019254" ~ "10 november 1782",
                                      ID == "dg01997"  ~ "10 september 1782",
                                      ID == "dg024257" ~ "10 june 1787",
                                      ID == "dg022624" ~ "29 october 1785",
                                      ID == "dg019518" ~ "17 february 1783",
                                      ID == "dg014578" ~ "17 march 1780",
                                      ID == "dg003501" ~ "17 april 1776",
                                      ID == "dg006166" ~ "31 january 1777",
                                      ID == "dg02198"  ~ "31 october 1783",
                                      ID == "dg016532" ~ "26 january 1781",
                                      ID == "dg016310" ~ "26 november 1780",
                                      ID == "dg016409" ~ "26 december 1780",
                                      ID == "dg018410" ~ "25 march 1782",
                                      ID == "dg010287" ~ "24 july 1778",
                                      ID == "dg010426" ~ "24 august 1778",
                                      ID == "dg019449" ~ "23 december 1783",
                                      ID == "dg007447" ~ "22 august 1777",
                                      ID == "dg020438" ~ "02 august 1783",
                                      ID == "dg015217" ~ "02 june 1780",
                                      ID == "dg022209" ~ "19 march 1785",
                                      ID == "dg021172" ~ "25 december 1783",
                                      ID == "dg009524" ~ "11 may 1778",
                                      ID == "dg009524" ~ "17 march 1781",
                                      ID == "dg009232" ~ "16 march 1778",
                                      ID == "dg006417" ~ "15 march 1777",
                                      ID == "dg012376" ~ "14 april 1779",
                                      ID == "dg017141" ~ "12 april 1781",
                                      ID == "dg011333" ~ "12 december 1778",
                                      ID == "dg003472" ~ "12 april 1776",
                                      ID == "dg018251" ~ "11 december 1781",
                                      ID == "dg020535" ~ "08 september 1783",
                                      ID == "dg0084"   ~ "20 september 1777",
                                      ID == "dg019107" ~ "11 september 1782",
                                      ID == "dg016138" ~ "08 october 1780",
                                      ID == "dg002129" ~ "03 october 1775",
                                      ID == "dg002210" ~ "19 october 1775",
                                      ID == "dg01891"  ~ "01 october 1781",
                                      ID == "dg002294" ~ "05 november 1775",
                                      ID == "dg008215" ~ "05 november 1777",
                                      ID == "dg019272" ~ "15 november 1782",
                                      ID == "dg007255" ~ "04 july 1777",
                                      ID == "dg020443" ~ "04 august 1783",
                                      ID == "dg003161" ~ "04 february 1776",
                                      ID == "dg02421"  ~ "01 december 1786",
                                      ID == "dg023393" ~ "07 august 1786",
                                      ID == "dg002138" ~ "05 october 1775",
                                      ID == "dg007373" ~ "05 august 1777",
                                      ID == "dg007373" ~ "05 november 1775",
                                      ID == "dg002294" ~ "05 november 1775",
                                      ID == "dg023289" ~ "07 june 1786",
                                      ID == "dg024321" ~ "01 august 1787",
                                      ID == "dg024332" ~ "08 august 1787",
                                      ID == "dg023437" ~ "20 august 1786",
                                      ID == "dg024402" ~ "24 october 1787",
                                      ID == "dg005508" ~ "05 december 1776",
                                      ID == "dg00145"  ~ "13 september 1774",
                                      ID == "dg00154"  ~ "16 september 1774",
                                      ID == "dg00181"  ~ "20 september 1774",
                                      ID == "dg00188"  ~ "23 september 1774",
                                      ID == "dg00194"  ~ "24 september 1774",
                                      ID == "dg001128" ~ "04 october 1774",
                                      ID == "dg001154" ~ "10 october 1774",
                                      ID == "dg001162" ~ "11 october 1774",
                                      ID == "dg001181" ~ "15 october 1774",
                                      ID == "dg001187" ~ "17 october 1774",
                                      ID == "dg001191" ~ "19 october 1774",
                                      ID == "dg001309" ~ "20 may 1775",
                                      ID == "dg0024"   ~ "11 september 1775",
                                      ID == "dg002163" ~ "09 october 1775",
                                      ID == "dg013351" ~ "07 august 1779",
                                      ID == "dg00714"  ~ "02 may 1777",
                                      ID == "dg008292" ~ "24 november 1777",
                                      ID == "dg014149" ~ "30 october 1779",
                                      ID == "dg024484" ~ "27 january 1788",
                                      ID == "dg001399" ~ "08 june 1775",
                                      ID == "dg024484" ~ "27 january 1788",
                                      ID == "dg019411" ~ "07 july 1783",
                                      ID == "dg010528" ~ "08 september 1778",
                                      ID == "dg002161" ~ "09 october 1775",
                                      ID == "dg019170" ~ "09 october 1782",
                                      ID == "dg00837"  ~ "01 october 1777",
                                      ID == "dg024173" ~ "12 april 1787",
                                      ID == "dg01553"  ~ "18 april 1780",
                                      ID == "dg024161" ~ "02 april 1787",
                                      ID == "dg02474"  ~ "12 december 1786",
                                      ID == "dg024374" ~  NA,
                                      ID == "dg016613" ~ "15 february 1781",
                                      ID == "dg014460" ~ "04 february 1780",
                                      ID == "dg021214" ~ "14 january 1784",
                                      ID == "dg003106" ~ "20 january 1776",
                                      ID == "dg015429" ~ "12 july 1780",
                                      ID == "dg010275" ~ "21 july 1778",
                                      ID == "dg015480" ~ "22 july 1780",
                                      ID == "dg013194" ~ "09 july 1779",
                                      ID == "dg020328" ~ "29 june 1783",
                                      ID == "dg018470" ~ "01 may 1782",
                                      ID == "dg003557" ~ "01 may 1776",
                                      ID == "dg018512" ~ "28 may 1782",
                                      ID == "dg003589" ~ "08 may 1776",
                                      ID == "dg015105" ~ "09 may 1780",
                                      ID == "dg024159" ~  NA,
                                      ID == "dg023449" ~  "17 august 1786",
                                      ID == "dg002393" ~  "29 november 1775",
                                      ID == "dg001205" ~  "24 october 1774",
                                      ID == "dg006376" ~  "01 march 1777",
                                      ID == "dg016152" ~  "10 october 1780",
                                      ID == "dg006387" ~  "04 january 1777",
                                      ID == "dg006303" ~  "19 february 1777",
                                      ID == "dg006274" ~  "15 february 1777",
                                      ID == "dg008329" ~  "01 december 1777",
                                      ID == "dg014286" ~  "01 december 1779",
                                      ID == "dg015279" ~  "12 june 1780",
                                      ID == "dg00781"  ~  "14 may 1777",
                                      ID == "dg020476" ~  "15 august 1783",
                                      ID == "dg017118" ~  "03 april 1781",
                                      ID == "dg018233" ~  "01 december 1781",
                                      ID == "dg018418" ~  "26 march 1782",
                                      ID == "dg023178" ~  "23 march 1786",
                                      ID == "dg025264" ~  "16 september 1788",
                                      ID == "dg017197" ~  "01 may 1781",
                                      TRUE ~ FinalDate)
        )

    data <- data %>%
        # Convert character column to date format
        mutate(FinalDate = lubridate::dmy(FinalDate)) %>%

        # the analyses (in shico and network analysis) are only meaningful
        # when there is a letter sent with at least some contents
        filter(nchar(Content2) > 50) %>%

        # Filter the data.frame for the date range between 1774 and 1789
        filter(FinalDate >= ymd("1774-01-01") & FinalDate <= ymd("1789-12-31"))

    return(data)
} # end of process_dates function


# Apply the process_dates function
combined_df2 <- process_dates(combined_df2)

# 3. SAVING PREPROCESSED DATA ----

## A1) ALL DATA IN CSV FILE ----
all_data <- combined_df2 %>%
    select(-date,-Content,-language) %>%
    rename(Text = Content2,
           Date = FinalDate)

# lowercase all column names
colnames(all_data) <- tolower(colnames(all_data))

write.csv(all_data, "data/processed/delegates/Letters of Delegates.csv", row.names = FALSE)

## A2) ALL DATA IN RDS FILE ----
saveRDS(all_data, file = "data/processed/delegates/Letters of Delegates.rds")

## B) FOR MAKING WORD2VEC MODELS (SHICO) ----
combined_df3 <- combined_df2 %>% select(ID,Year,Title)
write.csv(combined_df3, "data/raw/delegates/Letters of Delegates.csv", row.names = FALSE)

## C) LETTERS AS SEPARATE TEXT FILES ----
# For each of the 25 volumes, save each letter as a separate text file in a
# subfolder called after its volume number (e.g. "2"). These text files are
# input for later pre-processing and shico word2vec model building.
for (i in 1:25) {

    # Format the number with leading zero, if necessary
    formatted_number <- sprintf("%02d", i)

    #Construct the filename
    filename <- paste("Letters_of_Congress_Volume_", formatted_number, ".rds", sep = "")

    # Read the RDS file and store the data.frame
    current_dataframe <- readRDS(file.path("data/raw/delegates/volumes", filename)) %>%
        select(text) %>%
        separate(text,c('Volume', 'Content'), sep = '--', extra = "merge") %>%
        select(Content) %>%
        rename(text = Content) %>%

        # Editor notes should be removed at the bottom of a letter. Editor notes
        # are signaled using "RC ", "FC ", "MS " or "LB " after which between brackets
        # come the location
        mutate(text = sub("(RC|FC|MS|LB|Tr) \\(.*", "", text))

    # Create a folder for the current volume, if it does not exist yet
    folder_name <- paste0(i)
    if (!file.exists(folder_name)) {
        dir.create(folder_name)
    }

    # Loop through the rows and export to text-files
    for (j in 1:nrow(current_dataframe)) {

        # Create the file name for the txt file (e.g. 1/dg0011.txt")
        file_name <- file.path(folder_name, paste0("dg0", formatted_number, j, ".txt"))

        # Export the current row to the text-file
        write.table(current_dataframe[j, ], file = file_name,
                    row.names = FALSE, col.names = FALSE, sep = "\t")
    }
}

# note: standard cleaning of the letter texts occurs in the python script
# make_word2vec_models_for_delegates.py, found in w2v_models/delegates. This
# script is aimed at preprocessing the data to develop word2vec models to be
# used in shico

## D) LETTERS PER YEAR ----

# Define time period
#start_year <- min(combined_df2$Year, na.rm = T)
#end_year   <- max(combined_df2$Year, na.rm = T)

# For now, set them fixed
#start_year <- 1774
#end_year   <- 1789

# Iterate over the volume numbers/ and split the data.frame according to year
# for (yr in start_year:end_year) {
#     df_per_year <- combined_df2 %>% filter(yr == Year)
#     file_name <- paste0("data/processed/delegates/content/", yr, ".csv")
#     write.csv(df_per_year, file_name, row.names = FALSE)
# }


## E) FOR PATH-BASED NETWORK ANALYSIS ----

# Create source-target pairs: In the case of multiple authors or multiple
# recipients, these were split into multiple records with only one author and
# one recipient each. For example, the letter "Sarah Read to Benjamin and
# Deborah Franklin, 10 April 1734," was split into two records: one with
# ‘author: Sarah Read’ and ‘recipient: # Benjamin Franklin,’ and the other with
# author: ‘Sarah Read, recipient: Deborah Franklin.’

# replace " and " with ; as separator
combined_df2$To2    <- gsub(" and ", ";", combined_df2$To)
combined_df2$From2 <- gsub(" and ", ";", combined_df2$From)

sort(unique(combined_df2$To2))

# in case of multiple authors, these are split so that each author has its own row
combined_df2 <- combined_df2 %>%
    mutate(From2 = strsplit(as.character(From2), ";")) %>%
    unnest(From2) %>% mutate(From2 = trimws(From2))

# Next, building further on the separated authors and in case of multiple recipients,
# these are split as well so that each recipient has its own row
combined_df2 <- combined_df2 %>%
    mutate(To2 = strsplit(as.character(To2), ";")) %>%
    unnest(To2) %>% mutate(To2 = trimws(To2))

# TODO: checking for errors, doublings, etc in names in From2 and To2
#In to2: Abigail Adams 2d -> Abigail Adams

# Calculate the time difference between the current sending
# date and the earliest date in the dataset. This will result in a numeric value
# indicating the number of days between the current sending date and the earliest
# date in the dataset. This is required for the later pathpy network analysis.
# Store time differences in separate column called 'time'
combined_df2 <- combined_df2 %>%

    # first available date in dataset
    mutate(timestart   = min(FinalDate),
           time  = as.numeric(FinalDate - timestart)) %>%
    arrange(time) %>%
    select(-timestart)

# Create Ids matching names
# create a table of all people to map to numeric ids
correspondents <- unique(c(combined_df2$From, combined_df2$To))
length(correspondents) # 1540 unique nodes
head(correspondents,10)

# create ids for authors and recipients
combined_df2$sender_id   <- match(combined_df2$From, correspondents)
combined_df2$receiver_id <- match(combined_df2$To, correspondents)

# if from and to are NA, set ids also to NA
combined_df2$sender_id   <- ifelse((is.na(combined_df2$From)), NA, combined_df2$sender_id)
combined_df2$receiver_id <- ifelse((is.na(combined_df2$To)), NA, combined_df2$receiver_id)

# how many individuals are we dealing with?
n_distinct(combined_df2$sender_id)
n_distinct(combined_df2$receiver_id)

## F) NODES ----
# save file cales 'nodes' that contains a dictionary with ids and corresponding names
pp_nodes_sender   <- combined_df2 %>% filter(!is.na(sender_id)) %>% select(sender_id, From) %>%
    rename(person_id = sender_id, last_name = From)

pp_nodes_receiver <- combined_df2 %>% filter(!is.na(receiver_id)) %>% select(receiver_id, To) %>%
    rename(person_id = receiver_id, last_name = To)

nodes <- rbind(pp_nodes_sender,pp_nodes_receiver) %>% distinct(person_id, last_name)

# this data.set is used in the pathpy analysis
write.csv(nodes, paste("data/processed/delegates/","nodes.csv"), row.names = FALSE)

# Look into ids of founding fathers
fathers_names <- c("George Washington", "Benjamin Franklin", "John Adams",
                   "Alexander Hamilton","John Jay","Thomas Jefferson","James Madison")

# Extract numeric IDs given to the founding fathers
fathers_ids <- unique(combined_df2$sender_id[combined_df2$From %in% fathers_names])

# see the names and associated ids
table.ff <- combined_df2 %>%
            filter(sender_id %in% fathers_ids) %>%
            distinct(From, sender_id)
table.ff

combined_df2 <- combined_df2 %>%
    rename(year = Year) %>%
    mutate(year = as.integer(year))

## G) FOR PATHPY ANALYSIS ----
# letter selection function to obtain topic homogeneity in the letters. We merge
# with shico_results and count how often the shico words occur in each letter.
# we set a minimum of 3 words to be included (i.e. shico_word_count >= 3)

shico_word_merger <- function(shico_words_path, min_words) {

    letters <- combined_df2
    shico_words <- read.csv(shico_words_path, header = TRUE)

    political_data <- letters %>%
        left_join(shico_words, by = c("year" = "year")) %>%
        mutate(shico = str_split(tolower(vocabulary), " "),
               shico_word_count = pmap_dbl(list(tolower(Content2), shico), ~ sum(str_count(..1, ..2)))) %>%
        select(-shico,-years,-vocabulary) %>%
        ungroup() %>%
        filter(shico_word_count >= min_words)

    return(political_data)
}

# Filter letter data
shico_words_path <- "data/shico/delegates/shico_vocabulary_per_year.csv"
political_data <- shico_word_merger(shico_words_path, min_words = 3)

pp_data <- political_data %>%

    # pathpy analysis needs information on sender_id, receiver_id and time
    filter(!is.na(sender_id) & !is.na(receiver_id)) %>%

    rename(sending_date = FinalDate) %>%
    select(sender_id, receiver_id, sending_date)

# this data is used to for the subsequent pathpy analysis
write.csv(pp_data, paste("data/processed/delegates/","links.csv"), row.names = FALSE)

## H) FOR PLOTTING NETWORKS ----
saveRDS(combined_df2, "data/processed/delegates/delegates_plotting_data.rds")
