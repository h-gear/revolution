# Function description ----
#' GOAL: Transferring raw shico vocabulary output to a readable csv
#'
#' After we ran an analysis (i.e. concept search) in shico, we want to transfer
#' its results to R for further usage. The results consist, for each time period,
#' of the used seed words  and the produced time-dependent vocabulary. The
#' variables in the plain text vocabulary listed in shico are:
#'
#' - model for year
#' - seed word
#' - vocabulary
#'
#' PROCEDURE: the shico results are copied and pasted as html directly from the
#' shico results screen into a csv file, resulting in a single column with all
#' the results put together. This function reads in this csv file, and processes
#' the data to a readable format.
#'
#' Shico search parameters were set to:
#' Max terms 10
#' Max related terms 10
#' Minimum concept similarity 0,5
#' Word boost 1
#' Boost method Sum similarity
#' Algorithm Non-adaptive
#' Track direction Forward
#' Years in interval 4
#' Words per year 10
#' Weighing function Gaussian
#' Function shape 1
#' Do cleaning ? Yes

#' @param shico_path     Path to the raw results from shico, all in one column
#' @return               A preprocessed dataset with three columns: years, year,
#'                       vocabulary, in which, for each year, the seed words
#'                       and associated words are in one row.

# Libraries ----
library(tidyverse)

load_and_preprocess_shico <- function(shico_path) {

    shico <- read.csv(shico_path, sep = ';', header = TRUE) %>%
        rename(raw = Model.for.years) %>%
        mutate(raw = str_remove_all(raw, "\xa0"),
               group = cumsum(grepl("\\d+", raw))) %>%

        group_by(group) %>%
            summarise(vocabulary = paste(raw, collapse = " ")) %>%
        ungroup() %>%

        mutate(vocabulary = sapply(strsplit(vocabulary, " "), function(x) paste(unique(x), collapse = " ")),
               years      = substr(vocabulary, start = 1, stop = 9),
               vocabulary = substr(vocabulary, start = 11, stop = nchar(vocabulary)),
               vocabulary = str_replace_all(vocabulary, "\\s+", " ")) %>%

        slice(-1) %>%

        mutate(year      = str_split_fixed(years, "_", 2),
               year      = ceiling((as.numeric(year[,1]) + as.numeric(year[,2]))/2)) %>%

        select(years, year, vocabulary)

    return(shico)
}

# shico search terms for liberal politics  -------------------------------
# liberty, liberties, freedom, freedoms, right, rights, privileges, consent,
# authority, power, powers, constitution, constitutional, unconstitutional, law,
# laws, independent, independence, independency, sovereign, sovereignty, tax,
# taxes, taxation, duties

# Define raw shico datafile for revolutionary politics
shico_path_revo <- "data/shico/founders/shico_liberal_politics.csv"

# Function call to load and preprocess shico data
preprocessed_shico_revo <- load_and_preprocess_shico(shico_path_revo)

# Save preprocessed shico data as csv file for later use ----
write.csv(preprocessed_shico_revo, "data/processed/founders/liberal_shico_vocabulary_per_year.csv", row.names = FALSE)

# shico search terms for republican politics -----------------------------------
# subject,citizen,citizens,inhabitants,virtue,virtues,patriot,patriots,patriotic,
# patriotism,republic,republican,faction,people,empire,nation,tyranny,tyrant,
# despotism,oppression,luxury,corrupt,corruption,tories

# Define raw shico datafile for republican politics
shico_path_replu <- "data/shico/founders/shico_republican_politics.csv"

# Function call to load and preprocess shico data
preprocessed_shico_replu <- load_and_preprocess_shico(shico_path_replu)

# Save preprocessed shico data as csv for later use ----
write.csv(preprocessed_shico_replu, "data/processed/founders/republican_shico_vocabulary_per_year.csv", row.names = FALSE)
