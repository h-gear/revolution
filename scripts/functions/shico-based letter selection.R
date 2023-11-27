#' Function Title: shico_word_merger
#' 
#' Description: Make a subset of the data in which all letters are on the topic
#' of  interest. We want to create a homogeneous subset of letters all focused on 
#' political ideas based on the results from the shico word occurrences. 
#' This selection ensures that letters are all about the topic of interest, 
#' such as “republican politics” and "liberal politics". In addition: 
#' 
#' 1) this reduces the size of the dataset, making the network more manageable.
#' 2) the homogeneous datasets are input for the path-respecting temporal 
#'    analysis with pathpy 

#' @param letters_path     Path to the dataset including all retrieved letters
#' @param shico_words_path Path to the shico dataset with seed-associated words
#' @param min_words        Minimum amount of words that have to be in the letter
#'                         to be classified as a letter on political ideology
#'                         
#' @return                 A homogeneous dataframe of letters on the same topic 
#'                         that are filtered based on the amount of shico word 
#'                         occurrences 
#' @examples
#' How to use the function
#' result <- shico_word_merger(letters_path, shico_words_path_liberal, min_words = 7)
#' 
#' @author Thijs Vroegh

# libraries ----
library(tidyverse)

shico_word_merger <- function(letters_path, shico_words_path, min_words) {
    
    letters     <- read.csv(letters_path)
    shico_words <- read.csv(shico_words_path, header = TRUE)
    
    political_data <- letters %>%
        # TODO: check if in letter start.year has been changed into year!
        left_join(shico_words, by = c("start.year" = "year")) %>%
        mutate(shico = str_split(tolower(vocabulary), " "),
               shico_word_count = pmap_dbl(list(tolower(text), shico), ~ sum(str_count(..1, ..2)))) %>%
        select(-shico,-years,-vocabulary) %>%
        ungroup() %>% 
        filter(shico_word_count >= min_words)
    
    return(political_data)
}

# Define file path for dataset including all letters (i.e. still heteregenous)
letters_path     <- "../../Data/FoundingFathers/ffc_total.csv"

# Select shico results on concept of interest: liberal politics
shico_words_path_liberal <- "liberal_shico_vocabulary_per_year.csv"

# Select shico results on concept of interest: republican politics
shico_words_path_republican <- "republican_shico_vocabulary_per_year.csv"

# Filter letter data by applying shico_word_merger function ----
political_data_liberal    <- shico_word_merger(letters_path, shico_words_path_liberal, min_words = 7)
political_data_republican <- shico_word_merger(letters_path, shico_words_path_republican, min_words = 7)

pp_liberal <- political_data_liberal %>% 
    select(authors,recipients,sending_date, time,sender_id,receiver_id) %>% 
    arrange(time) # 10118 letters

pp_republican <- political_data_republican %>% 
    select(authors,recipients,sending_date, time,sender_id,receiver_id) %>% 
    arrange(time) # 10610 letters 

# The csv files below are used for the later temporal path-respecting analyses
write.csv(pp_liberal, "links_liberal.csv", row.names = FALSE)
write.csv(pp_republican, "links_republican.csv", row.names = FALSE)
