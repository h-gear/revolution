#                               Script header ----
#' =============================================================================
#' Title: Topic analysis on Founders Online letters
#'
#' Author: Thijs Vroegh
#'
#' Date script last modified: 20-01-2024
#'
#' Description:
#' This script performs a topic analysis on the Founders Online texts.
#' Ultimately, each letter is grouped by its dominant topic probability,
#' allowing to make subsets of letters on a specific topic.
#' In addition, the selection of letters based on the topic is a way to validate
#' the results obtained from shico. Right now, we can select a group of
#' homogeneous letters based on a minimum amount of word occurrences resulting
#' from the shico output. Remember, the results from shico account for the
#' dynamic evolution of words associated with the specified concept of interest.
#' Last, the results of the topic analysis is used as input for the shiny app to
#' allow for zooming in on the topic of interest through animated word clouds.
#' Also, the grouping of letters based on their main topic allows to make
#' cross-comparisons through network analyses specific to letter correspondences
#' on a specific topic

#' Purpose:
#' 1) get a sense on the diversity of topics present in the letters
#' 2) create subsets of the data that include homogeneous data on the
#'    two different political ideologies as specified by the lead applicant
#' 3) double check shico results
#' 4) create dataset with input for shiny app
#'
#' Input dataset
#' @param letters   The founders online preprocessed dataset with ids and letter
#'                  text

#' Output
#' @return          A data frame with an extra column denoting the major topic,
#'                  as well as topic probabilities on all topics for each letter
#' =============================================================================

# Import libraries ----
library(rJava)
library(textclean)
library(quanteda)
library(quanteda.textstats)
library(seededlda)
library(furrr)
library(igraph)
library(visNetwork)
library(tidyverse)
#library(sentopics)

library(NLP)
require(openNLP)
require(openNLPdata)

set.seed(1234)

# Reading in preprocessed data ----
#letters <- read.csv("../../Data/FoundingFathers/ffc_total.csv", header = TRUE)
letters  <- readRDS("data/processed/founders/ffc_preprocessed.rds")
glimpse(letters)

texts <- letters %>% select(id, text, year, authors, sending_date)

# sort data by date
texts <- texts[order(texts$sending_date),]
rownames(texts) <- NULL

# Count the occurrences of the word "liberty" to get an impression
sum(str_count(texts$text, "liberty")) #17883

# A function for displaying a letter
display_letter <- function(x) {
  a1 <- strsplit(x$text, '\n')[[1]]
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
# Show a letter
display_letter(texts[2309,])

# Text preprocessing ----

## 1. Part-of-speech tagging (nouns selection) ----
source("scripts/functions/pos_tag.R")

# because of the large dataset, we'll break it up here into 8 parts, POS tag
# each part and save it before continuing to the next part. In the end, all
# chunks are put together again, containing the complete dataset with only nouns

# Define the number of chunks
num_chunks <- 8

# Calculate the number of rows in each chunk
chunk_size <- ceiling(nrow(texts) / num_chunks)

# Create a list to store the POS tagged results
pos_tagged_data <- list()

# Initialize the start index
start_index <- 1

# Loop through each chunk
for (i in 1:num_chunks) {

  # Calculate the end index for the current chunk
  end_index <- min(start_index + chunk_size - 1, nrow(texts))

  # Extract the current chunk of data
  current_chunk <- texts[start_index:end_index, ]

  # Perform POS tagging on the text column of the current chunk
  corpus_nouns <- pos_tag(current_chunk$text, pos_filter = c("NN", "NNS"))

  # Add the current chunk to the list of POS tagged data
  pos_tagged_data[[i]] <- corpus_nouns

  # Save the current chunk to a file (adjust the filename as needed)
  write.csv(corpus_nouns, paste("data/interim/pos_tagged_chunk_", i, ".csv", sep = ""), row.names = T)

  # Clear memory (optional, depending on available resources)
  rm(current_chunk)
  rm(corpus_nouns)

  # Update the start index for the next chunk
  start_index <- end_index + 1
}

saveRDS(pos_tagged_data, file = "data/interim/pos_tagged_data.rds")

# reading the stored data on the pos-tagged data, if necessary
pos_tagged_data <- readRDS(file = "data/interim/pos_tagged_data.rds")

# Flatten the list into a single vector and combine chunks into one vector
corpus_nouns <- pos_tagged_data %>% unlist()
saveRDS(corpus_nouns, file = "data/interim/corpus_nouns.rds")

# reading the stored data on the corpus nouns
#corpus_nouns <- readRDS(file = "data/interim/corpus_nouns.rds")

## 2. Tokenize ----

# Creating a proper character object to tokenize
corp <- corpus_nouns %>% as.data.frame()

corp <- corp %>%
  mutate(doc_id = rownames(corp)) %>%
  rename(text = '.') %>% select(doc_id,text)

corpus_nouns <- corp %>% as.list()
corpus_nouns <- corpus_nouns[2] %>% unlist()

tokens <- tokens(corpus_nouns)

# Lower case
tokens <- tokens_tolower(tokens)
head(tokens,5)

# replace very common abbreviations with their full name
abbreviations <- c("govr", "genl", "colo ", "capt ", "captn",
              "arm’d", "servt", "benjn ", "brigr ", "humb ",
              "humbe ", "humbl ", "hume ", "humle ", "obdt",
              "obedt", "obt ", "obet ", "publick","vessells")

replace_abbrev <- c("governor", "general", "colonel ", "captain ", "captain",
              "armed", "servant", "benjamin ", "brigadier ", "humble ",
              "humble ", "humble ", "humble ", "humble ", "obedient",
              "obedient", "obedient ", "obedient ", "public","vessel")

tokens <- tokens_replace(tokens, abbreviations, replace_abbrev, valuetype = "fixed")

# create a list of stopwords and other words which we want to get rid of
all_stop_words <- c(quanteda::stopwords("en"),
                    stopwords::stopwords(source = "snowball"),
                    stopwords::stopwords(source = "smart"),
                    stopwords::stopwords(source = "nltk"),
                    stopwords::stopwords(source = "marimo"),
                    stopwords::stopwords(source = "stopwords-iso"),
                    "sir", "mr", "letter", "dear", "dr","dr—", "mrs", "pd", "sr",
                    "january", "february", "march", "april", "may", "june", "july",
                    "august", "september","october","november","december", "thos",
                    "b","c","p","h","go", "aug", "sept","oct","nov","dec", "janry",
                    " jany","febry", "feby", "decr", "octr", "octobr","jan",
                    "wm","tis", "ca","gw", "novr","monday","tuesday", "wednesday",
                    "thursday", "friday", "saturday","sunday","1st","2nd","3rd",
                    "4th","5th","6th","7th", "7nd","8th","9th","10th","11th",
                    "12th","13th","14th","15th","16th","17th","18th","19th","20th",
                    "21st","22th","22nd","23th","23rd", "24th","25th","26th","27th",
                    "28th","29th","30th","31th","31st", "22d","tem",
                    "dearest","oclock", "o’clock","thing","things","time","day",
                    "morning", "mr", "mr.","letter","mat","excellency","paper",
                    "les","circumstance","article","consequence",
                    "john","gentleman","reason","people","opinion","character",
                    "william", "favour","excelly","excellencys","excellency’s",
                    "williams","answer","shall","one","could","would", "upon",
                    "may","&","every","much","might", "with", "without","two",
                    "us","yet","since","also","therefore","however","never","ever",
                    "soon","say","take","give","well","see","mch","sir","mr",
                    "mr.","get","give","want","many","part","time", "wh","ditto",
                    "today","letter","esqr","mrs","letter","person","post",
                    "purpose","measure","mat","subject","circumstance",
                    "manner","moment","gentleman","yesterday","instant","pa",
                    "week","par","night","event","object","paper","month","favour",
                    "favor", "reason","regard", "principle","matter","instance",
                    "question","time","inst", "degree","","occasion","honble",
                    "hour","behalf","particular","van","word", "correspondence",
                    "issue","lettre","mr","franklin","adams","jay","franklin's",
                    "jefferson","thomas","james","hand","smith","servant",
                    "place","philadelphia","philada","virginia","boston","washington",
                    "ce","tomorrow","che","di","mi")

# These words in the stopwords list we actually do want to keep in the corpus
remove_from_stopwords <- c("right", "state","states","problem","order","orders",
                           "ordered","number","numbers","new","member","members",
                           "opposite","against","area","areas","end","ends",
                           "ending","ended","free", "from", "front","general",
                           "help","home","hopefully")

all_stop_words <- all_stop_words[!all_stop_words %in% remove_from_stopwords]

# select distinct stopwords
all_stop_words <- unique(all_stop_words) # 1451 unique stopwords

# remove stopwords from tokens
tokens <- tokens_remove(tokens,
                        pattern = all_stop_words,
                        padding = TRUE)

# remove punctuation etc.
tokens <- tokens(tokens,
                 remove_punct      = TRUE,
                 remove_numbers    = TRUE,
                 remove_symbols    = TRUE,
                 remove_url        = TRUE,
                 remove_separators = TRUE,
                 padding           = TRUE) # keep the original positions of tokens

head(tokens, 20)

# check multi-word expressions: https://quanteda.io/articles/pkgdown/examples/phrase.html
tokens_cap <- tokens_select(tokens,
                            pattern = "^[A-Z]",
                            valuetype = "regex",
                            case_insensitive = TRUE,
                            padding = TRUE)

# Discover multi-word expressions through statistical scoring of the associations of adjacent words
tstat_col_cap <- textstat_collocations(tokens_cap,
                                       min_count = 10,
                                       tolower   = F)

head(tstat_col_cap, 20)

tsta <- tstat_col_cap %>% filter(count > 50) %>%
  arrange(desc(count)) %>%
  select(collocation) %>% pull()

head(tsta,100)
sort(tsta)

# Convert multi-word expressions to tokens
multiword_tokens <- tokens(tsta)

# Update token list with multi-word expressions i.e., we combine adjacent
# tokens in the tokens object based on the multi-word expressions in the
# multiword_tokens object
tokens <- tokens_compound(tokens, pattern = phrase(multiword_tokens))
head(tokens, 300)

# remove stopwords from tokens
tokens <- tokens_remove(tokens,
                        pattern = all_stop_words,
                        padding = TRUE)

## 3. Lemmatization ----
# Get lemma table
#lemma <- read.delim("https://github.com/michmech/lemmatization-lists/raw/master/lemmatization-en.txt", sep = "\t")
lemma <- read.delim("data/external/lemmatization-en.txt", sep = "\t")
colnames(lemma) <- c("base", "variant")
lemma$base    <- tolower(lemma$base)
lemma$variant <- tolower(lemma$variant)

# remove duplicates
lemma <- lemma[!duplicated(lemma$variant),]

# lemmatize tokens
tokens_lemmatized <- tokens_replace(tokens,
                                    lemma$variant,
                                    lemma$base)

# convert tokens-object to a list for later analysis
token_list <- lapply(tokens_lemmatized, unlist)

# Count occurrences of the specified word in the tokens list
word_counts <- sapply(token_list, function(tokens) sum(tokens == "plantation"))
# Total occurrences of the word "plantation"
sum(word_counts) # 1335

## 4. Combine original letters with cleaned letter texts ----

# https://stackoverflow.com/questions/62396405/unlist-all-items-from-quanteda-tokens-object-into-data-frame
df_texts_cleaned <- data.frame(
          id           = seq_along(tokens_lemmatized),
          text_cleaned = sapply(tokens_lemmatized, paste, collapse = " "),
          row.names    = NULL) %>%
          mutate(text_cleaned = str_squish(text_cleaned))

glimpse(texts)
texts <- texts %>%
  mutate(id = as.numeric(id)) %>%
  left_join(df_texts_cleaned, by = "id")

## 5. Create DFM ----

# DFM based on lemmatized tokens
DFM_lemma <- dfm(tokens_lemmatized)

# Remove stopwords again that may have emerged from the lemmatization process
DFM_lemma <- DFM_lemma[, !(colnames(DFM_lemma) %in% all_stop_words)]

# most frequent terms
topfeatures(DFM_lemma, 50)

# see how min_docfreq / min_termfreq affects vocabulary size and number of
# dropped documents cf. Maier et al. (2020),https://doi.org/10.5117/CCR2020.2.001.MAIE
source("scripts/functions/dfm_trim_plot.R")

dfm_trim_plot(DFM_lemma, 1, 40, 1, min_freq = "doc")
dfm_trim_plot(DFM_lemma, 1, 40, 1, min_freq = "term")

# Terms that appear in fewer than 25 documents will be removed, and terms that
# appear in more than 75% of the total documents will be removed. This combination
# of trimming thresholds aims to remove both very rare terms (reducing noise)
# and very common terms (removing potentially uninformative terms like stop
# words).This helps to focus the analysis on terms that provide meaningful
# insights across the corpus
DFM <- dfm_trim(DFM_lemma, min_termfreq = 25, max_docfreq = 0.75, docfreq_type = "prop")

# most frequent terms
topfeatures(DFM, 50)

# Count the total occurrences of the word across the entire DFM
sum(DFM[, "right"])

# save DFM
saveRDS(DFM, file = "data/interim/DFM.rds")

# The precomputed object is in the data/interim folder
#DFM <- readRDS("data/interim/DFM.rds")

# DTM (document-term-matrix as in package topicmodels) for coherence function
DTM <- convert(DFM, to = "topicmodels")
saveRDS(DTM, file = "data/interim/DTM.rds")

# The precomputed object is in the data/interim folder
#DTM <- readRDS("data/interim/DTM.rds")

# Semi-supervised topic modeling with seeded LDA: model selection ----

## 1. Gauging the number of topics ----

# Find the number of topics that best fits the data. Here, we use the divergence()
# function which implements the Kullback-Leibler (KL) divergence. This measure
# is frequently used to measure the dissimilarities between word distributions
# (i.e., topics). The divergence score maximizes when the chosen number of topic
# k is optimal (Deveaud et al., 2014). When we minimize the overlap, or maximize
# the divergence, of topics, then we have some assurance that we have the most
# distinct and useful topics. To find the optimal number of topics, I fit topic
# models with 5 to 35 topics to get a rough estimate of the amount of topics

# Initial data exploration and experiments suggested an optimal topic number of
# between 20 and 30

# create a vector for number of topics to fit.
n <- seq(15, 35, by = 2)

# Since fitting LDA models is computationally expensive, the furrr package is
# used to fit all the models. We use distributed LDA with convergence detection
# to further speed up the analysis by setting the auto_iter = TRUE. See also
# https://koheiw.github.io/seededlda/articles/pkgdown/distributed.html

# Function fitting the LDA model and computing the divergence score, to be used
# for parallelizing the code with furrr
get_div_score <- function(DFM, k) {
  lda_fit <- textmodel_lda(DFM,
                           k,
                           batch_size = 0.01,
                           auto_iter  = TRUE,
                           verbose    = TRUE)
  return(divergence(lda_fit))
}

# setup four threads
plan(multisession, workers = 11)

# fit models in parallel
div_scores <- n %>%
  future_map_dbl(~ get_div_score(DFM, k = .x),
    furrr_options(seed = TRUE),
    .progress = TRUE)

# assemble a df with number of topics and divergence scores
div_score_df <- tibble(
                num_topics = n,
                div_score  = div_scores)

div_score_df %>%
  arrange(desc(div_score))

nr.topics <- div_score_df %>%
  arrange(desc(div_score)) %>%
  slice(1) %>%
  select(num_topics) %>%
  as.numeric()

# plot topic number vs divergence score
tiff(filename = "output/figures/Number of Topics vs Divergence.tiff", width = 6000, height = 4000, res = 450)
par(mfrow = c(1,1))

div_score_df %>%
  ggplot(aes(x = num_topics, y = div_score)) +
  geom_point() +
  ylab("Divergence Score") +
  xlab("Number of Topics") +
  geom_hline(yintercept = max(div_score_df$div_score),
             color      = "blue",
             linetype   = "dashed") +
  ggplot2::annotate("text",
           x     = 30,
           y     = max(div_score_df$div_score) + 0.01,
           label = round(max(div_score_df$div_score),3),
           color = "blue") +
    ggtitle("Number of Topics vs Divergence")
dev.off()

# -> These first results suggest to look in more detail around 29 topics

## 2. Testing candidate models ----

# https://koheiw.github.io/seededlda/articles/pkgdown/seeded.html
# https://maartengr.github.io/BERTopic/getting_started/guided/guided.html#example
# https://koheiw.github.io/seededlda/articles/pkgdown/distributed.html
# https://tutorials.quanteda.io/machine-learning/topicmodel/

## Keyword Dictionary
# We seed the topic model with a keyword dictionary (see below). We can pass as
# many keywords as we like per topic, although the model’s fit strongly depends
# on the quality of your keywords (i.e. good keywords occur in many documents
# and are unique to the concept of interest)

# By defining these topics, seededlda is more likely to model the
# defined seeded topics. However, seededlda is merely nudged towards
# creating those topics. In practice, if the seeded topics do not exist
# or might be divided into smaller topics, then they will not be modeled.
# Thus, seed topics need to be accurate to accurately converge towards them

#dict <- dictionary(file = "data/external/dict.yml")
dict <- dictionary(list(
  revolutionary_politics = c("liberty", "liberties",
  "freedom", "right*", "privileges", "consent", "authority", "authorities",
  "power*","constitution", "constitutional", "law*", "independent", "independence",
  "independency", "sovereign", "sovereignty", "tax", "taxes", "taxation", "duty", "duties"),

  republican_politics = c("subject", "citizen*", "inhabitant*", "virtue*",
  "patriot*", "patriotic", "patriotism", "republic", "republican*", "faction*",
  "people", "empire*","nation*", "tyranny", "tyrant*", "despotism", "oppression",
  "luxury", "corrupt", "corruption", "tories"),

  slavery_and_race = c("slave*", "slavery", "negro", "negroes", "blacks",
  "african*", "trade*","bondage*"))
)

print(dict)

# checking some word occurrences
word_counts <- sapply(token_list, function(tokens) sum(tokens == "luxury"))
# Total occurrences of the word luxury
sum(word_counts) # 387

word_counts <- sapply(token_list, function(tokens) sum(tokens == "savage"))
# Total occurrences of the word savage
sum(word_counts) # 818

# Given the seed topics, check for models with different amount of residual topics

# While the 29-topic model from above (still ignorant of any seed topics) has
# the highest divergence, maximal divergence does not necessarily mean that that
# number of topics yields maximal meaningfulness. So, next We'll explore multiple
# models with different amounts of residual topics which come on top of the three
# seed topics, and then explore the quality of the topics based on additional
# quality measures (i.e., coherence and exclusivity).

# Given the three pre-specified seed topics, we now have to look for the optimal
# amount of residual topics added to our specified ones.

# See https://dataprofessor.net/blog/nlp/gc_topic_model/gc_topic_model_best_fit

# Here, we choose the number of residual topics to inspect, again, those in
# addition to the 3 seed topics as formulated in the dictionary above
K_range <- c(23,24,25, 26, 27,28,29)

# So, these amount of topics plus, each time, the 3 seed topics, leads to
# overall model-testing with 26,27,28, 29 ,30,31,32 topics. This ranges around
# the 29 topics for which we had the maximum divergence found earlier

plan(multisession, workers = 11)

start <- Sys.time()

# By default, the algorithm fits LDA through as many as 2000 iterations for
# reliable results
lda_fits <- K_range %>%
  future_map(
    ~ textmodel_seededlda(x           = DFM,
                          dictionary  = dict,
                          residual    = .x,
                          batch_size  = 0.01,
                          auto_iter   = TRUE,
                          verbose     = TRUE),
    furrr_options(seed = TRUE))

end <- Sys.time()
end - start # Time difference of 1.125711 hours

saveRDS(lda_fits, file = "data/interim/lda_fits.rds")
#lda_fits <- readRDS(file = "data/interim/lda_fits.rds")

# If necessary, access the candidate models via:
# lda23_df <- as_tibble(terms(lda_fits[[1]]))

## 3. Metrics: exclusivity and coherence ----

exclusivity <- function(lda, dfm, lambda = 0, num.words = 0){

  #' Topic exclusivity
  #'
  #' We use LDAvis relevance score with lambda = 0 for putting emphasis on
  #' exclusivity. See Sievert, C., Shirley, K.E. (2014). LDAvis: A method for
  #' visualizing and interpreting topics. In: Proceedings of the workshop on
  #' interactive language learning, visualization, and interfaces, 63–70
  #' Modified from "exclusivity_LDAproto function" by A. Bitterman

  #' @param lda
  #' @param dfm
  #' @param lambda
  #' @param num.words

  #' @return

  #' @export

  #' @examples
  #'  exclusivity(lda, dfm, lambda = 0, num.words = 0)
  #'  exclusivity(lda, dfm, lambda = 0.6, num.words = 10)
  #'  exclusivity(lda, dfm, lambda = 0.6, num.words = 20)

  if (num.words == 0)
    num.words = dim(dfm)[2]

  pwt <- t(as.matrix(lda$phi))
  pw  <- colSums(dfm)/sum(dfm)
  res <- apply(pwt, 1, function(x, num.words, pw, lambda) {
    x <- lambda * log(x) + (1 - lambda) * log(x/pw)
    return((sort(x, decreasing = TRUE)[1:num.words]))
  }, num.words, pw, lambda)

  return(colMeans(res))
}

# Coherence: Modified from coherence_LDAproto function (A. Bitterman)
coherence <- function(model, DTM, N = 10) {

  # Based on Mimno, D., Wallach, H. M., Talley, E., Leenders, M., & McCallum, A.
  # (2011). Optimizing semantic coherence in topic models. In: Proceedings of
  # the Conference on Empirical Methods in Natural Language Processing
  # (pp. 262-272). Association for Computational Linguistics. Chicago

  # Ensure matrix or Matrix-format (convert if slam)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DTM)) {
    DTM <- sparseMatrix(i = DTM$i, j = DTM$j, x = DTM$v,
                        dims = c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }

  K <- model$K

  DTMBIN <- DTM > 0

  documentFrequency <- colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)

  topNtermsPerTopic <- sentopics::topWords(model, nWords = 10) %>%
    select(topic,word) %>%
    pivot_wider(names_from  = "topic",
                values_from = "word",
                values_fn   = list) %>%
    unnest(everything())

  topNtermsPerTopic <- as.matrix(topNtermsPerTopic)
  allTopicModelTerms <- unique(as.vector(topNtermsPerTopic))

  DTMBIN     <- DTMBIN[, allTopicModelTerms]
  DTMBINCooc <- t(DTMBIN) %*% DTMBIN
  DTMBINCooc <- t((DTMBINCooc + 1) / colSums(DTMBIN))
  DTMBINCooc <- log(DTMBINCooc)
  DTMBINCooc <- as.matrix(DTMBINCooc)

  coherence  <- rep(0, K)
  pb <- txtProgressBar(max = K)

  for (topicIdx in 1:K) {
    setTxtProgressBar(pb, topicIdx)
    topWordsOfTopic <- topNtermsPerTopic[,topicIdx]

    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m - 1)) {
        mTerm <- as.character(topWordsOfTopic[m])
        lTerm <- as.character(topWordsOfTopic[l])
        coherence[topicIdx] <- coherence[topicIdx] + DTMBINCooc[mTerm, lTerm]
      }
    }
  }
  close(pb)

  return(coherence)
}

# Inspect topic coherence and exclusivity of candidate models listed in lda_fits object

# First, we convert the seeded_lda models to a list of lda model objects with
# different k's
sentopics_lda_fits <- lapply(lda_fits, sentopics::as.LDA)
saveRDS(sentopics_lda_fits, file = "data/interim/sentopics_lda_fits.rds")

# Next, for each of the 7 topic models, we calculate coherence scores of each
# individual topic within that model
coh      <- lapply(sentopics_lda_fits, coherence, DTM, N = 25)
coh_mean <- unlist(lapply(coh, mean))

#Then, again for each topic model, we calculate the exclusivity scores of each
# individual topic within that model
exc      <- lapply(sentopics_lda_fits, exclusivity, DFM, num.words = 15)
exc_mean <- unlist(lapply(exc, mean))

# plot of scaled scores, including mean of semantic coherence & exclusivity
semcoh <- scale(coh_mean)
exclus <- scale(exc_mean)
semexc <- rowMeans(cbind(semcoh, exclus)) # mean of scaled sem & exc

tiff(filename = "output/figures/Mean Coherence & Exclusivity.tiff", width = 6000, height = 4000, res = 450)
par(mfrow = c(1,1))

plot(semcoh, type = "l", col = "blue", xaxt = "n", lwd = 2, ylim = c(-3, 3),
     main = "Topic Quality", ylab = "Scaled Score", xlab = "Number of Topics")
lines(exclus, type = "l", col = "black", lwd = 2)
lines(semexc, type = "l", col = "orange", lwd = 3)
abline(h = max(semexc), v = which.max(semexc), col = "gray", lwd = 2, lty = "dashed")
axis(1, at = 1:length(K_range), labels = K_range)
legend("bottomright", c("Semantic Coherence", "Exclusivity (LDAvis lambda = 0)",
                        "Mean Coherence & Exclusivity"),
       col = c("blue", "black", "orange"), lty = "solid", lwd = 1)
dev.off()

## 4. Semantic granularity ----

# we select two models of different granularity for further inspection

# select indices of desired amount of residual topics k
select <- K_range %in% c(26, 29)
candidates_inspect <- lda_fits[select]

## 5. Quality of single topics ----
par(mfrow = c(1, 2))

for (i in 1:length(candidates_inspect)) {

  plot(coh[select][[i]], exc[select][[i]], main = paste("K =", length(coh[select][[i]]),"Topic Quality"),
       ylab = "Exclusivity", xlab = "Coherence", col = "white")

  text(coh[select][[i]], exc[select][[i]], labels = paste("", 1:length(coh[select][[i]])), cex = 1.5)
}


## 6. Visualize topic similarity ----
#source("./helper_functions/topic_network.R")

topic_network <- function(model, thresh = 0.15){

  # The selected model and number of topics
  lda <- model
  K   <- lda$K

  # theta (document-topic probabilities)
  theta <- lda$theta
  colnames(theta) <- NULL

  # beta (word-topic probabilities) a.k.a. phi
  beta <- lda$phi
  colnames(beta) <- NULL

  # Extract the most representative words in each topic
  topwords <- sentopics::topWords(model, method = "probability", nWords = 10) %>%
    select(topic, word) %>%
    pivot_wider(names_from  = "topic",
                values_from = "word",
                values_fn   = list) %>%
    unnest(everything())

  topwords <- as.matrix(topwords)
  colnames(topwords) <- NULL
  topwords <- apply(topwords, 2, paste, collapse = ", ")

  prevalence <- colMeans(theta)

  # number of docs with theta > .5 per topic
  n_docs <- apply(theta, 2, function(x){unname(table(x > 0.5)[2])})

  # use first two top terms as initial topic labels
  label <- sapply(topwords, function(x) {paste(strsplit(x, ", ")[[1]][1:2], collapse = " ")})
  label <- unname(label)

  # add ID for disambiguation
  for (i in 1:K) {
    label[i] <- paste0("T", i, ": ", label[i])
  }

  # topic correlations
  cor_mat <- cor(beta)

  # omit small correlations to improve graph readability
  cor_mat[cor_mat < thresh] <- 0
  diag(cor_mat) <- 0 # needed for network plot

  # network of the word distributions over topics (topic relation)
  graph <- graph.adjacency(cor_mat, weighted = TRUE, mode = "lower")

  # edge labels
  edge_attr(graph, "name")  <- round(E(graph)$weight, 2)
  edge_attr(graph, "label") <- E(graph)$name

  # line thickness
  E(graph)$edge.width <- E(graph)$weight * 20

  # labels
  V(graph)$label <- label
  V(graph)$size  <- 10

  # Detect communities within the graph and add membership as vertex attribute
  # multi-level modularity optimization algorithm for finding community structure
  cd <- cluster_louvain(graph)
  V(graph)$community <- cd$membership

  # nodes
  nodes        <- as.data.frame(1:K)
  names(nodes) <- "id"
  nodes$label  <- label
  nodes$size   <- colMeans(theta)*100 # size by topic prevalence
  nodes$group  <- V(graph)$community

  # edges
  edges           <- get.data.frame(graph)
  names(edges)[3] <- "width"
  edges$label     <- as.character(round(edges$width, 2))
  edges$label     <- ifelse(nchar(edges$label) == 3, paste0(edges$label, "0"), edges$label)
  edges$width     <- edges$width * 4 # improve visibility in plot
  rbPal           <- colorRampPalette(c("grey", "cornflowerblue"))
  edges$color     <- rbPal(10)[as.numeric(cut(edges$width, breaks = 10))] # color edge label according to width

  set.seed(1234)

  visNetwork(nodes, edges) %>%
    visIgraphLayout(layout = "layout.fruchterman.reingold", physics = TRUE, smooth = TRUE) %>%

    visNodes(
      shape = "dot",
      font = list(size = 10, background = "white"),
      color = list(
        background = "#0085AF",
        border     = "#013848",
        highlight  = "#FF8000"
      ),
      shadow = list(enabled = TRUE, size = 10)
    ) %>%

    visEdges(
      label  = edges$label,
      font   = list(color = "slategray", size = 10),
      smooth = list(enabled = TRUE, type = "diagonalCross"),
      shadow = FALSE,
      color = list(color = "#0085AF", highlight = "#C62F4B", opacity = .5)
    ) %>%

    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
}

# create network of topics for different models to allow for comparison
topic_network(sentopics_lda_fits[[which(K_range == 26)]])
topic_network(sentopics_lda_fits[[which(K_range == 29)]])


## 7. Model selection ----
lda_res <- lda_fits[[which(K_range == 26)]]

# extract the most likely words for each topic and inspect these to come up
# with topic labels
topic_terms <- as.data.frame(seededlda::terms(lda_res, n = 25))

# Interpret each topic and change its label
topicnames <-
    c("01_Revolutionary politics",
      "02_Republican politics",
      "03_Maritime Trade and Slavery",
      "04_Military Operations and Strategies in Conflict",
      "05_Military Structure and Discipline in Armed Forces",
      "06_Infrastructure Development and Transportation Networks",
      "07_Real Estate and Property Transactions",
      "08_Positive Relationships and Personal Well-being",
      "09_The Physics of Motion and Observation",
      "10_Diplomacy and International Relations",
      "11_Personal Finance and Financial Management",
      "12_Education and Societal Progress",
      "13_Political Processes and Governance",
      "14_The Influence of Literature and Media on Society",
      "15_Government Policies and Decision-Making",
      "16_Territorial Expansion and Land Claims in History",
      "17_The Importance of Family and Relationships in Human Life",
      "18_Etiquette and Protocol in Social and Business Interactions",
      "19_Agriculture and the Economics of Crop Production",
      "20_Wine Selection and Delivery Services",
      "21_Professional Etiquette and Procedures in Government and Business",
      "22_Professional Development and Career Advancemen",
      "23_Strategic Planning and Decision-Making in Business",
      "24_French language")

# update the topic labels in theta
colnames(lda_res[["theta"]]) <- NULL
colnames(lda_res[["theta"]]) <- topicnames

# update the topic labels in phi
rownames(lda_res[["phi"]]) <- NULL
rownames(lda_res[["phi"]]) <- topicnames

# Convert seededLDA model to the lda-format used by the sentopics package
lda     <- sentopics::as.LDA(lda_res)
K       <- lda$K

# Topic Validity ----------------------------------------------------------

## get probability matrices ----

# theta (document-topic probabilities)
theta <- lda$theta
colnames(theta) <- paste("Topic", 1:K)

# beta (word-topic probabilities) a.k.a. phi
beta <- lda$phi
colnames(beta) <- paste("Topic", 1:K)

saveRDS(lda,   file = "data/interim/lda.rds")
saveRDS(theta, file = "data/interim/theta.rds")
saveRDS(beta,  file = "data/interim/beta.rds")

## top terms ----
topterms <- sentopics::topWords(lda, method = "probability", nWords = 10) %>%
  select(topic, word) %>%
  pivot_wider(names_from  = "topic",
              values_from = "word",
              values_fn   = list) %>%
  unnest(everything())

topterms <- as.matrix(topterms)
colnames(topterms) <- NULL
topterms <- apply(topterms, 2, paste, collapse = ", ")

## inspect texts ----

# with highest probability of addressing the topic (i.e., high theta)

#source("./helper_functions/get_topdocs.R")
get_topdocs <- function(model, texts, n = 10, text_ID = rownames(texts)) {

  theta <- model$theta
  K     <- lda$K

  # 10 most representative docs
  theta_tmp    <- as.data.frame(theta)
  theta_tmp$ID <- text_ID # for matching theta with texts

  topdocs <- list()

  for (i in 1:K) {
    # get row indices of top n
    tmp <- theta_tmp[order(-theta_tmp[,i]),]
    ids <- tmp$ID[1:n]

    topdocs_tmp <- texts[text_ID == ids[1],]
    for (j in 2:length(ids)) {
      topdocs_tmp <- rbind(topdocs_tmp, texts[text_ID == ids[j],])
    }

    # add theta probabilities
    topdocs_tmp$prob <- tmp[1:n,i]
    topdocs[[i]] <- topdocs_tmp
  }

  return(topdocs) # a list
}

# you may need to set a different text_ID
# Especially if you dropped docs during pre-processing! (cf. dfm_trim)
topdocs <- get_topdocs(lda, texts, n = 50, text_ID = rownames(texts))

# show most representative texts on the topic of interest, i.e. revolutionary politics
t <- 1 # select the first topic of interest
topterms[t]

# -2 to leave out the column with the letters
topdocs[[t]][,-2]

# top ten letters on revolutionary politics
topdocs[[t]][1:10,2]

## topic table ----

# topic prevalence (average topic probability across all letters)
prevalence <- colMeans(lda$theta)

# number of docs with theta > .5 per topic
n_docs <- apply(lda$theta, 2, function(x){unname(table(x > 0.5)[2])})

# data frame
topic_table <- data.frame("ID" = 1:K, topicnames, topterms, prevalence, n_docs)
saveRDS(topic_table, file = "data/interim/topic_table.rds")


# Topic analyses (final model) -----

# Summarize the topic proportions and their top words
plot(lda, nWords = 5)

# Display the most probable words
sentopics::plot_topWords(lda, nWords = 10)

## 1. Most-likely terms for each topic ----
seed_topics <- as.data.frame(seededlda::terms(lda_res, 25))

# save results
write.csv(seed_topics,"output/tables/seed_topics.csv")

## 2. Topic probabilities per letter ----

# https://www.kaggle.com/code/fajarkhaswara/topic-modelling-on-indonesian-twitter
# function to get probabilities of topic
get_doc_topic_probs <- function(slda) {
  out <- slda$theta %>%
    as_tibble(rownames = "id")
  return(out)
}

# get_word_topic_probs <- function(slda) {
#   out <- slda$phi %>%
#     as_tibble(rownames = "topic") %>%
#     pivot_longer(cols = !matches("topic"), names_to = "token", values_to = "prob")
#   return(out)
# }

topic_probabilities <- as.data.frame(get_doc_topic_probs(lda_res)) %>%
  mutate(id = row_number())

## 3. Most-likely topic for each letter based on the theta parameter ----
# main topic added as a new document-level variable in the texts dataframe
# TODO: set min_prob higher to what value????
texts$topic <- seededlda::topics(lda_res,  min_prob = 0.05)

## 4a. Distribution of amount of letters per main topic ----
topics_table          <- ftable(texts$topic)
topicsprop_table      <- as.data.frame(prop.table(topics_table))
topicsprop_table$Freq <- topicsprop_table$Freq * 100

ggplot(data = topicsprop_table, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Topics", y = "Topic %") +
  labs(title = "Topic proportions - Founders Online Archive") +
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1))


## 4b. Most prevalent topics ----

### overall ----
prevalence_sorted        <- topic_table$prevalence
names(prevalence_sorted) <- topic_table$topicnames
prevalence_sorted        <- sort(prevalence_sorted)

par(mfrow = c(1,1))
par(mar = c(2, 4.25, 1, 1) * 2.5)
barplot(prevalence_sorted, horiz = TRUE, las = 1, xlim = c(0, 0.06),
        main = "Topic prevalence")

## 4c. Prevalence by year ----

# original code by https://github.com/mponweiser/thesis-LDA-in-R/blob/master/application-pnas/trends.Rnw

# overall overview of topic prevalence by year
theta_mean_by_year_by <- by(lda$theta, texts$year, colMeans)
theta_mean_by_year    <- do.call("rbind", theta_mean_by_year_by)
years                 <- levels(factor(texts$year))
ts                    <- ts(theta_mean_by_year, start = as.integer(years[1]))

# plot
par(mfrow = c(3,1))

# select the first 3 topics which are of main interest
for (i in 1:3) {
  plot(ts[,i],
       col = "orange",
       ylim = c(0, 0.2),
       ylab = "",
       xlab = "",
       main = paste(i, topic_table$labels[i]),
       cex.main = 1,
       lwd = 2)
}

## 5. Visualize word probabilities per topic ----
options(repr.plot.width = 15, repr.plot.height = 15)

# Beta column contains the word X topic distributions
lda_top_words <- setNames(reshape2::melt(lda_res$phi),c("topic","term","beta")) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

lda_top_words[nchar(as.character(lda_top_words$term)) >= 3 & lda_top_words$topic != "other",] %>%
  mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = T) +
  facet_wrap(~ topic, scales = "free") +
  tidytext::scale_y_reordered()

## 6. LDAvis ----

# The LDAvis allows you to interactively visualize an LDA topic model. The major
# graphical elements include:
# 1) Default topic circles - K circles, one for each topic, whose areas are set
#    to be proportional to the proportions of the topics across the N total tokens
#    in the corpus
# 2) Red bars - represent the estimated number of times a given term was
#    generated by a given topic
# 3) Blue bars - represent the overall frequency of each term in the corpus
# 4) Topic-term circles - KxW circles whose areas are set to be proportional to
#    the frequencies with which a given term is estimated to have been generated
#    by the topics

sentopics::LDAvis(lda)


# Save dataframe for shiny animated wordcloud and network analyses ----

# transpose the seedtopics so that it can be merged with texts file
seed_topics <- t(seed_topics) %>% as.data.frame() %>%
  rownames_to_column("topic") %>%
  unite(col = 'terms', starts_with("V"), sep = ', ')

letters_with_topics <- texts %>%
  # merge in the topic probabilities per document, on id
  left_join(topic_probabilities, by = "id") %>%
  # merge in the topic terms, by topic number
  left_join(seed_topics, by = "topic")

# Save preprocessed data wit htopic information ----
write.csv(letters_with_topics,"data/processed/founders/letters_with_topic_info.csv")

# show distribution over topics for sample of letters
 letters_with_topics %>% sample_n(50) %>%
  select(id, matches("^[0-9]")) %>%
  pivot_longer(-id, names_to = 'topic',values_to = 'probability') %>%
  #mutate(topic = str_replace(topic,'prob_','')) %>%
  #inner_join(topiclabels,by='topic') %>%
  ggplot(aes(x = as.factor(id), y = probability)) +
  geom_bar(aes(fill = topic), stat = 'identity') + coord_flip() +
  ggtitle('Topic probability distribution per review') + theme_minimal() +

  theme(legend.position = "right",
        legend.text     = element_text(size = 6),
        legend.title    = element_blank(),
        plot.title      = element_text(hjust = 0.5, size = 12),
        axis.title      = element_text(size = 8),
        axis.text       = element_text(size = 8)) + scale_fill_viridis_d(option = "inferno")

#create dataframe for shiny animated wordcloud
 yr_topic_term_freq <- letters_with_topics %>%
  select(id,text, year,topic, terms) %>%
  rowwise() %>%
  mutate(terms = str_split(terms, ", ")) %>%
  # unnest to create a new row for each word
  unnest(terms) %>%

  # we group the data frame by 'year,' 'topic' and 'terms' (individual words)
  # and calculate the sum of the counts for each word using str_count in the
  # 'text' column
  group_by(year, topic, terms) %>%
    summarise(word_count = sum(str_count(text, terms))) %>%
    # the dataframe has separate rows for each individual word, with counts
    # per word, per year, and per topic

  group_by(year, topic) %>%

    # regroup the data frame by 'year' and 'group' and calculate the relative
    # occurrence of each word by dividing the word count by the sum of word counts
    # for that year and group
    mutate(relative_occurrence = word_count / sum(word_count)) %>%
  ungroup()

# save results
# TODO: specify proper directory
write.csv(yr_topic_term_freq,"yr_topic_term_freq.csv")


# Topic probability distribution differences for Founding Fathers ----

letters_with_topics_ff <- letters_with_topics %>%
  filter(authors == "Franklin, Benjamin" | authors == "Jefferson, Thomas" |
         authors == "Washington, George" | authors == "Adams, John"       |
         authors == "Hamilton, Alexander" | authors == "Jay, John"        |
         authors == "Madison, James"   ) %>%

  # select topics of main interest where we want to explore differences
  select(authors,`01_Revolutionary politics`,
                 `02_Republican politics`,
                 `03_Maritime Trade and Slavery`) %>%

  # filter out very low probabilities to enhance clarity
  filter(`01_Revolutionary politics`     >= 0.05 &
         `02_Republican politics`        >= 0.05 &
         `03_Maritime Trade and Slavery` >= 0.05)

table(letters_with_topics_ff$authors)


letters_with_topics_ff %>%
  # topic probabilities to rows
  pivot_longer(-authors, names_to = 'topic', values_to = 'probability') %>%
  mutate(authors = as.factor(authors))  %>%

  # create density plots per topic of interest for founding fathers
  ggplot(aes(x = probability, group = authors, fill = authors)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ topic, ncol = 4) +

  ggtitle('Topic probability distribution for political letters written by founding fathers') +
  theme_minimal()  +
  theme(legend.position = c(0.9, 0.2),
        legend.text     = element_text(size = 8),
        legend.title    = element_blank(),
        plot.title      = element_text(hjust = 0.5, size = 12),
        axis.title      = element_text(size = 8),
        axis.text       = element_blank()) + ylim(c(0,20))

# Interpretation ----
# In the context of topic probability distributions, high probability values
# indicate the likelihood that a particular topic is present or dominant in a
# given document or dataset. These probabilities are typically produced by topic
# modeling techniques like Latent Dirichlet Allocation (LDA) or similar methods.
# Here's how to interpret high probability values in topic probability
# distributions:

# Dominant Topics: A high probability for a specific topic in a document suggests
# that this topic is the most prominent or dominant topic within that document.
# In other words, it's the topic that best represents the content of the document.

# Relevance: High probability values indicate the relevance of a topic to a
# particular document. If you have a document with a high probability for
# "Topic A," it's likely that the content of that document is strongly related
# to "Topic A."

# Interpretation: When you see high probabilities for specific topics, you can
# interpret these as the most salient themes or subjects discussed in the
# document. These are the topics that the model believes are the primary focus
# of the content.

# Topic Mix: High probabilities don't exist in isolation. In a document, you may
# have several topics with high probabilities, and this reflects a mix of themes.
# However, the one with the highest probability is usually considered the
# dominant topic.

# Threshold: There's no universal threshold for what constitutes a "high"
# probability value. It depends on the context, your specific model, and your
# research goals. In practice, you might consider probabilities above a certain
#threshold (e.g., 0.5 or 0.7) as high, but this threshold is somewhat arbitrary.

# Uncertainty: Keep in mind that topic models may produce high probabilities for
# multiple topics in a document if the content is truly diverse.
# High probabilities for several topics might suggest ambiguity or diversity
# in the document's themes.

# It's essential to consider these high probability values in the broader
# context of the  analysis and research objectives. High probabilities are
# indicative of strong associations between topics and documents, but the exact
# interpretation can vary depending on your specific use case and dataset.

# Session information ----
sessioninfo::session_info()
