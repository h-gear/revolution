#                               1. Script header ----
#' =============================================================================
#' Description:
#' This script performs a dynamic topic analysis on the Founders Online texts,
#' analyzing how the prevalence of topics change over time. The script uses the
#' keyATM package to perform the analysis. It reads the cleaned text data from
#' semi_supervised_topic_modleing.R and prepares it for the analysis. It
#' then creates a dynamic keyATM model and fits it to the data.

#' Purpose:
#' a) Understand the evolution of topics over time
#' b) Use as an additional alternative to the shifting concepts in time (shico)
#'    approach
#'
#' For more background information and examples, see:
#' https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_dynamic.html
#' https://towardsdatascience.com/why-to-use-seeded-topic-models-in-your-next-project-and-how-to-implement-them-in-r-8502d15d6e8d
#' =============================================================================

# 2. Libraries ----
library(keyATM)
library(parallel)
library(quanteda)
library(tidyverse)
library(future)

# 3. Data ----
letters <- readRDS("data/processed/founders/texts.rds") %>%
    filter(!text_cleaned == "") %>%
    select(-text) %>%

    # Use cleaned text for the analysis (tokenized, lemmatized, lowercased, etc.)
    rename(text = text_cleaned) %>%

    # Focus on time period where there is sufficient amount of letter data
    filter(year >= 1750 & year <= 1825)

# OR
#DFM <- readRDS("data/interim/DFM.rds")
# remove documents without any word counts
#DFM0 <- dfm_subset(DFM, ntoken(DFM) > 0)

# 4. Create keyATM docs ----
# Transform data to a format suitable for the keyATM model
# When using a data.frame or tibble, texts should be stored in a column named `text`
keyATM_docs <- keyATM_read(texts = letters)
summary(keyATM_docs)

saveRDS(keyATM_docs, file = "data/interim/keyATM_docs.rds")
#keyATM_docs <- readRDS(file = "data/interim/keyATM_docs.rds")

# 5. Keywords ----
source("scripts/4_analysis_and_modeling/keywords.r")
print(keywords)

keywords_stem <- keywords %>%
    tokens() %>%
    tokens_replace(pattern     = lexicon::hash_lemmas$token,
                   replacement = lexicon::hash_lemmas$lemma) %>%
    quanteda::as.list()

# Keywords should appear reasonable times (typically more than 0.1% of the
# corpus) in the documents, i.e., high-frequency key words are better.
# The visualize_keywords() function plots the frequency of keywords by topic
key_viz_republican <- visualize_keywords(docs     = keyATM_docs,
                              keywords = keywords_stem['Republican_politics'])

key_viz_liberal <- visualize_keywords(docs     = keyATM_docs,
                              keywords = keywords_stem['Liberal_politics'])

#plotting
key_viz_republican
key_viz_liberal

saveRDS(values_fig(key_viz_republican), file = "data/interim/keyviz_republican.rds")
saveRDS(values_fig(key_viz_liberal), file = "data/interim/keyviz_liberal.rds")

save_fig(key_viz_republican, filename = "data/interim/keyviz_republican.png")
save_fig(key_viz_liberal, filename = "data/interim/keyviz_liberal.png")

# 6. Create decade time index variable ----
corp_letters <- corpus(letters)

# For dynamic seeded LDA models, it is necessary to create a period variable
# which starts at 1 and counts up by one for every period. In the code below,
# one period reflects 10 years, starting with the year 1720
vars <- quanteda::docvars(corp_letters)

vars_period <- vars %>%
    mutate(period = (year - 1750) %/% 10 + 1) %>%
    as_tibble() %>%
    arrange(period)

# checking variety of decades (missing periods are not allowed)
unique(vars_period$period)

# 7. Dynamic keyATM ----
# Set up parallel processing
num_cores <- detectCores()
cat("Number of CPU cores:", num_cores, "\n")

# Set the number of cores/threads to use for parallel processing
# Adjust the number according to your hardware capabilities

# Initialize the parallel backend with the specified number of cores
plan(multicore, workers = num_cores - 1)

# https://rpubs.com/Ankit164/SBIR_AWARDS
print("Start model fit")

tic <- Sys.time()

keyatm_dyn <- keyATM(
    docs              = keyATM_docs,                # text input
    model             = "dynamic",                  # select the model
    no_keyword_topics = 0,                          # nr of topics without keywords is set to 0, since we defined 31 topics to be 'all-inclusive'
    keywords          = keywords_stem,              # keywords
    model_settings    = list(time_index    = vars_period$period,
                             num_states    = 8),    # specifying the number of hidden states
    options           = list(seed          = 1234,
                             iterations    = 250,   # takes ca. 60-120 minutes
                             store_theta   = TRUE,  # get 90% CI in plot
                             store_pi      = TRUE,
                             verbose       = TRUE,
                             parallel_init = TRUE)) # enable parallel initialization

toc <- Sys.time()

print(paste0("Dynamic Model fit took ", round(toc - tic, digits = 2)))

saveRDS(keyatm_dyn, file = "data/interim/keyatm_model.rds")
save.keyATM_output(keyatm_dyn, file = "data/interim/keyatm_dynamic_model.rds")

# 8. Model evaluation ----
out <- readRDS(file = "data/interim/keyatm_model.rds")

## a) Topic-term inspection ----
# Terms as specified in the keywords as belonging to a topic are suffixed with a
# check mark. Words from another keyword topic are labeled with the name of that
# category
top_words(out)

## b) Diagnose and explore the fitted model ----
# If the model is working as expected, we would observe an increase trend
# for the log-likelihood and an decrease trend for the perplexity.
plot_modelfit(out)

# alphas -> topic proportions per time slice
# visualized the prior for the document-topic distribution
# alpha for each time slice per iteration describes the topic proportions
# for each time slice per iteration of the model fitting process (i.e., the MCMC chain)
# The alpha values are the Dirichlet priors for the document-topic distribution.
plot_alpha(out, show_topic = c(1,2), scales = "fixed")

base_model_pi <- plot_pi(out, show_topic = c(1,2))

# Examine how likely each topic appears in the corpus
# Expected proportions of the corpus belonging to each estimated topic
# along with the top three words associated with the topic
plot_topicprop(out,
               show_topic = c(1,2),
               #show_topic = 1:31,
               show_topwords = TRUE,
               n = 5
               )

# plots -> alpha for each time slice per iteration
fig_alpha <- plot_alpha(out)
fig_alpha

## c) Keyword importance for the topics ----
fig_pi <- plot_pi(out)
fig_pi

## d) Topic proportions over time ----
fig_timetrend <- plot_timetrend(out,
                                show_topic = c(1,2), # "1_Liberal_politics","2_Republican_politics"
                                time_index_label = vars$year[1:keyatm_dyn$N],
                                xlab = "Year",
                                show_point = F,
                                ci = 0.01)
fig_timetrend

save_fig(fig_timetrend, filename = "data/interim/timetrend_politics.png", width = 30, height = 15)

## d) sophisticated plot of politics ----
# see https://keyatm.github.io/keyATM/articles/pkgdown_files/Plot.html
values <- values_fig(fig_timetrend)

values %>%
    filter(Topic %in% c("1_Liberal_politics","2_Republican_politics")) %>%
    ggplot(., aes(x = time_index, y = Point, group = Topic)) +
    # ggplot(., aes(x = time_index, y = Proportion, group = Topic)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray75") +
    geom_line(aes(colour = Topic), alpha = 1, size = 0.8, color = "red") +
    geom_point(shape = 5, size = 2) +
    #geom_line(size=0.8, aes(y=rollmean(Point, 5, na.pad=TRUE))) +
    #geom_smooth(span = 0.01, size=1) +
    geom_vline(xintercept = c(1775, 1783), lty = "dashed") +
    facet_wrap(~ Topic, ncol = 1) +
    xlab("Year") + ylab(expression(paste("Mean of ", theta))) +
    ggthemes::theme_economist_white(gray_bg = FALSE) +
    scale_x_continuous(breaks = seq(1700, 1835, 5), labels = seq(1700, 1835, 5)) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45,
                                                               vjust = 0.5,
                                                               hjust = 1))

# OR

time_series <- values %>%
    # Focus on 2 main topics of interest on politics
    filter(Topic %in% c("1_Liberal_politics","2_Republican_politics")) %>%
    ggplot(., aes(x = time_index, y = Point, group = Topic)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray75") +
    geom_line(linewidth = 0.8, aes(colour = Topic)) +
    geom_point(shape = 5, size = 0.9) +
    geom_vline(xintercept = c(1775, 1783), lty = "dashed") +
    xlab("Year") + ylab(expression(paste("Mean of ", theta))) +
    annotate("text", x = 1835, y = 0.02, label = "Liberal politics", colour = "#F8766D") +
    annotate("text", x = 1835, y = 0.06, label = "Republican politics", colour = "#00BFC4") +
    ggthemes::theme_economist_white(gray_bg = FALSE) +
    theme(legend.position = "none")

tiff(filename = "data/interim/timetrend_politics.tiff", width = 6000, height = 4000, res = 450)
    par(mfrow = c(1,1))
    time_series
dev.off()


# 9. Theta (document-topic distribution) ----
theta            <- out$theta %>% as_tibble()
theta$index      <- seq(1, nrow(theta[,1]))
theta$date       <- letters$sending_date %>% as.Date()
theta            <- theta %>% mutate(year = year(date), month = month(date))
theta$year_month <- theta$year + (theta$month / 12)
theta            <- theta %>% arrange(date)

# information on historical time periods
events <- tibble(  start   = c(as.Date('1725-01-01'),as.Date('1775-04-19'),
                               as.Date('1783-09-04'),as.Date('1789-04-30'),
                               as.Date('1797-03-04'),as.Date('1801-03-04'),
                               as.Date('1809-03-04'),as.Date('1817-03-04')),
                     end   = c(as.Date('1775-04-18'),as.Date('1789-09-03'),
                               as.Date('1789-04-29'),as.Date('1797-03-03'),
                               as.Date('1801-03-03'),as.Date('1809-03-03'),
                               as.Date('1817-03-03'),as.Date('1825-01-01')),
                    event  = c("Colonial","Revolutionary War",
                               "Confederation Period","Washington Presidency",
                               "Adams Presidency","Jefferson Presidency",
                               "Madison Presidency","post-Madison Presidency"))

events$height <- 0.2
events$start <- events$start %>% as.Date()
events$end <- events$end %>% as.Date()

p <- theta %>% ggplot(aes(x = date, y = `1_Liberal_politics`))
p + scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    geom_rect(data = events, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              alpha = 0.4,
              fill  = c("lightblue"),
              color = "lightblue",
              lty   = "dashed") +
    ggrepel::geom_label_repel(data = events,
                     mapping = aes(x = start, y = height, label = event),
                     inherit.aes = FALSE,
                     hjust = 1,
                     nudge_x = -500) +
    # geom_vline(data = events, aes(xintercept = date), lty = "dashed") +
    geom_line(color = "black",
              alpha = 1,
              aes(y = zoo::rollmean(`1_Liberal_politics`, 100, na.pad = TRUE))) +
    ggthemes::theme_economist_white(gray_bg = FALSE) +
    labs(title = "DTM - Liberal politics in different periods")

topwords_table <- function(out, n = 10) {
    #' Reformats top_words() output
    #'
    #' @param out df, theta output
    #' @return in adequate format

    topwords_tmp <- out %>% top_words(n = n, show_keyword = T)
    topwords_tmp <- as_tibble(cbind(topic = names(topwords_tmp),
                                    t(topwords_tmp)))
    topwords <- topwords_tmp %>%
        unite("tmp",-c(topic), sep = ", ") %>%
        mutate(words = str_replace_all(tmp,
                                       pattern = "( \\[\U2713\\])|( \\[\\d*\\])",
                                       replacement = " [X]"),
               topic = str_replace_all(topic,
                                       pattern = "\\d*_",
                                       replacement = "")) %>%
        dplyr::select(-tmp)

    return(topwords)
}

topwords <- out %>% topwords_table(n = 10)


# Preprocess theta matrix for plots
preprocess_theta <- function(theta, csv, kind, short=FALSE) {
    #' @param theta df, how it comes out from keyatm_dyn model
    #' @return in adequate format

    theta <- theta %>% as_tibble()
    theta$index <- seq(1, nrow(theta[,1]))
    theta$date <- csv$sending_date %>% ymd()
    theta <- theta %>% mutate(year = year(date),
                              month = month(date),
                              year_month = format_ISO8601(date, precision = "ym") %>% ym(),
                              kind = kind,
                              short = short)
    theta <- theta %>% arrange(date)

    return(theta)

}

# Preprocess theta (document-topic distribution) for plotting purposes
theta <- out$theta %>%
    preprocess_theta(csv = letters, kind = "Dynamic")

df <- theta %>%
    dplyr::select(-c(kind, short, year_month, index, month, date))

df <- df %>%
    pivot_longer(-c(year)) %>%
    group_by(year, name) %>%
    summarise(n = sum(value)) %>%
    mutate(total_year = sum(n),
           percentage = n / total_year)

# Different colors for the topics
col_vector <-
    c(  '#469990',
        '#9A6324',
        '#5F4B8B',
        '#A0522D',
        '#800000',
        '#4169E1',
        '#CD5C5C',
        '#4682B4',
        '#9ACD32',
        '#B22222',
        '#20B2AA',
        '#FF6347',
        '#008080',
        '#8B008B',
        '#DAA520',
        '#BDB76B',
        '#DC143C',
        '#556B2F',
        '#00FFFF',
        '#D2691E',
        '#00BFFF',
        '#ADFF2F',
        '#DB7093',
        '#F4A460',
        '#1E90FF',
        '#8A2BE2',
        '#32CD32',
        '#FF69B4',
        '#FFA07A',
        '#7FFFD4',
        '#F0E68C')

plot_theta_shares <- df %>%
    ggplot(aes(x = make_date(year), y = percentage, fill = name)) +
    geom_area(alpha = 1, colour = "black", size = 0.2) +
    scale_fill_manual(values = col_vector) +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 5)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    labs(x = "", y = expr(widehat(theta)), fill = "") +
    guides(fill = guide_legend(nrow = 5))

tiff(filename = "data/interim/document_topic_distributions_over_time.tiff", width = 6000, height = 4000, res = 450)
par(mfrow = c(1,1))
plot_theta_shares
dev.off()

# 10. Session information ----
sessioninfo::session_info()

