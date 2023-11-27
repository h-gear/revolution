# Prevalence of topics over time with ATM ---- 

# KeyATM: https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_dynamic.html
# https://towardsdatascience.com/why-to-use-seeded-topic-models-in-your-next-project-and-how-to-implement-them-in-r-8502d15d6e8d
# https://rpubs.com/Ankit164/SBIR_AWARDS
# https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_dynamic.html

# Investigate and analyze how the prevalence of topics change over time

# Libraries ----
library(keyATM)
library(parallel)
library(quanteda)
library(tidyverse)
library(future)

# Read data (DFM) ----
DFM <- readRDS("RDS/DFM.RDS")

# Reading data ----
letters <- read.csv("../../Data/FoundingFathers/ffc_total.csv", header = TRUE)

# remove documents without any word counts
DFM0 <- dfm_subset(DFM, ntoken(DFM) > 0)

print("Create keyATM docs")

# transform DFM to a format suitable for the keyATM model
keyATM_docs <- keyATM_read(texts = DFM0)
summary(keyATM_docs)

saveRDS(keyATM_docs, file = "RDS/keyATM_docs.RDS")
#keyATM_docs <- readRDS(file = "RDS/keyATM_docs.RDS")

# Keywords ----
source("keywords.r")
print(keywords)

keywords_stem <- keywords %>% 
    tokens() %>% 
    tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
    quanteda::as.list()

## OR 

# keywords_stem <- keywords %>% 
#     tokens() %>% 
#     tokens_wordstem(language = quanteda_options("language_stemmer")) %>% 
#     quanteda::as.list()

# checking keywords: you want high frequency key words
key_viz <- visualize_keywords(docs     = keyATM_docs, 
                              keywords = keywords_stem)

saveRDS(values_fig(key_viz), file = "./RDS//keyviz_lemma.rds")

# Save the figure
save_fig(x = key_viz, filename = "./RDS//keyviz_lemma.png")

# Preparing time index ----
corp_letters <- corpus(letters)

# For dynamic seeded LDA models, it is necessary to create a period variable
# which starts at 1 and counts up by one for every period. In the code below, 
# one period reflects 10 years, starting with the year 1720 (or choose 1750??)
vars <- quanteda::docvars(corp_letters)

vars_period <- vars %>%
    #filter(start.year >= 1750) %>%
    # change 1720 to 1750?? reduxes size of model
    mutate(vars_period = (vars$start.year - 1720) %/% 10 + 1) %>% 
    as_tibble() %>% arrange(vars_period) 

# checking variety of decades (missing periods are not allowed) 
unique(vars_period$vars_period)

# use to create timestamp for every year!
#vars_period$period01 <- vars_period$start.year %>% 
#    factor %>% labels() %>% as.integer()

# Dynamic keyATM ----
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
    no_keyword_topics = 0,                          # number of topics without keywords
    keywords          = keywords_stem,              # keywords
    model             = "dynamic",                  # select the model
    model_settings    = list(time_index    = vars_period$vars_period, 
                             num_states    = 10),
    options           = list(seed          = 1234, 
                             iterations    = 250,   # takes ca. 90 minutes 
                             store_theta   = TRUE,  # store_theta to get 90% confidence interval in plot
                             store_pi      = TRUE, 
                             verbose       = TRUE, 
                             parallel_init = TRUE)) # enable parallel initialization

toc <- Sys.time()

print(paste0("Dynamic Model fit took ", round(toc - tic, digits = 2)))

# save the model
saveRDS(keyatm_dyn, file = "./RDS/keyatm_model.RDS")

save.keyATM_output(keyatm_dyn, file = "./RDS/keyatm_dynamic_model.RDS")


# Evaluate dynamic model ----------------------------------------------------------

out <- readRDS(file = "./RDS/keyatm_model.RDS")

# topics
top_words(out)
top_docs(out)
plot_modelfit(out)

plot_topicprop(out, show_topic = 1:25)

# plots -> alpha for each time slice per iteration
fig_alpha <- plot_alpha(out)
fig_alpha

# analyze the importance of keywords for the topics
fig_pi <- plot_pi(out)
fig_pi

# time trend over the time slices
fig_timetrend <- plot_timetrend(out, time_index_label = vars_period$start.year, xlab = "Year")
fig_timetrend

# Observe the Time Trends
fig_timetrend <- plot_timetrend(out, time_index_label = vars$start.year[1:keyatm_dyn$N], xlab = "Year")
fig_timetrend

# custom plot
values <- values_fig(fig_timetrend)

# Plot Customization ----
# https://keyatm.github.io/keyATM/articles/pkgdown_files/Plot.html

# sophisticated plot of republican revolution
values %>%
    filter(Topic %in% c("a1","a2","a3")) %>% # extract 3 main topics of interest, including revolutionary politics
    #filter(time_index >= 1750) %>%  
    ggplot(., aes(x = time_index, y = Point, group = Topic)) +
    # ggplot(., aes(x = time_index, y = Proportion, group = Topic)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray75") +
    geom_line(aes(colour = Topic), alpha = 1, size = 0.8, color = "red") +
    geom_point(shape = 5, size = 2) +
    #geom_line(size=0.8, aes(y=rollmean(Point, 5, na.pad=TRUE))) +
    #geom_smooth(span = 0.01, size=1) +
    geom_vline(xintercept = c(1914, 1938, 2001), lty = "dashed") +
    #facet_wrap(~Topic, ncol = 1) + 
    xlab("Year") + ylab(expression(paste("Mean of ", theta))) +
    ggthemes::theme_economist_white(gray_bg = FALSE) +
    scale_x_continuous(breaks = seq(1700, 1835, 5), labels = seq(1700, 1835, 5)) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

values %>%
    filter(Topic %in% c("a1","a2","a3")) %>% # extract 3 main topics of interest, including revolutionary politics
    #filter(time_index >= 1750) %>%  # from 1900
    ggplot(., aes(x = time_index, y = Point, group = Topic)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray75") +
    geom_line(linewidth = 0.8, aes(colour = Topic)) +
    geom_point(shape = 5, size = 0.9) +
    xlab("Year") + ylab(expression(paste("Mean of ", theta))) +
    annotate("text", x = 2005, y = 0.12, label = "Government", colour = "#F8766D") +
    annotate("text", x = 2005, y = 0.28, label = "Peace", colour = "#00BFC4") +
    ggthemes::theme_economist_white(gray_bg = FALSE) +
    theme(legend.position = "none")


# Session information ----
sessioninfo::session_info()

# (SKIP LOWER PART) ############################################################


theta <- out$theta %>% as_tibble()
theta$index <- seq(1, nrow(theta[,1]))
theta$date <- csv$date %>% as.Date()
theta <- theta %>% mutate(year = year(date), month = month(date))
theta$year_month <- theta$year + (theta$month / 12)
theta <- theta %>% arrange(date)

# check calculation of year quarter!
# theta$year_quarter <- theta$year + theta$month %/% 3


agg <- theta %>% group_by(year_month) %>% summarise(mean = mean(`2_war`, na.rm=T),
                                                    max = max(`2_war`, na.rm=T),
                                                    median = median(`2_war`, na.rm=T),
                                                    sd = sd(`2_war`, na.rm=T))

events <- tibble(start = c("1914-01-01", "1938-01-01", "2003-01-01", 
                           "1803-01-01", "1990-01-01", 
                           "1992-01-01", "1839-01-01", "1856-01-01", 
                           "1950-01-01", "1902-01-01", "1999-01-01", "1854-01-01", 
                           "2014-01-01", "1870-01-01", "1963-01-01", "2016-01-01"),
                 end = c("1918-12-31", "1945-12-31", "2011-12-31", "1815-12-31", 
                         "1991-12-31", 
                         "1996-12-31", "1842-12-31", "1860-12-31", "1953-12-31", 
                         "1903-12-31", "1999-12-31", "1856-12-31", "2014-12-31",
                         "1871-12-31", "1967-12-31", "2017-12-31"),
                 event = c("WW-1", "WW-2", "Iraq War", "Napoleonic Wars", 
                           "Gulf War", "Bosnian War", "Opium War-1", 
                           "Opium War-2", "Korean War", "Venezuelan Crisis", 
                           "Kosovo War", "Crimean War", "Russia annexes Crimea", "Franco-Prussian War*", "Aden Emergency", "Battle of Mosul"))
events$height <- 0.2
events$start <- events$start %>% as.Date()
events$end <- events$end %>% as.Date()

p <- theta %>% ggplot(aes(x=date, y=`2_war`))
p + 
    # geom_line(color = "black", alpha = 0.5) + 
    # geom_smooth(span=1/100, method = "loess", se = FALSE) +  # 1/10 and 1/100 works well
    scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    geom_rect(data = events, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"), color="lightblue", lty="dashed") +
    geom_label_repel(data = events, 
                     mapping = aes(x = start, y = height, label = event),
                     inherit.aes = FALSE,
                     hjust = 1,
                     nudge_x = -500) +
    # geom_vline(data = events, aes(xintercept = date), lty = "dashed") +
    geom_line(color="black", alpha=1, aes(y=zoo::rollmean(`2_war`, 100, na.pad=TRUE))) +
    ggthemes::theme_economist_white(gray_bg = FALSE) +
    # ggthemes::theme_wsj(color = "brown") + 
    labs(title = "Dynamic")

p <- agg %>% ggplot(aes(x=year_month, y=sd))
p + #geom_point() +
    geom_smooth(span=1/100, method = "loess") +
    geom_line()


# see out_theta_index

out <- readRDS("./dev.nosync/keyATM_lemma.rds")

topwords <- out %>% topwords_table(n = 10)
                
# read in comma-separated values with readtext
csv_date <- read_csv("./data.nosync/data_dates.csv")

#TODO: add function preprocess_theta

# create theta for plotting
theta <- out$theta %>% preprocess_theta(csv = csv_date, kind = "Dynamic")

df <- theta %>% 
    dplyr::select(-c(kind, short, year_month, index, month, date))

df <- df %>% 
    dplyr::select(-c(starts_with("Other_"))) %>% 
    pivot_longer(-c(year)) %>% 
    group_by(year, name) %>% 
    summarise(n = sum(value)) %>% 
    mutate(total_year = sum(n),
           percentage = n / total_year)

col_vector <-
    c(
        '#e6194b',
        '#3cb44b',
        '#ffe119',
        '#4363d8',
        '#f58231',
        '#9a6324',
        '#911eb4',
        '#46f0f0',
        '#f032e6',
        '#bcf60c',
        '#fabebe',
        '#008080',
        '#e6beff',
        '#fffac8',
        '#800000',
        '#aaffc3',
        '#808000',
        '#ffd8b1',
        '#000075',
        '#808080',
        '#ffffff',
        '#000000'
        
    )

df <- df %>% mutate(name = factor(name, levels = c("1_education",
                                                   "2_war",
                                                   "3_peace",
                                                   "4_treasury",
                                                   "5_transportation",
                                                   "6_energy",
                                                   "7_colony",
                                                   "8_vip",
                                                   "9_ireland",
                                                   "10_housing",
                                                   "11_crime",
                                                   "12_kingdom",
                                                   "13_europe",
                                                   "14_socialsecurity",
                                                   "15_election",
                                                   "16_vice",
                                                   "17_health",
                                                   "18_fishing",
                                                   "19_employment",
                                                   "20_agriculture",
                                                   "no_keyword")))


plot_theta_shares <- df %>% ggplot(aes(x = make_date(year), y = percentage, fill = name)) + 
    geom_area(alpha = 1, colour = "black", size = 0.2) +
    scale_fill_manual(values = col_vector) +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(date_breaks = "25 years", date_labels = "%Y") +
    labs(x = "", y = expr(widehat(theta)), fill = "") + 
    guides(fill = guide_legend(nrow = 3))

