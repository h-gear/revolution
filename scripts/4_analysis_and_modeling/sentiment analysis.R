# Sentiment analysis by topic per year

# Import libraries ----
library(sentopics)
library(tidytext)
library(ggridges)
library(tidyverse)
library(reshape2)
library(wordcloud)
library(zoo)
library(lubridate)

# Read data ----

# data with important hisotrical events
events <- read.csv("data/external/historical events.csv",sep = ";") %>%
    mutate(Start.Date = mdy(Start.Date),
           End.Date = mdy(End.Date))
           #End.Date = ifelse(is.na(End.Date),Start.Date,End.Date))
glimpse(events)

# data with topic analytical results
texts <- readRDS("data/processed/founders/texts.rds")

# ensure a dataframe with ids and each word is in a separate row
df_texts <- texts %>% select(id,Year,topic,text_cleaned) %>%
    separate_rows(text_cleaned) %>%
    rename(word = text_cleaned)

# check
#https://programminghistorian.org/en/lessons/sentiment-analysis-syuzhet
# calculate sentiment per topic per year

# NRC ----
sentiment_per_year <- df_texts %>%
    inner_join(get_sentiments("nrc"), by = "word") %>%
    group_by(topic, Year) %>%
    summarize(sentiment = mean(value),
              words = n()) %>%
    ungroup()

# AFINN ----
sentiment_per_year <- df_texts %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(topic, Year) %>%
        summarize(sentiment = mean(value),
                  words = n()) %>%
    ungroup()

glimpse(sentiment_per_year)
# plot sentiment development for individual topic
sentiment_per_year %>% filter(topic == "revolutionary politics") %>%
    ggplot(aes(x = make_date(Year,1,1), y = sentiment)) +
    geom_line() +
    # Add LOESS line in red with confidence interval
    geom_smooth(method = "loess", se = TRUE, color = "red", span =0.5) +
    # Rolling mean with window size of 5 years
    geom_line(aes(y = zoo::rollmean(sentiment, 5, fill = NA)), color = "blue", linetype = "solid") +
    theme_ridges() +
    theme_minimal() +
    geom_rect(data = events,
              aes(xmin = Start.Date, xmax = End.Date, ymin = -Inf, ymax = Inf),
              fill = "lightgray", alpha = 0.5) +
    geom_vline(data = events, aes(xintercept = Start.date), linetype = "solid", color = "black") +
    geom_vline(data = events, aes(xintercept = End.date), linetype = "solid", color = "black") +
    guides(color = guide_legend(title = "Legend Title"))

# Generate a dynamic color palette based on the number of unique topics
num_topics <- length(unique(sentiment_per_year$topic))
color_palette <- viridis::viridis_pal()(num_topics)

# Plot ridge graph with sentiment development for all topics per year
ggplot(sentiment_per_year, aes(x = Year, height = sentiment, y = as.factor(topic),fill = as.factor(topic))) +
    geom_ridgeline(min_height = -100, scale = 1/2,color = "light gray") +
    scale_fill_manual(values = color_palette) +
    guides(fill = "none") +
    ylab("Topical sentiment") +
    xlab("Date")


ggplot(sentiment_per_year %>% filter(topic =="revolutionary politics"), aes(Year, sentiment, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~as.factor(topic), ncol = 2, scales = "free_x")

# We can see in the Figure how the plot changes toward more positive or negative
# sentiment over the trajectory of the eighteenth century.

# worcloud plots
df_texts %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))

df_thijs <- df_texts %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 200)

