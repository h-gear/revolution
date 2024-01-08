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

# data with topic analytical results
texts <- readRDS("data/processed/founders/texts.rds") %>%
    mutate(sending_date = as_date(sending_date)) %>%
    mutate(year_month = floor_date(sending_date, unit = "month"))

# information on historical time periods
rects <- data.frame(xstart = c(as.Date('1725-01-01'),as.Date('1775-04-19'),as.Date('1783-09-04'),as.Date('1789-04-30'),as.Date('1797-03-04'),as.Date('1801-03-04'),as.Date('1809-03-04'),as.Date('1817-03-04')),
                    xend   = c(as.Date('1775-04-18'),as.Date('1789-09-03'),as.Date('1789-04-29'),as.Date('1797-03-03'),as.Date('1801-03-03'),as.Date('1809-03-03'),as.Date('1817-03-03'),as.Date('1825-01-01')),
                    col    = c("Colonial"           ,"Revolutionary War"  ,"Confederation Period","Washington Presidency","Adams Presidency","Jefferson Presidency","Madison Presidency","post-Madison Presidency")
)

# ensure a dataframe with ids and each word is in a separate row
# use year_month instead of Year when wanting to plot per month
df_texts <- texts %>% select(id,Year,topic,text_cleaned) %>%
    separate_rows(text_cleaned) %>%
    rename(word = text_cleaned)

# check
#https://programminghistorian.org/en/lessons/sentiment-analysis-syuzhet

# calculate sentiment per topic per year
# AFINN ----
sentiment_per_year <- df_texts %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(topic, Year) %>%
        summarize(sentiment = mean(value),
                  words = n()) %>%
    ungroup()
    # calculate the mean of the monthly means to get a single yearly mean
    #group_by(topic,year = year(year_month)) %>%
    #    mutate(mean_value = mean(sentiment)) %>%
    #ungroup()

# Sentiment development for individual topic ----
ggplot(sentiment_per_year %>%
    filter(topic =="revolutionary politics"), aes(Year, sentiment, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~as.factor(topic), ncol = 2, scales = "free_x")

# We can see in the Figure how the plot changes toward more positive or negative
# sentiment over the trajectory of the eighteenth century.

sentiment_per_year %>%
    filter(topic == "revolutionary politics") %>%
    ggplot(aes(x = make_date(Year, 1, 1), y = sentiment)) +
    geom_line() +
    geom_smooth(method = "loess", se = F, color = "red", span = 0.5) +
    geom_line(aes(y = zoo::rollmean(sentiment, 5, fill = NA)), color = "blue", linetype = "solid") +
    theme_ridges() +
    theme_minimal() +
    geom_vline(data = events, aes(xintercept = Start.Date),
               linetype = "solid", color = "light gray") +
    guides(color = guide_legend(title = "Legend Title"))

# Sentiment development for all topics ----

# Generate a dynamic color palette based on the number of unique topics
num_topics <- length(unique(sentiment_per_year$topic))
color_palette <- viridis::viridis_pal()(num_topics)

# Plot ridge graph with sentiment development for all topics per year
# We can see in the Figure how the plot changes toward more positive or negative
# sentiment over the trajectory of the eighteenth century.
ggplot(sentiment_per_year, aes(x = Year, height = sentiment, y = as.factor(topic),fill = as.factor(topic))) +
    geom_ridgeline(min_height = -100, scale = 1/2,color = "light gray") +
    scale_fill_manual(values = color_palette) +
    guides(fill = "none") +
    ylab("Topical sentiment") +
    xlab("Date")

# Wordcloud plots positive versus negative ----
df_texts %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))

wc_pos_neg <- df_texts %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 200)

