#' Goal:
#' This script creates an animated wordcloud of the most frequent terms
#' in the letters of the Founders Online dataset. The size of each term
#' is proportional to the number of occurrences of the term in the dataset.
#' The position of each term is fixed and is based on the average position
#' of the term across all years and topics.

#' Purpose:
#' To visualize the most frequent terms per topic in the letters of the
#' Founders Online dataset through time.

#' Input dataset
#' @param letters   A preprocessed dataset from "semi-supervised_topic_modeling.R"

#' Output
#' @return          An animated wordcloud
#' =============================================================================

# Set seed for reproducibility
set.seed(123)

# Import libraries ----
library(tidyverse)
library(ggwordcloud)
library(gganimate)

# Read data ----
# data in long format with the following columns:
# year, topic, topic-term, word_count (sum of term occurrences per year,topic,term)
mydata <- read.csv("data/processed/founders/yr_topic_term_freq.csv") %>%
  rename(term = terms,
         n    = word_count,
         prop = relative_occurrence) %>%
  select(-X) %>%
  # we are not interested in missing values or zero occurrences
  filter(!is.na(prop) & n >= 1) %>%

  # filter for a specific topic
  filter(topic == "01_Liberal politics")

# Calculate the average n value across all years
average_n <- mean(mydata$n)

# Calculate the distance of each term from the center based on n
mydata <- mydata %>%
  mutate(
    distance = sqrt((n / average_n) * 1000),  # Adjust the multiplier as needed
    angle    = cumsum(distance) + runif(length(term)) * 360 # add randomness
  )

# Calculate x and y coordinates based on polar coordinates
mydata <- mydata %>%
  mutate(
    x = distance * cos(angle),
    y = distance * sin(angle)
  )

# create fixed positions for each term
term_positions <- mydata %>%
  group_by(term) %>%
    summarize(x_avg = mean(x),
              y_avg = mean(y)) %>%
  ungroup()

# Merge fixed positions with mydata
mydata <- mydata %>% left_join(term_positions, by = "term") %>%
  arrange(year,topic,term)

gg <- mydata %>%
  ggplot(aes(x = x_avg, y = y_avg, label = term, size = n,color = as.factor(n))) +
  geom_text(aes(x = min(x_avg), y = min(y_avg), label = as.factor(year)),
                hjust = -2, vjust = -0.6, alpha = 0.2,
                col = "black", size = 17) +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_size = 1, eccentricity = .9) +
  scale_size_area(max_size = 40) +
  theme_void() +
  transition_states(as.factor(year), state_length = 1,transition_length = 4) +
  #ease_aes('linear') +
  enter_fade() +
  exit_fade() +
  scale_color_viridis_d() +
  view_follow(fixed_y = F, fixed_x = F)

# Show an animated wordcloud with each term positioned at its average position
# while the size of each term varies based on the n variable
animate(gg, width = 1000, height = 432, fps = 2,detail = 5)

glimpse(mydata)
