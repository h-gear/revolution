# https://stackoverflow.com/questions/61132650/is-there-a-way-to-animate-a-word-cloud-in-r
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# https://anderfernandez.com/en/blog/how-to-create-animations-in-r-with-gganimate/#Adjusting-our-animations-in-R
# https://github.com/jumpingrivers/blog/blob/main/blogs/clickable-wordcloud/app.R

library(tidyverse)       
library(ggwordcloud) 
library(gganimate)

# read data

# year, topic, topic-term (i.e., term), n (sum of term occurrences
# per year,topic, term)
mydata <- read.csv("yr_topic_term_freq.csv") %>% 
  rename(year = Year,
         term = terms,
         n    = word_count,
         prop = relative_occurrence) %>% 
  select(-X) %>% 
  # were are not interested in missing values or zero occurrences
  filter(!is.na(prop) & n >= 1)

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
  
glimpse(mydata)

# Merge fixed positions with mydata
mydata <- mydata %>% left_join(term_positions, by = "term") %>% 
  arrange(year,topic,term)

gg <- mydata %>%
  ggplot(aes(x = x_avg, y = y_avg, label = term, size = n,color = as.factor(n))) +
  geom_text(aes(x = min(x_avg), y = min(y_avg), label = as.factor(year)),
                hjust = -2, vjust = -0.6, alpha = 0.2, 
                col = "black", size = 17) +
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_size = 1, eccentricity = .9) +
  scale_size_area(max_size = 20) +
  theme_void() +
  transition_states(as.factor(year), state_length = 1,transition_length = 4) +
  #ease_aes('linear') +
  enter_fade() +
  exit_fade() + 
  scale_color_viridis_d() + 
  view_follow(fixed_y = F, fixed_x = F) #+
  #facet_wrap(~ sex)

# an animated word cloud with each term positioned at its average position while
# the size of each term varies based on the n variable
animate(gg, width = 1000, height = 432, fps = 2,detail = 5)

