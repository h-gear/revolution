# 0. GOAL ----
# Descriptive analysis of founders online dataset: details about the corpus

# 1. LIBRARIES ----
library(tidyverse)

# 2. READ DATA ----
ffc5 <- readRDS("data/ffc5.rds")
ffc6 <- readRDS("data/ffc_total.rds")

# 3. DESCRIPTIVE ANALYSIS ----

# Create table with background information on the periods
Period_table <- data.frame(n = c(1:8),
                           period = c("Colonial", "Revolutionary War", "Confederation Period",
                                      "Washington Presidency","Adams Presidency","Jefferson Presidency",
                                      "Madison Presidency","post-Madison Presidency"),
                           general = c("1706-1775", "1775-1783", "1783-1789","1789-1797","1797-1801","1801-1809","1809-1817","1817+"),
                           start   = c("1706-01-01", "1775-04-19","1783-09-04","1789-04-30","1797-03-04","1801-03-04","1809-03-04","1817-03-04"),
                           end     = c("1775-04-18", "1783-09-03","1789-04-29","1797-03-03","1801-03-03","1809-03-03","1817-03-03","1837-01-01")
)

# the number of unique authors in the dataset
n_distinct(ffc5$authors)

# the number of unique authors in the dataset
n_distinct(ffc5$recipients)

# The table below summarizes the number of documents and number of words per 
# corpus sub-period
data.table::setDT(ffc5)
by_period <- ffc5[, list(doc_n = .N, 
                         word_count = sum(doc_length)),
                  by = list(period)]

by_period %>% 
    left_join(Period_table %>% select(n, period)) %>% 
    arrange(n) %>% 
    janitor::adorn_totals(c('row')) %>%
    mutate_if(is.numeric, formatC, big.mark = ",") %>%
    select(-n) %>%
    knitr::kable()

# Frequency of Founders' writings by month
founders <- c('Washington, George', 'Adams, John', 'Jefferson, Thomas', 
              'Madison, James', 'Hamilton, Alexander', 'Franklin, Benjamin','Jay, John')

data.table::setDT(ffc6)

# The plot below summarizes amount of written letters by month for each founding
# father over an eighty year time period
ffc6[, list(doc_n = .N),by = list(Month_Yr,authors)] %>%
    filter(authors %in% founders) %>%
    filter(Month_Yr < as.Date('1835-01-01'),
           Month_Yr > as.Date('1745-01-01')) %>%
    
    ggplot(aes(x = Month_Yr, 
               y = doc_n, 
               color = authors,
               group = authors)) +
    geom_line(size = .5) +
    geom_vline(xintercept = as.Date(Period_table$start),
               linetype = 2, 
               color = 'black', 
               size = .25) +
    theme_minimal() +
    ggthemes::scale_color_stata() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("") + xlab("") +
    scale_x_date(labels = scales::date_format("%Y"),
                 breaks = scales::date_breaks('1 year')) +
    facet_wrap(~authors, 
               scales = "free_y", 
               ncol = 2) + 
    labs(title = "Founders' writings historically by month",
         subtitle = 'From 1775 1783 (Revolutionary War)')

# Frequency of >>received<< writings by Founders by month
ffc6[, list(doc_n = .N),by = list(Month_Yr,recipients)] %>%
    filter(recipients %in% founders) %>%
    filter(Month_Yr < as.Date('1835-01-01'),
           Month_Yr > as.Date('1745-01-01')) %>%
    
    ggplot(aes(x = Month_Yr, 
               y = doc_n, 
               color = recipients,
               group = recipients)) +
    geom_line(size = .5) +
    geom_vline(xintercept = as.Date(Period_table$start),
               linetype = 2, 
               color = 'black', 
               size = .25) +
    theme_minimal() +
    ggthemes::scale_color_stata() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("") + xlab("") +
    scale_x_date(labels = scales::date_format("%Y"),
                 breaks = scales::date_breaks('5 year')) +
    facet_wrap(~recipients, 
               scales = "free_y", 
               ncol = 2) + 
    labs(title = "Founders' receivings historically by month",
         subtitle = 'From 1745 to 1835')

ffc6 %>% filter(authors %in% founders & recipients %in% founders) %>% 
    group_by(authors) %>%
    summarize(messages = n()) %>%
    ggplot(aes(authors, messages)) +
    geom_col() +
    coord_flip()

ffc6 %>% filter(authors %in% founders & recipients %in% founders) %>% 
    group_by(recipients) %>%
    summarize(messages = n()) %>%
    ggplot(aes(recipients, messages)) +
    geom_col() +
    coord_flip()

# 4. SAVE PLOTS AND RESULTS ----
#TODO