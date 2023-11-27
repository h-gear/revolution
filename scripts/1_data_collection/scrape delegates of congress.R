# 0. GOAL ----
# Scrape and save all raw data on Letters of Delegates to Congress (1774-1789)
# from the website https://memory.loc.gov/ammem/amlaw/lwdglink.html
# for later temporal network and shico analysis

# 1. LOAD IN LIBRARIES ----
library(rvest)     # web scraping
library(xml2)      # html parsing
library(tidyverse) # data wrangling

# 2. SCRAPE TARGET WEBSITE ----
base_url <- "http://rs5.loc.gov"

all_volumes <- read_html("http://rs5.loc.gov/ammem/amlaw/lwdglink.html")
table_nodes <- all_volumes %>% html_nodes("table")

# Extract the href links from the table
href_links3 <- table_nodes[3] %>% html_nodes("a") %>% html_attr("href") # all volumes except 25
href_links4 <- table_nodes[4] %>% html_nodes("a") %>% html_attr("href") # volume 25

# List of URLs
urls3 <- paste0(base_url, href_links3)
urls4 <- paste0(base_url, href_links4)
urls <- c(urls3, urls4)
urls <-  urls[-c(2)] # this is not a volume

# urls is a vector containing the URLs you want to scrape
head(urls) #25 volumes

# if one want to scrape only one volume, use:
# urls <- "http://rs5.loc.gov/cgi-bin/query/r?ammem/hlaw:@field(DOCID+@lit(dg025T000)):"

# Initialize a list to store all the scraped text from volumes 1-25
scraped_text <- list()

# The outer loop iterates over each URL/ volume
for (url in urls) {
    # Read the HTML page for the volume
    page <- read_html(url)

    # Initialize an empty list to store the links to the texts within the URL/volume
    url_links <- list()

    # Get all the the links from the specific URL
    links <- page %>%
        html_nodes("a") %>%
        html_attr("href")

    # url links to each text document within the volume url
    links <- paste0(base_url, links)

    # Within each URL, another loop iterates over each link to a particular text
    # For each link, the code reads the HTML page, extracts the desired text,
    # and stores it in the url_links list using the link URL as the key

    for (link in links) {

        # Extract the URL of the link
        link_url <- link
        #link_url <- link %>% html_attr("href")

        # Read the HTML page of the link
        text <- read_html(link_url) %>%
                html_nodes("p") %>%
                # Extract the desired text from the link page
                html_text()

        Sys.sleep(5)  # important to not pull too much requests in few time

        # Store the text in the list of links within the URL
        url_links[[link_url]] <- text

        # store the id from the url
        id <- sub(".*lit[(]", "", link_url)
        id <- gsub('[^[:alnum:] ]','',id)

        url_links[[id]] <- id
    }

    # Store the list of links in the main scraped text list
    # T the url_links list is stored in the scraped_text list using the URL
    # as the key. By using nested loops and storing the scraped text in a list
    # within a list, multiple URLs are scraped, each containing a list of links
    scraped_text[[url]] <- url_links
}

# Function for very basic text cleaning and tidy storage in a dataframe
clean_text <- function(text) {

        text <- str_remove_all(text,"A Century of Lawmaking for a New Nation: U.S. Congressional Documents and Debates, 1774-1875")
        text <- str_remove_all(text,"Letters of Delegates to Congress:")
        text <- str_remove_all(text,"Link to date-related documents.")
        text <- str_remove_all(text,"NEXT SECTION .. NAVIGATOR")
        text <- str_remove_all(text,"PREVIOUS SECTION ..")

        # Remove rows starting with "Page"
        text <- text[!grepl("^Page", text)]

        # Filter out empty rows
        text <- text[nchar(text) > 0]

        # remove duplicate rows
        text <- unique(text)

        # Concatenate strings with space separator
        text <- paste(text, collapse = " ")

        # remove whitespace from start and end of string
        text <- str_trim(text)

    return(text)
    }

# apply cleaning function on scraped data
result1 <- map(scraped_text, ~ map(.x, clean_text)) %>%
           unlist() %>%
           as.data.frame() %>%
           `rownames<-`( NULL) %>%
           rename(text = ".")

# we only want data on id and volumes
result2 <- result1 %>% filter(str_detect(text, "Volume") | str_detect(text, "dg0"))

# 3. MATCH ID TO LETTER/ TEXT ----
# Create empty columns for ID and column B (where text will be put in)
result2$ID <- NA
result2$B  <- NA

# Assign even rows to column ID and odd rows to column B
result2$ID[seq_along(result2$text) %% 2 != 0] <- result2$text[seq_along(result2$text) %% 2 == 0]
result2$B[seq_along(result2$text)  %% 2 != 0] <- result2$text[seq_along(result2$text) %% 2 != 0]

result2 <- result2 %>% filter(!is.na(result2$ID)) %>% select(-text) %>% rename(text = B)

# 4. SAVING RAW DATA ----
# when one has scraped all volumes at once, it might be handy to split
# the data into 25 volumes separately for  file storage purposes

# Number of parts to split the data.frame into
volumes <- 25

# Iterate over the volume numbers and split the data.frame accordingly
for (volume in 25:volumes) {

    part      <- result2 %>% filter(str_detect(text, paste0("Volume ", volume, " ")) | str_detect(text, paste0("Volume: ", volume, " ")))
    file_name <- paste0("./data/raw/delegates/volumes/Letters_of_Congress_Volume_", volume, ".rds")
    saveRDS(part, file_name)
}

# COMBINE ALL VOLUME DATA
# Specify the directory path where the RDS files are stored
directory_path <- "./data/raw/delegates/volumes/"

# Get the list of file names in the directory with RDS extension
file_names <- list.files(directory_path, pattern = ".rds", full.names = TRUE)

# Initialize an empty data.frame to store the combined data
combined_df <- data.frame()

# Loop through the file names and read each RDS file, putting all volumes together
for (file_name in file_names) {
    data <- readRDS(file_name)
    combined_df <- bind_rows(combined_df, data)
}

# Saving the raw data as RDS file for further processing
saveRDS(combined_df, "data/raw/delegates/delegates_raw_data.rds")
