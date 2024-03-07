#' Goal:
#' This script creates an animated wordcloud of the most frequent terms
#' in the letters of the Founders Online dataset, accessible via a shiny app.
#'
#' Purpose:
#' To visualize the most frequent terms per topic in the letters of the
#' Founders Online dataset through time.

#' Input dataset
#' @param mydata   A preprocessed dataset from animated_wc.R

#' Output
#' @return          An animated wordcloud accessible via a shiny app
#' =============================================================================

# Set seed for reproducibility
set.seed(123)

# Import libraries ----
library(shiny)
library(dplyr)
library(gganimate)
library(ggwordcloud)

mydata <- read.csv("data/processed/founders/yr_topic_term_freq.csv") %>%
    rename(term = terms,
           n    = word_count,
           prop = relative_occurrence) %>%
    select(-X) %>%
    # we are not interested in missing values or zero occurrences
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

# Merge fixed positions with mydata
mydata <- mydata %>% left_join(term_positions, by = "term") %>%
    arrange(year,topic,term)

unique(mydata$topic)

topicnames <-
    c("01_Liberal politics",
      "02_Republican politics",
      "03_Maritime Trade and Slavery",
      "04_Government Proceedings and Agreements",
      "05_Human Interaction and Decision-Making",
      "06_Educational Institutions and Knowledge Advancement",
      "07_Literary Works and Print Publications",
      "08_Financial Transactions and Government Budgeting",
      "09_Diplomatic Correspondence and Government Relations",
      "10_Military Operations and Strategic Defense",
      "11_Business Endeavors and Personal Relationships",
      "12_Land Ownership and Property Rights",
      "13_Political Governance and Legislative Proceedings",
      "14_Financial Transactions and Monetary Matters",
      "15_Professional Appointments and Recommendations",
      "16_Military Service and Officer Hierarchy",
      "17_Military Logistics and Operational Supplies",
      "18_Social Etiquette and Personal Relations",
      "19_Government Relations and Negotiation Strategies",
      "20_Agricultural Practices and Crop Management",
      "21_International Relations and Diplomacy",
      "22_Transportation and Infrastructure Development",
      "23_Maritime Operations and Naval Affairs",
      "24_Family Relationships and Domestic Life",
      "25_Mail and Package Handling",
      "26_Financial Transactions and Economic Indicators",
      "27_Emotions and Philosophical Reflections on Life",
      "28_French words",
      "29_Health, Travel, and Seasonal Experiences",
      "30_Courtesy",
      "31_Legal Proceedings and Judicial System"
    )

ui <- fluidPage(
    titlePanel("Animated Word Cloud"),
    sidebarLayout(
        sidebarPanel(
            selectInput("topic", "Select Topic:", choices =topicnames),

            sliderInput("year_range", "Select Year Range:",
                        min   = min(mydata$year)  , max = max(mydata$year),
                        value = c(min(mydata$year), max(mydata$year)),
                        step = 1,sep = ""),

            # "freq" allows the user to set a minimum frequency value to display
            # only words with frequencies greater than or equal to the chosen value
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 1,  max = 50, value = 15),

            # filters words with a frequency less than or equal to the specified maximum frequency.
            sliderInput("max",
                        "Maximum Frequency:",
                        min = 1,  max = 1000,  value = 500),
                        #min = 1,  max = 50,  value = 25),
            sliderInput("animation_speed", "Animation Speed:",
                        min = 1, max = 10, value = 2,
                        step = 1)  # Set step size to 1# Adjust min, max, and value as needed
        ),
        mainPanel(
            style = "position: relative;",  # Add a relative position to the mainPanel
            imageOutput("wordcloud_plot", width = "120%")  # Adjust the width as needed
        )
    )
)

server <- function(input, output) {
    filtered_data <- reactive({
        mydata %>%
            filter(topic  == input$topic, #  ensures that only rows with the selected "topic" are included.
                   year   >= input$year_range[1], # filter the data within the specified year range defined by the user through the slider input "year_range."
                   year   <= input$year_range[2],
                   n      >= input$freq,
                   n      <= input$max) # filters words with a frequency less than or equal to the user's selected maximum frequency.
    })

    # Function to validate animation speed as a factor of 100
    validate_animation_speed <- function(speed) {
        valid_speeds <- c(1, 2, 4, 5, 10, 20, 25, 50, 100)  # Factors of 100
        if (speed %in% valid_speeds) {
            return(speed)
        } else {
            return(2)  # Default to 2 if invalid speed is selected
        }
    }

    # add the option to start or pause the wordcloud animation using gganimate's play_pause() function.
    # https://stackoverflow.com/questions/71810703/how-to-connect-user-input-with-gganimate-graph-in-r-shiny


    output$wordcloud_plot <- renderImage(
        {
            animation_speed <- validate_animation_speed(input$animation_speed)

            gg <- filtered_data() %>%
            ggplot(aes(x = x_avg, y = y_avg, label = term, size = n, color = as.factor(n))) +
            geom_text_wordcloud(rm_outside = TRUE, max_steps = 1, grid_size = 1, eccentricity = .9) +
            scale_size_area(max_size = 20) +

            geom_text(aes(x = min(x_avg), y = min(y_avg), label = as.factor(year)),
                       hjust = -4, vjust = -0.2, alpha = 0.2, col = "black", size = 14) +

            theme_void() +
            theme(plot.background   = element_rect(fill = "gray95"),
                   panel.background = element_rect(fill = "gray95")) +

            transition_states(as.factor(year), state_length = 0) +
            ease_aes('cubic-in-out') +
            enter_fade() +
            exit_fade() +
            scale_color_viridis_d() #+
            view_follow(fixed_y = FALSE, fixed_x = FALSE)

            # nframes = 100
         anim <- animate(gg, fps = input$animation_speed, end_pause = 10) # Use input value for speed

         #anim_save("outfile.gif", animate(gg)) # New
         anim_save("outfile.gif", anim)

         # Return a list containing the fileterm
         list(src = "outfile.gif",
              contentType = "image/gif",
              fps = animation_speed * 10,
              #fps = input$animation_speed,  # Use input value for speed
              detail = 5,
              width = 750, height = 632)
        },

        deleteFile = TRUE)
}

shinyApp(ui, server)

