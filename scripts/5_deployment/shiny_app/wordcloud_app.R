library(shiny)
library(dplyr)
library(gganimate)
library(ggwordcloud)

# https://stackoverflow.com/questions/71810703/how-to-connect-user-input-with-gganimate-graph-in-r-shiny
# https://stackoverflow.com/questions/35421923/how-to-create-and-display-an-animated-gif-in-shiny/53105252
# https://stackoverflow.com/questions/68150898/animated-bar-plot-not-working-displaying-on-r-shiny-web-app


ui <- fluidPage(
    titlePanel("Animated Word Cloud"),
    sidebarLayout(
        sidebarPanel(
            selectInput("topic", "Select Topic:", choices = c("other13", "other1")),
            
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
                       hjust = -4, vjust = -0.2, alpha = 0.2, col = "grey", size = 14) +
            
            theme_void() +
            theme(plot.background   = element_rect(fill = "black"),  # Set the background color to black
                   panel.background = element_rect(fill = "black")) +  # Set the panel background color to black
                
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

# DOWNLOADING WORDCLOUD2 OUTPUT AS PNG/JPG ON SHINY-R
# 
# library(shiny)
# library(htmlwidgets)
# library(webshot)
# library(wordcloud2)
# #webshot::install_phantomjs()
# 
# 
# ui <- shinyui(fluidpage(mainpanel(
#     wordcloud2output("wordcl"),
#     downloadbutton(outputid = "savecloud")    
# )))
# 
# server <- shinyserver(function(input, output, session) {
#     wordcl <- reactive ({
#         wordcloud2(demofreq, color = "random-light", backgroundcolor = "grey")
#     })
#     output$wordcl <- renderwordcloud2({
#         wordcl()
#     })   
#     output$savecloud <- downloadhandler(
#         fileterm = paste("wordcloud", '.png', sep=''),
#         content = function(file) {
#             owd <- setwd(tempdir())
#             on.exit(setwd(owd))
#             savewidget(wordcl(), "temp.html", selfcontained = false)
#             webshot("temp.html", delay =15, file = file, cliprect = "viewport")
#         }) 
# })
# 
# shinyapp(ui = ui, server = server)

