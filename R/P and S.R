# Load the necessary libraries
library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggpubr)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Netflix Analysis"),
  
  # Sidebar with a select input for the variable to plot against release year
  sidebarLayout(
    sidebarPanel(
      selectInput("var", 
                  "Select a variable to plot against release year:",
                  choices = c("type", "rating", "listed_in", "country")
      ),
      selectInput("film_type", 
                  "Select a variable to select film type:",
                  choices = c("Movie", "TV Show")
      ),
      h6("Linear Regression"),
      sliderInput(inputId = "year_range", label = "Select release year range",
                  min = min(netflix_titles$release_year), 
                  max = max(netflix_titles$release_year), 
                  value = c(min(netflix_titles$release_year), 
                            max(netflix_titles$release_year))),
      selectInput(inputId = "category", label = "Select a category", 
                  choices = c("Movie", "TV Show"), selected = "Movie")
    ),
    
    # Show a histogram and a pie chart of film types
    mainPanel(
      h2("Netflix Data Analysis"),
      plotOutput("histogram"),
      titlePanel("Type of Netflix Films Comparison"),
      plotOutput("piechart"),
      titlePanel("Top 10 Countries with the Most Movie Productions on Netflix"),
      DT::dataTableOutput("table"),
      plotOutput('film_comparision'),
      #Linear Regression
      titlePanel("Linear Regression"),
      plotOutput(outputId = "scatterplot"),
      titlePanel("Linear regression model Results"),
      verbatimTextOutput(outputId = "model_summary")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load the Netflix dataset
  netflix_titles <- read_csv("netflix_data.csv", show_col_types = 'FALSE')
  #Cleaning the loaded data Function
  clean_netflix_data<- function(netflix_data) {
    # Drop any rows with missing values
    netflix_data <- netflix_data[complete.cases(netflix_data),]
    
    # Convert release_year to numeric
    netflix_data$release_year <- as.numeric(netflix_data$release_year)
    
    # Convert duration to numeric (in minutes)
    netflix_data$duration <- as.numeric(gsub(" min", "", netflix_data$duration))
    
    # Remove leading/trailing whitespace from strings
    netflix_data$title <- trimws(netflix_data$title)
    netflix_data$director <- trimws(netflix_data$director)
    netflix_data$cast <- trimws(netflix_data$cast)
    netflix_data$country <- trimws(netflix_data$country)
    netflix_data$listed_in <- trimws(netflix_data$listed_in)
    netflix_data$description <- trimws(netflix_data$description)
    
    # Return cleaned dataset
    return(netflix_data)
  }
  #Data Cleaned
  netflix_titles <- clean_netflix_data(netflix_data)
  # Separate "listed_in" column into three "category" column, and take only the first.
  netflix_titles <- netflix_titles %>% separate(listed_in, c("Category1", "Category2", "Category3"), sep = ",") 
  netflix_titles <- netflix_titles %>% select(c(-"Category2", -"Category3"))
  
  # Separate "date_added" column into "date_added" and "year_added" column.
  netflix_titles <- netflix_titles %>% separate(date_added, c("date_added", "year_added"), sep = ",")
  
  # Create a new data frame of film types
  Film_Types <- netflix_titles %>% 
    group_by(type) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  # Define a histogram of film release years
  output$histogram <- renderPlot({
    ggplot(netflix_titles, mapping = aes(x=release_year, fill = !!sym(input$var))) +
      geom_histogram(color = "black", binwidth=2)+
      labs(title="Netflix Films Released by Year", x="Release Year", y="Total Film")+
      scale_fill_manual(values = c("Movie" = "#dc3545","TV Show" = "#0d6efd"))
  })
  
  # Define a pie chart of film types
  output$piechart <- renderPlot({
    ggplot(Film_Types, aes(x = "", y = perc, fill = type)) +
      geom_col() +
      geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      labs(title="") +
      theme_void() +
      scale_fill_manual(values = c("Movie" = "#dc3545","TV Show" = "#0d6efd"))
  })
  #Filter Table Data
  # Count the number of productions for each country
  country_counts <- netflix_titles %>%
    separate_rows(country, sep = ",") %>%
    group_by(country) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    top_n(10, count)
  
  output$table <- renderDataTable({
    country_counts
  })
  
  # Film Trends plot
  output$film_trend_plot <- renderPlot({
    df <- netflix_titles
    if (input$film_type != "All") {
      df <- filter(df, type == input$film_type)
    }
    ggplot(df, mapping = aes(x=release_year, fill = type)) +
      geom_histogram(color = "black", binwidth=2)+
      labs(title="Netflix Films Released by Year", x="Release Year", y="Total Film")+
      scale_fill_manual(values = c("Movie" = "#F6AE2D","TV Show" = "#00b22d"))
  })
  
  # Run a linear regression on the filtered data
  # Filter the data based on the user's input
  observe({
    # Get the year range from the input
    year_range <- input$year_range
    
    data_subset <- netflix_titles %>%
      filter(release_year >= input$year_range[1] & 
               release_year <= input$year_range[2] & 
               type == input$category)
    
    # Create a scatterplot of release year vs. duration
    output$scatterplot <- renderPlot({
      ggplot(data_subset, mapping = aes(x = release_year, y = duration)) +
        geom_point() +
        labs(title = "Netflix Film Duration by Release Year", x = "Release Year", y = "Duration (min)") +
        theme_bw()
    })
    
    # Run a linear regression model on the data and display the results
    output$model_summary <- renderPrint({
      lm_model <- lm(duration ~ release_year, data = data_subset)
      summary(lm_model)
    })
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

