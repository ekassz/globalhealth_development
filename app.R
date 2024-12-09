library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(janitor)
library(DT)

hispanic_countries <- c("Mexico" , "Colombia", "Spain", "Argentina" ,"Peru", "Venezuela", "Chile", "Guatemala", "Ecuador",
                     "Bolivia", "Dominican Republic", "Cuba", "Honduras", "Paraguay", "Nicaragua", "El Salvador",
                     "Costa Rica", "Panama", "Uruguay", "Puerto Rico", "Equatorial Guinea")

globalhealth_dev <- read.csv("data/global_health.csv") %>%
  clean_names()
#View(globalhealth_dev)

hispanic_health_dev <- globalhealth_dev %>%
  filter(country %in% hispanic_countries)%>% 
  filter(!is.na(life_expectancy), !is.na(year))

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Life Expectancy by Gender in Hispanic Countries"),
  sidebarLayout(
    sidebarPanel(
      helpText(tags$strong("Use the filters below to explore life expectancy data through Hispanic countries.")),
      helpText("You can select multiple countries or leave it blank to apply other filters."),
      selectInput(
        "countries",
        "Select Countries:",
        choices = NULL,  
        multiple = TRUE
      ),
      helpText(tags$strong("Check this box to include all countries in the analysis.")),
      checkboxInput("show_all", "Show All Countries", value = FALSE),
      helpText(tags$strong("Check this box to view a bar plot by country.")),
      checkboxInput("show_bar_plot", "Show Bar Plot by Country", value = FALSE),
      checkboxGroupInput("gender", "Select Gender:",
                         choices = c("Female", "Male"),
                         selected = c("Female", "Male")),
      sliderInput("population", "Select Population Range:",
                  min = min(hispanic_health_dev$total_population, na.rm = TRUE), 
                  max = max(hispanic_health_dev$total_population, na.rm = TRUE), 
                  value = c(min(hispanic_health_dev$total_population, na.rm = TRUE), 
                            max(hispanic_health_dev$total_population, na.rm = TRUE))),
      sliderInput("years", "Select Years:", 
                  min = 2012, max = 2021, value = c(2012,2021))
                  
    ),
    mainPanel(
      plotOutput("lifeExpectancyPlot", height = "800px"),
      conditionalPanel(
        condition = "input.show_bar_plot == true",
        plotOutput("barPlot", height = "800px")
      ),
      h3("Country Data Overview"),
      DT::DTOutput("filteredData")
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "countries", choices = unique(hispanic_health_dev$country))
  })
  
  filtered_data <- reactive({
    data <- hispanic_health_dev %>%
      filter(year >= input$years[1] & year <= input$years[2],
             total_population >= input$population[1] & total_population <= input$population[2]
             )
    
    if (!input$show_all && length(input$countries) > 0) {
      data <- data %>% filter(country %in% input$countries)
    }
    
    # Correct Data Transformation
    data <- data %>%
      pivot_longer(
        cols = c(life_expectancy_female, life_expectancy_male), 
        names_to = "Gender", 
        values_to = "life_expectancy_merged", 
        names_repair = "unique"
      ) %>%
      mutate(Gender = case_when(
        Gender == "life_expectancy_female" ~ "Female",
        Gender == "life_expectancy_male" ~ "Male"
      )) %>%
    filter(Gender %in% input$gender)
    
    if (nrow(data) == 0) return(NULL)  
    
    data
  })
  
  # Render ridge plot for life expectancy
  output$lifeExpectancyPlot <- renderPlot({
    data <- filtered_data()
    
    if (is.null(data)) return(NULL)  
    
    # Create ridge plot
    ggplot(data, aes(x = life_expectancy_merged, y = country, fill = Gender)) +
      geom_density_ridges(scale = 2, alpha = 0.7) +
      labs(
        title = "Life Expectancy Distribution by Gender and Country",
        x = "Life Expectancy",
        y = "Country",
        fill = "Gender"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),  
        axis.title.x = element_text(size = 16, face = "bold"),  
        axis.title.y = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20)
        
      )
  })
  
  output$barPlot <- renderPlot({
    if (!input$show_bar_plot) return(NULL)  
    
    data <- filtered_data()
    if (is.null(data)) return(NULL)
    
    # Create the bar plot
    ggplot(data, aes(x = year, y = life_expectancy_merged, fill = Gender)) +
      geom_col(position = "dodge", alpha = 0.8) + 
      labs(
        title = "Life Expectancy by Year, Gender, and Country Facet",
        x = "Year",
        y = "Life Expectancy",
        fill = "Gender"
      ) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20)
      ) +
      facet_wrap(~country, scales = "free_y")  
  })
  
  
  
  output$filteredData <- DT::renderDT({
    data <- hispanic_health_dev %>% 
      select(country, year,life_expectancy_female, life_expectancy_male, life_expectancy, female_population, male_population, total_population)
    if (nrow(data) == 0) return(NULL)
    datatable(data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
}

# Run app
shinyApp(ui, server)

