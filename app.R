library(shiny)
library(tidyr)

hispanic_countries <- c("Mexico" , "Colombia", "Spain", "Argentina" ,"Peru", "Venezuela", "Chile", "Guatemala", "Ecuador",
                     "Bolivia", "Dominican Republic", "Cuba", "Honduras", "Paraguay", "Nicaragua", "El Salvador",
                     "Costa Rica", "Panama", "Uruguay", "Puerto Rico", "Equatorial Guinea")

central_america <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama")

globalhealth_dev <- read.csv("data/global_health.csv")

hispanic_health_dev <- globalhealth_dev %>%
  filter(Country %in% hispanic_countries)
#View(hispanic_health_dev)

central_america_health_dev <- globalhealth_dev %>%
  filter(Country %in% central_america)
#View(central_america_health_dev)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Global Health and Development in South American Countries"),

)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)

