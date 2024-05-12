#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(dplyr)
library(reactable)
library(shiny)
library(tippy)
library(htmltools)


create_sample_dataset <- function(){
  
# Create the sample of some of the greatest (worst) names for grasses ever
# Personally - I would plant my lawn in Glorious Arrr, just so I could say to
# people in my well-crafted pirate accent: This lawn here be Glorious Arrr.
  
# Made sure to pick names that wouldn't be misinterpreted as real
  
a <- c('Superdupa','Supreme','Master','Glorious','The Best')
b <- c('AR25', 'NEA5', 'NEA2', 'Arrr', 'AR1000')

cultivar_names <- vector("character", length(a) * length(b))
for(i in 1:length(a)){
  for(j in 1:length(b)){
    cultivar_names[(i-1)*length(b) + j] <- paste0(a[i]," ", b[j])      
  }
}

# I am an unbiased statistican. Therefore, I will not biasedly choose Glorious
# to be the best.

# biased_statistican <- FALSE
# if (biased_statistican){
#   winter_rating['Glorious Arrr'] <- 9999
# }

winter_rating <- sample(c(3,4,4,5,5,5), length(a) * length(b), replace = TRUE)
early_spring_rating <- pmax(3,pmin(5,round(winter_rating + sample(c(0.5, 0, 0, 0,-0.51), length(a) * length(b), replace = TRUE))))
late_spring_rating <- pmax(3,pmin(5,round(early_spring_rating + sample(c(0.5, 0, 0, 0,-0.51), length(a) * length(b), replace = TRUE))))
summer_rating <- pmax(3,pmin(5,round(late_spring_rating + sample(c(0.5, 0, 0, 0,-0.51), length(a) * length(b), replace = TRUE))))
autumn_rating <- pmax(3,pmin(5,round(summer_rating + sample(c(0.5, 0, 0, 0,-0.51), length(a) * length(b), replace = TRUE))))

confidence <- runif(length(a) * length(b), 2.5, 15)

return(
  tibble(
    `Cultivar` = cultivar_names,
    `Confidence` = confidence,
    `Winter` = winter_rating,
    `Early Spring` = early_spring_rating,
    `Late Spring` = late_spring_rating,
    `Summer` = summer_rating,
    `Autumn` = autumn_rating,
    `Total Rating` = (`Winter` + `Early Spring` + `Late Spring` + `Summer` + `Autumn`),
    `Minimum Rating` = min(`Winter`, `Early Spring`, `Late Spring`, `Summer`, `Autumn`),
    `Star Rating` = case_when(
        `Total Rating` <= quantile(`Total Rating`, 0.1) ~ 1,
        `Total Rating` <= quantile(`Total Rating`, 0.25) ~ 2,
        `Total Rating` <= quantile(`Total Rating`, 0.50) ~ 3,
        `Total Rating` <= quantile(`Total Rating`, 0.75) ~ 4,
        TRUE ~ 5
      )
  )
)

}

sample_dataset <- create_sample_dataset()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Example of how to do conditional formatting in Shiny"),

    sidebarPanel(
      checkboxGroupInput("Stars","Star Rating", choices = c('5 Stars' = '5','4 Stars' = '4',
                                              '4 Stars' = '3','2 Stars' = '2','1 Star' = '1'), selected = c('1','2','3','4','5'))
    ),

        # Show a plot of the generated distribution
        mainPanel(
           reactableOutput("tableOutput")
        )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    stars_checked <- reactive(
      {
        number_of_stars <- as.numeric(input$Stars)
        isolate(number_of_stars)
        return(number_of_stars)
      }
    )
    # 
    output$tableOutput <- renderReactable({
      
      output_data <- sample_dataset %>%
        filter(`Star Rating` %in% input$Stars) %>%
        select(-`Total Rating`)
      
      reactable(output_data, minRows = 15)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

