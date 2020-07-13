#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This app displays demographic variables of municipalities of the canton
# of Neuchâtel

# Libraries ----

library(shiny)
library(tidyverse)
library(magrittr)

# Data ----

# Load data from .csv file
demographics <- read_csv("../data/demographics.csv")
to_factors <- c("GDENAME", "sexe", "nationalite_categorie", 
                "composante_demographique")
demographics[, to_factors] %<>% lapply(as.factor)

# Make list of municipalities
list_mun <- demographics$GDENAME %>% levels() %>% as.list()

# Make list of variables
list_variables <- demographics$composante_demographique %>%
    levels() %>% as.list()

# Make list of sexes
list_sexes <- demographics$sexe %>% levels() %>% as.list()

# Make list of nationalities
list_nationalities <- demographics$nationalite_categorie %>% 
    levels() %>% as.list()

# Frontend ----

ui <- fluidPage(

    # Application title
    titlePanel("Variables démographiques des commues neuchâteloises"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Municipality
            selectInput(
                inputId = "mun_name",
                label = "Commune",
                choices = list_mun
            ),
            
            # Demographic variable
            selectInput(
                inputId = "variable_name",
                label = "Variable démographique",
                choices = list_variables
            ),
            
            # Sex
            selectInput(
                inputId = "sex",
                label = "Sexe",
                choices = list_sexes
            ),
            
            # Nationalities
            selectInput(
                inputId = "nationality",
                label = "Suisses / Etrangers",
                choices = list_nationalities
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)



# Backend ----

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        # Filter the data to keep only what interests us
        data_for_plot <- demographics %>% 
            filter(GDENAME == input$mun_name) %>% 
            filter(composante_demographique == input$variable_name) %>% 
            filter(sexe == input$sex) %>% 
            filter(nationalite_categorie == input$nationality)
        
        # Make the plot
        pl <- ggplot(data_for_plot, aes(
            x = annee, 
            y = nbr_people, 
            col = GDENAME
            )
        )
        pl <- pl + geom_line()
        pl
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
