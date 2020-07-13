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
library(forecast)

# Data ----

# Load data from .csv file
demographics <- read_csv("demographics.csv")
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
            # Municipality ----
            selectInput(
                inputId = "mun_name",
                label = "Commune",
                choices = list_mun,
                selected = "Neuchâtel",
                multiple = TRUE
            ),
            
            # Demographic variable ----
            selectInput(
                inputId = "variable_name",
                label = "Variable démographique",
                choices = list_variables,
                selected = "Effectif au 31 décembre"
            ),
            
            # Sex ----
            selectInput(
                inputId = "sex",
                label = "Sexe",
                choices = list_sexes,
                selected = "Sexe - total"
            ),
            
            # Nationalities ----
            selectInput(
                inputId = "nationality",
                label = "Suisses / Etrangers",
                choices = list_nationalities,
                selected = "Nationalité - total"
            ),
            
            # Smoother ----
            radioButtons(
                inputId = "smoother",
                label = "Ajouter une moyenne mobile d'ordre q",
                choices = list("Oui", "Non"),
                selected = "Non"
            ),
            sliderInput(
                inputId = "q",
                label = "q",
                min = 1,
                max = 11,
                value = 5,
                step = 2
            ),
            
            # Log scale
            radioButtons(
                inputId = "logscale",
                label = "Echelle",
                choices = list("Normale", "Logarithmique"),
                selected = "Normale"
            )
        ),

        # Main panel ---
        mainPanel(
            
            # Display line plot
            plotOutput(outputId = "line_plot"),
            
            # Text about source of the data
            p("Les données proviennent de l'Office Fédéral de",
              "la Statistique (OFS). Lien px (utilisable uniquement avec",
              "un logiciel spécial: "
              ),
            a("https://www.bfs.admin.ch/bfsstatic/dam/assets/9566432/master"),
            
            # Download dataset ----
            downloadButton(
                outputId = "download_data", 
                label = "Télécharger les données (csv)"
           )
        )
    )
)



# Backend ----

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Store dataset in input
    # input$dataset <- demographics
    # data_to_download <- reactive({
    #     input$dataset
    # })
    
    # Outputs ----
    
    # Line plot
    output$line_plot <- renderPlot({
        
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
        pl <- pl + geom_point()
        pl <- pl + geom_line(linetype = "dashed")
        
        # Labels
        pl <- pl + labs(x = "Année", y = input$variable_name, col = "Commune")
        
        # Smoother
        if (input$smoother == "Oui"){
            data_ma <- data_for_plot$nbr_people %>% ma(order = input$q)
            pl <- pl + geom_line(aes(y = data_ma), size = 1)
        }
        # Log scale
        if (input$logscale == "Logarithmique"){
            pl <- pl + scale_y_continuous(trans='log10')
        }
        
        # Display
        pl
    })
    
    # Downloadable csv of selected dataset----
    output$download_data <- downloadHandler(
        filename = "demographics.csv",
        content = function(file) {
            write.csv(
                x = demographics,
                file = file, 
                row.names = FALSE
            )
        }
    )
}

# Run the application ----
shinyApp(ui = ui, server = server)
