library(shiny)
library(plotly)

shinyUI(fluidPage(

    titlePanel("Solly Kasab Final Project"),
    helpText("Note: Plotly's maps are quite slow, so please be patient whilst the map loads."),
    hr(),
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), #removes minor ticks on year selector
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        label = h3("Year of Election"),
                        min = 2000,
                        max = 2016,
                        step = 4,
                        value = 2000,
                        sep = ''),
            radioButtons("party", label = h3("Party"),
                         choices = list("Two-Party" = "both","Democrat" = "democrat","Republican" = "republican", "Other" = "other"))
            # checkboxInput("shift","Show margin shift from previous election (2004 onward)",value = FALSE)
        ),

        mainPanel(
            plotlyOutput("distPlot")
            #,tableOutput("table")
        )
    )
))
