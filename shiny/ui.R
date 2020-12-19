library(shiny)
library(plotly)
library(shinycssloaders)
library(DT)

shinyUI(
    navbarPage("",
        tabPanel("Maps",              

            titlePanel("Solly Kasab Final Project"),
            helpText("Note: Plotly's maps are quite slow, so please be patient whilst the map loads."),
            helpText("(in some cases, very patient)"),
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
                                 choices = list("Two-Party" = "both","Democrat" = "democrat","Republican" = "republican", "Other" = "other")),
                    conditionalPanel(condition = "input.year != 2000", checkboxInput("shift","Show margin shift from previous election (2004 onward)",value = FALSE)),
                    actionButton("go", label = "Visualize election!"),
                ),
    
            mainPanel(
                conditionalPanel(condition = "input.go", withSpinner(plotlyOutput("plot"),type = 3, size = 2, color.background = "#FFFFFF")))
            )
        ),
        tabPanel("County Data",
                 fluidPage(
                     conditionalPanel(condition = "input.go", withSpinner(DTOutput("counties"),type = 3, size = 1, color.background = "#FFFFFF"))
                )
        )
    )
)

