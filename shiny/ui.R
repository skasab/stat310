library(shiny)
library(plotly)
library(shinycssloaders)

shinyUI(fluidPage(

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
            checkboxInput("shift","Show margin shift from previous election (2004 onward)",value = FALSE),
            actionButton("go", label = "Visualize election!"),
        ),

        mainPanel(
            # conditionalPanel("input.shift == FALSE || input.year != 2000" , plotlyOutput("distPlot")),
            # conditionalPanel("input.shift == TRUE && input.year == 2000" , p("Cannot select margin map for year 2000"))
            # #,tableOutput("table")
            plotlyOutput("plot") %>% withSpinner(type = 3, size = 2, color.background = "#FFFFFF")
        )
    )
))
