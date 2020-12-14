library(shiny)
library(tidyverse)
library(plotly)
library(rjson)

shinyServer(function(input, output, session) {

    counties <- rjson::fromJSON(file='https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json')
    data <- read_csv("data/countypres_2000-2016.csv")
    
    
    #remove green party candidate and add to Other total for 2000 election
    others2000 <- data %>% 
        filter(year == 2000, candidate %in% c("Ralph Nader", "Other")) %>% 
        group_by(FIPS) %>% 
        mutate(candidatevotes = sum(candidatevotes)) %>% 
        filter(candidate == "Other")
    
    # combine with the rest of the data
    data <- data %>%
        filter((year != 2000 | candidate != "Other")) %>%
        bind_rows(others2000) %>%
        filter(candidate != "Ralph Nader")
    
    #remove empty data points and generally clean data for usage
    data <- data %>% 
        drop_na(FIPS) %>%
        replace_na(list(party = "other",candidatevotes = 0)) %>%
        filter(totalvotes != 0) %>%
        filter(state != "Alaska") %>%
        mutate(fips5 = sprintf("%05d",FIPS)) %>%
        select(-version,office) %>% 
        mutate(percent = candidatevotes/totalvotes) %>%
        arrange(year,state,FIPS)
    
    getPrior <- function(theYear,theParty,theFips) {
        returnValue <- filter(data,year == theYear - 4, party == theParty, FIPS == theFips)
        if(nrow(returnValue) == 0) {
            tmp <- filter(data,year == theYear, party == theParty, FIPS == theFips)
            return(tmp$percent)
        }
        return(returnValue$percent)
    }
    
    margins <- data %>%
        filter(year != 2000) %>%
        mutate(tmp = pmap(list(year,party,FIPS),getPrior)) %>%
        mutate(priorPercent = flatten_dbl(tmp)) %>%
        select(-tmp)
    
    dataFiltered <- eventReactive(input$go, {
        if(input$party == "both") {
            parties = c("democrat","republican")   
        }
        else {
            parties = input$party
        }
        
        dataFiltered <- data %>% filter(year == input$year, party %in% parties)
        return(dataFiltered)
    })
    
    marginsFiltered <- eventReactive(input$go, {
        if(input$party == "both") {
            parties = c("democrat","republican")   
        }
        else {
            parties = input$party
        }
        
        marginsFiltered <- margins %>% filter(year == input$year, party %in% parties)
        return(marginsFiltered)
    })
    
    candidates <- reactiveValues(democrat = NULL, republican = NULL, other = NULL)
    
    observeEvent(input$go,{
        if(input$year == 2000) {
            candidates$democrat <-  "Al Gore"
            candidates$republican <- "George W. Bush"
        }
        else if(input$year == 2004) {
            candidates$democrat <-  "John Kerry"
            candidates$republican <- "George W. Bush"
        }
        else if(input$year == 2008) {
            candidates$democrat <-  "Barack Obama"
            candidates$republican <- "John McCain"
        }
        else if(input$year == 2012) {
            candidates$democrat <-  "Barack Obama"
            candidates$republican <- "Mitt Romney"
        }
        else if(input$year == 2016) {
            candidates$democrat <-  "Hillary Clinton"
            candidates$republican <- "Donald Trump"
        }
        
        candidates$other <- "Other"
        # print(candidates$democrat)
        # print(candidates$republican)
        # print(candidates$other)
    })
    
    stylings <- reactiveValues(colorScale = NULL, colorScaleTitle = NULL, plotTitle = NULL, reverse = NULL)
    
    observeEvent(input$go,{
        if(input$shift == TRUE) {
            base <- "Vote Shift To"
        }
        else {
            base <- "Vote Percent For"
        }
        
        if(input$party == "democrat") {
            stylings$colorScale <- "Blues"
            stylings$colorScaleTitle <- paste(base,candidates$democrat)
            stylings$plotTitle <- paste(input$year,"Presidential Election, Votes for",candidates$democrat)
            stylings$reverse <- TRUE
        }
        else if(input$party == "republican") {
            stylings$colorScale <- "Reds"
            stylings$colorScaleTitle <- paste(base,candidates$republican)
            stylings$plotTitle <- paste(input$year,"Presidential Election, Votes for",candidates$republican)
            stylings$reverse <- FALSE
        }
        else if(input$party == "other") {
            stylings$colorScale <- "Greys"
            stylings$colorScaleTitle <- paste(base,candidates$other)
            stylings$plotTitle <- paste(input$year,"Presidential Election, Votes for",candidates$other)
            stylings$reverse <- TRUE
        }
        else if(input$party == "both") {
            stylings$colorScale <- "Bluered"
            stylings$colorScaleTitle <- "Two-Party Vote<br>(Scale: 1 = More Republican, 0 = More Democratic)"
            stylings$plotTitle <- paste(input$year,"Presidential Election")
            stylings$reverse <-  FALSE
        }
        
        if(input$shift == TRUE) {
            stylings$plotTitle = paste("Shift in Presidential Vote Margin,",(input$year - 4),"to",input$year) 
            if(input$party == "democrat") {
                stylings$colorScale <- "RdBu"
                stylings$reverse <-  TRUE
            }
            if(input$party == "republican") {
                stylings$colorScale <- "RdBu"
                stylings$reverse <-  FALSE
            }
            if(input$party == "both") {
                stylings$colorScale <- "Picnic"
                stylings$colorScaleTitle <- "Two-Party Vote<br>(Scale: (+) = More Republican, (-) = More Democratic)"
                stylings$reverse <-  FALSE
            }
        }

        # print(stylings$colorScale)
        # print(stylings$colorScaleTitle)
        # print(stylings$plotTitle)
        # print(stylings$reverse)
    })
    
    output$table <- renderTable(dataFiltered())

    getValuePlot <- function() {
        plotdata <- dataFiltered()
        
        plotdata <- plotdata %>% 
            group_by(input$year, state, FIPS, fips5) %>% 
            mutate(tmp1 = sum(candidatevotes),tmp2 = tmp1 - candidatevotes, oppPercent = tmp2 / totalvotes) %>%
            select(-tmp1,tmp2)
        
        usaConfig <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        fig <- plot_ly() %>% 
            add_trace(
                type="choropleth",
                geojson=counties,
                locations=plotdata$fips5,
                z=plotdata$percent,
                hoverinfo = 'text',
                # text = paste0(plotdata$county, " County<br>",
                #               round(plotdata$percent * 100),"% ",
                #               case_when(
                #                   input$party == "democrat" ~ candidates$democrat,
                #                   input$party == "republican" ~ candidates$republican,
                #                   input$party == "other" ~ candidates$other,
                #                   input$party == "both" ~ paste0(candidates$republican,"<br>",round(plotdata$oppPercent * 100),"%",candidates$democrat))),
                # 
                text = {
                    base <- paste0(plotdata$county, " County<br>",round(plotdata$percent * 100),"% ")
                    
                    case_when(
                        input$party == "democrat" ~ paste0(base,candidates$democrat),
                        input$party == "republican" ~ paste0(base,candidates$republican),
                        input$party == "other" ~ paste0(base,candidates$other),
                        input$party == "both" ~ case_when(
                            plotdata$oppPercent > plotdata$percent ~ paste0(plotdata$county, " County<br>",round(plotdata$oppPercent * 100),"% ",candidates$democrat,"<br>",round(plotdata$percent * 100),"% ",candidates$republican),
                            TRUE ~ paste0(base,candidates$republican,"<br>",round(plotdata$oppPercent * 100),"% ",candidates$democrat)))
                },
                zmin=0,
                zmid=.5,
                zmax=1,
                colorscale = stylings$colorScale,
                reversescale = stylings$reverse,
                marker=list(line=list(
                    width=.1,
                    color="#111111")
                )) %>%
            colorbar(title = stylings$colorScaleTitle) %>%
            layout(title = stylings$plotTitle) %>%
            layout(geo = usaConfig)
        
        return(fig)
    }
    
    getMarginPlot <- function() {
        plotdata <- marginsFiltered()
    
        plotdata <- plotdata %>% 
            mutate(marginShift = percent - priorPercent)
                
        usaConfig <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        fig <- plot_ly() %>% 
            add_trace(
                type="choropleth",
                geojson=counties,
                locations=plotdata$fips5,
                z=plotdata$marginShift,
                hoverinfo = 'text',
                # text = paste0(plotdata$county, " County<br>",
                #               round(plotdata$percent * 100),"% ",
                #               case_when(
                #                   input$party == "democrat" ~ candidates$democrat,
                #                   input$party == "republican" ~ candidates$republican,
                #                   input$party == "other" ~ candidates$other,
                #                   input$party == "both" ~ paste0(candidates$republican,"<br>",round(plotdata$oppPercent * 100),"%",candidates$democrat))),
                # 
                text = {
                    base <- paste0(plotdata$county, " County<br>",abs(round(plotdata$marginShift * 100)),"% ")
                    
                    case_when(
                        input$party == "democrat" ~ case_when(
                            plotdata$marginShift >= 0 ~ paste0(base,"towards ",candidates$democrat, " since ", (input$year - 4)),
                            plotdata$marginShift < 0 ~ paste0(base,"away from ",candidates$democrat, " since ", (input$year - 4))),
                        input$party == "republican" ~ case_when(
                            plotdata$marginShift >= 0 ~ paste0(base,"towards ",candidates$republican, " since ", (input$year - 4)),
                            plotdata$marginShift < 0 ~ paste0(base,"away from ",candidates$republican, " since ", (input$year - 4))),
                        input$party == "other" ~ case_when(
                            plotdata$marginShift >= 0 ~ paste0(base,"towards ",candidates$other, " since ", (input$year - 4)),
                            plotdata$marginShift < 0 ~ paste0(base,"away from ",candidates$other, " since ", (input$year - 4))),
                        input$party == "both" ~ case_when(
                            plotdata$marginShift >= 0 ~ paste0(base,"towards the Republican (",candidates$republican,") since ", (input$year - 4)),
                            plotdata$marginShift < 0 ~ paste0(base,"towards the Democrat (",candidates$democrat,") since ", (input$year - 4))))
                },
                zmin=--1,
                zmid=0,
                zmax=1,
                colorscale = stylings$colorScale,
                reversescale = stylings$reverse,
                marker=list(line=list(
                    width=.1,
                    color="#111111")
                )) %>%
            colorbar(title = stylings$colorScaleTitle) %>%
            layout(title = stylings$plotTitle) %>%
            layout(geo = usaConfig)
        
        return(fig)
    }
    
    thePlot <- eventReactive(input$go, {
        if(input$shift) {
            if(input$year == 2000) {
                
            }
            else {
                return(getMarginPlot())
            }
        }
        else {
            return(getValuePlot())
        }  
    })
    
    output$plot <- renderPlotly({
        thePlot()
    })    


})
