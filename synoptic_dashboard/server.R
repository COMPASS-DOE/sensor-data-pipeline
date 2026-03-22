#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(googledrive)
library(janitor)
library(purrr)
library(shiny)
library(tidyverse)


## Call global to read in all the functions
source("global.R")

# Define server logic
shinyServer(function(input, output) {

  ## Create reactive aquatroll dataframe
  reactive_df <- reactive({

     sapflow <- withProgress(process_sapflow(), message = "Updating sapflow...") %>%
         filter(Timestamp > two_weeks_ago)
     teros <- withProgress(process_teros(), message = "Updating TEROS...") %>%
         filter(TIMESTAMP > two_weeks_ago)
     aquatroll <- withProgress(process_the_troll(), message = "Updating AquaTroll...") %>%
         filter(datetime > two_weeks_ago)
    # aquatroll <- readRDS("./test_data/aquatroll.rds")
    # teros <- readRDS("./test_data/teros.rds")
    # sapflow <- readRDS("./test_data/sapflow.rds")

    #browser()
    #x <<- aquatroll
    list(aquatroll = aquatroll,
         teros = teros,
         sapflow = sapflow)
  })

  output$map <- renderLeaflet({
      leaflet(site_locations) %>%
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                           label = ~Site,
                            popup = ~Location) %>%
          setView(lng = -80.25, lat = 40.19, zoom = 6)
  })

  output$sf_table <- DT::renderDT(reactive_df()$sapflow %>%
                                          tail(n = 10))

  output$troll_table <- DT::renderDT(reactive_df()$aquatroll %>%
                                          tail(n = 10))

  output$teros_table <- DT::renderDT(reactive_df()$teros %>%
                                          tail(n = 10))

  output$troll_ts <- renderPlotly({

    b <- reactive_df()$aquatroll %>%
      ggplot(aes(x = datetime, y = .data[[input$select]], color = location)) +
      geom_line() +
      facet_wrap(~site, ncol = 1, scales = "free") +
      labs(x = "")

    ggplotly(b)
    }
  )

  output$teros_ts <- renderPlotly({
    
    b <- reactive_df()$teros %>%
      filter(grepl(input$select_teros, Site, ignore.case = TRUE)) %>%
      ggplot(aes(x = TIMESTAMP, y = value, color = Logger)) +
      geom_point() +
      facet_wrap(~variable, ncol = 1, scales = "free") +
      labs(x = "")
    
    ggplotly(b)
  }
  )
  
  output$sapflow_ts <- renderPlotly({

      s <- reactive_df()$sapflow %>%
          filter(Site == input$selectsf) %>%
          group_by(Site, Location, Port) %>%
          drop_na(Value) %>%
          ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port)), group = Location) +
          geom_line() +
          facet_wrap(~Location) +
          labs(color = "Port", ncol = 1)

      ggplotly(s)
  })

})
