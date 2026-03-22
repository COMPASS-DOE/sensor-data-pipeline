#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui <- dashboardPage(

  dashboardHeader(title = "COMPASS Synoptic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
      menuItem("Sapflow", tabName = "sapflow", icon = icon("tree")),
      menuItem("TEROS", tabName = "teros", icon = icon("temperature-high")),
      menuItem("AquaTroll", tabName = "troll", icon = icon("water"))#,
      #menuItem("Battery", tabName = "battery", icon = icon("car-battery")),
      #menuItem("Alerts", tabName = "alerts", icon = icon("comment-dots"))
      )
    ),
  dashboardBody(
      tags$head(
          # Include our custom CSS
          includeCSS("style.css")
      ),
    tabItems(
      tabItem(tabName = "dashboard",
              h3("Welcome to the COMPASS Synoptic Dashboard - BETA!"),
              h5("Data currently takes a long time to process and for graphs to load, click on the
                 navigation tabs to begin the data processing. We have added progress bars while you wait :)"),
              leafletOutput("map", height = "calc(80vh - 100px)", width = "80%")#,

              # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
              #               draggable = TRUE, top = 90, left = "auto", right = 30, bottom = "auto",
              #               width = 330, height = "auto"
              #)
      ),

      ## Loading MSM data for no good reason
     tabItem(tabName = "sapflow",
            selectInput("selectsf", label = h3("Site:"),
                         choices = list("Old Woman Creek" = "OWC",
                                        "Portage River" = "PTR",
                                        "Crane Creek" = "CRC",
                                        "Moneystump Marsh" = "MSM",
                                        "Goodwin Island" = "GWI"),
                         selected = "GWI"),
            plotlyOutput("sapflow_ts"),
            DT::DTOutput("sf_table")),

      tabItem(tabName = "teros",
              DT::DTOutput("teros_table")),

      tabItem(tabName = "troll",
              selectInput("select", label = h3("Select box"),
                          choices = list("Water Level (m below surface)" = "wl_below_surface_m",
                                         "Temperature" = "temperature",
                                         "DO (mg/L)" = "rdo_concen"),
                          selected = "wl_below_surface_m"),
              plotlyOutput("troll_ts"),
              DT::DTOutput("troll_table"))
    )
  ),
  skin = "purple"

  )
