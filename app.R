# ----------------------------
# start of shiny app -Anna
# ----------------------------

#Attach packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(USAboundariesData)
library(USAboundaries)
library(devtools)
library(leaflet)

# ----------------------------
#read in data
# ----------------------------
#mt <- read_csv("")

# ---------------------------
# create `ui` 
# ---------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Montana Wildfires"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fires Causes", tabName = "causes", icon = icon("fire")),
      menuItem("Search by Fire", tabName = "search", icon = icon("search")),
      menuItem("Containment Time", tabName = "time", icon = icon("clock"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "causes",
        fluidRow(
          box(title = "Causes Map",
              checkboxGroupInput("mt_causes",
                                 "Choose a fire cause:",
                                 choices = c(unique(mt$stat_cause_descr)))),
          box(leafletOutput(outputId = "mt_cause_map"))
        )
      ),
      tabItem(
        tabName = "search",
        fluidRow(
          box(title = "Montana Wildfires by Name",
              selectInput("mt_fire_name",
                          "Search a specific wildfire by name:", choices = c(unique(mt$source_reporting_unit_name)))),
          box(gt_output(outputId = "mt_fire_summary")),
          box(plotOutput(outputId = "name_plot"))
        )
      ),
      tabItem(
        tabName = "time", 
        fluidRow(
          box(title = "Containment Time by Reporting Source Over the Years",
              selectInput("source_time", 
                          "Choose a reporting source:",
                          choices = c(unique(mt$source_reporting_unit_name)))),
          box(plotOutput(outputId = "time_plot"))
        )
      )
    )
  )
)

tmap_mode("view")
# ------------------------------
# building the server
# -------------------------------
server <- function(input, output){
  mt_df <- reactive({
    mt_sf %>% 
      filter(stat_cause_descr == input$mt_causes)
  })
  
  output$mt_cause_map <- renderLeaflet({
    tm <- tm_basemap("Stamen.TerrainBackground")+
      tm_shape(mt_df())+
      tm_dots(label = "Campfire", col = "orange",
              size = 0.02)
  })
  
  mt_df <- reactive({
    mt_sf %>% 
      filter(fire_name == input$mt_fire_name)
  })
  
  output$mt_fire_summary <- render_gt({
    mt %>% 
      gt() %>% 
      tab_header(
        title = "Fire Summary"
      ) %>% 
      cols_label(
        fire_name = "Name of Fire",
        fire_year = "Year",
        stat_cause_descr = "Cause of Fire",
        fire_size = "Fire Area",
        latitude = "Latitude",
        longitude = "Longitude",
        state = "State",
        source_reporting_unit_name = "Name of Reporting Source"
      )
  })
  
  output$name_plot <- renderPlot({
    ggplot()+
      geom_sf(data = counties_mt)+
      geom_point(data = mt_df(), aes (x = longitude, y = latitude))
  })
  
  mt_df <- reactive({
    mt_date_combined %>% 
    filter(source_reporting_unit_name == input$source_time) %>% 
      group_by(fire_year) %>% 
      summarize(
        mean_interval = round(mean(interval),2)
      )
  })
  
  output$time_plot <- renderPlot({
    ggplot(data = mt_date_combined(), aes(x = fire_year, y = mean_interval))+
      geom_line(color = "darkgreen")
  })
}


# --------------------------------
shinyApp(ui = ui, server = server)









