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
library(kableExtra)
library(lubridate)

# ----------------------------
#read in data
# ----------------------------
counties_mt <- us_counties(states = "Montana")
plot(st_geometry(counties_mt))

#adding data set
mt <- read_csv("montana_fire.csv") %>% 
  clean_names() %>% 
  select(-fire_code, -fire_size_class)

#making an sf data frame
mt_sf <- st_as_sf(mt, coords = c("longitude", "latitude"), crs = 4326)

#changing the data from julian to normal
mt_date <- mt %>% 
  mutate(new_dis_date = as.Date(discovery_date, origin = structure(-2440588, class = "Date"))) %>% 
  mutate(new_cont_date = as.Date(cont_date, origin = structure(-2440588, class = "Date")))

#combining data and time for containment to find the difference between the discovery time and containment time
mt_date_combined <- mt_date %>% 
  mutate(final_dis_date = lubridate::ymd_hm(paste(mt_date$new_dis_date, mt_date$discovery_time))) %>% 
  mutate(final_cont_date = lubridate::ymd_hm(paste(mt_date$new_cont_date, mt_date$cont_time))) %>% 
  mutate(interval = difftime(final_cont_date,final_dis_date,units = "min")) %>% 
  mutate(interval = round(interval/60, 2)) %>% 
  drop_na(final_cont_date, final_dis_date)

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
          box(tableOutput(outputId = "mt_fire_summary")),
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
    leaflet(mt_df())+
      tm_basemap("Stamen.TerrainBackground")+
      tm_dots(label = "Campfire", col = "orange",
              size = 0.02)
  })
  
  mt_df2 <- reactive({
    mt_sf %>% 
      filter(fire_name == input$mt_fire_name)
  })
  
  output$mt_fire_summary <- renderTable({
    fire_summary_table()
  })
  
  output$name_plot <- renderPlot({
    ggplot()+
      geom_sf(data = counties_mt)+
      geom_point(data = mt_df2(), aes (x = longitude, y = latitude))
  })
  
  mt_df3 <- reactive({
    mt_date_combined %>% 
      filter(source_reporting_unit_name == input$source_time) %>% 
      group_by(fire_year) %>% 
      summarize(
        mean_interval = round(mean(interval),2)
      )
  })
  
  output$time_plot <- renderPlot({
    ggplot(data = mt_df3(), aes(x = fire_year, y = mean_interval))+
      geom_line(color = "darkgreen")
  })
}


# --------------------------------
shinyApp(ui = ui, server = server)
















