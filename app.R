library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Montana Wildfires"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "intro", icon = icon("fire-alt")),
      menuItem("Causes", tabName = "causes", icon = icon("bolt")),
      menuItem("Search By Fire", tabName = "search", icon = icon("search")),
      menuItem("Containment Time", tabName = "time", icon = icon("clock"))),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        fluidRow(
          box(title = "Overview",
              selectInput("",
                          "",
                          choices = c(unique(mt$stat_cause_descr)))),
          box(plotOutput(outputId = ""))
        )
      ),
      tabItem(
        tabName = "causes",
        fluidRow(
          box(title = "Causes",
              checkboxGroupInput(inputID = "checkGroup",label = "Pick a cause:",
                          choices = c(unique(mt$stat_cause_descr)))),
          box(plotOutput(outputId = "")))
        ),
      tabItem(
        tabName = "search",
        fluidRow(
          box(title = "Search by Fire",
              textInput(inputId = "search",label = "Search Fire Name", value = "Enter text..."),
          box(plotOutput(outputId = "")))
      ),
      tabItem(
        tabName = "time",
        fluidRow(
          box(title = "Containment Time",
              selectInput(inputID = "select",
                          label = "Pick a National Forest/County",
                          choices = c(unique(mt$)))),
          box(plotOutput(outputId = "")))
    )
  )
)

server <- function(input, output) {
  
  species_df <- reactive({
    starwars %>%
      filter(species == input$)
  })
  
  output$sw_plot <- renderPlot({
    ggplot(species_df(), aes(x = homeworld)) +
      geom_bar() +
      coord_flip()
  })
  
}

shinyApp(ui = ui, server = server)
