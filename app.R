library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(glue)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "SPLAT"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Comparison", tabName = "complot", icon = icon("chart-area")),
      menuItem("User data", tabName = "userdata", icon = icon("th")),
      menuItem("Download", tabName = "download", icon = icon("download"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "complot",

        fluidRow(column(
          width = 8,
          tags$h1("SPLAT: Survey Planning and Logistics Analysis Tool")
        )),

        fluidRow(
          box(
            title = "1. Speed and rates input", width = 3,
            numericInput("survey_speed_input", "Survey speed (km/h)", value = 35, min = 10, max = 80, step = 5),
            numericInput("labour_rate", "Input the labour rate ($/hour):", value = 25, step = 5),
            numericInput("mileage_rate", "Input the mileage rate ($/km):", value = 0.89, step = 0.1)
          ),
          box(
            title = "2. Survey distances", width = 3,
            # numericInput("trans_dist_1", "Transect distance 1 (km):", value = 50, step = 10),
            sliderInput("trans_dist_1", "Transect distance 1 (km):", min= 0, max = 350, value = 30, step = 5),
            sliderInput("trans_dist_2", "Transect distance 2 (km):", min= 0, max = 350, value = 60, step = 5),
            sliderInput("trans_dist_3", "Transect distance 3 (km):", min= 0, max = 350, value = 90, step = 5),
            sliderInput("trans_dist_4", "Transect distance 4 (km):", min= 0, max = 350, value = 150, step = 5)
          ),
          box(
            title = "3. Study durations", width = 3,
            # numericInput("study_day1", "Study duration 1 (days):", value = 10, step = 5),
            sliderInput("study_day1", "Study duration 1 (days):", min= 10, max = 90, value = 10, step = 1),
            sliderInput("study_day2", "Study duration 2 (days):", min= 10, max = 90, value = 30, step = 1),
            sliderInput("study_day3", "Study duration 3 (days):", min= 10, max = 90, value = 50, step = 1)
          ),
          box(
            title = "4. Budget", width = 3,
            radioButtons("maxcost", "Display maximum budget limit?", c("Yes" = "yes", "No" = "no")),
            numericInput("maxcost_amount", "Enter your maximum budget ($)", value = 0, step = 500)
          )
        ),
        
        fluidRow(
          box(
            title = "Comparitve plot", width = 8,
            plotOutput("panel_plot")
          ),
          box(
            title = "Study cost for daily surveys", width = 4,
            tableOutput("cost_data") #TODO sort this table by distance
          )
        )
      ),
      
      tabItem(
        tabName = "userdata",
       
        fluidRow(
          box(
            title = "Data frame", width = 8,
            dataTableOutput("cost_data_dynamic")
          )
        )
      ),
      
      tabItem(
        tabName = "download",
        
        fluidRow(
          box(
            title = "Download data", width = 4,
            downloadButton("downloadData", "Download cost analysis data")
          )
        ),
        
        fluidRow(
          box(
            title = "Download plot", width = 4,
            downloadButton("downloadPlot", "Download cost analysis plot")
          )
        )
      )
    )
  )
)

source("functions/generate_cost_data.R")
source("functions/cost_plot.R")

server <- function(input, output, session) {
  
  cost_table <- reactive({
    generate_cost_data(
      input$mileage_rate, input$labour_rate,
      input$trans_dist_1, input$trans_dist_2,
      input$trans_dist_3, input$trans_dist_4,
      input$survey_speed_input, input$study_day1,
      input$study_day2, input$study_day3
    )
  })

  cost_panel_plot <- reactive({
    
    cost_plot(
      cost_table(), input$survey_speed_input,
      input$study_day1,
      input$study_day2, input$study_day3, input$maxcost, input$maxcost_amount
    )
    
  })
  
  output$cost_data <- renderTable(
    {
      cost_table() %>%
        filter(carcass_detection == 100) %>%
        distinct(transect_distance, study_duration_days, total_cost)
    },
    caption = "Costs of surveys at 100% detection (i.e., daily surveys)"
  )

  output$cost_data_dynamic <- renderDataTable(
    {
      cost_table() %>%
        select(mileage_cost, labour_cost, total_cost, survey_interval)

      # https://datatables.net/reference/option/
    },
    options = list(pagelength = 5)
  )

  output$panel_plot <- renderPlot(
    {
      cost_panel_plot()
    },
    res = 96,
    width = 800, height = 500
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " cost_analysis_data.csv")
    },
    content = function(file) {
      write.csv(cost_table(), file, row.names = FALSE)
    }
  )
    output$downloadPlot <- downloadHandler(
      filename = function(){
        paste0(Sys.Date(), " cost_analysis_data.png")
        },
      content = function(file){
        ggsave(file,
               plot = cost_panel_plot())
      }
  )
}

shinyApp(ui = ui, server = server)
