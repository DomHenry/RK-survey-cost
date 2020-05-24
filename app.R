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
      menuItem("Comparison", tabName = "complot", icon = icon("dashboard")),
      menuItem("User data", tabName = "userdata", icon = icon("th"))
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
            title = "Speed and rates input", width = 4,
            numericInput("survey_speed_input", "Survey speed (km/h)", value = 35, min = 10, max = 80, step = 5),
            numericInput("labour_rate", "Input the labour rate ($/hour):", value = 25, step = 5),
            numericInput("mileage_rate", "Input the mileage rate ($/km):", value = 0.89, step = 0.1),
            radioButtons("maxcost", "Display maximum budget limit?", c("Yes" = "yes", "No" = "no")),
            numericInput("maxcost_amount", "Enter your maximum budget ($)", value = 0, step = 500)
          ),
          box(
            title = "Survey distances", width = 4,
            numericInput("trans_dist_1", "Transect distance 1 (km):", value = 50, step = 10),
            numericInput("trans_dist_2", "Transect distance 2 (km):", value = 60, step = 10),
            numericInput("trans_dist_3", "Transect distance 3 (km):", value = 70, step = 10),
            numericInput("trans_dist_4", "Transect distance 4 (km):", value = 80, step = 10)
          ),
          box(
            title = "Study durations", width = 4,
            numericInput("study_day1", "Study duration 1 (days):", value = 10, step = 5),
            numericInput("study_day2", "Study duration 2 (days):", value = 30, step = 5),
            numericInput("study_day3", "Study duration 3 (days):", value = 60, step = 5)
          )
        ),
        fluidRow(
          box(
            title = "Download data", width = 2,
            downloadButton("downloadData", "Download cost analysis data")
          )
        ),

        fluidRow(
          box(
            title = "Comparitve plot", width = 4,
            plotOutput("panel_plot")
          ),
          box(
            title = "Study cost for daily surveys", width = 4,
            tableOutput("cost_data")
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
      cost_plot(
        cost_table(), input$survey_speed_input,
        input$study_day1,
        input$study_day2, input$study_day3, input$maxcost, input$maxcost_amount
      )
    },
    res = 96,
    # width = 800, height = 800
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " cost_analysis_data.csv")
    },
    content = function(file) {
      write.csv(cost_table(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
