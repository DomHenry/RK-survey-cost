library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(glue)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = "RoadSPLAT: Roadkill Survey Planning and Logistics Analysis Tool",
    titleWidth = 700
  ),

  dashboardSidebar(
    width = 150,
    sidebarMenu(
      menuItem("Comparison", tabName = "complot", icon = icon("chart-area")),
      menuItem("User data", tabName = "userdata", icon = icon("th")),
      menuItem("Download", tabName = "download", icon = icon("download")),
      menuItem("Citation", tabName = "citation", icon = icon("file-alt"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "complot",

        fluidRow(
          column(
            width = 3,
            box(
              title = "1. Speed and rates input",
              width = NULL,
              status = "primary", solidHeader = TRUE,
              numericInput("survey_speed_input", "Survey speed (km/h)", value = 35, min = 10, max = 80, step = 5),
              numericInput("labour_rate", "Input the labour rate (cost/hour):", value = 25, step = 5),
              numericInput("mileage_rate", "Input the mileage rate (cost/km):", value = 0.89, step = 0.1)
            ),
            box(
              title = "2. Survey distances (km)",
              width = NULL,
              status = "primary", solidHeader = TRUE,
              sliderInput("trans_dist_1", "Transect distance 1:", min = 0, max = 350, value = 30, step = 5),
              sliderInput("trans_dist_2", "Transect distance 2:", min = 0, max = 350, value = 60, step = 5),
              sliderInput("trans_dist_3", "Transect distance 3:", min = 0, max = 350, value = 90, step = 5),
              sliderInput("trans_dist_4", "Transect distance 4:", min = 0, max = 350, value = 150, step = 5)
            )
          ),

          column(
            width = 3,
            box(
              title = "3. Study durations (days)",
              width = NULL,
              status = "primary", solidHeader = TRUE,
              sliderInput("study_day1", "Study duration 1:", min = 10, max = 90, value = 10, step = 1),
              sliderInput("study_day2", "Study duration 2:", min = 10, max = 90, value = 30, step = 1),
              sliderInput("study_day3", "Study duration 3:", min = 10, max = 90, value = 50, step = 1)
            ),
            box(
              title = "4. Budget",
              status = "primary", solidHeader = TRUE,
              width = NULL,
              radioButtons("maxcost", "Display maximum budget limit?", c("No" = "no", "Yes" = "yes"),
                selected = "no"
              ),
              numericInput("maxcost_amount", "If yes, enter your maximum budget", value = 0, step = 500)
            )
          ),

          column(
            width = 6,
            box(
              title = "Comparitve plot",
              status = "success", solidHeader = TRUE,
              width = NULL,
              plotOutput("panel_plot")
            ),
            box(
              title = "Cost of daily surveys (100% detection)",
              status = "success", solidHeader = TRUE,
              width = NULL,
              plotOutput("max_plot")
            )
          )
        )
      ),

      tabItem(
        tabName = "userdata",

        fluidRow(
          box(
            title = "Data used to generate facetted line plot", width = 8,
            status = "primary", solidHeader = TRUE,
            dataTableOutput("cost_data_dynamic")
          ),
          box(
            title = "Data used to generate bar plot", width = 4,
            status = "primary", solidHeader = TRUE,
            tableOutput("cost_data")
          )
        )
      ),

      tabItem(
        tabName = "download",

        fluidRow(
          box(
            title = "Download data used in plots (CSV)", width = 4,
            status = "primary", solidHeader = TRUE,
            downloadButton("downloadData", "Comparative cost analysis data"),
            br(),
            br(),
            downloadButton("downloadData_maxcost", "Maximum cost data")
          )
        ),

        fluidRow(
          box(
            title = "Download plots (PNG)", width = 4,
            status = "primary", solidHeader = TRUE,
            downloadButton("downloadPlot_comparison", "Comparative line plot"),
            br(),
            br(),
            downloadButton("downloadPlot_maxcost", "Maximum cost bar plot")
          )
        )
      ),

      tabItem(
        tabName = "citation",

        fluidRow(
          box(
            title = "Citation", width = 4,
            status = "primary", solidHeader = TRUE,
            p("If you have found this tool useful for your research please consider using the following citation: XXX"),
            br(),
            p("Source code for this app hosted at this", a("GitHub repo", href = "https://github.com/DomHenry/RK-survey-cost"))
          )
        )
      )
    )
  )
)


source("functions/generate_cost_data.R")
source("functions/cost_plot.R")
source("functions/cost_max_plot.R")

server <- function(input, output, session) {
  
  transect_dists <- reactive({
    c(input$trans_dist_1, input$trans_dist_2,
    input$trans_dist_3, input$trans_dist_4)
  })
  
  study_days <- reactive({
    c(input$study_day1,
      input$study_day2, input$study_day3)
  })
  
  mileage <- reactive(input$mileage_rate)
  labour <- reactive(input$labour_rate)
  speed <- reactive(input$survey_speed_input)
  maxcost_yn <- reactive(input$maxcost)
  maxcost_amount <- reactive(input$maxcost_amount)
  
  cost_table <- reactive({
    generate_cost_data(
      mileage(), 
      labour(),
      transect_dists(),
      speed(),
      study_days()
    )
  })

  cost_panel_plot <- reactive({
    cost_plot(
      cost_table(), 
      speed(),
      maxcost_yn(), 
      maxcost_amount()
    )
  })

  max_cost_plot <- reactive({
    cost_max_plot(
      cost_table(), study_days(), transect_dists()
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
      DT::datatable(cost_table() %>%
        filter(carcass_detection != 100) %>%
        select(
          persistence, carcass_detection,
          transect_distance, study_duration_days, total_cost
        ))
      # https://rstudio.github.io/DT/
      # https://datatables.net/reference/option/
    },
    options = list(pagelength = 5)
  )

  output$panel_plot <- renderPlot(
    {
      cost_panel_plot()
    },
    res = 96 # ,
    # width = 800,
    # height = 500
  )

  output$max_plot <- renderPlot(
    {
      max_cost_plot()
    },
    res = 96 # ,
    # width = 800,
    # height = 500
  )


  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " cost_analysis_data1.csv")
    },
    content = function(file) {
      write.csv(cost_table(), file, row.names = FALSE)
    }
  )

  output$downloadData_maxcost <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " cost_analysis_data2.csv")
    },
    content = function(file) {
      write.csv(cost_table() %>%
        filter(carcass_detection == 100) %>%
        distinct(transect_distance, study_duration_days, total_cost),
      file,
      row.names = FALSE
      )
    }
  )

  output$downloadPlot_comparison <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " cost_analysis_plot1.png")
    },
    content = function(file) {
      ggsave(file,
        plot = cost_panel_plot()
      )
    }
  )

  output$downloadPlot_maxcost <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " cost_analysis_plot2.png")
    },
    content = function(file) {
      ggsave(file,
        plot = max_cost_plot()
      )
    }
  )
}

shinyApp(ui = ui, server = server)
