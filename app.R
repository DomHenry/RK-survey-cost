library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(glue)
library(shinydashboard)

generate_cost_data <- function(mileage_rate, labour_rate,
                               trans_dist_1,trans_dist_2,
                               trans_dist_3,trans_dist_4,survey_speed_input,
                               study_day1, study_day2, study_day3){
  
  four_transect_distance <- c(trans_dist_1,trans_dist_2,trans_dist_3,trans_dist_4)
  three_study_days <- c(study_day1, study_day2, study_day3)
  
  survey_lookup <- as_tibble(expand.grid(
    survey_interval = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 15),
    study_duration_days = three_study_days)) %>%
    rowwise() %>%
    mutate(optimal_num_surveys = length(seq(from = 1, to = study_duration_days, by = 1 + survey_interval)))
  
  cost <- expand.grid(persistence = c("1","2","3","5",">10"),
                      carcass_detection = c(50,75,100),
                      study_duration_days = three_study_days,
                      transect_distance = four_transect_distance,
                      survey_speed = survey_speed_input) %>% 
    as_tibble() %>% 
    mutate(survey_interval = case_when(persistence == "1"  & carcass_detection == 100 ~ 0,
                                       persistence == "2"  & carcass_detection == 100 ~ 0,
                                       persistence == "3"  & carcass_detection == 100 ~ 0,
                                       persistence == "5"  & carcass_detection == 100 ~ 0,
                                       persistence == ">10" & carcass_detection == 100 ~ 0,
                                       persistence == "1"  & carcass_detection == 75 ~ 1,
                                       persistence == "2"  & carcass_detection == 75 ~ 2,
                                       persistence == "3"  & carcass_detection == 75 ~ 4,
                                       persistence == "5"  & carcass_detection == 75 ~ 5,
                                       persistence == ">10" & carcass_detection == 75 ~ 8,
                                       persistence == "1"  & carcass_detection == 50 ~ 3,
                                       persistence == "2"  & carcass_detection == 50 ~ 4,
                                       persistence == "3"  & carcass_detection == 50 ~ 6,
                                       persistence == "5"  & carcass_detection == 50 ~ 8,
                                       persistence == ">10" & carcass_detection == 50 ~ 15)) %>% 
    left_join(survey_lookup, by = c("survey_interval" = "survey_interval",
                                    "study_duration_days" = "study_duration_days")) %>% 
    mutate(total_distance_driven = transect_distance * optimal_num_surveys,
           hours_worked = total_distance_driven/survey_speed,
           hours_per_day = hours_worked/optimal_num_surveys,
           mileage_cost = mileage_rate * total_distance_driven,
           labour_cost = labour_rate * hours_worked,
           total_cost = labour_cost + mileage_cost) %>% 
    mutate_at(vars(persistence, carcass_detection,
                   transect_distance, study_duration_days,
                   survey_speed), as_factor)
  
  return(cost)
}

cost_plot <- function(x,survey_speed_input,
                      study_day1, study_day2, study_day3, maxcost){
  
  cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  detect_labs <- c("50" = "50% detection",
                   "75" = "75% detection")
  
  p <- x %>% 
    mutate(carcass_detection = as.character(carcass_detection),
           study_duration_days = glue("{study_duration_days} days")) %>% 
    
    filter(!carcass_detection == 100) %>% 
    ggplot(aes(x = persistence, y = total_cost,color = transect_distance, group = carcass_detection))+
    geom_point(size = 2)+
    geom_line(aes(group = transect_distance), size = 1)+
    labs(title = glue("Speed: {survey_speed_input}km/h"))+
    scale_colour_manual(values=cbp2[c(3,4,5,7)])+
    labs(color = str_wrap("Transect distance (km)", width = 15))+
    xlab("Mean carcass persistence (days)") +
    ylab("Total study cost")+
    theme_bw()+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          strip.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))+
    scale_y_continuous(labels = scales::dollar_format()) + 
    facet_grid(carcass_detection ~ study_duration_days,
               labeller = labeller(carcass_detection = detect_labs))
  
  if(maxcost == "yes"){
    
    p <- p + 
       geom_hline(yintercept = 3500, col = "red", linetype = "dashed", size = 0.8)
    return(p)
    
  } else {
    
    return(p)
  }
  
}



ui <- dashboardPage(
  dashboardHeader(title = "SPLAT: Survey Planning and Logistics Analysis Tool"),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      box(title = "Speed and rates input", width = 4,
          numericInput("survey_speed_input", "Survey speed (km/h)", value = 35, min = 10, max = 80, step = 5),
          numericInput("labour_rate", "Input the labour rate ($/hour):", value = 25, step = 5),
          numericInput("mileage_rate", "Input the mileage rate ($/km):", value = 0.89, step = 0.1),
          radioButtons("maxcost", "Maximum budget", c("Yes" = "yes", "No" = "no"))),
      box(title = "Survey distances",width = 4,
          numericInput("trans_dist_1", "Transect distance 1 (km):", value = 50, step = 10),
          numericInput("trans_dist_2", "Transect distance 2 (km):", value = 60, step = 10),
          numericInput("trans_dist_3", "Transect distance 3 (km):", value = 70, step = 10),
          numericInput("trans_dist_4", "Transect distance 4 (km):", value = 80, step = 10)),
      box(title = "Study durations",width = 4,
          numericInput("study_day1", "Study duration 1 (days):", value = 10, step = 5),
          numericInput("study_day2", "Study duration 2 (days):", value = 30, step = 5),
          numericInput("study_day3", "Study duration 3 (days):", value = 60, step = 5))
      
    ),
    fluidRow(
      box(title = "Download data", width = 2,
          downloadButton("downloadData", "Download cost analysis data"))
    ),
    
    fluidRow(
      box(title = "Comparitve plot", width = 4,
          plotOutput("panel_plot")),
      box(title = "Daily survey costs", width = 4,
          tableOutput("cost_data"))
    ),
    
    fluidRow(
      box(title = "Data frame", width = 8,
          dataTableOutput("cost_data_dynamic"))
    )
  )
)

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
      cost_plot(cost_table(), input$survey_speed_input,
                input$study_day1, 
                input$study_day2, input$study_day3, input$maxcost)
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
