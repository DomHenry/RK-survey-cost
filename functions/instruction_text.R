# https://shiny.rstudio.com/articles/tag-glossary.html

in_text <- tags$div(
  tags$p("Welcome to RoadSPLAT, an app that allows you to compare the costs and logistical effort of 
         roadkill surveys across a range of survey designs and carcass persistence of focal 
         taxa. The app has three components:"),
  tags$p(
    tags$ol(
      tags$li("'Survey Design', where the actual interactive analysis is run"),
      tags$li("'Downloads', where figures and data from the analysis can be downloaded"),
      tags$li("'Citations' where you can find information on the recommended citation 
              of the app and associated paper published in the Journal of Environmental Management.")
    )
  ),
  tags$b(tags$p("Survey Design")), 
  tags$p("The Survey Design analysis takes a number of user defined inputs in the blue boxes 
         (via numeric inputs and sliders) across four main input categories (speed and rates, 
         survey distance, study duration, and budget). Each category is numbered and will 
         allow you to sequentially select values that are relevant to the type of survey designs you 
         wish to compare."),
  tags$p(tags$em(tags$b("Survey speed")), "is the average speed of the vehicle driven along the transects,",
         tags$em(tags$b("labour rate")),"is the cost per hour of employing staff who conduct the surveys, and the 
         ", tags$em(tags$b("mileage rate")), "is the cost per km of the vehicle used for the transects (note that mileage should include the combined cost of fuel and maintenance)."),
  tags$p("You then have a choice to select four different", tags$em(tags$b("survey distances")) ,
  "which represent the one-way distance of a roadkill survey transect."),
  tags$p("There are three options for the ", tags$em(tags$b("study durations"))," which indicate the total time available to 
         complete the roadkill surveys."),
  tags$p("The budget box gives an option to plot a red, dotted horizontal line which represents the 
         ", tags$em(tags$b("maximum funds"))," available for the surveys based on the sum of mileage and labour costs."),
  tags$p("The primary outputs are two figures (in green boxes) which compare how costs change in 
         relation to the mean carcass persistence across the various survey scenarios at either 75% or 50% 
         detection rates (which arise from conducting an alternative to daily surveys). The costs of doing 
         daily surveys (100%) are plotted in the figure below the comparative plot. For more detail 
         on the methodology behind the analysis see the accompanying journal article", a("here.", 
                                                                                         href = "https://doi.org/10.1016/j.jenvman.2021.112664")),
  tags$b(tags$p("Downloads")),
  tags$p("In the Download tab you can download these figures (in PNG format) and the data that 
         are used to generate them in case you want to make you own plots or interrogate some 
         other aspects of the underlying data. Apart from the plot data, the comparative cost CSV has 
         the following columns which give extra detail of for each survey design: recommended survey
         interval (days), total optimal number of surveys, total distance driven for the study, 
         hours worked during surveys (both daily and total), and a breakdown of mileage and labour costs."),
  tags$b(tags$p("Citations")),
  tags$p("Here you'll find links to the peer-reviewed publication, the 
         GitHub repo for the R Shiny app, and the recommended citation for both."),
  tags$p("If you experience any problems feel free to contact us at science@ewt.org.za")
)