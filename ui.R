library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Reliability"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("subsystem 1",
          numericInput('Ttime1', 'Input Total time:', 20000, min = 0, step = 1000),
          numericInput('Amode_fail_rate1', 'Input A mode Failure Rate:', 1000, step = 100),
          numericInput('Bmode_fail_rate1', 'Input B mode Failure Rate:', 100, step = 50),
          numericInput('Bmode_FEF1', 'Input B mode Fix effectiveness factor:', 0.9, step = 0.1,min = 0, max = 1),
          numericInput('Cost_Increment1', 'cost increment:', 5000000, step = 1000000),
          sliderInput('Reliability_Investment_input1', 'Input Reliability Investment :', min = 0, max = 20000000,value = 20000000, step = 500000),
          numericInput('C01', 'Operating cost:', 1000000, step = 1000000),
          numericInput('Ci1', 'Initial cost:', 20000, step = 10000),
          width=3),
      tabPanel("subsystem 2",
               numericInput('Ttime2', 'Input Total time:', 20000, min = 0, step = 1000),
               numericInput('Amode_fail_rate2', 'Input A mode Failure Rate:', 500, step = 100),
               numericInput('Bmode_fail_rate2', 'Input B mode Failure Rate:', 200, step = 50),
               numericInput('Bmode_FEF2', 'Input B mode Fix effectiveness factor:', 0.8, step = 0.1,min = 0, max = 1),
               numericInput('Cost_Increment2', 'cost increment:', 4000000, step = 1000000),
               sliderInput('Reliability_Investment_input2', 'Input Reliability Investment :', min = 0, max = 20000000,value = 20000000, step = 500000),
               numericInput('C02', 'Operating cost:', 800000, step = 1000000),
               numericInput('Ci2', 'Initial cost:', 75000, step = 10000),
               width=3),
      id = "panelName")
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      
      tabPanel("MTTF", plotOutput("MTTF",height = "700px")),
      tabPanel("Replacement Units", plotOutput("repParts",height = "700px")),
      tabPanel("Cost", plotOutput("Cost",height = "700px")),
      tabPanel("Fleet Size", plotOutput("fleetSize",height = "700px")),
      tabPanel("Affordability", plotOutput("Aff",height = "700px")),
      tabPanel("Text", textOutput("MTTF_Output"))
      )
    
  )
  
))
)
