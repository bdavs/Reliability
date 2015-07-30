library(shiny)

shinyUI(fluidPage(
  titlePanel("Reliability"),
  sidebarLayout(sidebarPanel(
#     div(
#       style = 'height:900px; overflow-y: scroll',  #adds a scroll bar to sidebar
      tabsetPanel(
        tabPanel(
          "subsystem 1",
          numericInput(
            'Ttime1', 'Input Total time (hrs):', 20000, min = 0, step = 1000
          ),
          numericInput('Amode_fail_rate1', 'Input A mode Failure Rate (hrs/failure):', 1000, step = 100),
          numericInput('Bmode_fail_rate1', 'Input B mode Failure Rate (hrs/failure):', 100, step = 50),
          numericInput(
            'Bmode_FEF1', 'Input B mode Fix effectiveness factor:', 0.9, step = 0.1,min = 0, max = 1
          ),
          numericInput('Cost_Increment1', 'Cost Increment ($):', 5000000, step = 1000000),
          # sliderInput('Reliability_Investment_input1', 'Input Reliability Investment :', min = 0, max = 100000000,value = 60000000, step = 1000000),
           numericInput( 'Reliability_Investment_input1', 'Input Reliability Investment ($):', min = 0, max = 100000000,value = 60000000, step = 1000000 ),
          numericInput('C01', 'Operating cost ($):', 1000000, step = 1000000),
          numericInput('Ci1', 'Initial cost ($):', 200000, step = 10000),
          width = 3
        ),
        tabPanel(
          "subsystem 2",
          numericInput(
            'Ttime2', 'Input Total time (hrs):', 20000, min = 0, step = 1000
          ),
          numericInput('Amode_fail_rate2', 'Input A mode Failure Rate (hrs/failure):', 500, step = 100),
          numericInput('Bmode_fail_rate2', 'Input B mode Failure Rate (hrs/failure):', 200, step = 50),
          numericInput(
            'Bmode_FEF2', 'Input B mode Fix effectiveness factor:', 0.8, step = 0.1,min = 0, max = 1
          ),
          numericInput('Cost_Increment2', 'Cost Increment ($):', 4000000, step = 1000000),
          numericInput('Reliability_Investment_input2', 'Input Reliability Investment ($):', min = 0, max = 100000000,value = 60000000, step = 1000000),
          numericInput('C02', 'Operating cost ($):', 800000, step = 1000000),
          numericInput('Ci2', 'Initial cost ($):', 75000, step = 10000),
          width = 3
        ),
#         tabPanel("subsystem 3",
#                  dateRangeInput('date3', 'How long will the project last?', startview = "decade"),
#                  numericInput('Amode_fail_rate3', 'Input A mode Failure Rate:', 500, step = 100),
#                  numericInput('Bmode_fail_rate3', 'Input B mode Failure Rate:', 200, step = 50),
#                  numericInput('Bmode_FEF3', 'Input B mode Fix effectiveness factor:', 0.8, step = 0.1,min = 0, max = 1),
#                  numericInput('Cost_Increment3', 'cost increment:', 4000000, step = 1000000),
#                  numericInput('MTTR', 'Mean time to repair:', 20, step = 10),
#                  numericInput('CostR', 'Cost to repair:', 1000, step = 100),
#                  numericInput('CostD', 'Cost to dispose:', 2000, step = 100),
#                  sliderInput('Reliability_Investment_input3', 'Input Reliability Investment :', min = 0, max = 100000000,value = 60000000, step = 1000000),
#                  numericInput('RDTE', 'Additional RDT&E costs:', 1000000, step = 10000),
#                  numericInput('C03', 'Operating cost:', 800000, step = 1000000),
#                  numericInput('Ci3', 'Initial cost:', 75000, step = 10000),
#                  #downloadButton('testing'),
#                  width=3),
#         tabPanel("Sensitivities",
#                  fileInput("filename","upload file for sensitivity testing")),
#         
#         
#         tabPanel("[+]"),
        id = "panelName"
      )
    
  ),
  
  # Show a plot of the generated outputs
  mainPanel(
    tabsetPanel(
      tabPanel("MTTF", plotOutput("MTTF",height = "700px")),
      tabPanel("Replacement Units", plotOutput("repParts",height = "700px")),
      tabPanel("Cost", plotOutput("Cost",height = "700px")),
      tabPanel("Fleet Size", plotOutput("fleetSize",height = "700px")),
      #tabPanel("potato", plotOutput("pt",height = "700px")),
      # tabPanel("Max Fleet Size", plotOutput("maxFleet",height = "700px")),
      tabPanel("Affordability", plotOutput("Aff",height = "700px")),
      tabPanel("Availablilty", plotOutput("ava",height = "700px"))
      # tabPanel("Tradeoff Analysis",plotOutput("TA",height = "700px"))
      # tabPanel("Text", textOutput("MTTF_Output"))
    )
    
  ))
))
