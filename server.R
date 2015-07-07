library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(reshape2)
library(ggplot2)#ggplot function
source("main.R")#Source for our reliabilty models


shinyServer(function(input, output) {#reactive shiny fuction
  
  reliability <- new.env()
  Subsystem <- reactive({
    
              #Subsystem1
               reliability$sub1$Reliability_Investment <- seq(0, input$Reliability_Investment_input1, by = 1000)
               reliability$sub1$MTTF <- MTTF_Function(reliability$sub1$Reliability_Investment,input$C01,input$Cost_Increment1,input$Amode_fail_rate1,input$Bmode_fail_rate1,input$Bmode_FEF1)
               reliability$sub1$minunit <- repParts(input$Ttime1,reliability$sub1$MTTF)
               reliability$sub1$cst <- Cost(reliability$sub1$minunit, input$Ci1)
               reliability$sub1$Aff <- AFF(input$Ci1, reliability$sub1$cst)
               reliability$sub1$numUnits <- NumUnits(1000000000,reliability$sub1$Reliability_Investment,reliability$sub1$cst)
              #Subsystem2
               reliability$sub2$Reliability_Investment <- seq(0, input$Reliability_Investment_input2, by = 1000)
               reliability$sub2$MTTF <- MTTF_Function(reliability$sub2$Reliability_Investment,input$C02,input$Cost_Increment2,input$Amode_fail_rate2,input$Bmode_fail_rate2,input$Bmode_FEF2)
               reliability$sub2$minunit <- repParts(input$Ttime2,reliability$sub2$MTTF)
               reliability$sub2$cst <- Cost(reliability$sub2$minunit, input$Ci2)
               reliability$sub2$Aff <- AFF(input$Ci2, reliability$sub2$cst)
               reliability$sub2$numUnits <- NumUnits(1000000000,reliability$sub2$Reliability_Investment,reliability$sub2$cst)
              #Unit
               reliability$unitCost <- UnitCost(reliability$sub1$cst,reliability$sub2$cst)
               reliability$unitInvestment <- UnitInvestment(reliability$sub1$Reliability_Investment,reliability$sub2$Reliability_Investment)
               reliability$numUnits <- NumUnits(1000000000,reliability$unitInvestment,reliability$unitCost)
               reliability


    
  })
  
  Reliability <- reactive({
    i=as.integer(input$subsystem)
    reli <- new.env()
    switch(input$panelName,
           "subsystem 1" = {reli = Subsystem()$sub1},
           "subsystem 2" = {reli = Subsystem()$sub2},
            "3" = {reli = Subsystem()$sub3},
            "4" = {reli = Subsystem()$sub4})
    reli
  })
  
  output$MTTF <- renderPlot({ #reactive function, basically Main()
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$MTTF)
    p
  })
  output$repParts <- renderPlot({ 
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$minunit)
    p
  })
  output$Cost <- renderPlot({ 
    if (is.null(Subsystem()$sub2$cst)){
      plot_data <- data.frame(x = Reliability()$Reliability_Investment, "Subsystem 1" = Subsystem()$sub1$cst)
    }
    else{
      plot_data <- data.frame(x = Reliability()$Reliability_Investment, "Subsystem 1" = Subsystem()$sub1$cst, "Subsystem 2" = Subsystem()$sub2$cst)
    }
    plot_data_long <- melt(plot_data, id = "x")
    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Cost")+ xlab("Reliability Investment")
    p
  })
  output$fleetSize <- renderPlot({ 
    X <- if(Subsystem()$sub1$Reliability_investment[length(Subsystem()$sub1$Reliability_investment)] > Subsystem()$sub2$Reliability_investment[length(Subsystem()$sub2$Reliability_investment)]) Subsystem()$sub1$Reliability_investment else Subsystem()$sub2$Reliability_investment
    if(length(X) > length(Subsystem()$sub1$numUnits)){
    for(i in length(Subsystem()$sub1$numUnits)+1:length(X)){
      Subsystem()$sub1$numUnits[i] <- NA
    
    }}
    plot_data <- data.frame(x = X, "Subsystem 1" = Subsystem()$sub1$numUnits, "Subsystem 2" = Subsystem()$sub2$numUnits)
    plot_data_long <- melt(plot_data, id = "x")
    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Units")+ xlab("Reliability Investment")
    p
  })
  output$Aff <- renderPlot({ 
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$Aff)
    p
  })
  
  
  
  output$MTTF_Output<-  renderText({
   Reliability()$numUnits
  })
  output$ui <- renderUI({
    
  })
  
  
  
  
})
