library(shiny)#used for displaying everything
library(gdata) #Used for read.xls function
library(emdbook)#lambertW function
library(reshape2)#3d plots
library(ggplot2)#ggplot function
source("main.R")#Source for our reliabilty models


shinyServer(function(input, output) {
  #reactive shiny fuction
  
  reliability <- new.env()
  Subsystem <- reactive({
   
     #Subsystem1
    reliability$sub1$Reliability_Investment <-
      seq(0, input$Reliability_Investment_input1, len = 1000) #makes array of 1000 values for the reliability investment
    reliability$sub1$MTTF <- #calculates MTTF for subsystem 1 and stores it in this vailable
      MTTF_Function(  reliability$sub1$Reliability_Investment,input$C01,input$Cost_Increment1,input$Amode_fail_rate1,input$Bmode_fail_rate1,input$Bmode_FEF1 )
    reliability$sub1$minunit <- #calculates the number of replacement parts required
      repParts(input$Ttime1,reliability$sub1$MTTF)
    reliability$sub1$cst <- #calculates cost of subsystem 1
      Cost(reliability$sub1$minunit, input$Ci1)
    reliability$sub1$Aff <- AFF(input$Ci1, reliability$sub1$cst) #calculates affordability 
    reliability$sub1$numUnits <- #calculates fleet size based on fixed budget of 1 billion
      NumUnits(1000000000,reliability$sub1$Reliability_Investment,reliability$sub1$cst)
   
     #Subsystem2
    reliability$sub2$Reliability_Investment <-
      seq(0, input$Reliability_Investment_input2, len = 1000)
    reliability$sub2$MTTF <-
      MTTF_Function(reliability$sub2$Reliability_Investment,input$C02,input$Cost_Increment2,input$Amode_fail_rate2,input$Bmode_fail_rate2,input$Bmode_FEF2)
    reliability$sub2$minunit <-
      repParts(input$Ttime2,reliability$sub2$MTTF)
    reliability$sub2$cst <-
      Cost(reliability$sub2$minunit, input$Ci2)
    reliability$sub2$Aff <- AFF(input$Ci2, reliability$sub2$cst)
    reliability$sub2$numUnits <-
      NumUnits(1000000000,reliability$sub2$Reliability_Investment,reliability$sub2$cst)
   
     #Unit
    reliability$unitCost <- #calculates cost of both subsystems 
      UnitCost(reliability$sub1$cst,reliability$sub2$cst)
    reliability$unitInvestment <- #calculates total investment from both subsystems
      UnitInvestment(
        reliability$sub1$Reliability_Investment,reliability$sub2$Reliability_Investment
      )
    reliability$numUnits <- #calculates the number of units possible based on both subsystems
      NumUnits(1000000000,100000000,reliability$unitCost)
    #return variable
    reliability
    
  })
  
  Reliability <- reactive({ #allows work to be done on the current selected subsystem (probably not the best way of doing this)
    reli <- new.env()
    switch(input$panelName,
           "subsystem 1" = {
             reli = Subsystem()$sub1
           },
           "subsystem 2" = {
             reli = Subsystem()$sub2
           })
    reli
  })
  
  
  #Mean time to falure vs reliability investment
  output$MTTF <- renderPlot({
    
    plot_data <-
      data.frame(x = Reliability()$Reliability_Investment,y = Reliability()$MTTF)
    p <-
      ggplot(data = plot_data, aes(x = x,y = y)) + geom_point(color = "blue") +
      xlab("Reliability Investment ($)") + ylab("Mean time to Failure (hrs)") + ggtitle("MTTF")
    p <- p + theme(axis.title.y = element_text(size = rel(1), angle = 90)) + theme(axis.title.x = element_text(size = rel(1), angle = 00))
    p <- p + theme(title = element_text(size = rel(2), angle = 00))
    p
  })
  
  #number of replacement part
  output$repParts <- renderPlot({
    p <-
      qplot(Reliability()$Reliability_Investment,Reliability()$minunit)+ xlab("Reliability Investment ($)") + ylab("Replacement parts") + ggtitle("Replacements")
    p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
    p
  })
  
  
  #cost per subsystem (and total system) vs investment
  output$Cost <- renderPlot({
    #if(Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)] > Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)]) X <-  Subsystem()$sub1$Reliability_Investment else X <- Subsystem()$sub2$Reliability_Investment
    X <-  Subsystem()$sub1$Reliability_Investment #currently only scaled for subsystem 1,  above statement will pick the larger of the two
    
    y1 <- Subsystem()$sub1$cst + X / 30  #a scaled reliability investment is added to demonstrate example minima on cost
    y2 <- Subsystem()$sub2$cst + X / 30
    y3 <- y1 + y2 #Subsystem()$numUnits
    
    length(y3) = length(y1)  = length(y2) = length(X)
    #allowing multiple lines be plotted on one graph
    plot_data <-
      data.frame(
        x = X, "Subsystem 1" = y1, "Subsystem 2" = y2, "System" = y3
      )
    
    #plot_data <- data.frame(x = Reliability()$Reliability_Investment, "Subsystem 1" = Subsystem()$sub1$cst, "Subsystem 2" = Subsystem()$sub2$cst)
    
    plot_data_long <- melt(plot_data, id = "x")
    p <-
      ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point()  +
      ylab("Cost ($)") + xlab("Reliability Investment ($)")
    p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
   
    p
  })
  
  
  #total fleet size
  output$fleetSize <- renderPlot({
    if (Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)] > Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)])
      X <-  Subsystem()$sub1$Reliability_Investment
    else
      X <- Subsystem()$sub2$Reliability_Investment
    y1 <- Subsystem()$sub1$numUnits
    y2 <- Subsystem()$sub2$numUnits
    y3 <- Subsystem()$numUnits
    length(y3) = length(y1)  = length(y2) = length(X)
    plot_data <-
      data.frame(
        x = X, "Subsystem 1" = y1, "Subsystem 2" = y2, "System" = y3
      )
    plot_data_long <- melt(plot_data, id = "x")
    
    p <-
      ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +
      ylab("Units") + xlab("Reliability Investment ($)")
    p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
    p
  })
  
  #affordability plot
  output$Aff <- renderPlot({
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$Aff) + xlab("Reliability Investment ($)") + ylab("Affordability") + ggtitle("Affordability")
    p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
    p
  })
  
  #this is an example plot. currently plots the contours of a volcano
  output$TA <- renderPlot({  
    p <- ggplot(melt(volcano), aes(x = Var1, y = Var2, fill = value)) + geom_tile()  +
      scale_fill_gradient("Desirability",low = "red",high = "blue") + xlab("Reliability Investment ($)") +
      ylab("Cost") + ggtitle("Desirability")
    p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
  })
  
  #displays text, not currently used
  output$MTTF_Output <-  renderText({
    Reliability()$numUnits
  })
  
  #availability plot
  output$ava <- renderPlot({ 
    plot_data <-
      data.frame(x = Reliability()$Reliability_Investment,y = (Reliability()$MTTF / input$Ttime1) )
    p <-
      ggplot(data = plot_data, aes(x = x,y = y)) + geom_point(color = "black") + xlab("Reliability Investment ($)") +
      ylab("System Availability")
    p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
    p
  })

  
})
