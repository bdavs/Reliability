library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(emdbook)
library(reshape2)
library(ggplot2)#ggplot function
source("main.R")#Source for our reliabilty models


shinyServer(function(input, output) {#reactive shiny fuction
  
  reliability <- new.env()
  Subsystem <- reactive({
<<<<<<< HEAD
    #Subsystem1
    reliability$sub1$Reliability_Investment <- seq(0, input$Reliability_Investment_input1, len = 1000)
    reliability$sub1$MTTF <- MTTF_Function(reliability$sub1$Reliability_Investment,input$C01,input$Cost_Increment1,input$Amode_fail_rate1,input$Bmode_fail_rate1,input$Bmode_FEF1)
    reliability$sub1$minunit <- repParts(input$Ttime1,reliability$sub1$MTTF)
    reliability$sub1$cst <- Cost(reliability$sub1$minunit, input$Ci1)
    reliability$sub1$Aff <- AFF(input$Ci1, reliability$sub1$cst)
    reliability$sub1$numUnits <- NumUnits(1000000000,reliability$sub1$Reliability_Investment,reliability$sub1$cst)
    #Subsystem2
    reliability$sub2$Reliability_Investment <- seq(0, input$Reliability_Investment_input2, len = 1000)
    reliability$sub2$MTTF <- MTTF_Function(reliability$sub2$Reliability_Investment,input$C02,input$Cost_Increment2,input$Amode_fail_rate2,input$Bmode_fail_rate2,input$Bmode_FEF2)
    reliability$sub2$minunit <- repParts(input$Ttime2,reliability$sub2$MTTF)
    reliability$sub2$cst <- Cost(reliability$sub2$minunit, input$Ci2)
    reliability$sub2$Aff <- AFF(input$Ci2, reliability$sub2$cst)
    reliability$sub2$numUnits <- NumUnits(1000000000,reliability$sub2$Reliability_Investment,reliability$sub2$cst)
    #Unit
    reliability$unitCost <- UnitCost(reliability$sub1$cst,reliability$sub2$cst)
    reliability$unitInvestment <- UnitInvestment(reliability$sub1$Reliability_Investment,reliability$sub2$Reliability_Investment)
    reliability$numUnits <- NumUnits(1000000000,100000000,reliability$unitCost)
    #return variable
    reliability
=======
    
              #Subsystem1
               reliability$sub1$Reliability_Investment <- seq(0, input$Reliability_Investment_input1, by = 1000)
               reliability$sub1$MTTF <- MTTF_Function(reliability$sub1$Reliability_Investment,input$C01,input$Cost_Increment1,input$Amode_fail_rate1,input$Bmode_fail_rate1,input$Bmode_FEF1)
               reliability$sub1$minunit <- repParts(input$Ttime1,reliability$sub1$MTTF)
               reliability$sub1$cst <- Cost(reliability$sub1$minunit, input$Ci1)
               reliability$sub1$Aff <- AFF(input$Ci1, reliability$sub1$cst)
               reliability$sub1$numUnits <- NumUnits(1000000000,reliability$sub1$Reliability_Investment,reliability$unitCost)#reliability$sub1$cst)
              #Subsystem2
               reliability$sub2$Reliability_Investment <- seq(0, input$Reliability_Investment_input2, by = 1000)
               reliability$sub2$MTTF <- MTTF_Function(reliability$sub2$Reliability_Investment,input$C02,input$Cost_Increment2,input$Amode_fail_rate2,input$Bmode_fail_rate2,input$Bmode_FEF2)
               reliability$sub2$minunit <- repParts(input$Ttime2,reliability$sub2$MTTF)
               reliability$sub2$cst <- Cost(reliability$sub2$minunit, input$Ci2)
               reliability$sub2$Aff <- AFF(input$Ci2, reliability$sub2$cst)
               reliability$sub2$numUnits <- NumUnits(1000000000,reliability$sub2$Reliability_Investment,reliability$unitCost)#reliability$sub2$cst)
              #Unit
               reliability$unitCost <- UnitCost(reliability$sub1$cst,reliability$sub2$cst)
               reliability$unitInvestment <- UnitInvestment(reliability$sub1$Reliability_Investment,reliability$sub2$Reliability_Investment)
               reliability$numUnits <- NumUnits(1000000000,100000000,reliability$unitCost)
               
               reliability
>>>>>>> b55d5f4038b260fb12b52a13a4499ac4756f9d9f
  })
  
  Reliability <- reactive({
    i=as.integer(input$subsystem)
    reli <- new.env()
    switch(input$panelName,
           "subsystem 1" = {reli = Subsystem()$sub1},
           "subsystem 2" = {reli = Subsystem()$sub2},
<<<<<<< HEAD
           "3" = {reli = Subsystem()$sub3},
           "4" = {reli = Subsystem()$sub4})
    reli
  })
  
=======
            "3" = {reli = Subsystem()$sub3},
            "4" = {reli = Subsystem()$sub4})
    reli
  })
>>>>>>> b55d5f4038b260fb12b52a13a4499ac4756f9d9f
  #Mean time to falure vs reliability investment
  output$MTTF <- renderPlot({ #reactive function, basically Main()
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$MTTF)
    p
  })
<<<<<<< HEAD
  
=======
>>>>>>> b55d5f4038b260fb12b52a13a4499ac4756f9d9f
  #number of replacement part
  output$repParts <- renderPlot({ 
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$minunit)
    p 
  })
<<<<<<< HEAD
   
  #cost per subsystem vs investment
  output$Cost <- renderPlot({ 
    #if(Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)] > Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)]) X <-  Subsystem()$sub1$Reliability_Investment else X <- Subsystem()$sub2$Reliability_Investment
    X <-  Subsystem()$sub1$Reliability_Investment
=======
  #cost per subsystem vs investment
  output$Cost <- renderPlot({ 

    if(Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)] > Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)]) X <-  Subsystem()$sub1$Reliability_Investment else X <- Subsystem()$sub2$Reliability_Investment
>>>>>>> b55d5f4038b260fb12b52a13a4499ac4756f9d9f
    y1 <- Subsystem()$sub1$cst
    y2 <- Subsystem()$sub2$cst
    y3 <- y1 + y2 #Subsystem()$numUnits
    length(y3) = length(y1)  = length(y2) = length(X)
<<<<<<< HEAD
    plot_data <- data.frame(x = X, "Subsystem 1" = y1)#, "Subsystem 2" = y2, "System" = y3)
    
    #plot_data <- data.frame(x = Reliability()$Reliability_Investment, "Subsystem 1" = Subsystem()$sub1$cst, "Subsystem 2" = Subsystem()$sub2$cst)
    
    plot_data_long <- melt(plot_data, id = "x")
    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Cost ($)")+ xlab("Reliability Investment ($)")
    p
  })
  
=======
    plot_data <- data.frame(x = X, "Subsystem 1" = y1, "Subsystem 2" = y2, "System" = y3)
    
    #plot_data <- data.frame(x = Reliability()$Reliability_Investment, "Subsystem 1" = Subsystem()$sub1$cst, "Subsystem 2" = Subsystem()$sub2$cst)

    plot_data_long <- melt(plot_data, id = "x")
    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Cost")+ xlab("Reliability Investment")
    p
  })
>>>>>>> b55d5f4038b260fb12b52a13a4499ac4756f9d9f
  #total fleet size
  output$fleetSize <- renderPlot({ 
    if(Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)] > Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)]) X <-  Subsystem()$sub1$Reliability_Investment else X <- Subsystem()$sub2$Reliability_Investment
    y1 <- Subsystem()$sub1$numUnits
    y2 <- Subsystem()$sub2$numUnits
    y3 <- Subsystem()$numUnits
    length(y3) = length(y1)  = length(y2) = length(X)
    plot_data <- data.frame(x = X, "Subsystem 1" = y1, "Subsystem 2" = y2, "System" = y3)
    plot_data_long <- melt(plot_data, id = "x")
<<<<<<< HEAD
    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Units")+ xlab("Reliability Investment ($)")
    p
  })
  
  output$maxFleet <- renderPlot({
    #x <- Subsystem()$sub1$Reliability_Investment
    #y <- Subsystem()$sub2$Reliability_Investment
    #xc <- Subsystem()$sub1$cst
    #yc <- Subsystem()$sub2$cst
    #zfunc <- function(x,y) {NumUnits(1000000000,x+y,100000000)}
    #z <- outer(x,y,zfunc)
    #p <- image(z)
    
    pp <- function (n) {
      x <-  seq(0, Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)], len=n)
      y <- seq(0, Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)], len=n)
      xc <- seq(Subsystem()$sub1$cst[1],Subsystem()$sub1$cst[length(Subsystem()$sub1$cst)],len =n)
      yc <- seq(Subsystem()$sub2$cst[1],Subsystem()$sub2$cst[length(Subsystem()$sub2$cst)],len =n)
      
      df <- expand.grid(x=x, y=y)
      dz <- expand.grid(x=xc, y=yc)
      
      df2 <- outer(x,y,FUN = function(xx,yy){NumUnits(1000000000,xx+yy,dz$x+dz$y)})
      df$r <- UnitInvestment(df$x,df$y)
      df$z <- NumUnits(1000000000,df$r,UnitCost(dz$x,dz$y))
      df2
    }
    
    #p <- ggplot(pp(50), aes(x=x,y=y))
    pl <- pp(20)
    #q <- image(pl)
    p <- ggplot(melt(pl, id.vars = c("Var1","Var2")), aes(x=Var1,y=Var2,fill=value))
    
    #p <- p + geom_tile(aes(fill=z)) +xlab("subsystem1") +ylab("subsystem2")
    p <- p + geom_tile() +xlab("subsystem1") +ylab("subsystem2")
    
    p
    
    #qplot(x, y, data=pp(100), geom="tile", fill=z)
  })
  
=======
    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Units")+ xlab("Reliability Investment")
    p
  })
  output$maxFleet <- renderPlot({
    x <- Subsystem()$sub1$Reliability_Investment
    y <- Subsystem()$sub2$Reliability_Investment
    xc <- Subsystem()$sub1$cst
    yc <- Subsystem()$sub2$cst
    zfunc <- function(x,y) {NumUnits(1000000000,x+y,100000000)}
    z <- outer(x,y,zfunc)
    p <- image(z)
    p
  })
>>>>>>> b55d5f4038b260fb12b52a13a4499ac4756f9d9f
  output$Aff <- renderPlot({ 
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$Aff)
    p
  })
  
<<<<<<< HEAD
  output$MTTF_Output<-  renderText({
    Reliability()$numUnits
  })
  
  output$ui <- renderUI({
  })
  
=======
  
  
  output$MTTF_Output<-  renderText({
   Reliability()$numUnits
  })
  output$ui <- renderUI({
    
  })
  
  
  
  
>>>>>>> b55d5f4038b260fb12b52a13a4499ac4756f9d9f
})
