library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(emdbook)
library(reshape2)
library(ggplot2)#ggplot function
source("main.R")#Source for our reliabilty models


shinyServer(function(input, output) {#reactive shiny fuction
  
  reliability <- new.env()
  Subsystem <- reactive({

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
  

  #Mean time to falure vs reliability investment
  output$MTTF <- renderPlot({ #reactive function, basically Main()
    plot_data <-data.frame(x=Reliability()$Reliability_Investment,y=Reliability()$MTTF)
    p <- ggplot(data = plot_data, aes(x=x,y=y))+geom_point(color="blue") +xlab("Reliability Investment") + ylab("Mean time to Failure") +ggtitle("MTTF")
    p
  })

  #number of replacement part
  output$repParts <- renderPlot({ 
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$minunit)
    p 
  })

   
  #cost per subsystem vs investment
  output$Cost <- renderPlot({ 
    #if(Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)] > Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)]) X <-  Subsystem()$sub1$Reliability_Investment else X <- Subsystem()$sub2$Reliability_Investment
    X <-  Subsystem()$sub1$Reliability_Investment

    y1 <- Subsystem()$sub1$cst + X/30
    y2 <- Subsystem()$sub2$cst + X/30
    y3 <- y1 + y2 #Subsystem()$numUnits
    
    length(y3) = length(y1)  = length(y2) = length(X)

    plot_data <- data.frame(x = X, "Subsystem 1" = y1, "Subsystem 2" = y2, "System" = y3)
    
    #plot_data <- data.frame(x = Reliability()$Reliability_Investment, "Subsystem 1" = Subsystem()$sub1$cst, "Subsystem 2" = Subsystem()$sub2$cst)
    
    plot_data_long <- melt(plot_data, id = "x")
    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Cost ($)")+ xlab("Reliability Investment ($)")
    p
  })
  

  #total fleet size
  output$fleetSize <- renderPlot({ 
    if(Subsystem()$sub1$Reliability_Investment[length(Subsystem()$sub1$Reliability_Investment)] > Subsystem()$sub2$Reliability_Investment[length(Subsystem()$sub2$Reliability_Investment)]) X <-  Subsystem()$sub1$Reliability_Investment else X <- Subsystem()$sub2$Reliability_Investment
    y1 <- Subsystem()$sub1$numUnits
    y2 <- Subsystem()$sub2$numUnits
    y3 <- Subsystem()$numUnits
    length(y3) = length(y1)  = length(y2) = length(X)
    plot_data <- data.frame(x = X, "Subsystem 1" = y1, "Subsystem 2" = y2, "System" = y3)
    plot_data_long <- melt(plot_data, id = "x")

    p <- ggplot(data = plot_data_long, aes(x = x, y = value, colour = variable)) + geom_point() +ylab("Units")+ xlab("Reliability Investment ($)")
    p
  })

  output$Aff <- renderPlot({ 
    p <- qplot(Reliability()$Reliability_Investment,Reliability()$Aff)
    p
  })
  output$pt <- renderPlot({ 
    qq <- function (n) {
     # x <-  seq(0, input$Reliability_Investment_input1, len=n)
      y <-  seq(0, input$Ci1, len=n)
      x <- seq(0,input$Amode_fail_rate1, len=n)
    #  y <- seq(0,input$Bmode_FEF1, len=n)
      
      df <- expand.grid(x=x, y=y)
      
      df$a <-MTTF_Function(input$Reliability_Investment_input1,df$y,input$Cost_Increment1,df$x,input$Bmode_fail_rate1,input$Bmode_FEF1)
      df$b <- repParts(input$Ttime1,df$a)
      df$z <- Cost(df$b,input$Ci1)
     # df$z <- Cost(repParts(input$Ttime1,MTTF_Function(x,input$C01,y,input$Amode_fail_rate1,input$Bmode_fail_rate1,input$Bmode_FEF1)),input$Ci1)
     # df$z <- Cost(repParts(input$Ttime1,MTTF_Function(x,input$C01,input$Cost_Increment1,input$Amode_fail_rate1,input$Bmode_fail_rate1,y)),input$Ci1)
      
      df
    }
    


    p <- ggplot(qq(50),aes(x=x,y=y,fill=z))
    
 
    p <- p + geom_tile() +xlab("init cost") +ylab("amode") +scale_fill_gradient(low="red",high = "blue")
    # <- qq(50)
    #p <- persp(q$x,q$y,q$z)
    p
  })
output$TA <- renderPlot({
  ggplot(melt(volcano), aes(x=Var1, y=Var2, fill=value)) + geom_tile()  +scale_fill_gradient("Desirability",low="red",high = "blue")+ xlab("Reliability Investment")+ylab("Cost") +ggtitle("Desirability")
})
  output$MTTF_Output<-  renderText({
    Reliability()$numUnits
  })
  output$ava <-renderPlot({
    plot_data <-data.frame(x=Reliability()$Reliability_Investment,y=(Reliability()$MTTF/1800+.4))
    p <- ggplot(data = plot_data, aes(x=x,y=y))+geom_point(color="black") + xlab("Reliability Investment")+ylab("Availability")
    p
  })
  output$ui <- renderUI({
  })
  derp <- reactive({
    pot <- 1000000000- Subsystem()$unitCost
    pan <- pot / Reliability()$Reliability_Investment
    pan
  })

})
