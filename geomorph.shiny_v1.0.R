library(shiny)

#gm.shiny <- function(x) {
  runApp(list(
  ui = fluidPage( 
    h3( "Explore shape space"), 
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, selectInput(inputId = "plot_x",
                    label = "x-axis",
                    choices = colnames(x$scores),
                    selected = colnames(x$scores)[1])),
          column(6, selectInput(inputId = "plot_y",
                    label = "y-axis",
                    choices = colnames(x$scores),
                    selected = colnames(x$scores)[2]))
          ),
        
        sliderInput(inputId = "pt_mag",
                    label = "Scatter point size",
                    min = 1, max = 3, value = 1, step = 0.1),
        
        selectInput(inputId = "grp_var",
                    label = "Grouping variable",
                    choices = c("none", names(x$groups)))
        
        
      ),
      
      mainPanel(
        plotOutput(outputId = "scatter", 
                   click = "plot_click"),
        fluidRow(
          column(9, plotOutput(outputId = "shapes")),
          column(3, conditionalPanel(condition = "!is.null(input.plot_click)", 
                                     selectInput(inputId = "tps_type",
                                                 label = "Plot method",
                                                 choices = c("TPS", "vector", "points", "surface"),
                                                 selected = "TPS")),
                    conditionalPanel(condition = "!is.null(input.plot_click)",
                                           sliderInput(inputId = "sh_mag",
                                                 label = "Magnification",
                                                 min = 1, max = 10, value = 1, step = 1)))
        )
        
      )
    )
  ),
  
  server = function(input, output){
    output$scatter <- renderPlot({
      a1 <- input$plot_x; a2 <- input$plot_y
      par(mgp = c(2, 0.5, 0), mar = c(4, 4, 0.1, 1))
      plot(x$scores[,c(a1, a2)], asp = 1, font.lab=2, cex.lab=1.5, pch = 21,
           xlab = paste(a1, " - ", round(100*x$imp[2, a1], 2), "%", sep=""),
           ylab = paste(a2, " - ", round(100*x$imp[2, a2], 2), "%", sep=""),
           bg = x$groups[[input$grp_var]], cex = input$pt_mag)
      if(input$grp_var!="none"){
        legend("topleft", legend = levels(x$groups[[input$grp_var]]), 
               pch = 21, pt.bg = unique(x$groups[[input$grp_var]]), cex=1.25)
      }
    })
    
    output$shapes <- renderPlot({
      if(is.null(input$plot_click)) {plot.new()} else {
        pred.coords <- c(input$plot_click$x, input$plot_click$y)
        pred <- shape.predictor(x$gpa.coords, x$scores[,c(input$plot_x, input$plot_y)], 
                              pred = pred.coords)
        par(mai = c(0,0,0,0), fig=c(0, 1, 0.5, 1))
        plotRefToTarget(x$consensus, pred[[1]], links = x$links, mag = input$sh_mag,
                      method = input$tps_type)}
    })
    }
  ))
#}
