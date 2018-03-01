library(shiny)
library(geomorph)

# USER INTERFACE ####
ui <- fluidPage(
  # Title
  title = "Explore shape space",
  plotOutput(outputId = "scatter", 
             click = clickOpts(id="plot_click")),
  hr(),
             
  fluidRow(
    column(4, 
           h4("Explore shape space"),
           # Procrustes residuals - expand to tps, nts, morfologica?
           fileInput(inputId = "gpa_data",
                     label = "Import superimposed data",
                     accept = c(".bin", ".RData"),
                     buttonLabel = "Browse"),
           # Specimen info file
           fileInput(inputId = "spec_file",
                     label = "Import specimen info",
                     accept = c(".txt", ".csv"),
                     buttonLabel = "Browse")
    ),
    column(2,
           # Choose plot type
           selectInput(inputId = "plot_type",
                       label = "Plot type",
                        choices = c("PCA", "Allometry"),
                        selected = "PCA"),
           conditionalPanel(condition = "input.plot_type == 'PCA'",
                            selectInput(inputId = "plot_x",
                                        label = "Select x-axis",
                                        choices = paste("PC", 1:5, sep=""))),
           conditionalPanel(condition = "input.plot_type == 'PCA'",
                            selectInput(inputId = "plot_y",
                                        label = "Select y-axis",
                                        choices = paste("PC", 1:5, sep=""),
                                        selected = "PC2")),
           conditionalPanel(condition = "input.plot_type == 'Allometry'",
                            selectInput(inputId = "allometry_type",
                                        label = "Allometry plot type",
                                        choices = c("CAC", "RegScore", "PredLine"))),
            # Define size var CHANGE TO DROPDOWN selectInput WITH NAMES FROM FILE
           conditionalPanel(condition = "input.plot_type == 'Allometry'",
                            textInput(inputId = "CS",
                                        label = "Indicate size variable"))),
           
    column(3
      # Grouping variable
      # Needs to feed from colnames(gp_file)
      #selectInput(inputId = "gps",
      #            label = "Grouping variable",
      #            choices = XXX,
      #            selected = XXX[1])
    ),
    column(3, 
            # Magnitude for deformation grids
            sliderInput(inputId = "mag",
                         label = "Magnitude",
                         min = 1, max = 10, value = 1, step = 1),
            # Links file
            fileInput(inputId = "links",
                      label = "Import links",
                      accept = ".txt",
                      buttonLabel = "Browse")
           
    )
  )
)


# SERVER ####
server <- function(input, output) {
  # Shape space scatterplot
  output$scatter <- renderPlot(
    if(is.null(input$gpa_data)){
      plot.new()
    } else {
      # Calculate plot data
      sh <- get(load(input$gpa_data[1,"datapath"]))
      if(input$plot_type=="PCA"){
        x <- prcomp(two.d.array(sh))$x  #Link to geomorph pca function
        a1 <- input$plot_x; a2 <- input$plot_y      
        layout(matrix(1:2, nrow=1), widths = c(3, 1))
        par(mgp=c(1.75, 0.5, 0))
        plot(x[,c(a1, a2)], asp = 1, font.lab=2, cex.lab=1.5)  # Use geomorph PCA plotting
      }
      if (input$plot_type=="Allometry"){
        mod1 <- procD.allometry(sh ~ input$CS)
        plot.procD.allometry(x = mod1, 
                             method = input$allometry_type)
      }
    }
)
  
}

# RUN ####
shinyApp(ui = ui, server = server)
