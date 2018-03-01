library(shiny)
library(geomorph)

# USER INTERFACE ####
ui <- fluidPage(
  # Title
  titlePanel("Explore shape space"),
  # Sidebar layout with input and output definitions
  sidebarLayout(position = "left",
                
    # Main panel for graphs
    mainPanel(
      # Output: scatterplot
      plotOutput(outputId = "scatter", 
                 click = clickOpts(id="plot_click"))
    ),
    
    # Sidebar panel for options
    sidebarPanel(
      # Procrustes residuals - expand to tps, nts, morfologica?
      fileInput(inputId = "gpa_data",
                label = "Import superimposed coordinates",
                accept = c(".bin", ".RData"),
                buttonLabel = "Browse files"),
      # Choose plot type
      # ADD ALLOMETRY OPTIONS IN THE FUTURE
      selectInput(inputId = "plot_type",
                  label = "Plot type",
                  choices = c("PCA", "Allometry"),
                  selected = "PCA"),
      # Choose axes: 
      #selectInput(inputId = "plot_x",
      #            label = "Choose x axis",
      #            choices = paste("PC", 1:axisN, sep=""),
      #            selected = "PC1"),
      #selectInput(inputId = "plot_y",
      #            label = "Choose y axis",
      #            choices = paste("PC", 1:axisN, sep=""),
      #            selected = "PC2"),
      
      
      # Groups file
      fileInput(inputId = "gp_file",
                label = "Import group info",
                accept = c(".txt", ".csv"),
                buttonLabel = "Browse files"),
      # Grouping variable
      # Needs to feed from colnames(gp_file)
      #selectInput(inputId = "gps",
      #            label = "Grouping variable",
      #            choices = XXX,
      #            selected = XXX[1])
      
      # Magnitude for deformation grids
      sliderInput(inputId = "mag",
                   label = "Magnitude",
                   min = 1, max = 10, value = 1, step = 1)
      
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
        axisN <- ncol(x)
      }
      # Chosen axes (to be removed in the future)
#      a1 <- output$plot_x; a2 <- output$plot_y       #Link to axis choice widget input
      a1 <- 1; a2 <- 2
      plot(x[,c(a1, a2)], asp = 1)  # Use geomorph PCA plotting
    }
)
  
}

# RUN ####
shinyApp(ui = ui, server = server)
