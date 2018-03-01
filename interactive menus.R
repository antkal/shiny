runApp(list(
  ui = fluidPage(
    fileInput(inputId = "spec_info",
              label = "Import specimen info",
              accept = c(".txt", ".csv"),
              buttonLabel = "Browse"),
    # Grouping var selector
    uiOutput("gps")
    
  ),
  server = function(input, output) {
    # Menu from group info
    output$gps <- renderUI({
      if(is.null(input$spec_info)){
        selectInput("groups", "Select grouping var", "")
      } else {
      x <- read.csv(input$spec_info[1,"datapath"])
      selectInput("groups", "Select grouping var", colnames(x))
    }})

      }
  
))

