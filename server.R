options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(plyr)


shinyServer(function(input, output,session) {
  
  datasetInput <- reactive({
    inFile <- input$data
    
    if (is.null(inFile))
      return(NULL)
    
    library(osmar)
    ua3 <- get_osm(complete_file(), source = osmsource_file(inFile$datapath))
  #  ua3 <- get_osm(complete_file(), source = osmsource_file("map(4).osm"))
   detach("package:osmar", unload=TRUE)
  ua3
  })
  
  datasetInputTwo <- reactive({
    inFile <- input$dataTwo
    
    if (is.null(inFile))
      return(NULL)
    
    library(osmar)
    ua3 <- get_osm(complete_file(), source = osmsource_file(inFile$datapath))
    #  ua3 <- get_osm(complete_file(), source = osmsource_file("map(4).osm"))
    detach("package:osmar", unload=TRUE)
    ua3
  })
  
  getFilters <- reactive({
    inFile <- input$data
    codes2 <- 'NULL'
      if(!is.null(inFile$datapath)){
     # library(osmar)
      ua5 <-  datasetInput()
      ua4 <- datasetInputTwo()
        #get_osm(complete_file(), source = osmsource_file(inFile$datapath))
     #detach("package:osmar", unload=TRUE)
      codes <- unique(ua5$nodes$tags)
     codesTwo <- unique(ua4$nodes$tags)
      codes <- as.character(codes$k)
     codesTwo <- as.character(codesTwo$k)
      codes2 <- unique(c(codes2,codes,codesTwo))
    }
    return(codes2)
  })
  
  observe({
    updateSelectInput(session,"osm1",label = "Filter1", choices = getFilters())
      })
  
  plotData <- reactive({ 
    inFile <- input$data
    
    if (is.null(inFile))
      return(NULL)
  ua3 <- datasetInput()
   ua3 <- ua3$nodes$tags
   ua3 <- ddply(ua3,"k",summarise,count=length(na.omit(v))) 
   ua3 <- eval(parse(text=paste("ua3[ua3$k == \"", input$osm1, "\", ]",sep=""))) 
  })
  
  plotData2 <- reactive({ 
    inFile <- input$dataTwo
    
    if (is.null(inFile))
      return(NULL)
    ua3 <- datasetInputTwo()
    ua3 <- ua3$nodes$tags
    ua3 <- ddply(ua3,"k",summarise,count=length(na.omit(v))) 
    ua3 <- eval(parse(text=paste("ua3[ua3$k == \"", input$osm1, "\", ]",sep=""))) 
  })
  
  output$table <- renderTable({
    inFile <- input$data
        if (is.null(inFile))
      return(NULL)
    ua3 <- datasetInput()
    ua3 <- ua3$nodes$tags
    ua3 <- ddply(ua3,"k",summarise,count=length(na.omit(v))) 

  })

output$table2 <- renderTable({
  plotData()
  
})

output$table3 <- renderTable({
  plotData2()
  
})
  
 # output$filter <- renderTable({
#    ua3 <-  datasetInput()
 #   ua3 <- unique(ua3$nodes$tags$k)
#  })
  

  output$downloadTest <- downloadHandler(
    filename = function() { paste(input$file1, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
})
