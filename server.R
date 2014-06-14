options(shiny.maxRequestSize=30*1024^2) # increases memory avaliable for uploading .osm files - default is 5mb(?)

library(shiny)
library(plyr)
library(sp)
library(rgeos)
library(rgdal) 
library(ggplot2)
library(reshape2)

shinyServer(function(input, output,session) {
  # 1st input file:
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
  # 2nd input file:
  datasetInputTwo <- reactive({ 
    inFile <- input$dataTwo
    
    if (is.null(inFile))
      return(NULL)
    
    library(osmar)
    ua3 <- get_osm(complete_file(), source = osmsource_file(inFile$datapath))
       detach("package:osmar", unload=TRUE)
    ua3
  })
  
  textInput <- reactive({
    inFile <- input$data
    if (is.null(inFile))
      return(NULL)
    text <- inFile$name   
    text
  }) 
  
  textInput2 <- reactive({
    inFile <- input$dataTwo
      if (is.null(inFile))
      return(NULL)
      text <- inFile$name  
    text
  }) 
  
  # drop down filter list:
  getFilters <- reactive({
    inFile <- input$data
    codes2 <- 'NULL'
      if(!is.null(inFile$datapath)){
       ua5 <-  datasetInput()
      ua4 <- datasetInputTwo()
       codes <- unique(ua5$nodes$tags)
     codesTwo <- unique(ua4$nodes$tags)
      codes <- as.character(codes$k)
     codesTwo <- as.character(codesTwo$k)
      codes2 <- unique(c(codes2,codes,codesTwo))
    }
    return(codes2)
  })
  # update drop down list as inpupt files change:
  observe({
    updateSelectInput(session,"osm1",label = "Filter1", choices = getFilters())
      })
  
  plotData <- reactive({ 
    inFile <- input$data
    ifEmpty <- data.frame("Load", "Data")
    colnames(  ifEmpty ) <- c("k","count")
    if (is.null(inFile))
      return(ifEmpty)
    allsites <- data.frame("Load Data")
    colnames(allsites) <- c("")
  ua3 <- datasetInput()
   ua3 <- ua3$nodes$tags
   ua3 <- ddply(ua3,"k",summarise,count=length(na.omit(v))) 
  ua3 <- lapply(input$osm1, function(x){
    ua3 <- eval(parse(text=paste("ua3[ua3$k == \"", x, "\", ]",sep=""))) 
   return(ua3) 
  })

 ua3 <- as.data.frame(do.call(rbind,ua3))

  })
  
plotData2 <- reactive({ 
    inFile <- input$dataTwo
       ifEmpty <- data.frame(list("Load", "Data"))
    colnames(  ifEmpty ) <- c("k","count")
    ifEmpty$k <- as.character(ifEmpty$k)
    ifEmpty$count <- as.character(ifEmpty$count)
    if (is.null(inFile))
      return(ifEmpty)
    ua3 <- datasetInputTwo()
    ua3 <- ua3$nodes$tags
    ua3 <- ddply(ua3,"k",summarise,count=length(na.omit(v))) 
    ua3 <- lapply(input$osm1, function(x){
      ua3 <- eval(parse(text=paste("ua3[ua3$k == \"", x, "\", ]",sep=""))) 
      return(ua3) 
  })
  
  ua3 <- as.data.frame(do.call(rbind,ua3))
  
  })
  
plotData3 <- reactive({
    inFile <- input$data
   if (is.null(inFile))
      return(NULL)
    library(osmar) 
   ua3 <- datasetInput()
   source("areaTable.R")
   ua3 <- areaTable(ua3)
   return(ua3)
   })
  
plotData4 <- reactive({
  inFile <- input$dataTwo
   if (is.null(inFile))
    return(NULL)
  library(osmar) 
  ua3 <- datasetInputTwo()
  source("areaTable.R")
  ua3 <- areaTable(ua3)
  return(ua3)
})
  
output$text1 <- renderText({
   text <- textInput()
   })
output$text2 <- renderText({
 text <- textInput2() 
})
output$text3 <- renderText({
  text <- textInput()
})
output$text4 <- renderText({
  text <- textInput2() 
})

output$table <- renderTable({
    inFile <- input$data
        if (is.null(inFile))
      return(NULL)
    ua3 <- datasetInput()
    ua3 <- ua3$nodes$tags
    ua3 <- ddply(ua3,"k",summarise,count=length(na.omit(v))) 

  })


output$table2 <- renderDataTable({
  dataset <- plotData()
  dataset$k <- as.character(dataset$k)
  dataset$count <- as.character(dataset$count)
  return(dataset)
 }, 
 options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10))

output$table3 <-renderDataTable({
  dataset <- plotData2()
  dataset$k <- as.character(dataset$k)
 dataset$count <- as.character(dataset$count)
  return(dataset)
 }, 
 options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10))


output$table4 <- renderTable(
  plotData3()
  
 )

output$table5 <- renderTable(
  plotData4()
  
)
  
# for csv output? not tested yet:
  output$downloadTest <- downloadHandler(
    filename = function() { paste(input$file1, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
})
