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
  codes <- ua3$nodes$tags
  codes$k <- as.character(codes$k)
  codes$v <- as.character(codes$v)
  codes$key <-paste(codes[,2],codes[,3],sep="=")
  ua3$c <- codes
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
    codes <- ua3$nodes$tags
    codes$k <- as.character(codes$k)
    codes$v <- as.character(codes$v)
    codes$key <-paste(codes[,2],codes[,3],sep="=")
    ua3$c <- codes
    ua3    
  })
  
  textInput <- reactive({
    inFile <- input$data
    if (is.null(inFile))
      return('Awaiting file')
    text <- inFile$name   
    text
  }) 
  
  textInput2 <- reactive({
    inFile <- input$dataTwo
      if (is.null(inFile))
      return('Awaiting file')
      text <- inFile$name  
    text
  }) 
  
  # drop down filter list:
  getFilters <- reactive({
    inFile <- input$data
    inFile2 <- input$dataTwo
    codes2 <- 'Choose:'
          if(!is.null(inFile)){
       ua5 <-  datasetInput()    
          ua4 <- datasetInputTwo()
       codes2 <- unique(c(ua5$c$key,ua4$c$key,codes2))
     codes2 <- sort(codes2)
          }
    return(codes2)
  })
  # update drop down list as inpupt files change:
  observe({
    updateSelectInput(session,"osm1",label = "Key=Value", choices = getFilters())
      })
  
  plotData <- reactive({ 
    inFile <- input$data
  #  ifEmpty <- data.frame("Load", "Data")
   # colnames(  ifEmpty ) <- c("key","count")
    if (is.null(inFile))
      return(NULL)
   ua3 <- datasetInput()
  ua3 <- ua3$c
     ua3 <- ddply(ua3,"key",summarise,count=length(na.omit(v))) 
  ua3 <- lapply(input$osm1, function(x){
    ua3 <- eval(parse(text=paste("ua3[ua3$key == \"", x, "\", ]",sep=""))) 
   return(ua3) 
  })

 ua3 <- as.data.frame(do.call(rbind,ua3))

  })
  
plotData2 <- reactive({ 
    inFile <- input$dataTwo
  #     ifEmpty <- data.frame(list("Load", "Data"))
  #  colnames(  ifEmpty ) <- c("key","count")
    if (is.null(inFile))
      return(NULL)
    ua3 <- datasetInputTwo()
    ua3 <- ua3$c
    ua3 <- ddply(ua3,"key",summarise,count=length(na.omit(v))) 
    ua3 <- lapply(input$osm1, function(x){
      ua3 <- eval(parse(text=paste("ua3[ua3$key == \"", x, "\", ]",sep=""))) 
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

output$plotArea1 <- renderPlot({
  inFile <- input$data
  if (is.null(inFile))
    return(NULL)
    data3 <-   plotData3()
    data3<- as.data.frame(cbind( data3[1], data3[3])) 
    print(qplot(as.character(unlist(Landuse)), weight = as.numeric(data3$'Area in football fields'), ylab = colnames(data3[2]), xlab = colnames(data3[1]),data = data3, geom = "bar",fill=as.character(unlist(Landuse))))
   })

output$plotArea2 <- renderPlot({
  inFile <- input$dataTwo
  if (is.null(inFile))
    return(NULL)
  data3<- plotData4()
  data3<- as.data.frame(cbind( data3[1], data3[3])) 

 print(qplot(as.character(unlist(Landuse)), weight = as.numeric(data3$'Area in football fields'), ylab = colnames(data3[2]), xlab = colnames(data3[1]),data = data3, geom = "bar",fill=as.character(unlist(Landuse))))

})
            
output$table2 <- renderDataTable({
  dataset <- plotData()
  dataset$key <- as.character(dataset$key)
  #dataset$c <- NULL
  dataset$count <- as.numeric(dataset$count)
  return(dataset)
 }, 
 options = list(aLengthMenu = F, iDisplayLength = 10,bFilter = FALSE,bMenu=F))

output$table3 <-renderDataTable({
  dataset <- plotData2()
  dataset$key <- as.character(dataset$key)
 # dataset$c <- NULL
 dataset$count <- as.numeric(dataset$count)
  return(dataset)
 }, 
 options = list(aLengthMenu = F, iDisplayLength = 10,bFilter = FALSE,bMenu=F))


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
