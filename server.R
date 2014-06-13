options(shiny.maxRequestSize=30*1024^2) # increases memory avaliable for uploading .osm files - default is 5mb(?)

library(shiny)
library(plyr)
library(sp)
library(rgeos)
library(rgdal) 
library(ggplot2)

shinyServer(function(input, output,session) {
  # 1st input file:
  datasetInput <- reactive({  
    inFile <- input$data
    
    if (is.null(inFile))
      return(NULL)
    
    library(osmar)
    ua3 <- get_osm(complete_file(), source = osmsource_file(inFile$datapath))
  #  ua3 <- get_osm(complete_file(), source = osmsource_file("map(4).osm"))
  # ua4 <- get_osm(complete_file(), source = osmsource_file("dunbar.osm"))
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
  
  plotData3 <- reactive({
    inFile <- input$data
    
    if (is.null(inFile))
      return(NULL)
    library(osmar) 
   ua3 <- datasetInput()
    bg_ids2 <- find(ua3, way(tags(k == "building" & v != "NA")))    # find ways tagged 'building' in ua3
   ga_ids <- find(ua3, way(tags(k == "building" & v == "garage")))
   pkg_ids <- find(ua3, way(tags(k == "amenity" & v == "parking")))
   pg_ids <- find(ua3, way(tags(k == "leisure" & v == "playground"))) 
   pk_ids <- find(ua3, way(tags(k == "leisure" & v == "park")))
   gd_ids <- find(ua3, way(tags(k == "leisure" & v == "garden")))  
   sp_ids <- find(ua3, way(tags(k== "leisure" & v == "pitch")))
   gf_ids <- find(ua3, way(tags(k== "leisure" & v== "golf_course")))
   wd_ids <- find(ua3, way(tags(k == "natural" & v =="wood")))
   
   bg_ids2 <- find_down(ua3, way(bg_ids2))
   ga_ids <- find_down(ua3, way(ga_ids))
   pkg_ids <- find_down(ua3, way(pkg_ids))
   pg_ids <- find_down(ua3, way(pg_ids))
   pk_ids<- find_down(ua3, way(pk_ids))
   gd_ids <- find_down(ua3, way(gd_ids))
   sp_ids <- find_down(ua3, way(sp_ids))
   gf_ids <- find_down(ua3, way(gf_ids))
   wd_ids <- find_down(ua3, way(wd_ids))
  
   bg2_sub <- subset(ua3, ids = bg_ids2)# find nodes in ways tagged 'building'
   if (length(unique( bg2_sub$ways$tags$id)) == 0){
     remove(bg_sub2)
   }
   ga_sub <- subset(ua3, ids =  ga_ids)
   if (length(unique( ga_sub$ways$tags$id)) == 0){
     remove(ga_sub)
   }     
   pkg_sub <- subset(ua3, ids =  pkg_ids)
   if (length(unique( pkg_sub$ways$tags$id)) == 0){
     remove(pkg_sub)
   }
   pg_sub <- subset(ua3, ids = pg_ids)
   if (length(unique( pg_sub$ways$tags$id)) == 0){
     remove(pg_sub)
   }
   pk_sub <- subset(ua3, ids =  pk_ids)
   if (length(unique( pk_sub$ways$tags$id)) == 0){
     remove(pk_sub)
   }
   gd_sub <-subset(ua3, ids = gd_ids)
   if (length(unique(gd_sub$ways$tags$id)) == 0){
     remove(gd_sub)
   }
   sp_sub <- subset(ua3, ids =  sp_ids)
   if (length(unique( sp_sub$ways$tags$id)) == 0){
     remove(sp_sub)
   }
   gf_sub <- subset(ua3, ids =  gf_ids)
   if (length(unique( gf_sub$ways$tags$id)) == 0){
     remove(gf_sub)
   }
   wd_sub <- subset(ua3, ids =  wd_ids)
   if (length(unique(wd_sub$ways$tags$id)) == 0){
     remove(wd_sub)
   }
   
   remove(ids_sub)
   
   ids_sub <-  ls(pattern = "_sub")
   ids_sub <- lapply( ids_sub,function(x){
     id_sub <- as.name(x)   
     return( id_sub)
   })
          
   polys <- lapply(ids_sub ,function(x){
                     
     lan <- na.omit(as_sp(eval(x), "polygons"))
     
     return(lan)
   })
   
    detach("package:osmar", unload=TRUE) # remove osmar here to avoid conflicts
   
   utms <- lapply(polys, function(x){
     
     utm <- spTransform(x,CRS("+proj=utm"))
     return(utm)
   })
   
   area <- lapply(utms,function(x){
     
     areas <- gArea(x) / 10000
     return(areas)
   })

  remove(ids_sub)
  land <-  ls(pattern = "_sub")
  dflan <- as.data.frame(cbind(land,area))
  

  return(dflan)
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

output$table4 <- renderTable({
  plotData3 ()
  
})
  
# for csv output? not tested yet:
  output$downloadTest <- downloadHandler(
    filename = function() { paste(input$file1, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
})
