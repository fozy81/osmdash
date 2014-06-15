areaTable <- function(ua3){
All_buildings <- find(ua3, way(tags(k == "building" & v != "NA")))    # find ways tagged 'building' in ua3
Domestic_garages <- find(ua3, way(tags(k == "building" & v == "garage")))
Car_Parking <- find(ua3, way(tags(k == "amenity" & v == "parking")))
Play_areas <- find(ua3, way(tags(k == "leisure" & v == "playground"))) 
Public_parks <- find(ua3, way(tags(k == "leisure" & v == "park")))
Gardens_area <- find(ua3, way(tags(k == "leisure" & v == "garden")))  
Sports_pitchs <- find(ua3, way(tags(k== "leisure" & v == "pitch")))
Golf_courses <- find(ua3, way(tags(k== "leisure" & v== "golf_course")))
Woodland_areas <- find(ua3, way(tags(k == "natural" & v =="wood")))

landuse <-  ls(pattern = "_")
ids <- lapply( landuse,function(x){
  id <- as.name(x)   
  return( id)
})

findDown <- lapply(ids ,function(x){
  
  find <- find_down(ua3, way(eval(x)))
  
  return(find)
})

subsetIds <- lapply(findDown, function(x){
  
  subIds <- subset(ua3, ids = x)
  return(subIds)
})

for (i in 1:length(subsetIds)){
  subsetIds[[i]][5]<- landuse[i]
}

rmIds <- lapply(subsetIds, function(x){
  
  if (length(unique(x$ways$tags$id)) > 0){
    return(x) }
  
})

rmIds <-  rmIds[!unlist(lapply( rmIds, is.null))]

polys <- lapply(rmIds ,function(x){
  
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

land <- lapply(rmIds, function(x){
  names <- x[5]
  return(names)
})
dfLand <- as.data.frame(cbind(land,area))
colnames(dfLand) <- c("Landuse","Area in hectares")
dfLand$'Area in football fields'<- round(unlist(dfLand$'Area in hectares') *0.62, digits=1)

return(dfLand)
}
