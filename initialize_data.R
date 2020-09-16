load_libraries <- function() {
    install.packages("rworldmap")
    install.packages("spdep")
    #install.packages('spacestat') #not installing correctly
    #library(spacestat) 
    library(readxl)
    library(tidyverse)
    library(data.table)
    library(dplyr)
    library(readr)
    library(ggplot2)
    library(rworldmap)
    library(maptools)
    library(maps)
    library(RColorBrewer)
    library(repr)
    library(ggmap) #DataViz module requirement
    library(plotly) #DataViz module requirement
    library(ggthemes) #DataViz module requirement
    library(rgdal) #DataViz module requirement
    library(leaflet) #DataViz module requirement
    options(stringsAsFactors = FALSE)
    library(rgeos)
    library(spdep)
    library(classInt)
    library(sp)
  
    print("libraries loaded successfully")
}

loadFiles <- function(p) {
    if(FALSE) {
    "       
        Input: Directory of CSVs
        Output: Dataframe of merged CSVs
        Process: Loads all files from data folder into a list and merges all files into one data frame
        Verified by: BP, WCM, JAW, EWGS
        Author: EWGS
        Last Modified: 6/12
    "
    } #Documentation
    
    df <- list.files(path = p, pattern = ".csv", full.names = TRUE) %>% 
        lapply(read_csv, col_types = cols()) %>% #modified to supress the status messages 
        bind_rows
    
    message(paste("Files in \"", path, "\" loaded.", sep = ""))
    return(df)
}

reqCols <- function(df) {
    if(FALSE) {
    "
        Input: Raw dataframe
        Output: Subset of dataframe for needed columns
        Process: Hardcoded columns to be used, na omitted, deduplicated rows
        Verified by: BP, WCM, JAW, EWGS
        Author: JAW
        Last Modified: 6/12
    "
    } #Documentation

    df <- subset(df, select = c(
        'time',
        'latitude',
        'longitude',
        'depth',
        'mag',
        'id',
        'updated'
    ))
    df <- na.omit(df)
    df <- unique(df)
    
    message("Subset Completed.")
    return(df)
}

parseDt <- function(df, col) {
    if(FALSE) {
    "
        Input: Dataframe with a POSIXct datetime column
        Output: Adds a column with the date, time, year into their own columns
        Process: Isolates the dates and times from df$time and df$updated into their own columns.
        Verified by: BP, WCM, JAW, EWGS
        Author: EWGS
        Last Modified: 6/12
    "
    } #Documentation
    
    #New columns will be named by: col + "_suffix"
    newDate = paste(col, "Date", sep = "_")
    newTime = paste(col, "Time", sep = "_")
    newYear = paste(col, "Year", sep = "_")
    
    df[, newDate] <- as.Date(df[[col]])
    df[, newTime] <- as.ITime(strftime(df[[col]], format = "%H:%M:%S"))
    df[, newYear] <- as.numeric(format(df[[col]], '%Y'))
    
    message(paste("Parsed column: ", col, ".", sep = ""))
    return(df)
}

tospdf <- function(df) {
    if(FALSE) {
    "
        Input: Raw dataframe
        Output: Subset of dataframe for needed columns
        Process: Hardcoded columns to be used, na omitted, deduplicated rows
        Verified by: 
        Author: JAW
        Last Modified: 6/13
    "
    } #Documentation
    df <- na.omit(df)
    df <- subset(df, select = c(
        'time',
        'latitude',
        'longitude',
        'depth',
        'mag',
        'id',
        'updated'
    ))
    #we select all the columns from the general df

    lats <- df$latitude
    longs <- df$longitude
    #we want to pick out the lat lon cordinates to store the location of each of the df attributes

    quakeattributes <- df[,c('depth','mag','time', 'id', 'updated')]
    #here we then assign the rest of the coulmns to the attributes
    
    adddf <- as.data.frame(quakeattributes)
    #make attributes into it's own df
    
    ptcoords <- cbind(as.numeric(longs),as.numeric(lats))
    #we want to perpare the cordinates for our SpatialPointsDataFrame function
    
    spts <- SpatialPointsDataFrame(ptcoords,data=adddf, proj4string=CRS("+proj=longlat +ellps=WGS84"))
    #we now have a spatial points df that we can use to create maps and in our analysis
    message(paste("spatial df created"))
    return(spts)
}

regions_data <- function(dfsp) {
#jessica

    #transform data into another datum for better readability 
    tpts <- spTransform(dfsp,  CRS("+init=epsg:4087"))

    #load region with homogeneous seismic conditions 
    tectonicdata = "~/jupyter/cs2019_Group11/GroupProducts/fe.kmz"
    tectonicFeatures <- readOGR(tectonicdata)

    #also transform data into same regions as the earthquake data for later comparisons
    transTectonicFeatures <- spTransform(tectonicFeatures,  CRS("+init=epsg:4087"))

    #add empty names row to be added to 
    tpts@data$newname = "empty"

    #start for loop to go through each seismic homogeneous region
    for (i in 1:nrow(transTectonicFeatures)) {
        region = transTectonicFeatures[i, ]  

    #here we determine which region each earthquake belongs to
    selFeat <- overGeomGeom(region, tpts, returnList = TRUE, fn = NULL)

    #Unlist the list
    intSet = unlist(selFeat)
    
    #print to check to ensure that it ran correctly
    #print(i)
    
    #we want to now add an attribute that captures which region each earthquake belongs to
    #to be named there must be at least one earthquake 
    if (length(intSet) > 0){
        tpts@data[intSet,]$newname <- i}
    #if no earthquakes in the region then none are named
    else {
        print(paste("No earthquakes in region: ", i))}
     }
    return(tpts)

}

base_world <- function() {
##Jessica
#this adds tectonic plates to a map for vis 

    world_map <- map_data("world") 
    # this gives the corrdinates for the world's continents to be plotted

    #options(repr.plot.width=44, repr.plot.height=40)
    # I have to make the plot size much larger since 
    #I will be facet wrapping, this way all plots are visible 

    p = ggplot() + coord_fixed() +
      xlab("") + ylab("") 
    #Here I create a blank canvas to map on

    #Add map to base plot
    world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                               colour="light green", fill="light green") 
    # This is where the continents are drawn on my canvas creating the map

    base_world = world_messy + theme_void() 
    #This is where I add my simple theme to the map
    
    return(base_world)
}

base_world_plates <- function() {
##Jessica
#this adds tectonic plates to a map for vis 

    world_map <- map_data("world") 
    # this gives the corrdinates for the world's continents to be plotted

    #options(repr.plot.width=44, repr.plot.height=40)
    # I have to make the plot size much larger since 
    #I will be facet wrapping, this way all plots are visible 

    p = ggplot() + coord_fixed() +
      xlab("") + ylab("") 
    #Here I create a blank canvas to map on

    #Add map to base plot
    world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                               colour="light green", fill="light green") 
    # This is where the continents are drawn on my canvas creating the map

    base_world = world_messy + theme_void() 
    #This is where I add my simple theme to the map
    
    tectonicdata = "~/jupyter/cs2019_Group11/GroupProducts/plate-boundaries.kmz"
    tectonicFeatures <- readOGR(tectonicdata)
    
    base_world_regions = base_world + geom_path(data = tectonicFeatures, 
          aes(x = long, y = lat, group = group),
          color = 'gray', size = .4)
    ##This now becomes a base map for any further mapping  
    return(base_world_plates)
}

base_world_regions <- function() {
##Jessica
#this adds Flinn-Engdahl regions to a map for vis 
    
    world_map <- map_data("world") 
    # this gives the corrdinates for the world's continents to be plotted

    #options(repr.plot.width=44, repr.plot.height=40)
    # I have to make the plot size much larger since 
    #I will be facet wrapping, this way all plots are visible 

    p = ggplot() + coord_fixed() +
      xlab("") + ylab("") 
    #Here I create a blank canvas to map on

    #Add map to base plot
    world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                               colour="light green", fill="light green") 
    # This is where the continents are drawn on my canvas creating the map

    base_world = world_messy + theme_void() 
    #This is where I add my simple theme to the map
    
    regiondata = "~/jupyter/cs2019_Group11/GroupProducts/fe.kmz"
    regionFeatures <- readOGR(regiondata)
    
    base_world_regions = base_world + geom_path(data = regionFeatures, 
          aes(x = long, y = lat, group = group),
          color = 'gray', fill = 'white', size = .2)
    ##This now becomes a base map for any further mapping  
    return(base_world_regions)
}

kmeans_clust_viz <- function(df, n) {
    #Bill
    #creating K Means clusters based on latitude and longitude for each row
    #*****MUST HAVE 'latitude' AND 'longitude' COLUMN NAMES*****

    #set seed for random number generation
    set.seed(15)

    #setting n=12 for k means clusters for 12 tectonic regions
    clusters <- kmeans(select(df,latitude,longitude), n)
    #print(typeof(clusters))
    #print(str(clusters))

    #adding cluster to data frame
    df$cluster <- as.factor(clusters$cluster)
    #head(df_clust)

    #showing cluster for each data point on top of the base_world viz Jessica created earlier
    clust_map <- base_world_regions + geom_point(aes(x = longitude[], y = latitude[], colour = as.factor(cluster)),data = df)
    clust_map   
}


near_neighbors <- function(n, tpts) {
 #Jessica calculate the nearest neighbors per a given region
 # n is region of interest, tpts is a spatial dataframe 
  #if only one value in region will return one entry,
  #otherwise returns summary of the nearest neighbors  
  #the max searching distance is the max distance between nearest points
    
#n is the region which I am wanting to look at within tpst df

    region <- subset(tpts, newname == n)
#I only want earthquakes in that region 
if (length(region) > 1){
        nieghRegion <- knn2nb(knearneigh(region, k=1))
#I am now going to determine the maximum distance to the closest point

    unlist <- max(unlist(nbdists(nieghRegion, region)))
#I am going to now use that distance as the thershold so that every point has at least on neighbor

    gc.nb <- dnearneigh(region, 0, unlist)
    #now i am going to determine the neighbours of region points by Euclidean distance
print(n)
    print(c(summary(gc.nb)))
    return(gc.nb) }
    else {
        print(c(n, "one entry"))}
     }


b.guten <-
function(magn,m0=min(magn)){ #
            x = subset(magn,subset=(magn>m0))
            x = x-m0
            n  = length(x)
            beta = n/sum(x)
            samplevar = beta/sum(x)
    b  = beta/log(10)
    se = sqrt(samplevar/log(10))
            return(c(b,se))
            }
