#rm(list=ls())
source("./scripts/FUNCTIONS_HistoricDataPrep.R")

##### Species observation data

# Read in historic bird counts
DC.birds.1 <- read.csv("./data/bird data/DC/DCDC_data.csv")

MN.birds.2 <- read.csv("./data/bird data/MN/MN18_data.csv")
MN.birds.4 <- read.csv("./data/bird data/MN/MNSP_data.csv")
#note that MN.birds.1 and .3 were originally included, but now removed

PA.birds.1 <- read.csv("./data/bird data/PA/PA2E_data.csv")
PA.birds.2 <- read.csv("./data/bird data/PA/PA2Q_data.csv")
PA.birds.3 <- read.csv("./data/bird data/PA/PAPI_data.csv")


#Clean up species names, remove duplicates, and collapse sub-species
DC.birds.1 <- clean_sppNames(DC.birds.1)

MN.birds.2 <- clean_sppNames(MN.birds.2)
MN.birds.4 <- clean_sppNames(MN.birds.4)

PA.birds.1 <- clean_sppNames(PA.birds.1)
PA.birds.2 <- clean_sppNames(PA.birds.2)
PA.birds.3 <- clean_sppNames(PA.birds.3)

#Create a species list to make all bird counts the same structure
spp <- as.vector(c(DC.birds.1$species.name, 
                   MN.birds.2$species.name,
                   MN.birds.4$species.name,
                   PA.birds.1$species.name,
                   PA.birds.2$species.name,
                   PA.birds.3$species.name))

#Remove duplicates from this species list (vector)
spp <- unique(spp)

#Create template data frame for city-specific data frames
spp <- as.data.frame(spp)
names(spp) <- "species" #change column name to 'species'

MN.birds <- survey_merge(X = MN.birds.2,
                         Y = MN.birds.4,
                         XYspecies.col = "species.name",
                         all.species = spp)

PA.merge1 <- survey_merge(X = PA.birds.1,
                          Y = PA.birds.2,
                          XYspecies.col = "species.name",
                          all.species = spp)

PA.birds <- survey_merge(X = PA.merge1,
                         Y = PA.birds.3,
                         XYspecies.col = "species.name",
                         all.species = spp)

#Making DC dataset match others in name and format
DC.birds <- DC.birds.1[,!colnames(DC.birds.1) %in% "common.name"] #remove common names
DC.birds <- merge(x = spp, #merge DC.birds with standard set of species to keep form consistent across cities
                  y = DC.birds,
                  by.x = "species",
                  by.y =  "species.name",
                  all.x = TRUE,
                  all.y = FALSE)
DC.birds[is.na(DC.birds)] <- 0 #replace all NA's with 0's
names(DC.birds) <- sub(pattern = "X*", #remove the "X" from survey years
                       replacement = "", #replace "X" with noting (aka erase it)
                       x = names(DC.birds))


#Aggregate surveys to five-year periods (e.g., 1901, 1902, 1904 all become 1900)
MN.birds <- survey_collapse(MN.birds) 
PA.birds <- survey_collapse(PA.birds) 
DC.birds <- survey_collapse(DC.birds)

#Rename rows in each data frame
row.names(MN.birds) <- paste("MN",row.names(MN.birds), sep="")
row.names(PA.birds) <- paste("PA",row.names(PA.birds), sep="")
row.names(DC.birds) <- paste("DC",row.names(DC.birds), sep="")

#Combine surveys into single df
bird.data <- rbind(MN.birds, PA.birds, DC.birds) #combine all data frames
bird.data <- bird.data[,!(colSums(bird.data) == 0)] #identify species whose names are present in the dataset but themselves were not seen (likely marked as 'seen in the week, but not on the day')


##### Survey-specific data

keep.landscape <- c("city",
                    "year_rounded",
                    "prcnt_park_area",
                    "mean_park_EIratio",
                    "park_clumpy")

#Process landscape metrics for DC
process_greenspace(city.file = "./data/municipal shapefiles/DC/DistrictOfColumbia_municipal.shp",
                   city.name = "DC",
                   data.path = "./data/park shapefiles/DC",
                   resolution = 20)

park_metrics <- park_metrics[,which(names(park_metrics) %in% keep.landscape)] #remove landscape variables that we won't use in our model
park_metrics$survey <- paste(park_metrics$city, park_metrics$year_rounded, sep="")
DC.park.metrics <- park_metrics


#Process landscape metrics for MN
process_greenspace(city.file = "./data/municipal shapefiles/MN/Minneapolis_municipal.shp",
                   city.name = "MN",
                   data.path = "./data/park shapefiles/MN",
                   resolution = 20)

park_metrics <- park_metrics[,which(names(park_metrics) %in% keep.landscape)] #remove landscape variables that we won't use in our model
park_metrics$survey <- paste(park_metrics$city, park_metrics$year_rounded, sep="")
MN.park.metrics <- park_metrics

#Process landscape metrics for PA
process_greenspace(city.file = "./data/municipal shapefiles/PA/Pittsburgh_municipal.shp",
                   city.name = "PA",
                   data.path = "./data/park shapefiles/PA",
                   resolution = 20)

park_metrics <- park_metrics[,which(names(park_metrics) %in% keep.landscape)] #remove landscape variables that we won't use in our model
park_metrics$survey <- paste(park_metrics$city, park_metrics$year_rounded, sep="")
PA.park.metrics <- park_metrics


#Add climate data to dataset
#Note: I originally wrote this chunk of code as a function, but it would not work properly. Hence it is repeated in long-form for each city here.

library(sf) #load library
library(rnoaa) #load library

#DC climate
city.file <- "./data/municipal shapefiles/DC/DistrictOfColumbia_municipal.shp"
buffer <- 50
min.year <- 1900
max.year <- 2020
city.name <- "DC"

{ #begin code chunk
  # READ IN DATA
  municipal <- st_read(dsn=city.file, quiet = TRUE)
  
  #Find center of city
  city.center <- st_centroid(municipal) #calculate XY of geographic center of city
  city.center <- as.character(city.center$geometry) #isolate geometry of point
  city.center <- gsub(x=city.center,
                      pattern = "c\\(", #removes leading 'c' and open parentheses
                      replacement = "") #replaces it with nothing (aka erases it)
  city.center <- gsub(x=city.center,
                      pattern = "\\)", #removes closing parentheses
                      replacement = "") #replaces it with nothing (aka erases it)
  city.center <- strsplit(x = city.center, #split the value by the comma to get each value by itself
                          split = ",")
  city.center <- data.frame(long = as.numeric(city.center[[1]][1]), #make lat and long into their own dataframe
                            lat = as.numeric(city.center[[1]][2]))
  
  #Find weather stations
  station.data <- ghcnd_stations() #download dataset from NOAA 
  
  #Extract stations within buffer distance of city center
  stations <- meteo_distance(station_data = station.data, #all possible stations
                             lat = city.center$lat, #latitude of city center
                             long = city.center$long, #longitude of city center
                             units = deg, #specifies decimal degrees for lat/long
                             radius = buffer) #search distance from city center
  
  loop.years <- seq(min.year,max.year, by=1) #create a vector of years to loop through
  weather.data <- data.frame()
  
  for(i in 1:length(loop.years)){ #begin year-specific data processing for loop
    i.start.time <- Sys.time() #record time at start of loop
    
    #Define date ranges
    min.date <- paste(loop.years[i],"-12-01", sep="") #minimum date
    
    max.date <- paste(loop.years[i]+1,"-2-28", sep="") #maximum date
    
    #Pull weather data
    i.weather.data <- (meteo_pull_monitors(monitors = as.vector(stations$id),
                                           date_min = min.date,
                                           date_max = max.date,
                                           var = c("TMIN","TMAX")))
    
    #Remove records from stations without temperature data
    i.weather.data <- i.weather.data[!is.na(i.weather.data$tmax),] #remove all records without a daily maximum temperature
    i.weather.data <- i.weather.data[!is.na(i.weather.data$tmin),] #remove all records without a daily minimum temperature
    
    #Calculate average daily temp
    i.weather.data$tmax <- i.weather.data$tmax*0.1 #values are stored in tenths of degrees C. This restores it to degrees C.
    i.weather.data$tmin <- i.weather.data$tmin*0.1 #values are stored in tenths of degrees C. This restores it to degrees C.
    i.weather.data$avg.temp <- (i.weather.data$tmax + i.weather.data$tmin)/2 #calculates the average temperature for the day
    
    #Specify survey year (Records for Jan and Feb show data for NEXT calander year, but same survey year)
    i.weather.data$survey <- paste(city.name,loop.years[i], sep="")
    
    weather.data <- rbind(weather.data, i.weather.data) #add this year's data to data frame holding data from all years.
    
    i.stop.time <- Sys.time() #record time at end of loop
    run.time <- i.stop.time - i.start.time #calculate the time it took to run the loop
    time.left <- run.time*(length(loop.years)-i) #estimate time left before all data is processed
    
    slices <- c(i, length(loop.years)-i) #create "proportions" for progress bar (pie chart
    pie(slices, #initiate progress pie chart
        col = c("green","red"), #color complete with gree, incomplete with red
        labels = "", #remove labels
        main = paste(round(time.left), units(time.left), " remaining" ,"\n", "(estimated)"), #main label estimates time remaining
        sub = paste("Now processing:","\n",loop.years[i+1], sep=""), cex=.75) #subtitle says which year the process is working on 
  } #end year-specific data processing for loop
  
  
  ######################
  
  weather.data$split.by <- paste(weather.data$id,weather.data$survey, sep="-") #create a column that specifies both the survey year and survey site
  
  #Calculate average temp per site
  weather.split <- split(weather.data, weather.data$split.by) #split data frame by site and survey year
  
  calc.avg.temp <- function(x) { #begin sub-function
    
    temp.df <- data.frame(site = x$id[1], #site name
                          year = x$survey[1], #survey year
                          temp.C = mean(x$avg.temp)) #average winter temp
    
  } #end sub-function
  
  weather.data <- lapply(weather.split, calc.avg.temp) #apply the averaging function to the split dataset
  weather.data <- do.call("rbind", weather.data) #recombine all data frames into a single data frame
  
  station.locs <- stations[,c("id","latitude","longitude")] #create data frame of only station names and coordinates
  station.locs <- station.locs[!duplicated(station.locs), ] #remove duplicate rows to maintain clean merges
  
  #Merge coordinates and temperature data
  weather.data <- merge(x = weather.data, 
                        by.x = "site",
                        y = station.locs,
                        by.y = "id",
                        all.x = TRUE,
                        all.y = FALSE)
  
  #Determine which sites are urban
  unique.stations <- weather.data[match(unique(weather.data$site), weather.data$site),] #thin dataset to data frame of unique weather stations
  unique.stations <- st_as_sf(unique.stations, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84") #convert data frame to simple feature (spatial) using appropriate crs
  
  municipal <- st_transform(municipal, crs = st_crs(5070)) #Change the coordinate reference system to Albers equal area conic (contiguous USA)
  unique.stations <- st_transform(unique.stations, crs = st_crs(5070)) #Change the coordinate reference system to Albers equal area conic (contiguous USA)
  urban.stations <- st_filter(unique.stations, municipal) #save points that lie within the boundaries of the shapefile
  urban.stations <- as.vector(urban.stations$site) #convert to vector of names of sites that fall within urban boundaries
  
  weather.data$type <- ifelse(weather.data$site %in% urban.stations, "urban", "non-urban") #create a column in weather.data. If the station name is listed in the previously created vector, use "urban" as the type. Otherwise use "rural" (understanding that these points may not actually be rural)
  
  #Group data by age class to arrive at urban and non-urban average temperature
  weather.data$year.head <- substr(weather.data$year,0,5) #first three values of sample year. To be left alone.
  weather.data$sample.year.rounded.tail <- substr(weather.data$year,6,6) #gets last value in year, which will be what is rounded
  weather.data$sample.year.rounded.tail <- ifelse(as.numeric(weather.data$sample.year.rounded.tail) <= 4, "0", "5") #Rounds last value to appropriate age class
  weather.data$year_rounded <- paste(weather.data$year.head,weather.data$sample.year.rounded.tail, sep="") #Combines rounded year head and tail into single year
  
  #Split data and calculate heat islands
  data.split <- split(weather.data, weather.data$year_rounded) #splits data frame into a list of data frames where each data frame contains values from a specific 5-year sampling period
  problem.years.urban <- c()
  problem.years.nonurban <- vector()
  
  calculate.survey.temp <- function(x){#begin sub-function
    
    n.urban <- nrow(x[which(x$type %in% "urban"), ]) #count how many records are from urban stations
    n.nonurban <- nrow(x[which(x$type %in% "non-urban"), ]) #count how many records are from non-urban stations
    
    if(n.urban == 0) {problem.years.urban <- append(problem.years.urban, x$survey[1])}
    if(n.nonurban == 0) {problem.years.nonurban <- append(problem.years.nonurban, x$survey[1])}
    
    avg.regional.temp <- mean(x$temp.C) #calculate the regional temperature by averaging data across all stations
    
    if(n.urban>0 & n.nonurban > 0){#begin conditions specific to when there is both urban and non-urban temperature data
      avg.urb.temp <- mean(x[which(x$type %in% "urban"), "temp.C"]) #calculate the urban temperature by averaging data across urban stations
      avg.nonurb.temp <- mean(x[which(x$type %in% "non-urban"), "temp.C"]) #calculate the non-urban temperature by averaging data across non-urban stations
      UHI <- avg.urb.temp - avg.nonurb.temp #calculate urban heat island by subtracting non-urban temp from urban temp (in Celsius)
    } #end conditions specific to when there is both urban and non-urban temperature data
    
    if(!(n.urban>0 & n.nonurban > 0)){#begin conditions specific to when either urban or nonurban data are missing
      UHI <- NA #Gives value of NA since it can't be calculated
    } #begin conditions specific to when either urban or nonurban data are missing
    
    
    #Create new data frame as product of function
    new.df <- data.frame(survey = paste(x$year[1],sep=""),
                         regional.C = avg.regional.temp,
                         UHI.C = UHI)
    
  } #end sub-function
  
  processed_weather <- lapply(data.split, calculate.survey.temp) #apply above function to the split dataset
  processed_weather <- do.call("rbind", processed_weather) #collapse back to a single data frame
  
  DC.climate.data <- processed_weather
  
  DC.climate.data$survey <- row.names(DC.climate.data) #Rename surveys to be rownames, since surveys are not rounded and won't completely merge
}#end chunk of code

#MN climate
city.file <- "./data/municipal shapefiles/MN/Minneapolis_municipal.shp"
buffer <- 50
min.year <- 1900
max.year <- 2020
city.name <- "MN"

{ #begin code chunk
  # READ IN DATA
  municipal <- st_read(dsn=city.file, quiet = TRUE)
  municipal <- st_transform(municipal, crs = "+proj=longlat +datum=WGS84") #unlike DC, this needed to be converted for functionality
  
  #Find center of city
  city.center <- st_centroid(municipal) #calculate XY of geographic center of city
  city.center <- as.character(city.center$geometry) #isolate geometry of point
  city.center <- gsub(x=city.center,
                      pattern = "c\\(", #removes leading 'c' and open parentheses
                      replacement = "") #replaces it with nothing (aka erases it)
  city.center <- gsub(x=city.center,
                      pattern = "\\)", #removes closing parentheses
                      replacement = "") #replaces it with nothing (aka erases it)
  city.center <- strsplit(x = city.center, #split the value by the comma to get each value by itself
                          split = ",")
  city.center <- data.frame(long = as.numeric(city.center[[1]][1]), #make lat and long into their own dataframe
                            lat = as.numeric(city.center[[1]][2]))
  
  #Find weather stations
  station.data <- ghcnd_stations() #download dataset from NOAA 
  
  #Extract stations within buffer distance of city center
  stations <- meteo_distance(station_data = station.data, #all possible stations
                             lat = city.center$lat, #latitude of city center
                             long = city.center$long, #longitude of city center
                             units = deg, #specifies decimal degrees for lat/long
                             radius = buffer) #search distance from city center
  
  loop.years <- seq(min.year,max.year, by=1) #create a vector of years to loop through
  weather.data <- data.frame()
  
  for(i in 1:length(loop.years)){ #begin year-specific data processing for loop
    i.start.time <- Sys.time() #record time at start of loop
    
    #Define date ranges
    min.date <- paste(loop.years[i],"-12-01", sep="") #minimum date
    
    max.date <- paste(loop.years[i]+1,"-2-28", sep="") #maximum date
    
    #Pull weather data
    i.weather.data <- (meteo_pull_monitors(monitors = as.vector(stations$id),
                                           date_min = min.date,
                                           date_max = max.date,
                                           var = c("TMIN","TMAX")))
    
    #Remove records from stations without temperature data
    i.weather.data <- i.weather.data[!is.na(i.weather.data$tmax),] #remove all records without a daily maximum temperature
    i.weather.data <- i.weather.data[!is.na(i.weather.data$tmin),] #remove all records without a daily minimum temperature
    
    #Calculate average daily temp
    i.weather.data$tmax <- i.weather.data$tmax*0.1 #values are stored in tenths of degrees C. This restores it to degrees C.
    i.weather.data$tmin <- i.weather.data$tmin*0.1 #values are stored in tenths of degrees C. This restores it to degrees C.
    i.weather.data$avg.temp <- (i.weather.data$tmax + i.weather.data$tmin)/2 #calculates the average temperature for the day
    
    #Specify survey year (Records for Jan and Feb show data for NEXT calander year, but same survey year)
    i.weather.data$survey <- paste(city.name,loop.years[i], sep="")
    
    weather.data <- rbind(weather.data, i.weather.data) #add this year's data to data frame holding data from all years.
    
    i.stop.time <- Sys.time() #record time at end of loop
    run.time <- i.stop.time - i.start.time #calculate the time it took to run the loop
    time.left <- run.time*(length(loop.years)-i) #estimate time left before all data is processed
    
    slices <- c(i, length(loop.years)-i) #create "proportions" for progress bar (pie chart
    pie(slices, #initiate progress pie chart
        col = c("green","red"), #color complete with gree, incomplete with red
        labels = "", #remove labels
        main = paste(round(time.left), units(time.left), " remaining" ,"\n", "(estimated)"), #main label estimates time remaining
        sub = paste("Now processing:","\n",loop.years[i+1], sep=""), cex=.75) #subtitle says which year the process is working on 
  } #end year-specific data processing for loop
  
  
  ######################
  
  weather.data$split.by <- paste(weather.data$id,weather.data$survey, sep="-") #create a column that specifies both the survey year and survey site
  
  #Calculate average temp per site
  weather.split <- split(weather.data, weather.data$split.by) #split data frame by site and survey year
  
  calc.avg.temp <- function(x) { #begin sub-function
    
    temp.df <- data.frame(site = x$id[1], #site name
                          year = x$survey[1], #survey year
                          temp.C = mean(x$avg.temp)) #average winter temp
    
  } #end sub-function
  
  weather.data <- lapply(weather.split, calc.avg.temp) #apply the averaging function to the split dataset
  weather.data <- do.call("rbind", weather.data) #recombine all data frames into a single data frame
  
  station.locs <- stations[,c("id","latitude","longitude")] #create data frame of only station names and coordinates
  station.locs <- station.locs[!duplicated(station.locs), ] #remove duplicate rows to maintain clean merges
  
  #Merge coordinates and temperature data
  weather.data <- merge(x = weather.data, 
                        by.x = "site",
                        y = station.locs,
                        by.y = "id",
                        all.x = TRUE,
                        all.y = FALSE)
  
  #Determine which sites are urban
  unique.stations <- weather.data[match(unique(weather.data$site), weather.data$site),] #thin dataset to data frame of unique weather stations
  unique.stations <- st_as_sf(unique.stations, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84") #convert data frame to simple feature (spatial) using appropriate crs
  
  municipal <- st_transform(municipal, crs = st_crs(5070)) #Change the coordinate reference system to Albers equal area conic (contiguous USA)
  unique.stations <- st_transform(unique.stations, crs = st_crs(5070)) #Change the coordinate reference system to Albers equal area conic (contiguous USA)
  urban.stations <- st_filter(unique.stations, municipal) #save points that lie within the boundaries of the shapefile
  urban.stations <- as.vector(urban.stations$site) #convert to vector of names of sites that fall within urban boundaries
  
  weather.data$type <- ifelse(weather.data$site %in% urban.stations, "urban", "non-urban") #create a column in weather.data. If the station name is listed in the previously created vector, use "urban" as the type. Otherwise use "rural" (understanding that these points may not actually be rural)
  
  #Group data by age class to arrive at urban and non-urban average temperature
  weather.data$year.head <- substr(weather.data$year,0,5) #first three values of sample year. To be left alone.
  weather.data$sample.year.rounded.tail <- substr(weather.data$year,6,6) #gets last value in year, which will be what is rounded
  weather.data$sample.year.rounded.tail <- ifelse(as.numeric(weather.data$sample.year.rounded.tail) <= 4, "0", "5") #Rounds last value to appropriate age class
  weather.data$year_rounded <- paste(weather.data$year.head,weather.data$sample.year.rounded.tail, sep="") #Combines rounded year head and tail into single year
  
  #Split data and calculate heat islands
  data.split <- split(weather.data, weather.data$year_rounded) #splits data frame into a list of data frames where each data frame contains values from a specific 5-year sampling period
  problem.years.urban <- c()
  problem.years.nonurban <- vector()
  
  calculate.survey.temp <- function(x){#begin sub-function
    
    n.urban <- nrow(x[which(x$type %in% "urban"), ]) #count how many records are from urban stations
    n.nonurban <- nrow(x[which(x$type %in% "non-urban"), ]) #count how many records are from non-urban stations
    
    if(n.urban == 0) {problem.years.urban <- append(problem.years.urban, x$survey[1])}
    if(n.nonurban == 0) {problem.years.nonurban <- append(problem.years.nonurban, x$survey[1])}
    
    avg.regional.temp <- mean(x$temp.C) #calculate the regional temperature by averaging data across all stations
    
    if(n.urban>0 & n.nonurban > 0){#begin conditions specific to when there is both urban and non-urban temperature data
      avg.urb.temp <- mean(x[which(x$type %in% "urban"), "temp.C"]) #calculate the urban temperature by averaging data across urban stations
      avg.nonurb.temp <- mean(x[which(x$type %in% "non-urban"), "temp.C"]) #calculate the non-urban temperature by averaging data across non-urban stations
      UHI <- avg.urb.temp - avg.nonurb.temp #calculate urban heat island by subtracting non-urban temp from urban temp (in Celsius)
    } #end conditions specific to when there is both urban and non-urban temperature data
    
    if(!(n.urban>0 & n.nonurban > 0)){#begin conditions specific to when either urban or nonurban data are missing
      UHI <- NA #Gives value of NA since it can't be calculated
    } #begin conditions specific to when either urban or nonurban data are missing
    
    
    #Create new data frame as product of function
    new.df <- data.frame(survey = paste(x$year[1],sep=""),
                         regional.C = avg.regional.temp,
                         UHI.C = UHI)
    
  } #end sub-function
  
  processed_weather <- lapply(data.split, calculate.survey.temp) #apply above function to the split dataset
  processed_weather <- do.call("rbind", processed_weather) #collapse back to a single data frame
  
  MN.climate.data <- processed_weather
  
  MN.climate.data$survey <- row.names(MN.climate.data) #Rename surveys to be rownames, since surveys are not rounded and won't completely merge
}#end chunk of code


#PA climate
city.file <- "./data/municipal shapefiles/PA/Pittsburgh_municipal.shp"
buffer <- 50
min.year <- 1900
max.year <- 2020
city.name <- "PA"

{ #begin code chunk
  # READ IN DATA
  municipal <- st_read(dsn=city.file, quiet = TRUE)
  municipal <- st_transform(municipal, crs = "+proj=longlat +datum=WGS84") #unlike DC, this needed to be converted for functionality
  
  #Find center of city
  city.center <- st_centroid(municipal) #calculate XY of geographic center of city
  city.center <- as.character(city.center$geometry) #isolate geometry of point
  city.center <- gsub(x=city.center,
                      pattern = "c\\(", #removes leading 'c' and open parentheses
                      replacement = "") #replaces it with nothing (aka erases it)
  city.center <- gsub(x=city.center,
                      pattern = "\\)", #removes closing parentheses
                      replacement = "") #replaces it with nothing (aka erases it)
  city.center <- strsplit(x = city.center, #split the value by the comma to get each value by itself
                          split = ",")
  city.center <- data.frame(long = as.numeric(city.center[[1]][1]), #make lat and long into their own dataframe
                            lat = as.numeric(city.center[[1]][2]))
  
  #Find weather stations
  station.data <- ghcnd_stations() #download dataset from NOAA 
  
  #Extract stations within buffer distance of city center
  stations <- meteo_distance(station_data = station.data, #all possible stations
                             lat = city.center$lat, #latitude of city center
                             long = city.center$long, #longitude of city center
                             units = deg, #specifies decimal degrees for lat/long
                             radius = buffer) #search distance from city center
  
  loop.years <- seq(min.year,max.year, by=1) #create a vector of years to loop through
  weather.data <- data.frame()
  
  for(i in 1:length(loop.years)){ #begin year-specific data processing for loop
    i.start.time <- Sys.time() #record time at start of loop
    
    #Define date ranges
    min.date <- paste(loop.years[i],"-12-01", sep="") #minimum date
    
    max.date <- paste(loop.years[i]+1,"-2-28", sep="") #maximum date
    
    #Pull weather data
    i.weather.data <- (meteo_pull_monitors(monitors = as.vector(stations$id),
                                           date_min = min.date,
                                           date_max = max.date,
                                           var = c("TMIN","TMAX")))
    
    #Remove records from stations without temperature data
    i.weather.data <- i.weather.data[!is.na(i.weather.data$tmax),] #remove all records without a daily maximum temperature
    i.weather.data <- i.weather.data[!is.na(i.weather.data$tmin),] #remove all records without a daily minimum temperature
    
    #Calculate average daily temp
    i.weather.data$tmax <- i.weather.data$tmax*0.1 #values are stored in tenths of degrees C. This restores it to degrees C.
    i.weather.data$tmin <- i.weather.data$tmin*0.1 #values are stored in tenths of degrees C. This restores it to degrees C.
    i.weather.data$avg.temp <- (i.weather.data$tmax + i.weather.data$tmin)/2 #calculates the average temperature for the day
    
    #Specify survey year (Records for Jan and Feb show data for NEXT calander year, but same survey year)
    i.weather.data$survey <- paste(city.name,loop.years[i], sep="")
    
    weather.data <- rbind(weather.data, i.weather.data) #add this year's data to data frame holding data from all years.
    
    i.stop.time <- Sys.time() #record time at end of loop
    run.time <- i.stop.time - i.start.time #calculate the time it took to run the loop
    time.left <- run.time*(length(loop.years)-i) #estimate time left before all data is processed
    
    slices <- c(i, length(loop.years)-i) #create "proportions" for progress bar (pie chart
    
    pie(slices, #initiate progress pie chart
        col = c("green","red"), #color complete with green, incomplete with red
        labels = "", #remove labels
        main = paste(round(time.left), units(time.left), " remaining" ,"\n", "(estimated)"), #main label estimates time remaining
        sub = paste("Now processing:","\n",loop.years[i+1], sep=""), cex=.75) #subtitle says which year the process is working on 
  } #end year-specific data processing for loop
  
  
  ######################
  
  weather.data$split.by <- paste(weather.data$id,weather.data$survey, sep="-") #create a column that specifies both the survey year and survey site
  
  #Calculate average temp per site
  weather.split <- split(weather.data, weather.data$split.by) #split data frame by site and survey year
  
  calc.avg.temp <- function(x) { #begin sub-function
    
    temp.df <- data.frame(site = x$id[1], #site name
                          year = x$survey[1], #survey year
                          temp.C = mean(x$avg.temp)) #average winter temp
    
  } #end sub-function
  
  weather.data <- lapply(weather.split, calc.avg.temp) #apply the averaging function to the split dataset
  weather.data <- do.call("rbind", weather.data) #recombine all data frames into a single data frame
  
  station.locs <- stations[,c("id","latitude","longitude")] #create data frame of only station names and coordinates
  station.locs <- station.locs[!duplicated(station.locs), ] #remove duplicate rows to maintain clean merges
  
  #Merge coordinates and temperature data
  weather.data <- merge(x = weather.data, 
                        by.x = "site",
                        y = station.locs,
                        by.y = "id",
                        all.x = TRUE,
                        all.y = FALSE)
  
  #Determine which sites are urban
  unique.stations <- weather.data[match(unique(weather.data$site), weather.data$site),] #thin dataset to data frame of unique weather stations
  unique.stations <- st_as_sf(unique.stations, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84") #convert data frame to simple feature (spatial) using appropriate crs
  
  municipal <- st_transform(municipal, crs = st_crs(5070)) #Change the coordinate reference system to Albers equal area conic (contiguous USA)
  unique.stations <- st_transform(unique.stations, crs = st_crs(5070)) #Change the coordinate reference system to Albers equal area conic (contiguous USA)
  urban.stations <- st_filter(unique.stations, municipal) #save points that lie within the boundaries of the shapefile
  urban.stations <- as.vector(urban.stations$site) #convert to vector of names of sites that fall within urban boundaries
  
  weather.data$type <- ifelse(weather.data$site %in% urban.stations, "urban", "non-urban") #create a column in weather.data. If the station name is listed in the previously created vector, use "urban" as the type. Otherwise use "rural" (understanding that these points may not actually be rural)
  
  #Group data by age class to arrive at urban and non-urban average temperature
  weather.data$year.head <- substr(weather.data$year,0,5) #first three values of sample year. To be left alone.
  weather.data$sample.year.rounded.tail <- substr(weather.data$year,6,6) #gets last value in year, which will be what is rounded
  weather.data$sample.year.rounded.tail <- ifelse(as.numeric(weather.data$sample.year.rounded.tail) <= 4, "0", "5") #Rounds last value to appropriate age class
  weather.data$year_rounded <- paste(weather.data$year.head,weather.data$sample.year.rounded.tail, sep="") #Combines rounded year head and tail into single year
  
  #Split data and calculate heat islands
  data.split <- split(weather.data, weather.data$year_rounded) #splits data frame into a list of data frames where each data frame contains values from a specific 5-year sampling period
  problem.years.urban <- c()
  problem.years.nonurban <- vector()
  
  calculate.survey.temp <- function(x){#begin sub-function
    
    n.urban <- nrow(x[which(x$type %in% "urban"), ]) #count how many records are from urban stations
    n.nonurban <- nrow(x[which(x$type %in% "non-urban"), ]) #count how many records are from non-urban stations
    
    if(n.urban == 0) {problem.years.urban <- append(problem.years.urban, x$survey[1])}
    if(n.nonurban == 0) {problem.years.nonurban <- append(problem.years.nonurban, x$survey[1])}
    
    avg.regional.temp <- mean(x$temp.C) #calculate the regional temperature by averaging data across all stations
    
    if(n.urban>0 & n.nonurban > 0){#begin conditions specific to when there is both urban and non-urban temperature data
      avg.urb.temp <- mean(x[which(x$type %in% "urban"), "temp.C"]) #calculate the urban temperature by averaging data across urban stations
      avg.nonurb.temp <- mean(x[which(x$type %in% "non-urban"), "temp.C"]) #calculate the non-urban temperature by averaging data across non-urban stations
      UHI <- avg.urb.temp - avg.nonurb.temp #calculate urban heat island by subtracting non-urban temp from urban temp (in Celsius)
    } #end conditions specific to when there is both urban and non-urban temperature data
    
    if(!(n.urban>0 & n.nonurban > 0)){#begin conditions specific to when either urban or nonurban data are missing
      UHI <- NA #Gives value of NA since it can't be calculated
    } #begin conditions specific to when either urban or nonurban data are missing
    
    
    #Create new data frame as product of function
    new.df <- data.frame(survey = paste(x$year[1],sep=""),
                         regional.C = avg.regional.temp,
                         UHI.C = UHI)
    
  } #end sub-function
  
  processed_weather <- lapply(data.split, calculate.survey.temp) #apply above function to the split dataset
  processed_weather <- do.call("rbind", processed_weather) #collapse back to a single data frame
  
  PA.climate.data <- processed_weather
  
  PA.climate.data$survey <- row.names(PA.climate.data) #Rename surveys to be rownames, since surveys are not rounded and won't completely merge
  
} #end loop
#}#end chunk of code


#Calculate population density for each survey year
MN.census <- process_census(city.file = "./data/municipal shapefiles/MN/Minneapolis_municipal.shp",
                            census.file = "./data/census data/MN_census.csv",
                            city.name = "MN")

DC.census <- process_census(city.file = "./data/municipal shapefiles/DC/DistrictOfColumbia_municipal.shp",
                            census.file = "./data/census data/DC_census.csv",
                            city.name = "DC")

PA.census <- process_census(city.file = "./data/municipal shapefiles/PA/Pittsburgh_municipal.shp",
                            census.file = "./data/census data/PA_census.csv",
                            city.name = "PA")


#Read in survey effort data
DCDC.effort<- read.csv("./data/bird data/DC/DCDC_effort.csv")

MN18.effort <- read.csv("./data/bird data/MN/MN18_effort.csv")
MNSP.effort <- read.csv("./data/bird data/MN/MNSP_effort.csv")

PA2E.effort <- read.csv("./data/bird data/PA/PA2E_effort.csv")
PA2Q.effort <- read.csv("./data/bird data/PA/PA2Q_effort.csv")
PAPI.effort <- read.csv("./data/bird data/PA/PAPI_effort.csv")

#Impute survey effort
imp.DCDC.effort <- impute_effort(effort_df =  DCDC.effort,
                                 imputed_field = "Participants")

imp.MN18.effort <- impute_effort(effort_df =  MN18.effort,
                                 imputed_field = "Participants")

imp.MNSP.effort <- impute_effort(effort_df =  MNSP.effort,
                                 imputed_field = "Participants")

imp.PA2E.effort <- impute_effort(effort_df =  PA2E.effort,
                                 imputed_field = "Participants")

imp.PA2Q.effort <- impute_effort(effort_df =  PA2Q.effort,
                                 imputed_field = "Participants")

imp.PAPI.effort <- impute_effort(effort_df =  PAPI.effort,
                                 imputed_field = "Participants")

#Collapse survey efforts into 5 year buckets
DCDC.effort <- effort_collapse(imp.DCDC.effort, "DC")

#MN0Y.effort <- effort_collapse(imp.MN0Y.effort, "MN")
MN18.effort <- effort_collapse(imp.MN18.effort, "MN")
#MNMI.effort <- effort_collapse(imp.MNMI.effort, "MN")
MNSP.effort <- effort_collapse(imp.MNSP.effort, "MN")

PA2E.effort <- effort_collapse(imp.PA2E.effort, "PA")
PA2Q.effort <- effort_collapse(imp.PA2Q.effort, "PA")
PAPI.effort <- effort_collapse(imp.PAPI.effort, "PA")

#Merge effort dataframes to sum across years
DC.effort <- DCDC.effort
names(DC.effort) <- c("survey","total.effort") #renaming columns to be consistent with other cities, which require summing across surveys

MN.effort <- merge(x = MN18.effort,
                   y = MNSP.effort,
                   by= "survey",
                   all.x = TRUE,
                   all.y = TRUE)

MN.effort$total.effort <- rowSums(MN.effort[,-which(names(MN.effort) %in% "survey")], na.rm = TRUE) #Add all effort values (that is, all values from columns NOT named "survey")
MN.effort <- MN.effort[,c("survey","total.effort")] #reduce to only necessary columns

PA.effort <- merge(x = PA2E.effort,
                   y = PA2Q.effort,
                   by= "survey",
                   all.x = TRUE,
                   all.y = TRUE)

PA.effort <- merge(x = PA.effort,
                   y = PAPI.effort,
                   by= "survey",
                   all.x = TRUE,
                   all.y = TRUE)

PA.effort$total.effort <- rowSums(PA.effort[,-which(names(PA.effort) %in% "survey")], na.rm = TRUE) #Add all effort values (that is, all values from columns NOT named "survey")
PA.effort <- PA.effort[,c("survey","total.effort")] #reduce to only necessary columns

#Combine all city-specific data
DATA.DC <- merge(DC.park.metrics,
                 DC.effort,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.DC <- merge(DATA.DC,
                 DC.census,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.DC <- merge(DATA.DC,
                 DC.climate.data,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.MN <- merge(MN.park.metrics,
                 MN.effort,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.MN <- merge(DATA.MN,
                 MN.census,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.MN <- merge(DATA.MN,
                 MN.climate.data,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.PA <- merge(PA.park.metrics,
                 PA.effort,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.PA <- merge(DATA.PA,
                 PA.census,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

DATA.PA <- merge(DATA.PA,
                 PA.climate.data,
                 by="survey",
                 all.x=TRUE,
                 all.y=FALSE)

#Remove landscape data for years in which we don't have bird surveys
DATA.DC <- DATA.DC[DATA.DC$survey %in% rownames(bird.data),] #remove rows for survey periods that don't have bird data
DATA.MN <- DATA.MN[DATA.MN$survey %in% rownames(bird.data),] #remove rows for survey periods that don't have bird data
DATA.PA <- DATA.PA[DATA.PA$survey %in% rownames(bird.data),] #remove rows for survey periods that don't have bird data


#Scale all continuous variables specific to the city, not the entire dataset (mean centered on city, divided by std of entire dataset)
temp.global.df <- rbind(DATA.DC, DATA.MN, DATA.PA) #create a temporary global dataframe
effort.col <- temp.global.df$total.effort #save effort column
temp.global.df <- temp.global.df[,-which(names(temp.global.df) %in% "total.effort")] #remove effort comun from the data set
effort.col <- DATA.DC$total.effort #save effort column
DATA.DC <- DATA.DC[,-which(names(DATA.DC) %in% c("total.effort","Pop.Est","UHI.C"))] #remove effort, population estimate, and urban heat island columns from the data set (effort to be added back later)
backtrans.vals <- data.frame("city" = NA, #create an empty table to add to for future reference
                             "param" = NA,
                             "local.mean" = NA,
                             "local.sd" = NA,
                             "global.mean" = NA,
                             "global.sd" = NA)
for(q in 3:ncol(DATA.DC)) {#Begin scaling for-loop for DC
  backtrans.vals[q,"city"] <- DATA.DC[1,"city"] #fill city column
  backtrans.vals[q,"param"] <- names(DATA.DC)[q] #fill DCrameter column
  backtrans.vals[q,"local.mean"] <- mean(as.numeric(DATA.DC[,q]), na.rm=TRUE) #calculate city-specific mean
  backtrans.vals[q,"local.sd"] <- sd(as.numeric(DATA.DC[,q]), na.rm=TRUE) #calculate city-specific standard deviation
  backtrans.vals[q, "global.mean"] <- mean(as.numeric(temp.global.df[,q]), na.rm=TRUE) #calculate mean across all cities
  backtrans.vals[q, "global.sd"] <- sd(as.numeric(temp.global.df[,q]), na.rm=TRUE) #calculate standard deviation across all cities
  
  DC.backtrans <- backtrans.vals #save city-specific backtransformation dataframe
  
  DATA.DC[,q] <- as.numeric(DATA.DC[,q]) - backtrans.vals[q,"local.mean"] #mean-center based on local mean
  DATA.DC[,q] <- DATA.DC[,q] / backtrans.vals[q, "global.sd"] #standardize by dividing by global standard deviation
  DATA.DC$total.effort <- effort.col
}#end scaling for-loop for DC

effort.col <- DATA.MN$total.effort #save effort column
DATA.MN <- DATA.MN[,-which(names(DATA.MN) %in% c("total.effort","Pop.Est","UHI.C"))] #remove effort, population estimate, and urban heat island columns from the data set (effort to be added back later)
for(q in 3:ncol(DATA.MN)) {#Begin scaling for-loop for MN
  backtrans.vals[q,"city"] <- DATA.MN[1,"city"] #fill city column
  backtrans.vals[q,"param"] <- names(DATA.MN)[q] #fill MNrameter column
  backtrans.vals[q,"local.mean"] <- mean(as.numeric(DATA.MN[,q]), na.rm=TRUE) #calculate city-specific mean
  backtrans.vals[q,"local.sd"] <- sd(as.numeric(DATA.MN[,q]), na.rm=TRUE) #calculate city-specific standard deviation
  backtrans.vals[q, "global.mean"] <- mean(as.numeric(temp.global.df[,q]), na.rm=TRUE) #calculate mean across all cities
  backtrans.vals[q, "global.sd"] <- sd(as.numeric(temp.global.df[,q]), na.rm=TRUE) #calculate standard deviation across all cities
  
  MN.backtrans <- backtrans.vals #save city-specific backtransformation dataframe
  
  DATA.MN[,q] <- as.numeric(DATA.MN[,q]) - backtrans.vals[q,"local.mean"] #mean-center based on local mean
  DATA.MN[,q] <- DATA.MN[,q] / backtrans.vals[q, "global.sd"] #standardize by dividing by global standard deviation
  DATA.MN$total.effort <- effort.col
}#end scaling for-loop for MN


effort.col <- DATA.PA$total.effort #save effort column
DATA.PA <- DATA.PA[,-which(names(DATA.PA) %in% c("total.effort","Pop.Est","UHI.C"))] #remove effort, population estimate, and urban heat island columns from the data set (effort to be added back later)
for(q in 3:ncol(DATA.PA)) {#Begin scaling for-loop for PA
  backtrans.vals[q,"city"] <- DATA.PA[1,"city"] #fill city column
  backtrans.vals[q,"param"] <- names(DATA.PA)[q] #fill parameter column
  backtrans.vals[q,"local.mean"] <- mean(as.numeric(DATA.PA[,q]), na.rm=TRUE) #calculate city-specific mean
  backtrans.vals[q,"local.sd"] <- sd(as.numeric(DATA.PA[,q]), na.rm=TRUE) #calculate city-specific standard deviation
  backtrans.vals[q, "global.mean"] <- mean(as.numeric(temp.global.df[,q]), na.rm=TRUE) #calculate mean across all cities
  backtrans.vals[q, "global.sd"] <- sd(as.numeric(temp.global.df[,q]), na.rm=TRUE) #calculate standard deviation across all cities
  
  PA.backtrans <- backtrans.vals #save city-specific backtransformation dataframe
  
  DATA.PA[,q] <- as.numeric(DATA.PA[,q]) - backtrans.vals[q,"local.mean"] #mean-center based on local mean
  DATA.PA[,q] <- DATA.PA[,q] / backtrans.vals[q, "global.sd"] #standardize by dividing by global standard deviation
  DATA.PA$total.effort <- effort.col
}#end scaling for-loop for PA

#Combine all backtrans into single dataframe and remove empty rows
backtrans.vals <- rbind(DC.backtrans,MN.backtrans,PA.backtrans) #combine data
backtrans.vals <- backtrans.vals[complete.cases(backtrans.vals), ] #remove NA's

#Combine all other landscape data into a single dataframe
all.metrics <- rbind(DATA.DC,DATA.MN,DATA.PA) #combine scaled survey-specific data into a single data frame

names(all.metrics) <- c("survey",
                        "city",
                        "year",
                        "prcnt_park_area",
                        "park_EIratio",
                        "park_clumpy",
                        "Pop.Dens",
                        "regional.C",
                        "effort")

DATA.surveys <- all.metrics #rename data frame

##### Diversity metrics
process_sppDiv(bird.data) #Process diversity metrics
DATA.diversity <- spp.div.df #Rename

##### Traits data

#Load in functional traits
library(traitdata) #Note to future users, traitdata must be downloaded from GitHub. Remove the vignettes argument if you are running into issues. Use the following with the 'remotes' package loaded: remotes::install_github("RS-eco/traitdata", force=T)
data("bird_behav") #read in bird behavior traits
data("elton_birds") #read in elton traits REMOVE IF OTHER DATA BASE WORKS OUT!

retain.elton <- c("scientificNameStd", #standardized scientific name for merging
                  "Diet.Inv", #proportion of (small) invertebrates in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Vend", #proportion of endotherms in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Vect", #proportion of ectotherms in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Vfish", #proportion of fish in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Vunk", #proportion of unknown material in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Scav", #proportion of carrion in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Fruit", #proportion of fruit in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Nect", #proportion of nectar in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.Seed", #proportion of seeds in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "Diet.PlantO", #proportion of other plant material in diet: Specific diet items contribute to ecological niche and have cascading implications on ecosystem
                  "ForStrat.watbelowsurf", #proportion of foraging occurring below water: Foraging behavior elicits specific species-environment interactions that contribute to the species’ ecological role, and might be also limited by habitat loss in urban areas
                  "ForStrat.wataroundsurf", #proportion of foraging occurring at water surface: Foraging behavior elicits specific species-environment interactions that contribute to the species’ ecological role, and might be also limited by habitat loss in urban areas
                  "ForStrat.ground", #proportion of foraging occurring at ground level: Foraging behavior elicits specific species-environment interactions that contribute to the species’ ecological role, and might be also limited by habitat loss in urban areas
                  "ForStrat.understory", #proportion of foraging occurring in understory: Foraging behavior elicits specific species-environment interactions that contribute to the species’ ecological role, and might be also limited by habitat loss in urban areas
                  "ForStrat.midhigh", #proportion of foraging occurring in mid-canopy: Foraging behavior elicits specific species-environment interactions that contribute to the species’ ecological role, and might be also limited by habitat loss in urban areas
                  "ForStrat.canopy", #proportion of foraging occurring in canopy: Foraging behavior elicits specific species-environment interactions that contribute to the species’ ecological role, and might be also limited by habitat loss in urban areas
                  "ForStrat.aerial", #proportion of foraging occurring in above the canopy: Foraging behavior elicits specific species-environment interactions that contribute to the species’ ecological role, and might be also limited by habitat loss in urban areas
                  "BodyMass.Value") #body mass: General indicator of body size, small species might fare better given anthropogenic pressures

retain.behavioral <- c("scientificNameStd", #standardized scientific name for merging"NestPlacement", #how exposed nests tend to be for species: Nesting areas in cities greatly differ from those in non-urban areas
                       "LogRangeSize", #log of home range size for species: Urban areas have smaller habitats, so species with larger ranges might not be able to persist. Alternatively, large-ranged birds might out perform since they can obtain resources from habitat patches across the city.
                       "LogClutchSize") #log of number of eggs per clutch: Species with greater offspring may be pre-adapted to urban life

#Thin functional traits to only defined traits of interest
thin_behav <- as.data.frame(bird_behav[,retain.behavioral])
thin_elton <- as.data.frame(elton_birds[,retain.elton])

#Create a diet breadth category, which is not included in elton traits
thin_elton$Diet.Breadth <- rowSums(thin_elton[,c("Diet.Inv","Diet.Vend","Diet.Vect","Diet.Vfish","Diet.Vunk","Diet.Scav","Diet.Fruit","Diet.Nect","Diet.Seed","Diet.PlantO")] > 0) #count how many diet categories occupy greater than 0% of species diet

#Create a foraging breadth category, which is not included in elton traits
thin_elton$Forage.Breadth <- rowSums(thin_elton[,c("ForStrat.watbelowsurf","ForStrat.wataroundsurf","ForStrat.ground","ForStrat.understory","ForStrat.midhigh","ForStrat.canopy","ForStrat.aerial")] > 0) #count how many diet categories occupy greater than 0% of species diet

#Thin functional traits to only species of interest
thin_behav <- subset(thin_behav, thin_behav$scientificNameStd %in% colnames(bird.data))
thin_elton <- subset(thin_elton, thin_elton$scientificNameStd %in% colnames(bird.data))

#Elton Traits and the Bird Behavior datasets both contain two similar but different entries for the following species: Setophaga pinus, Haemorhous mexicanus
#After reviewing the entries and comparing the proposed data against descriptions of the same species in public databases, I have determined that the following should be removed as they were incorrectly labelled
thin_elton <- thin_elton[-184, ] #removes inaccurate Setophaga pinus record - may need to be updated if database is ever changed
thin_elton <- thin_elton[-151, ] #removes inaccurate Haemorhous mexicanus record - may need to be updated if database is ever changed

thin_behav <- thin_behav[-144,]#removes inaccurate Haemorhous mexicanus record - may need to be updated if database is ever changed
thin_behav <- thin_behav[-211,] #removes inaccurate Setophaga pinus record - may need to be updated if database is ever changed

#Further thin elton to only traits of interest
thin_elton <- thin_elton[,c("scientificNameStd","BodyMass.Value","Diet.Breadth","Forage.Breadth")]

traits.df <- as.data.frame(colnames(bird.data)) #create a dataframe consisting of species names only
names(traits.df) <- "species" #rename column

traits.df <- merge(traits.df,
                   thin_behav,
                   by.x="species",
                   by.y="scientificNameStd",
                   all.x=TRUE,
                   all.y=FALSE)

traits.df <- merge(traits.df,
                   thin_elton,
                   by.x="species",
                   by.y="scientificNameStd",
                   all.x=TRUE,
                   all.y=FALSE)

#Identify species with missing trait data
nrow(traits.df[rowSums(is.na(traits.df)) > 0,]) #Idenfity number of species which have missing trait data
missing.data <- traits.df[rowSums(is.na(traits.df)) > 0,] #For author's reference, species with missing data are stored as a data frame

#Filling in missing values manually
#Taxonomic debate. Some records listed under Hesperiphona vespertina instead of Coccothraustes vespertinus
traits.df[traits.df$species %in% "Coccothraustes vespertinus","LogRangeSize"] <- 15.18745
traits.df[traits.df$species %in% "Coccothraustes vespertinus","LogClutchSize"] <- 1.25276
traits.df[traits.df$species %in% "Coccothraustes vespertinus","BodyMass.Value"] <- 57.3
traits.df[traits.df$species %in% "Coccothraustes vespertinus","Diet.Breadth"] <- 4
traits.df[traits.df$species %in% "Coccothraustes vespertinus","Forage.Breadth"] <- 2

#Taxonomic debate. Some records listed under Gallinago gallinago instead of Gallinago delicata
traits.df[traits.df$species %in% "Gallinago delicata","LogRangeSize"] <- 16.81647
traits.df[traits.df$species %in% "Gallinago delicata","LogClutchSize"] <- 1.25276
traits.df[traits.df$species %in% "Gallinago delicata","BodyMass.Value"] <- 112.94
traits.df[traits.df$species %in% "Gallinago delicata","Diet.Breadth"] <- 3
traits.df[traits.df$species %in% "Gallinago delicata","Forage.Breadth"] <- 2

#Taxonomic debate. Some records listed under Larus minutus instead of Hydrocoloeus minutus
traits.df[traits.df$species %in% "Hydrocoloeus minutus","LogRangeSize"] <- 15.93445
traits.df[traits.df$species %in% "Hydrocoloeus minutus","LogClutchSize"] <- 0.91629
traits.df[traits.df$species %in% "Hydrocoloeus minutus","BodyMass.Value"] <- 118
traits.df[traits.df$species %in% "Hydrocoloeus minutus","Diet.Breadth"] <- 2
traits.df[traits.df$species %in% "Hydrocoloeus minutus","Forage.Breadth"] <- 3

#Taxonomic debate. Some records listed under Larus atricilla instead of Leucophaeus atricilla
traits.df[traits.df$species %in% "Leucophaeus atricilla","LogRangeSize"] <- 14.26326
traits.df[traits.df$species %in% "Leucophaeus atricilla","LogClutchSize"] <- 0.91629
traits.df[traits.df$species %in% "Leucophaeus atricilla","BodyMass.Value"] <- 307.41
traits.df[traits.df$species %in% "Leucophaeus atricilla","Diet.Breadth"] <- 3
traits.df[traits.df$species %in% "Leucophaeus atricilla","Forage.Breadth"] <- 3

#Taxonomic debate. Some records listed under Otus asio instead of Megascops asio
traits.df[traits.df$species %in% "Megascops asio","LogRangeSize"] <- 15.41482
traits.df[traits.df$species %in% "Megascops asio","LogClutchSize"] <- 1.25276
traits.df[traits.df$species %in% "Megascops asio","BodyMass.Value"] <- 179.99
traits.df[traits.df$species %in% "Megascops asio","Diet.Breadth"] <- 2
traits.df[traits.df$species %in% "Megascops asio","Forage.Breadth"] <- 4

#Taxonomic debate. Some records listed under Melanitta nigra instead of Melanitta americana
traits.df[traits.df$species %in% "Melanitta americana","LogRangeSize"] <- 15.32585
traits.df[traits.df$species %in% "Melanitta americana","LogClutchSize"] <- 1.94591
traits.df[traits.df$species %in% "Melanitta americana","BodyMass.Value"] <- 1049.98
traits.df[traits.df$species %in% "Melanitta americana","Diet.Breadth"] <- 4
traits.df[traits.df$species %in% "Melanitta americana","Forage.Breadth"] <- 1

#Behavioral traits not available. Range and clutch are from common blackbird (Turdus merula), a relative.
traits.df[traits.df$species %in% "Molothrus ater","LogRangeSize"] <- 16.38348
traits.df[traits.df$species %in% "Molothrus ater","LogClutchSize"] <- 1.38629

#Name was misspelled in database. Additionally: Taxonomic debate. Some records listed under Nycticorax violaceus instead of Nyctanassa violacea
traits.df[traits.df$species %in% "Nyctanassa violacea","LogRangeSize"] <- 15.24972
traits.df[traits.df$species %in% "Nyctanassa violacea","LogClutchSize"] <- 1.60943
traits.df[traits.df$species %in% "Nyctanassa violacea","BodyMass.Value"] <- 681.67
traits.df[traits.df$species %in% "Nyctanassa violacea","Diet.Breadth"] <- 3
traits.df[traits.df$species %in% "Nyctanassa violacea","Forage.Breadth"] <- 2

#Taxonomic debate. Some records listed under Leiothlypis celata instead of Oreothlypis celata
traits.df[traits.df$species %in% "Oreothlypis celata","LogRangeSize"] <- 15.82813
traits.df[traits.df$species %in% "Oreothlypis celata","LogClutchSize"] <- 1.50407
traits.df[traits.df$species %in% "Oreothlypis celata","BodyMass.Value"] <- 9.19
traits.df[traits.df$species %in% "Oreothlypis celata","Diet.Breadth"] <- 3
traits.df[traits.df$species %in% "Oreothlypis celata","Forage.Breadth"] <- 1

#Taxonomic debate. Some records listed under Leiothlypis peregrina or Vermivora peregrina instead of Oreothlypis peregrina
traits.df[traits.df$species %in% "Oreothlypis peregrina","LogRangeSize"] <- 15.37950
traits.df[traits.df$species %in% "Oreothlypis peregrina","LogClutchSize"] <- 1.79175
traits.df[traits.df$species %in% "Oreothlypis peregrina","BodyMass.Value"] <- 8.9
traits.df[traits.df$species %in% "Oreothlypis peregrina","Diet.Breadth"] <- 3
traits.df[traits.df$species %in% "Oreothlypis peregrina","Forage.Breadth"] <- 3

#Taxonomic debate. Some records listed under Leiothlypis ruficapilla instead of Oreothlypis ruficapilla
traits.df[traits.df$species %in% "Oreothlypis ruficapilla","LogRangeSize"] <- 14.83524
traits.df[traits.df$species %in% "Oreothlypis ruficapilla","LogClutchSize"] <- 1.50407
traits.df[traits.df$species %in% "Oreothlypis ruficapilla","BodyMass.Value"] <- 8.09
traits.df[traits.df$species %in% "Oreothlypis ruficapilla","Diet.Breadth"] <- 3
traits.df[traits.df$species %in% "Oreothlypis ruficapilla","Forage.Breadth"] <- 3

#Taxonomic debate. Some records listed under Picoides bubescens or Dryobates pubescens instead of Picoides pubescens
traits.df[traits.df$species %in% "Picoides pubescens","LogRangeSize"] <- 16.36497
traits.df[traits.df$species %in% "Picoides pubescens","LogClutchSize"] <- 1.50407
traits.df[traits.df$species %in% "Picoides pubescens","BodyMass.Value"] <- 25.55
traits.df[traits.df$species %in% "Picoides pubescens","Diet.Breadth"] <- 3
traits.df[traits.df$species %in% "Picoides pubescens","Forage.Breadth"] <- 3

#Taxonomic debate. Some records listed under Pagurus carolinensis or Parus carolinensis instead of Poecile carolinensis
traits.df[traits.df$species %in% "Poecile carolinensis","LogRangeSize"] <- 14.65386
traits.df[traits.df$species %in% "Poecile carolinensis","LogClutchSize"] <- 1.70474
traits.df[traits.df$species %in% "Poecile carolinensis","BodyMass.Value"] <- 9.99
traits.df[traits.df$species %in% "Poecile carolinensis","Diet.Breadth"] <- 2
traits.df[traits.df$species %in% "Poecile carolinensis","Forage.Breadth"] <- 2

#Taxonomic debate. Some records listed under Troglodytes troglodytes instead of Troglodytes hiemalis
traits.df[traits.df$species %in% "Troglodytes hiemalis","LogRangeSize"] <- 16.89792
traits.df[traits.df$species %in% "Troglodytes hiemalis","LogClutchSize"] <- 1.87180
traits.df[traits.df$species %in% "Troglodytes hiemalis","BodyMass.Value"] <- 9.74
traits.df[traits.df$species %in% "Troglodytes hiemalis","Diet.Breadth"] <- 5
traits.df[traits.df$species %in% "Troglodytes hiemalis","Forage.Breadth"] <- 2

#Identify species with missing trait data
nrow(traits.df[rowSums(is.na(traits.df)) > 0,]) #Ready to move on

#Scale functional traits 
for(t in 2:ncol(traits.df)) {#Begin scaling for-loop for DC
  traits.df[,t] <- scale(as.numeric(traits.df[,t]))
}#end scaling for-loop

DATA.traits <- traits.df #save data frame

#Calculate function diversity metrics 
library(FD) #Load 'FD' package. 

#Prep traits.df for the FD package specifically
rownames(traits.df) <- traits.df$species #add rownames to traits.df
traits.df <- traits.df[,!(names(traits.df) %in% c("species", #remove any non-numeric columns (coded out of order so some of these may not exist yet anyway)
                                                  "RangeGroup",
                                                  "ClutchGroup",
                                                  "MassGroup",
                                                  "DietGroup",
                                                  "ForageGroup",
                                                  "FunctGroup"))]

fd_metrics <- dbFD(x = as.matrix(traits.df), #traits used to construct functional groups
                   a = bird.data) #survey data 

#Create dataframe with functional metrics
fd_df <- as.data.frame(fd_metrics$FRic) #Functional richness
names(fd_df) <- "funRich"
fd_df$funDiverg <- fd_metrics$FDiv #Functional divergence
fd_df$funEve <- fd_metrics$FEve #Functional evenness
fd_df$year <- row.names(fd_df)

#Join functional metrics to species metrics
DATA.diversity <- merge(x = DATA.diversity,
                        by.x = "survey",
                        y = fd_df,
                        by.y = "year",
                        all.x = TRUE,
                        all.y = FALSE)



#Determine functional group for each species based on if their trait values are within one standard deviation of the mean
DATA.traits$RangeGroup <- ifelse(DATA.traits$LogRangeSize < -1, "small range",
                                 ifelse(DATA.traits$LogRangeSize > 1, "large range", "average range"))

DATA.traits$ClutchGroup <- ifelse(DATA.traits$LogClutchSize < -1, "small clutch",
                                  ifelse(DATA.traits$LogClutchSize > 1, "large clutch", "average clutch"))

DATA.traits$MassGroup <- ifelse(DATA.traits$BodyMass.Value < -1, "small mass",
                                ifelse(DATA.traits$BodyMass.Value > 1, "large mass", "average mass"))

DATA.traits$DietGroup <- ifelse(DATA.traits$Diet.Breadth < -1, "diet specialist",
                                ifelse(DATA.traits$Diet.Breadth > 1, "diet generalist", "average diet"))

DATA.traits$ForageGroup <- ifelse(DATA.traits$Forage.Breadth < -1, "forage specialist",
                                  ifelse(DATA.traits$Forage.Breadth > 1, "forage generalist", "average forage"))

DATA.traits$FunctGroup <- paste(DATA.traits$RangeGroup,
                                DATA.traits$ClutchGroup,
                                DATA.traits$MassGroup,
                                DATA.traits$DietGroup,
                                DATA.traits$ForageGroup,
                                sep=" + ")

#Create survey data by functional group, not species
funct.obs <- as.data.frame(t(bird.data)) #transpose bird.data so each row is a species and each column is a survey
funct.obs$species <- rownames(funct.obs) #create a column of bird names
funct.obs <- merge(funct.obs,
                   DATA.traits,
                   by = "species",
                   all.x = TRUE,
                   all.y = FALSE)


forage.obs <- process_functGroups(funct.obs, "ForageGroup")
diet.obs <- process_functGroups(funct.obs, "DietGroup")
clutch.obs <- process_functGroups(funct.obs, "ClutchGroup")
mass.obs <- process_functGroups(funct.obs, "MassGroup")
range.obs <- process_functGroups(funct.obs, "RangeGroup")

#Structure functional observation dataset such that each row is a different functional group in a different survey (allowing for trait group to be a covariate)
forage.obsXsurv <- format_functional(forage.obs)
diet.obsXsurv <- format_functional(diet.obs)
mass.obsXsurv <- format_functional(mass.obs)
range.obsXsurv <- format_functional(range.obs)
clutch.obsXsurv <- format_functional(clutch.obs)

#Combine these data bases
obsXsurvey <- rbind(forage.obsXsurv,
                    diet.obsXsurv,
                    mass.obsXsurv,
                    range.obsXsurv,
                    clutch.obsXsurv)

#Join park metrics data to observation data of functional groups
obsXsurvey <- merge(x = obsXsurvey,
                    y = DATA.surveys,
                    by = "survey",
                    all.x = TRUE)

obsXsurvey <- obsXsurvey[complete.cases(obsXsurvey),] #remove cases where there is bird data or park data but not the other (incomplete cases)

#Dummy code for different trait levels (but not for the average condition, just small and large)
obsXsurvey$small <- ifelse(obsXsurvey$funct.group %in% "2small", 1, 0)
obsXsurvey$large <- ifelse(obsXsurvey$funct.group %in% "3large", 1, 0)

#combine all functional observations such that each row is a survey and each column contains data for a single functional group
funct.obs <- cbind(forage.obs,
                   diet.obs,
                   clutch.obs,
                   mass.obs,
                   range.obs)
funct.obs$survey <- rownames(funct.obs)


#Combining various datasets into a single dataset called DATA
DATA <- merge(DATA.diversity,
              DATA.surveys,
              by = "survey",
              all.x = FALSE,
              all.y = TRUE)

DATA <- merge(DATA,
              funct.obs,
              by="survey",
              all.x = TRUE,
              all.y = FALSE)

#Create a directory and save datasets, then clean up environment
path.name <- paste("./",Sys.Date(),"_variables", sep="") #create directory path

#Save variables
dir.create(path = path.name) #create directory

write.csv(x = DATA.surveys,
          file = paste(path.name,"/DATA.surveys.csv", sep=""))
write.csv(x = DATA.diversity,
          file = paste(path.name,"/DATA.diversity.csv", sep=""))
write.csv(x = DATA.traits,
          file = paste(path.name,"/DATA.traits.csv", sep=""))
write.csv(x = backtrans.vals,
          file = paste(path.name,"/backtrans.vals.csv", sep=""))
write.csv(x = DATA,
          file = paste(path.name,"/DATA.complete.csv", sep=""))
write.csv(x = bird.data,
          file = paste(path.name,"/DATA.species.obs.csv", sep=""))
write.csv(x = funct.obs,
          file = paste(path.name,"/DATA.functional.obs.by.survey.csv", sep=""))
write.csv(x = obsXsurvey,
          file = paste(path.name,"/DATA.functional.obs.by.funct.by.survey.csv", sep=""))


#Clean up any unnecessary objects in environment       
rm(list=ls()[! ls() %in% c("DATA.species","DATA.surveys","DATA.diversity","DATA.traits", "backtrans.vals", "DATA", "obsXsurvey")])


#Proceed to Script 2!
