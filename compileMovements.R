#Purpose----
#Compile coordinates from HERG positions collected from Ecotone Biologgers
#via GSM, GPRS, and base station (UHF) signals.  
#The code also collates other data taken from birds (e.g., bird measurements) 
#and field sites (e.g., nest locations of birds with biologgers) 

#Load packages----

library(tidyverse)
library(mapview)
library(sf)
library(lubridate)
library(stringi)
library(leaflet)
library(leafem)
library(leafpop)
library(leaflet.extras)

#Import field data (e.g., bird measurements) collected via an iPad----

gsmMeta <- read_csv("C://Users/HynesD/Documents/ecotone/gsmMetadata/gsmMetadata19May2022R.csv")

gsmMetaR <- gsmMeta %>% slice(-1) %>% #remove test file
  mutate(`Bird Mass` = coalesce(`Bird Mass`, Weight)) %>% #clean up column, variable names
  mutate(GullTaggedDateTime = mdy_hms(`Date gull tagged`)) %>%
  select(GullTaggedDateTime, 
         Band,
         FieldReadableCode = Code,
         FdReadCodeColour = "Code colour",
         FdReadBkGrdColour = "Background colour",
         BiologgerID = "GSM ID...12",
         ClutchSize18May = "18 May Clutch size",
         ClutchSize19May = "Clutch size 19 May",                    
         ClutchStatus19May = "Clutch Status 19 May",
         TotalHead = Head,
         Culmen,
         BillDepth = `Bill Depth`,
         Wing,
         Tarsus,
         Mass = "Bird Mass",
         P1:P10,
         Comments = Notes) %>%
  drop_na(BiologgerID) %>% #drop birds that weren't fitted with Ecotones; birds that are fitted = LARI01, LARI02, LARI03, etc.
  mutate(ClutchStatus19May = fct_recode(ClutchStatus19May, `Nest kept intact` = "Intact", `Nest destroyed` = "Destroyed"))
  
         

#Nest locations (taken with a handheld Garmin)----

garmin <- read_csv("C://Users/HynesD/Documents/ecotone/gsmMetadata/gsmNests.csv", skip = 22, n_max = 15)

gsmNests <-
  garmin %>% st_as_sf(coords = c("lon", "lat"),
                      crs = 4326,
                      remove = FALSE) %>% 
  mutate(BiologgerID = stri_sub_replace(name, 4, 3, value = "i")) %>% #clean up bird IDs (i.e., BiologgerIDs)
  mutate(BiologgerID = str_to_upper(BiologgerID)) %>%
  select(BiologgerID, Latitude = lat, Longitude = lon)

#GSM-GPRS data from Amazon Web Services (via Global Sensor)----

gsm <- read_csv("C://Users/HynesD/Documents/ecotone/aws/ALLGPS.csv")

gsmR <-
  gsm %>% st_as_sf(
    coords = c("Longtitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  mutate(PositionDateTime = ymd_hms(GPSTime)) %>% #clean up and make datetime ADT
  mutate(PositionDateTime = with_tz(PositionDateTime, "America/Halifax")) %>%
  filter(PositionDateTime > "2022-05-18 23:59") %>% #take positions after 18 May 2022
  select(
    GpsNumber,
    PositionDateTime,
    Latitude,
    Longitude = Longtitude,
    BatteryVoltage,
    BiologgerID = GpsDescription,
    GPSIntervals,
    VHFTelemetry,
    Activity,
    GSMSignal,
    Temperature,
    Type:OrdDate
  ) %>%
  mutate(BiologgerID = str_to_upper(BiologgerID))

#Base station data----
#Covert .txt file into CSVs (one per bird apparently) via the NGAnalyser software

#directory of files with the converted (txt to CSV) base station data 
directory <- "C://Users/HynesD/Documents/ecotone/baseStationData/20220520081926_2" 

#convert into a data frame
files <- list.files(directory, pattern = "*.csv", full.names = TRUE)
compList <- lapply(files, read_csv2)
bsdf <- do.call(rbind, compList)

#clean up names
bsdf2 <- bsdf %>%
  mutate(PositionDateTime = ymd_hms(Date_2)) %>% 
  mutate(PositionDateTime = with_tz(PositionDateTime, "America/Halifax")) %>% 
  filter(PositionDateTime > "2022-05-18 23:59") %>% 
  mutate(Latitude = as.numeric(stri_sub_replace(Latitude, 3, 2, value = "."))) %>% #base station data drops decimal from lat/lon (?); added here
  mutate(Longitude = as.numeric(stri_sub_replace(Longitude, 4, 3, value = "."))) %>%
  mutate(Voltage = as.numeric(stri_sub_replace(Voltage, 2, 1, value = "."))) %>%
  mutate(Temperature = as.numeric(stri_sub_replace(Temperature, 3, 2, value = "."))) %>%
  drop_na(Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  mutate(Speed = as.numeric(Speed)) %>%
  add_column(Type = "UHF") %>%
  select(BiologgerID = `Logger ID`, PositionDateTime, Latitude, Longitude, BatteryVoltage = Voltage, Temperature, Speed, Type) %>%
  mutate(BiologgerID = stri_sub_replace(BiologgerID, 4, 3, value = "I")) 

#Bind GSM-GPRS and base station data----

posi <- bind_rows(gsmR, bsdf2) %>%
  group_by(BiologgerID) %>%
  mutate(diffMin = difftime( #get times since biologger deployment 
    PositionDateTime,
    lag(PositionDateTime, 1, default = PositionDateTime[1]),
    unit = "mins"
  ) %>%
    as.numeric() %>%
    cumsum())

#join movement data with site metadata
all <- posi %>% left_join(gsmMetaR, by = "BiologgerID") 

#join nest locations with site metadata; take fewer variables for better map presentation 
all2 <-
  gsmNests %>% left_join(gsmMetaR, by = "BiologgerID") %>% select(
    BiologgerID,
    Latitude,
    Longitude,
    ClutchSize18May,
    ClutchSize19May,
    ClutchStatus19May
  )

#Renesters---
#Locations of three nests destroyed on 2022-06-07 by Manon Sorais

renest <- tribble(~BiologgerID, ~Latitude, ~Longitude, ~EggCount,
                    "LARI02", 45.10153, -61.54247, 3,
                    "LARI09", 45.10118, -61.54253, 1,
                    "LARI11", 45.10274, -61.54509, 3
                    ) %>%
  add_column(NestDestroyed = ymd("2022-06-07"))

#mapviewOptions(fgb = FALSE)


#Map movement data----

#set colours for movement data (warmer = more recent position; cooler = older position)

pal1 <- colorNumeric(palette = colorRampPalette(c("#B200B2", "#FFFF66"))(length(all$diffMin)),
                     domain = all$diffMin)

#colours for nests
nestCol <- colorFactor(palette = c("red1", "deepskyblue"), all2$ClutchStatus19May)



map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB",
                   options = providerTileOptions(maxNativeZoom=19,maxZoom=100)) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery",
                   options = providerTileOptions(maxNativeZoom=19,maxZoom=100)) %>%
  setView(-61.54, 45.10, zoom = 13) %>%
  #addMapPane("birds", zIndex = 430) %>%
  addCircleMarkers(
    data = all,
    lat=~Latitude, 
    lng=~Longitude,
    group = ~BiologgerID,
    popup = popupTable(all, zcol=c(6, 2, 29, 5, 3,4, 12)),
    color = ~pal1(diffMin),
    stroke = TRUE,
    #color = "black", fillColor = "deepskyblue", stroke = TRUE, 
    weight = 3, fillOpacity = 0.8,
    #options = pathOptions(pane = "birds")
  ) %>%
  addCircleMarkers(
    data = all2,
    lat=~Latitude, 
    lng=~Longitude,
    group = ~ClutchStatus19May,
    label = ~BiologgerID,
    labelOptions = labelOptions(noHide = T, textsize = "15px", textOnly = TRUE, direction = "center"),
    popup = popupTable(all2),
    stroke = TRUE,
    color = ~nestCol(ClutchStatus19May),
    #layerId = "Nests",
    #fillColor = "lawngreen", 
    weight = 3, fillOpacity = 0.6
    #options = pathOptions(pane = "birds")
  ) %>%
  addCircleMarkers(
    data = renest,
    lat=~Latitude, 
    lng=~Longitude,
    group = "Renester (destroyed)",
    label = ~BiologgerID,
    labelOptions = labelOptions(noHide = T, textsize = "15px", textOnly = TRUE, direction = "center"),
    popup = popupTable(renest),
    stroke = TRUE,
    color = "lawngreen",
    #layerId = "Nests",
    #fillColor = "lawngreen", 
    weight = 3, fillOpacity = 0.6
    #options = pathOptions(pane = "birds")
  )  %>%
  addLayersControl(baseGroups = c("CartoDB", "Imagery"),  
                   overlayGroups = c("Nest kept intact", "Nest destroyed", "Renester (destroyed)", "LARI01", "LARI02", "LARI03", "LARI04",
                                     "LARI05", "LARI06", "LARI07", "LARI08", "LARI09", "LARI10",
                                                                             "LARI11", "LARI12", "LARI13", "LARI14", "LARI15"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("LARI01", "LARI02", "LARI03", "LARI04",
              "LARI05", "LARI06", "LARI07", "LARI08", "LARI09", "LARI10",
              "LARI11", "LARI12", "LARI13", "LARI14", "LARI15")) %>%
  addFullscreenControl() %>% addScaleBar() %>% addResetMapButton() %>%
  addMeasure(primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers", primaryAreaUnit = "hectares")

  
map

#save map  
#mapshot(map, "C://Users/HynesD/Documents/ecotone/maps/gpsGsmHergPositions20220607.html")



###end----



#Files for Manon


#nests <- gsmNests %>% left_join(gsmMetaR, by = "BiologgerID") %>% rename(Nest.Latitude = Latitude, Nest.Longitude = Longitude)

#nestKml <- nests %>% select(Name = BiologgerID, Description = ClutchStatus19May)
#st_write(nestKml, "C://Users/HynesD/Documents/ecotone/maps/gpsGsmNests.kml", driver = "kml", append = FALSE)

#write_csv(gsmMetaR, "C://Users/HynesD/Documents/ecotone/CountryIslandHergProject/GullData/GullMetadata.csv")


#filter by gull

#lar10 <- all %>% filter(BiologgerID == "LARI10")

#lar <- all %>% filter(BiologgerID %in% c("LARI08", "LARI10"))

#to do
#gganimate

