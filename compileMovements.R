###run----
library(tidyverse)
library(mapview)
library(sf)
library(lubridate)
library(fuzzyjoin)
library(stringi)
library(leaflet)
library(leafem)
library(leafpop)
library(leaflet.extras)

#Field data from iPad----

gsmMeta <- read_csv("C://Users/HynesD/Documents/ecotone/gsmMetadata/gsmMetadata19May2022R.csv")

gsmMetaR <- gsmMeta %>% slice(-1) %>%
  mutate(`Bird Mass` = coalesce(`Bird Mass`, Weight)) %>%
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
  drop_na(BiologgerID) %>%
  mutate(ClutchStatus19May = fct_recode(ClutchStatus19May, `Nest kept intact` = "Intact", `Nest destroyed` = "Destroyed"))
  
         
         
         

#GPS data from AWS portal----

#gsm <- read_csv("C://Users/HynesD/Documents/ecotone/larinowaExports/gps_pos_202205.csv")
gsm <- read_csv("C://Users/HynesD/Documents/ecotone/aws/ALLGPS.csv")


# gsmR <- gsm %>% st_as_sf(coords = c("Longtitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
#   mutate(PositionDateTime = ymd_hms(GPSTime)) %>% 
#   mutate(PositionDateTime = with_tz(PositionDateTime, "America/Curacao")) %>% 
#   filter(PositionDateTime > "2022-05-18 23:59") %>%
#   select(GpsNumber, PositionDateTime, Latitude, Longitude = Longtitude, BatteryVoltage, BiologgerID = GpsDescription,
#          GPSIntervals, VHFTelemetry, Activity, GSMSignal)

gsmR <- gsm %>% st_as_sf(coords = c("Longtitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
   mutate(PositionDateTime = ymd_hms(GPSTime)) %>% 
   mutate(PositionDateTime = with_tz(PositionDateTime, "America/Halifax")) %>% 
   filter(PositionDateTime > "2022-05-18 23:59") %>%
   select(GpsNumber, PositionDateTime, Latitude, Longitude = Longtitude, BatteryVoltage, BiologgerID = GpsDescription,
          GPSIntervals, VHFTelemetry, Activity, GSMSignal, Temperature, Type:OrdDate) %>%
   mutate(BiologgerID = str_to_upper(BiologgerID))
  #  group_by(BiologgerID) %>%
  #  mutate(
  #   diffMin = difftime(PositionDateTime, lag(PositionDateTime,1, default = PositionDateTime[1] ), unit = "mins") %>% 
  #     as.numeric() %>%
  #     cumsum()
  # )


#Nest locations----
garmin <- read_csv("C://Users/HynesD/Documents/ecotone/gsmMetadata/gsmNests.csv", skip = 22, n_max = 15)

gsmNests <- garmin %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% mutate(BiologgerID = stri_sub_replace(name, 4, 3, value = "i")) %>%
  mutate(BiologgerID = str_to_upper(BiologgerID)) %>%
  select(BiologgerID, Latitude = lat, Longitude = lon)


#Base station data----

directory <- "C://Users/HynesD/Documents/ecotone/baseStationData/20220520081926"
files <- list.files(directory, pattern = "*.csv", full.names = TRUE)

compList <- lapply(files, read_csv2)

bsdf <- do.call(rbind, compList)

bsdf2 <- bsdf %>%
  mutate(PositionDateTime = ymd_hms(Date_2)) %>% 
  mutate(PositionDateTime = with_tz(PositionDateTime, "America/Halifax")) %>% 
  filter(PositionDateTime > "2022-05-18 23:59") %>% 
  mutate(Latitude = as.numeric(stri_sub_replace(Latitude, 3, 2, value = "."))) %>%
  mutate(Longitude = as.numeric(stri_sub_replace(Longitude, 4, 3, value = "."))) %>%
  mutate(Voltage = as.numeric(stri_sub_replace(Voltage, 2, 1, value = "."))) %>%
  mutate(Temperature = as.numeric(stri_sub_replace(Temperature, 3, 2, value = "."))) %>%
  drop_na(Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  mutate(Speed = as.numeric(Speed)) %>%
  add_column(Type = "UHF") %>%
  select(BiologgerID = `Logger ID`, PositionDateTime, Latitude, Longitude, BatteryVoltage = Voltage, Temperature, Speed, Type) %>%
  mutate(BiologgerID = stri_sub_replace(BiologgerID, 4, 3, value = "I")) 
  # group_by(BiologgerID) %>%
  # mutate(
  #   diffMin = difftime(PositionDateTime, lag(PositionDateTime,1, default = PositionDateTime[1] ), unit = "mins") %>% 
  #     as.numeric() %>%
  #     cumsum()
  # )

#Bind GSM and base station data

posi <- bind_rows(gsmR, bsdf2) %>%
    group_by(BiologgerID) %>%
    mutate(
     diffMin = difftime(PositionDateTime, lag(PositionDateTime,1, default = PositionDateTime[1] ), unit = "mins") %>% 
       as.numeric() %>%
       cumsum()
   )
  
all <- posi %>% left_join(gsmMetaR, by = "BiologgerID") 
  
all2 <- gsmNests %>% left_join(gsmMetaR, by = "BiologgerID") %>% select(BiologgerID, Latitude, Longitude, ClutchSize18May, ClutchSize19May, ClutchStatus19May)

#Renesters destroyed 7 June 2022

renest <- tribble(~BiologgerID, ~Latitude, ~Longitude, ~EggCount,
                    "LARI02", 45.10153, -61.54247, 3,
                    "LARI09", 45.10118, -61.54253, 1,
                    "LARI11", 45.10274, -61.54509, 3
                    ) %>%
  add_column(NestDestroyed = ymd("2022-06-07"))

#mapviewOptions(fgb = FALSE)

#map1 <- mapview(all, layer.name = "GPS-GSM HERG Position", col.regions = "orange", cex = 10, alpha.regions = 0.5)
# map1 <- mapview(all, zcol = "BiologgerID", burst = TRUE, layer.name = "GPS-GSM HERG Position", cex = 10, alpha.regions = 0.5, legend = FALSE)
# map1
# 
# map2 <- mapview(all2, zcol = "ClutchStatus19May", layer.name = "GPS-GSM HERG Nest", cex = 10, alpha.regions = 0.5,
#                 col.regions=list("red","blue"))
# map2
# both <- map1 + map2
# both
# 
# 
# mapshot(both, "C://Users/HynesD/Documents/ecotone/gpsGsmHergPositions.html")

# may13 <- read_csv("C://Users/HynesD/Documents/countryIsland/GullsCI_13May2022.csv")
# m13 <- may13 %>% st_as_sf(coords = c("longitude", "lattitude"), crs = 4326)

#Map----

pal1 <- colorNumeric(
  palette = colorRampPalette(c("#B200B2", "#FFFF66"))(length(all$diffMin)), 
  domain = all$diffMin)

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
  addFullscreenControl() %>% addScaleBar() %>% addResetMapButton() %>% addMeasure()
  
map
  
mapshot(map, "C://Users/HynesD/Documents/ecotone/maps/gpsGsmHergPositions20220606.html")



###end----



#Manon


nests <- gsmNests %>% left_join(gsmMetaR, by = "BiologgerID") %>% rename(Nest.Latitude = Latitude, Nest.Longitude = Longitude)

nestKml <- nests %>% select(Name = BiologgerID, Description = ClutchStatus19May)
st_write(nestKml, "C://Users/HynesD/Documents/ecotone/maps/gpsGsmNests.kml", driver = "kml", append = FALSE)

write_csv(gsmMetaR, "C://Users/HynesD/Documents/ecotone/CountryIslandHergProject/GullData/GullMetadata.csv")


#filter by gull

lar10 <- all %>% filter(BiologgerID == "LARI10")

lar <- all %>% filter(BiologgerID %in% c("LARI08", "LARI10"))


##gganimate

