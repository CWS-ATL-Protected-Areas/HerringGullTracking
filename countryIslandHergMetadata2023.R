#Gull Tracking Metadata Workflow----

library(tidyverse)
library(sf)
library(mapview)
library(cowplot)
library(scico)

# To do: Code molt scores-----
# pScores <- tribble(
#   ~Score, ~Condition,
#   0, "an old feather",
#   1, "old feather missing or new feather completely in pin",
#   2, "new feather just emerging from the sheath, up to one third grown",
#   3, "new feather between one and two thirds grown",
#   4, "new feather > two thirds grown, but waxy sheath still at its base",
#   5, "new feather fully grown with no trace of sheath at its base")


#Import Survey123 data----  
survey123 <- read_csv("data/GullTagging_0.csv")

#metadata from 2022 to get correct nest treatments
data2022 <-read_csv("data/gullMetadata.csv") %>%
  mutate(BiologgerID = gsub("I","",BiologgerID))#standardize formatting


#Sarah Gutowsky's written field notes
#Original images at "https://007gc-my.sharepoint.com/personal/sarah_neima_ec_gc_ca/Documents/PA%20Monitoring%20Team/Data/countryIslandGullTracking/NestNotes_20230524_091533.jpg"

writtenNotes <- tribble(
  ~BiologgerID, ~NestTreatment, ~ClutchSize, ~ClutchSizeDate, 
  "LAR13", "Intact", 2, "2023-05-10",
  "LAR13", "Intact", 3, "2023-05-22",
  "LAR02", "Intact", 3, "2023-05-19",
  "LAR02", "Intact", 3, "2023-05-23",
  "LAR02", "Destroyed", 1, "2023-06-08",
  "SOM08", "Intact", 3, "2023-05-19",
  "SOM08", "Intact", 3, "2023-05-23",
  "SOM08", "Destroyed", 1, "2023-06-08",
  "LAR04", "Destroyed", 3, "2023-05-20",
  "LAR04", "Destroyed", 2, "2023-05-10",
  "SOM11", "Intact", 1, "2023-05-10",
  "SOM11", "Intact", 3, "2023-05-23",
  "SOM06", "Destroyed", 2, "2023-05-10",
  "SOM06", "Destroyed", 3, "2023-05-23",
  "SOM10", "Intact", 2, "2023-05-10",
  "SOM10", "Intact", 3, "2023-05-23",
  "SOM15", "Destroyed", 2, "2023-05-10",
  "SOM15", "Destroyed", 3, "2023-05-23",
  "SOM02", "Intact", 3, "2023-05-23",
  "SOM13", "Intact", 1, "2023-05-10",
  "SOM13", "Intact", 3, "2023-05-23",
  "SOM12", "Destroyed", 3, "2023-05-10",
  "SOM12", "Destroyed", 3, "2023-05-23",
  "SOM14", "Destroyed", 2, "2023-05-10",
  "SOM14", "Destroyed", 3, "2023-05-23",
  "SOM04", "Intact", 3, "2023-05-23",
  "SOM07", "Destroyed", 2, "2023-05-10",
  "SOM07", "Destroyed", 3, "2023-05-23",
  "SOM05", "Intact", 1, "2023-05-10",
  "SOM05", "Intact", 3, "2023-05-23",
  "SOM09", "Destroyed", 1, "2023-05-10",
  "SOM09", "Destroyed", 4, "2023-05-23",
  "SOM03", "Intact", 2, "2023-05-10",
  "SOM03", "Intact", 3, "2023-05-23",
  "SOM01", "Destroyed", 2, "2023-05-10",
  "SOM01", "Destroyed", 3, "2023-05-23"
  )

treatment2023 <- writtenNotes %>%
  group_by(BiologgerID) %>%
  filter(ClutchSizeDate == max(ClutchSizeDate))
  
#Clean up Survey123 data----

metaData2023 <- survey123 %>% 
  slice(-1) %>% #remove test file
  unite("BiologgerID", "GSM ID", "GPS ID", na.rm = TRUE) %>% #Clean up IDs
  mutate(across(c("BiologgerID"), ~na_if(., ""))) %>% #Convert values to NA
  mutate(Code = str_to_upper(Code)) %>% #Standardize text formatting
  mutate(BiologgerID = str_to_upper(BiologgerID)) %>% #Standardize text formatting
  mutate(BiologgerID = case_when(BiologgerID == "LAR9" ~ "LAR09", #Recode IDs
                                 BiologgerID == "1" ~ "SOM01",
                                 BiologgerID == "2" ~ "SOM02",
                                 BiologgerID == "3" ~ "SOM03",
                                 BiologgerID == "4" ~ "SOM04",
                                 BiologgerID == "5" ~ "SOM05",
                                 BiologgerID == "6" ~ "SOM06",
                                 BiologgerID == "7" ~ "SOM07",
                                 BiologgerID == "8" ~ "SOM08",
                                 BiologgerID == "9" ~ "SOM09",
                                 BiologgerID == "10" ~ "SOM10",
                                 BiologgerID == "11" ~ "SOM11",
                                 BiologgerID == "12" ~ "SOM12",
                                 BiologgerID == "13" ~ "SOM13",
                                 BiologgerID == "14" ~ "SOM14",
                                 BiologgerID == "15" ~ "SOM15",
                                 TRUE ~ BiologgerID)) %>%
  mutate(DateTimeTaggedUTC = mdy_hms(`Date and time gull tagged`)) %>% #Add UTC column 
  mutate(Site = case_when(Site == 1 ~ "Country Island", #Recode site name
                          is.na(Site) ~ "Country Island")) %>% 
  mutate(Species = case_when(Species == 1 ~ "Herring Gull", #Recode species names
                             Species == 2 ~ "Great Black-backed Gull")) %>%
  mutate(Band = as.numeric(gsub("-", "", Band))) %>% #Standardize text formatting
  mutate(Wing = case_when(Wing == 43.5 ~ 435, TRUE ~ Wing)) %>% #Fix error
  add_column(KiteMWeightGrams = 19) %>% #Add device weight
  add_column(WeighedWithKiteM = "N") %>% #Add column indicating whether bird was weighed with device
  mutate( #Add birds weighed with device
    WeighedWithKiteM = case_when(
      BiologgerID == "LAR10" ~ "Y",
      BiologgerID == "LAR07" ~ "Y",
      BiologgerID == "LAR02" ~ "Y",
      BiologgerID == "SOM01" ~ "Y",
      BiologgerID == "SOM02" ~ "Y",
      BiologgerID == "SOM03" ~ "Y",
      BiologgerID == "SOM04" ~ "Y",
      BiologgerID == "SOM05" ~ "Y",
      BiologgerID == "SOM06" ~ "Y",
      BiologgerID == "SOM07" ~ "Y",
      BiologgerID == "SOM08" ~ "Y",
      BiologgerID == "SOM09" ~ "Y",
      BiologgerID == "SOM10" ~ "Y",
      BiologgerID == "SOM11" ~ "Y",
      BiologgerID == "SOM12" ~ "Y",
      BiologgerID == "SOM13" ~ "Y",
      BiologgerID == "SOM14" ~ "Y",
      BiologgerID == "SOM15" ~ "Y",
      is.na(BiologgerID) ~ NA_character_,
      TRUE ~ WeighedWithKiteM
    )
  ) %>%
  mutate(Mass = coalesce(`Bird Mass`, Weight, `Bird Weight`)) %>% #Clean up column, variable names
  mutate(Mass = case_when(WeighedWithKiteM == "Y" ~ (Mass-KiteMWeightGrams),
                          TRUE ~ Mass)) %>%
  mutate(across(P1:P10, ~ case_when(.x == 5 ~ 0, #Birds' feathers are old, so everything "new" should be "0", right :)?
                                    .x == 31 ~ 1, #Fix error; assumed "3" was input accidentally, "1" true value based on other scores 
                                    is.na(.x) ~ 0,
                                    TRUE ~ .x))) %>%
  mutate(#Recode BanderID with names
    Bander = case_when(
      `Lead tagger` == 1 ~ "Karel Allard", 
      `Lead tagger` == 3 ~ "Sarah Gutowsky",
      `Lead tagger` == 4 ~ "Doug Hynes",
      `Lead tagger` == 5 ~ "Mark Mallory",
      `Lead tagger` == 7 ~ "Other"
      #TRUE ~ NA_character_
    )
  ) %>% 
  mutate(#Recode assistants with names
    BandAssistants = case_when(
      `Assistants` == 1 ~ "Karel Allard", 
      (`Assistants` == 2 & `DateTimeTaggedUTC` > as.POSIXct("2023-01-01")) ~ "Lindsay Colyn",
      (`Assistants` == 2 & `DateTimeTaggedUTC` < as.POSIXct("2023-01-01")) ~ "Megan Boucher",
      `Assistants` == 3 ~ "Sarah Gutowsky",
      `Assistants` == 4 ~ "Doug Hynes",
      `Assistants` == 5 ~ "Mark Mallory",
      `Assistants` == 6 ~ "Sarah Neima", 
      `Assistants` == 7 ~ "Other",
      `Assistants` == 234 ~ "Megan Boucher, Sarah Gutowsky, Doug Hynes", # possible to clean this up to replace multiple within a single string?
      `Assistants` == 75 ~ "Other, Mark Mallory",
      `Assistants` == 24 ~ "Megan Boucher, Doug Hynes",
      `Assistants` == 231 ~ "Megan Boucher, Sarah Gutowsky, Karel Allard",
      `Assistants` == 23 ~ "Megan Boucher, Sarah Gutowsky",
      `Assistants` == 35 ~ "Sarah Gutowsky, Mark Mallory",
      #TRUE ~ NA_character_
    )
  ) %>%
  mutate(clutchSizeOnTagDate = coalesce(`Clutch size`, `Clutch size 23-May`)) %>% #add clutch size on date of tagging
  full_join(select(data2022, ClutchStatus19May, BiologgerID), 
            by = c("BiologgerID" = "BiologgerID")) %>% #bring in 2022 nest treatment
  full_join(select(treatment2023, NestTreatment, BiologgerID), 
            by = c("BiologgerID" = "BiologgerID")) %>% #bring in 2023 nest treatment
  rename("NestTreatment2022" = "ClutchStatus19May",
         "NestTreatment2023" = "NestTreatment") %>%
  mutate(nestRelativeToColony = case_when(
    `Nest relative to colony` == 1 ~ "inside",
    `Nest relative to colony` == 2 ~ "outside",
    `BiologgerID` %in% c("LAR15","LAR11","LAR06","LAR05") ~"outside",
    is.na(`BiologgerID`) ~ NA_character_,
    TRUE ~ "inside"
    )
  ) %>%
  
  select(DateTimeTaggedUTC, #Select variables needed; edit as necessary 
         Site,
         Species,
         BandNumber = Band,
         FieldReadableCode = Code,
         FdReadBkGrdColour = "Background colour",
         FdReadCodeColour = "Code colour",
         BiologgerID,
         NestID = "Nest ID",
         clutchSizeOnTagDate,
         NestTreatment2022,
         NestTreatment2023,
         nestRelativeToColony,
         TotalHead = Head,
         Culmen,
         BillDepth = `Bill Depth`,
         Wing,
         Tarsus,
         Mass,
         P1:P10,
         #WeighedWithKiteM,
         #KiteMWeightGrams,
         Bander,
         BandAssistants,
         Comments = Notes)
#not run
  #mutate(P7 = case_when(is.na(P7) ~ 5, TRUE ~ P7)) 
  #filter(str_detect(BiologgerID, "SOM")) 
  #add_row(NestID = "E16") %>%
  #add_row(NestID = "E17")
  #mutate(BanderID = as.factor(BanderID)) %>%
  #mutate(BandNumber = str_replace_all(BandNumber, "-", ""))
  #mutate(ClutchStatus19May = fct_recode(ClutchStatus19May, `Nest kept intact` = "Intact", `Nest destroyed` = "Destroyed"))


#Join nest locations from '22 and '23----
#Code could be cleaned up a bit
#read 2023 data 
nests2023 <- st_read("data/garminNests2023.kml") %>% st_zm() %>%
  filter(str_detect(Name, "E")) %>% #subset only nests from 2023
  slice(-c(1, 14,15)) %>% #remove nest EMF and incorrect points for SOM14 & 15 (taken at camp)
  mutate(Longitude = sf::st_coordinates(.)[,1], Latitude = sf::st_coordinates(.)[,2]) %>%
  select(NestID = Name, Latitude, Longitude) %>%
  st_drop_geometry()

#read 2022 data 
garmin2022 <- read_csv("data/garminNests2022.csv", skip = 22, n_max = 15)
nests2022 <-
  garmin2022 %>% st_as_sf(coords = c("lon", "lat"),
                      crs = 4326,
                      remove = FALSE) %>% 
  #mutate(BiologgerID = stri_sub_replace(name, 4, 3, value = "I")) %>% #clean up bird IDs (i.e., BiologgerIDs)
  mutate(BiologgerID = str_to_upper(name)) %>% #standarize text formatting
  select(BiologgerID, Latitude = lat, Longitude = lon) %>%
  st_drop_geometry()

#Join everything
metaData2023R <- metaData2023 %>% 
  left_join(nests2023, by = "NestID") %>%
  left_join(nests2022, by = "BiologgerID") %>%
  unite("Latitude", "Latitude.x", "Latitude.y", na.rm = TRUE) %>%
  unite("Longitude", "Longitude.x", "Longitude.y", na.rm = TRUE) %>%
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Latitude = as.numeric(Latitude)) 
  #st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)




#Sex birds with discriminant function----
#Robertson et al. 2016 URL: https://doi.org/10.1675/063.039.sp125

data <- metaData2023R %>% filter(!Species == "Great Black-backed Gull") %>%
  mutate(Score = 2.893 * BillDepth + 0.892 * TotalHead + 0.136 * Wing - 232) %>%
  mutate(Sex = case_when(Score > 0 ~ "Male", Score < 0 ~ "Female"))




#Export clean metadata----
write.csv(data, "CountryIslandGullMetadata_clean2023-08-03.csv", row.names = FALSE)




#Plot DFs----


dataR <- data %>% filter(!is.na(BiologgerID)) %>%
  rowid_to_column("Bird")


p <-
  ggplot(data = dataR,
         aes(Score, Bird, colour = Sex, label = BiologgerID)) +
  #geom_point(size= 2.5, pch =21, stroke = 1) +
  geom_text(check_overlap = FALSE, show_guide = FALSE,
            vjust = 1, nudge_x = 0.05, nudge_y = 1, 
            fontface = "bold", size = 4.5) +
            #hjust = , vjust = -0.8) +
  # stat_ellipse(type = "t",
  #              linetype = 2,
  #              size = 1) +
  theme_half_open(12) +
  background_grid() +
  scico::scale_color_scico_d(palette = "bam",
                             begin = 0.1,
                             end = 0.9) +
  labs(colour  = "Sex", x = "Discriminant score", y = "Bird") +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(colour="grey49"), 
        axis.text.y = element_text(colour="grey49")) +
  theme(legend.position = c(0.87, 0.88)) +
  geom_vline(xintercept = 0, linetype = 2) +
  #ylim(0, 30) +
  expand_limits(x = c(-18, 20)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
  # geom_segment(aes(x = 0.25, y = 32, xend = 5, yend = 32), 
  #              linewidth = 1, lineend = "butt", 
  #              arrow = arrow(length = unit(0.1, "inches"))) +
  # geom_segment(aes(x = -0.25, y = 32, xend = -5, yend = 32), 
  #              linewidth = 1, lineend = "butt", 
  #              arrow = arrow(length = unit(0.1, "inches")))
  
p

p2 <- ggdraw(add_sub(p, expression(paste("D = 2.893BD + 0.892HL + 0.136WL - 232 (Robertson et al., 2016)")), size = 8))

p2


# p3 <- ggdraw(add_sub(p2, "Females", vpadding=grid::unit(0, "lines"),
#                      y = 41, x = 0.46))                                   
# p3
# p4 <- ggdraw(add_sub(p3, "Males", vpadding=grid::unit(0, "lines"),
#                      y = 42, x = 0.59))
# p4
#ggsave("C://Users/HynesD/Desktop/TotalHeadVsBillDepth.pdf", p, width = 7, height = 6.5)


#Other plots----

dataR2 <- dataR %>% select(BiologgerID, TotalHead:Mass, Score, Sex) %>%
  pivot_longer(c(2:7), names_to = "metric", values_to = "value")

ggplot(data = dataR2, aes(Score, value, colour = Sex)) +
  geom_point() +
  stat_ellipse() +
  stat_smooth() +
  labs(x = "Discriminant score", y = "Value (mm)") +
  scico::scale_color_scico_d(palette = "bam",
                             begin = 0.1,
                             end = 0.9) +
  facet_grid(metric ~ ., scales = "free") +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_half_open(12) +
  theme(legend.position = "none")

ggplot(data = dataR, aes(Tarsus, Wing, colour = Score >0, label = Sex)) +
   geom_point() +
   geom_label()


#To do: create nest history table----
