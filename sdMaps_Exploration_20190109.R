library(tidyverse)
library(broom)
library(rgdal)
library(rgeos)
library(tictoc)
library(maps)
library(maptools)

#create theme for plots
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

theme_nothing <- theme_bw() + ditch_the_axes
theme_set(theme_nothing)

#map data for business improvement districts in SD
#pretty much just want this to have names and labels, won't use outlines probably
bidMap <- readOGR(dsn = "./maps/bids_datasd", layer = "bids_datasd")

bidNames <- bidMap@data

bidNames <- bidNames %>% 
  mutate(labpt = map(bidMap@polygons, "labpt"),
         labpt = str_extract(labpt, "[^c(].*[^)]")) %>% 
  separate(labpt, into = c("long", "lat"), sep = ",", convert = TRUE)

bidProject <- bidMap@proj4string

#map data for roads in SD
#this will make up the bulk of the plot
roadMap2 <- readOGR(dsn = "./maps/roads_datasd", layer = "roads_datasd")

#metadata for each road segment
#adding "slotID" which is essentially just the original order 
#of the data in the object.  However, this guarrantees I get
#the match to the coordinate data and can do joins.  In the end, may be duplicative
#of objectid and roadsegid, but playing it safe right now. Also this is easy
roadNames <- roadMap@data %>% 
  mutate(slotID = 1:nrow(roadNames))

roadProj <- proj4string(roadMap)

#coordinates for each road segment
roadMap <- tidy(roadMap)
#piece is 1 for all entries, group is redundant with id, removing those
roadMap <- roadMap %>% 
  select(-(piece:group)) %>% 
  mutate(slotID = as.integer(slotID) + 1)
#saving this cause remaking every session with tidy is slow
saveRDS(roadMap, "roadmap.rds")

#note tried using california outline/san diego county outline from maps package,
#but could not for the life of me get the projection from the maps package 
#to line up with the road data. used spTransform and the applied projection
#from roads, using various std CRS for the stuff retrieved from maps, nothing worked

#adding road class to roadMap data
#want to be able to filter what "types" of roads are visualized
#ie potentially take out alleys, paper streets, unpaved streets, etc
roadMap <- roadMap %>%
  left_join(roadNames[,c("slotID", "segclass")], by = "slotID") %>% 
  filter(!(segclass %in% c("A", "W", "P")))

#horrible coastal outline, TIGER Census outlines
californiaMap <- readOGR(dsn = "./maps/california", layer = "CA_State_TIGER2016")
californiaMap <- spTransform(californiaMap, CRS(roadProj))
californiaMap <- tidy(californiaMap)

#better coastal outline can get to line up with roads, doesn't do great for bays
californiaMap <- readOGR(dsn = "./maps/california/California_Counties", layer = "California_Counties")
californiaData <- californiaMap@data
californiaMap <- spTransform(californiaMap, CRS(roadProj))
californiaMap <- tidy(californiaMap)
sanDiegoMap <- filter(californiaMap, id == 36)

#all these maps are from http://rdw.sandag.org/, best option I could easily locate to
#give detailed coastal shapes
#county map, coastal outline, doesn't include bays
countymap <- readOGR(dsn = "./maps/california/County_Boundary", layer = "COUNTY_BOUNDARY")
countymap <- spTransform(countymap, CRS(roadProj))
countymap <- tidy(countymap)

#inlet and harbor bay details
harbormap <- readOGR(dsn = "./maps/california/Harbor_Bay", layer = "Harbor_Bay")
harbormap <- spTransform(harbormap, CRS(roadProj))
harbormap <- tidy(harbormap)

#pacific map, doesn't include harbor bay, extends N & S of SD county
pacificmap <- readOGR(dsn = "./maps/california/Pacific_Ocean", layer = "PACIFIC_OCEAN")
pacificmap <- spTransform(pacificmap, CRS(roadProj))
pacificmap <- tidy(pacificmap)

#need to find the line for the bottom of the county (ie usa/mexico)
bottomCounty <- countymap %>% 
  #southernmost point in county is SW corner next to mexico
  arrange(lat) %>% 
  #remove everything W of southernmost point (coastal border)
  filter(long > .[1, "long"]) %>%
  arrange(order) 

#DECENT!!!
p <- ggplot(data = harbormap, aes(x = long, y = lat, group = group)) +
  geom_path(data = countymap, color = NA) +
  geom_polygon(data = harbormap, fill = "lightsteelblue1") +
  geom_polygon(data = pacificmap, fill = "lightsteelblue1") +
  geom_path(data = bottomCounty, aes(x = long, y = lat), color = "grey", size = 1) +
  geom_path(data = test, aes(x = long, y = lat, group = slotID), color = "red") +
  coord_equal(ratio = 1, xlim = c(6244786, 6350258), ylim = c(1776062, 1936020))
  

p


