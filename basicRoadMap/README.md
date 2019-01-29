Making a Basic Map of San Diego Area Roads with R
================
Camille Sultana
1/11/2019

-   [Project description](#project-description)
-   [Load packages](#load-packages)
-   [Read in and format map data](#read-in-and-format-map-data)
    -   [County boundary](#county-boundary)
    -   [Map labels](#map-labels)
    -   [Road data](#road-data)
-   [Unified SD area map](#unified-sd-area-map)
    -   [Base map](#base-map)
    -   [Add roads](#add-roads)
    -   [Add labels](#add-labels)
    -   [Combine all](#combine-all)

Project description
-------------------

**Project Goal** Create map of San Diego area roads.

**Project Description** Clean and organize geospatial data to be compatible with tidyverse packages. Visualize geospatial data utilizing ggplot techniques. Present work with RMarkdown.

**Learning goals** -- making maps in ggplot -- r mapping tools/packages (rgdal, maptools, maps, etc) -- CRS

Please note statements regarding coordinate reference systems (CRS) and working with map data are based on a brief scouring of web resources. Find a more reputable source to learn the ins and outs of CRS! Don't take my word for it.

Load packages
-------------

Read in and format map data
---------------------------

Unless otherwise noted map data was acquired as shapefiles from the SANDAG GIS Data Warehose (<http://rdw.sandag.org>). Shapefiles are imported using the rgdal::readOGR function, and then converted into data frames with tidy::broom to enable plotting using the ggplot2 package.

### County boundary

I originally tried using the California outline/SD county outline from maps package, but I could get the projection from the maps package to line up with the road data from SANDAD. Multiple efforts using spTransform failed. Eventually I realized that the coastal resolution from those datasets was too poor for my purposes anyways. Also tried using TIGER boundaries from the US Census bureau, but these also had very poor coastal resolution.

#### Read in data

``` r
#all these maps are from http://rdw.sandag.org/, best option I could easily locate to
#give detailed coastal shapes

paste0(rootDir,"/maps/california/County_Boundary")

#county map, coastal outline, doesn't include bays
countymap <- readOGR(dsn = paste0(rootDir,"/maps/california/County_Boundary"), layer = "COUNTY_BOUNDARY")
#inlet and harbor bay details, turns out these are redundant with pacific map
# harbormap <- readOGR(dsn = "./maps/california/Harbor_Bay", layer = "Harbor_Bay")
#pacific map, doesn't include harbor bay, extends N & S of SD county
pacificmap <- readOGR(dsn = paste0(rootDir,"/maps/california/Pacific_Ocean"), layer = "PACIFIC_OCEAN")

#check to see if the CSR/projections are identical
allProj <- purrr::map_chr(list(pacificmap, countymap), proj4string)

#check to make sure all CSR are the same
length(unique(allProj))

#save current projection to compare against later
currentCSR <- proj4string(pacificmap)

#convert to data frames so can use ggplot later
countymap <- tidy(countymap)
pacificmap <- tidy(pacificmap)
# harbormap <- tidy(harbormap)
```

#### Quick view boundaries

``` r
#quick plots of map boundaries, check to make sure everything looks good

#county map in grey/blue
c <- ggplot(data = countymap, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = countymap, color = "blue", fill = "grey")
#pacific map in red/yellow
p <- geom_polygon(data = pacificmap, color = "yellow", fill = "red")
#different zoom levels
zoom <- coord_equal(ratio = 1, xlim = c(6240000, 6315000), ylim = c(1770000, 1880000))
zoom2 <- coord_equal(ratio = 1, xlim = c(6245000, 6270000), ylim = c(1850000, 1870000))

#full range county and pacific map
c + p
```

![](README_files/figure-markdown_github/county%20boundary%20quickmap-1.png)

The Pacific and SD county map look well aligned. The Pacific map coastline goes far above and below the extent of SD county which will make plots look nice later.

``` r
#zoom in of county and pacific map
c + p + labs(title = "County & Pacific") + zoom
#county map only
c + labs(title = "County only") + zoom
```

![](README_files/figure-markdown_github/map%20zoom-1.png)![](README_files/figure-markdown_github/map%20zoom-2.png)

The Pacific map shows SD coastal boundaries with high resolution. The SD county map lacks most of these details.

``` r
#close zoom in of mission bay area
c + p + zoom2
```

![](README_files/figure-markdown_github/map%20zoom2-1.png)

In this quickly made plot, island shapes detailed by the Pacific map are filled the same color as the ocean. Will need to identify what data corresponds to the islands so they can match the county land in color.

#### Modify map components

``` r
#to find islands
#create a label for all pieces in pacificmap dataframe
#using pieces and not groups to disambiguate 0.2 and 0.20
pacLabel <- pacificmap %>%
  group_by(piece) %>%
  mutate(rowNum = 1:n()) %>%
  filter(rowNum == 1)

#plot with piece labels to identify islands
c + p + zoom2 + geom_text(data = pacLabel, aes(label = piece), color = "blue", size = 2.5)
```

![](README_files/figure-markdown_github/find%20islands-1.png)

``` r

#create an isIsland group to be able to control fill
islandGroup <- c(5, 19, 20, 21, 18) #island pieces
pacificmap <- pacificmap %>%
  mutate(isIsland = piece %in% islandGroup)

#new geom with fill as aes
i <- geom_polygon(data = pacificmap, color = "yellow", aes(fill = isIsland))

#plots filling islands same color as county
c + i + zoom2 + scale_fill_manual(values = c("TRUE" = "grey", "FALSE" = "red"))
```

![](README_files/figure-markdown_github/find%20islands-2.png)

``` r

#zooming out to make sure everything else still looks okay
c + i + zoom + scale_fill_manual(values = c("TRUE" = "grey", "FALSE" = "red"))
```

![](README_files/figure-markdown_github/find%20islands-3.png)

``` r
#want to get rid of west coast county line so don't have to worry about
#order for pacific and sdmap. When zooming into SD area only bottom boundary
#will be visible

#find the line for the bottom of the county (ie usa/mexico)
noWestCounty <- countymap %>%
  #southernmost point in county is SW corner next to mexico
  arrange(lat) %>%
  #remove everything W of southernmost point (coastal border)
  filter(long > .[1, "long"]) %>%
  arrange(order)

#map with border only and pacific
tmpMap <- ggplot(data = pacificmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = pacificmap, color = "blue", aes(fill = isIsland)) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "blue"), guide = FALSE) +
  geom_path(data = noWestCounty, color = "black")

tmpMap
```

![](README_files/figure-markdown_github/us%20mexico%20border-1.png)

``` r
tmpMap + zoom
```

![](README_files/figure-markdown_github/us%20mexico%20border-2.png)

Islands are now colored properly, and SD area map shows only southern boundary.

### Map labels

#### Read in data

Acquire data to be able to label map with a few municipalities in SD area and select neighborhoods in SD.

``` r
#labels for neighborhoods
#can't use readOGR here, throws error: Incompatible geometry: 4
neighLabels <- st_read(paste0(rootDir,"/maps/Community_Points/Community_Points.shp"))

#labels for municipalities in SD county
cityLabels <- readOGR(dsn = paste0(rootDir,"/maps/Municipal_Boundaries"), layer = "MUNICIPAL_BOUNDARIES")

#check to see if projections are the same as previously utilized maps

#first make sure projections for all datasets are the same
#return CRS string for each set of map data
neighProj <- attributes(neighLabels$geometry)$crs$proj4string
cityProj <- proj4string(cityLabels)

#combine all CSR into one list
allCSR <- list(current = currentCSR, neigh = neighProj, city = cityProj)

#any() call will return TRUE if any of the CSR don't match
areDup <- duplicated(allCSR)
if (any(!areDup[-1])) {
  print(areDup)
  purrr::map(allCSR, print)
}
```

The neighProj coordinate reference system (CRS) string differs only in that it doesn't have the ellipse (ellps) and the geocentric location (towgs84) specified. However, the two CRS are actually identical as they have the same datum (NAD83) which defines the globe (ellipse) shape and origin of the coordinate axes. So we can move on and convert these relatively complex geometry/shape structures into simple dataframes. The label data has the same CRS as the map boundary data.

#### Convert to dataframe

``` r
#store projection to compare against data downloaded later
currentProj <- cityProj

#convert from simple feature object to dataframe
neighNames <- data_frame(NAME = neighLabels$COMMUNITY) %>%
  mutate(long = purrr::map_dbl(neighLabels$geometry, function(x) {x[[1]]}),
         lat = purrr::map_dbl(neighLabels$geometry, function(x) {x[[2]]}))

#check to make sure all names are unique
head(count(neighNames, NAME, sort = T))

#cityLabels actually contains the boundaries for all munic in SD county
#only interested in the placement for the label

#get data (interested in city name) for each polygon in structure
cityNames <- cityLabels@data %>% select(NAME, CODE)
#extract label location for each city from boundary polygon data and add to dataframe
cityNames <- cityNames %>%
  mutate(labpt = purrr::map(cityLabels@polygons, "labpt"),
         labpt = str_extract(labpt, "[^c(].*[^)]")) %>%
  separate(labpt, into = c("long", "lat"), sep = ",", convert = TRUE) %>%
  #make all names uppercase, no funny business with strings not matching
  mutate(NAME = toupper(NAME)) %>%
  #remove labels for county
  filter(!(str_detect(NAME, "COUNTY")))

#check to see if there is one label location for each city
head(count(cityNames, NAME, sort = T))
## # A tibble: 6 x 2
##   NAME                 n
##   <fct>            <int>
## 1 Mission Bay Park     7
## 2 East Village         2
## 3 Mira Mesa            2
## 4 Miramar              2
## 5 Rancho Bernardo      2
## 6 San Pasqual          2
## # A tibble: 6 x 2
##   NAME            n
##   <chr>       <int>
## 1 SAN DIEGO       5
## 2 VISTA           3
## 3 ESCONDIDO       2
## 4 OCEANSIDE       2
## 5 CARLSBAD        1
## 6 CHULA VISTA     1
```

There are multiple label locations for a few cities and neighborhoods. For these locations we can overlay with google maps to determine which one is the best choice. However, google maps uses the CRS WGS84. The data from SANDAG will need to be converted to this projection to line up with google maps.

#### Google Maps Label Data

``` r
#get google map for san diego area
sanGoogle <- get_googlemap(center = c(lon = -117.187756, lat = 33.000356), zoom = 10, scale = 2, maptype ='terrain', color = 'color')

#NOTE google maps uses the CRS WGS84 which has the code EPSG: 4326.

#convert pacific map data
pacTmp <- readOGR(dsn = paste0(rootDir,"/maps/california/Pacific_Ocean"), layer = "PACIFIC_OCEAN")
pacTmp <- spTransform(pacTmp, CRS("+init=EPSG:4326"))
pacTmp <- tidy(pacTmp)

#convert city label data
cityTmp <- readOGR(dsn = paste0(rootDir,"/maps/Municipal_Boundaries"), layer = "MUNICIPAL_BOUNDARIES")
cityTmp <- spTransform(cityTmp, CRS("+init=EPSG:4326"))
cityNTmp <- cityTmp@data %>% select(NAME, CODE)

#extract label location for each city from boundary polygon data and add to dataframe
cityNTmp <- cityNTmp %>%
  mutate(labpt = purrr::map(cityTmp@polygons, "labpt"),
         labpt = str_extract(labpt, "[^c(].*[^)]")) %>%
  separate(labpt, into = c("long", "lat"), sep = ",", convert = TRUE) %>%
  #make all names uppercase, no funny business with strings not matching
  mutate(NAME = toupper(NAME)) %>%
  #remove labels for county
  filter(!(str_detect(NAME, "COUNTY"))) %>%
  group_by(NAME) %>%
  mutate(total = sum(n()), label = row_number())

#plot pacific math shape to verify that did CRS conversion correctly
ggmap(sanGoogle) + geom_path(data = pacTmp, aes(long, lat, group = group), color = "red")
```

![](README_files/figure-markdown_github/city%20labels%20google%20map-1.png)

``` r

#plot city labels to see where they overlap with google map
ggmap(sanGoogle) + geom_text(data = cityNTmp, aes(label = CODE, x = long, y = lat, color = factor(label))) + scale_color_manual(values = c("1" = "black", "2" = "green", "3" = "blue", "4" = "red", "5" = "orange"))
```

![](README_files/figure-markdown_github/city%20labels%20google%20map-2.png)

The matching coastal boundary from the SANDAG database confirms that the SANDAG CRS conversion to the google maps definition (WGS84) was correct. The city label locations from SANDAG don't have good overlap with the location on google maps, including labels that have only one location in the database (see CB, Carlsbad; CO, Coronado Island). Rather than using SANDAG data in the labels, it would likely be more accurate to use geocode data pulled from google.

``` r
#get geocode data for cities

#getting unique city names from SANDAG data
cityNTmp <- cityNTmp %>%
  select(NAME, CODE, label) %>%
  filter(label == 1) %>%
  #"City, CA" string to pass to geocode
mutate(geoNAME = paste(NAME, "CA", sep = ", "))

#get geolocation of cities from google and concatenate back to dataframe with labels
#note need to get google cloud account set up to do this
geoLOC = geocode(cityNTmp$geoNAME)
cityNTmp <- data.frame(cityNTmp, geoLOC)

#check to make sure good overlap between geocode data and google maps location
ggmap(sanGoogle) + geom_text(data = cityNTmp, aes(label = CODE, x = lon, y = lat))
```

![](README_files/figure-markdown_github/cityNames%20geocode-1.png)

The city labels now match up with labels provided by google maps, and are in sensical locations (eg Oceanside OC is on the coast, Coronado Island CO is on the island).

``` r
#takes a df with lat and long
#and converts to a spatial points df
#then converts from old CRS to a new input CRS (input as string)
#returns a df back
gmapCSR <- function(data, oldCRS, newCRS){
  #convert dataframe to spatial points data frame
  coordinates(data) <- cbind(data$long, data$lat)
  proj4string(data) <- CRS(oldCRS)
  #convert the CRS
  data <- spTransform(data, newCRS)
  #convert back to dataframe
  data <- tidy(data) %>%
    mutate(long = coords.x1, lat = coords.x2) %>%
    select(-coords.x1, -coords.x2)
  #return df
  data
}
```

``` r
#get geolocations of neighborhoods from google

#convert neighborhood names to ggmap CRS
neighNamesSp <- gmapCSR(neighNames, currentCSR, "+init=EPSG:4326")

#there are some neighborhood names in the SANDAG list that seem overly detailed
#specifying "east" or "west"
#cut the east/west off these names,
#want to cut names with three words total, not two or less
neighFilter <- "[A-Z][a-z]+ [A-Z][a-z]+ ((West)|(East))$"
neighExtract <- "^[A-Z][a-z]+ [A-Z][a-z]+"
neighNamesSp <- neighNamesSp %>%
  mutate(NAME = as.character(NAME),
         NAME = if_else(str_detect(NAME, neighFilter),
                  str_extract(NAME, neighExtract), NAME)
    ) %>%
  #want only one row for each unique neighborhood name
  group_by(NAME) %>%
  mutate(rowNum = row_number()) %>%
  filter(rowNum == 1) %>%
  ungroup()

#create geocode string to get location from google maps
#some of these are not actually neighborhoods but
#unincorporated communities outside San Diego
#for neighborhoods want geocode input to be X, San Diego, CA
#for communities want X, CA

#super rough approximation of San Diego border
#anything southwest of this point considered in SD, everything else outside SD (city)
sdBord = tibble(lat = 32.951014, long = -116.994026)

#add geocode string
neighNamesSp <- neighNamesSp %>%
  mutate(geoNAME = ifelse(long < sdBord$long & lat < sdBord$lat,
                          paste(NAME, "San Diego", "CA", sep = ", "),
                          paste(NAME, "CA", sep = ", ")))

#get geolocation of cities from google and concatenate back to dataframe with labels
geoLOC = geocode(neighNamesSp$geoNAME)
neighNamesSp <- data.frame(neighNamesSp, geoLOC)

#get zoomed in version of San Diego map that shows some neighborhoods
sanZoomGoogle <- get_googlemap(center = c(lon = -117.125104, lat = 32.739566), zoom = 12, scale = 2, maptype ='terrain', color = 'color')

#plot labels
ggmap(sanZoomGoogle) + geom_text(data = neighNamesSp, aes(label = NAME, x = lon, y = lat.1), size = 2, color = "red")
```

![](README_files/figure-markdown_github/neighborhood%20geocode-1.png)

``` r
ggmap(sanGoogle) + geom_text(data = neighNamesSp, aes(label = NAME, x = lon, y = lat.1), size = 2, color = "red")
```

![](README_files/figure-markdown_github/neighborhood%20geocode-2.png)

Obviously not all these labels can be plotted at once. However, a quick scan indicates the labels are in their proper place.

``` r
#convert labels from google to current CRS
neighNamesSp <- neighNamesSp %>%
  select(-long, -lat) %>%
  rename(long = lon, lat = lat.1)
neighNamesSp <- gmapCSR(neighNamesSp, "+init=EPSG:4326", currentCSR)
cityNTmp <- cityNTmp %>%
  rename(long = lon)
cityNTmp <- gmapCSR(cityNTmp, "+init=EPSG:4326", currentCSR)
```

### Road data

#### Read in data

``` r
#note eval is FALSE for my purposes to prevent dealing with 
#parsing the road data everytime. Should change to true
#for first evaluation on your machine

#map data for roads in SD
#this will make up the bulk of the plot
roadMap <- readOGR(dsn = paste0(rootDir,"/maps/Roads_All"), layer = "ROADS_ALL")

#metadata for each road segment
#adding "slotID" which is essentially just the original order
#of the data in the object.  However, this guarrantees I get
#the match to the coordinate data and can do joins.  In the end, may be duplicative
#of objectid and roadsegid, but playing it safe right now. Also this is easy
roadNames <- roadMap@data
roadNames <- mutate(roadNames, slotID = 1:nrow(roadNames))

#check CRS
roadProj <- proj4string(roadMap) #CRS for roadMap
allCSR <- list(current = currentCSR, road = roadProj) #combine all CSR into one list
areDup <- duplicated(allCSR) #any() call will return TRUE if any of the CSR don't match
if (any(!areDup[-1])) {
  print(areDup)
  purrr::map(allCSR, print)
}
#road Map CRS the same as pacific/county map

#coordinates for each road segment
roadMap <- tidy(roadMap)

#organize road data
roadMap <- roadMap %>%
  #piece is 1 for all entries, group is redundant with id, removing those
  select(-(piece:group)) %>%
  #join map data to metadata in names with slotID
  mutate(id = as.integer(id) + 1) %>%
  rename(slotID = id) %>%
  left_join(roadNames[,c("slotID", "SEGCLASS", "ROADID", "ROADSEGID", "ADDSEGDT", "RD30FULL")], by = "slotID") %>%
  #get rid of alleys, walkways, and "paper" roads in dataset
  filter(!(SEGCLASS %in% c("A", "W", "P")))

#saving this cause remaking every session with tidy is slow
saveRDS(roadMap, "roadmap.rds")
```

#### Select SD metro area roads

``` r
#note eval is FALSE for my purposes to prevent dealing with 
#parsing the road data everytime. Should change to true
#for first evaluation on your machine

#will only show data for roads south and west of here
sdBord = tibble(lat = 32.959865, long = -116.931829)
#convert CSR
sdBord <- gmapCSR(sdBord, "+init=EPSG:4326", currentCSR)

sdRoadMap <- roadMap %>%
  filter(long < sdBord$long, lat < sdBord$lat)

#fixing the dates for addsegdt, currently a factor
sdRoadMap <- sdRoadMap %>%
  mutate(ADDSEGDT = ymd(ADDSEGDT),
         ADDYEAR = year(ADDSEGDT),
         #converting to character, mixed numeric and character factor
         #levels are a pain to deal with later
         SEGCLASS = as.character(SEGCLASS))


#saving this so don't have to load in giant roadMap everytime and save all filtering, mutating work
saveRDS(sdRoadMap, paste0(rootDir,"/variables/", "sdRoadMap.rds"))
```

``` r
sdRoadMap <- readRDS(paste0(rootDir,"/variables/","sdRoadMap.rds"))

#quick plot to make sure everything is working
ggplot(data = sdRoadMap[1:300000,]) +
  geom_path(aes(x = long, y = lat, group = slotID), color = "red", size = 0.2)
```

![](README_files/figure-markdown_github/read%20in%20saved%20sdroad%20data-1.png)

Cool looks like a bunch of road bits.

#### Refine road mapping

``` r
#coordinate limits for test map
#creating these plots takes forever on my litte computer
#better to play around with a smaller dataset when trying out plots
sdBord = tibble(lat = c(32.843787, 32.756155) , long = c(-117.116684, -117.207780))
#convert CSR
sdBord <- gmapCSR(sdBord, "+init=EPSG:4326", currentCSR)

sdMini <- sdRoadMap %>%
  filter(long < sdBord$long[1], lat < sdBord$lat[1], long > sdBord$long[2], lat > sdBord$lat[2])

#all roads at once
ggplot(data = sdMini) +
  geom_path(aes(x = long, y = lat, group = slotID), color = "red", size = 0.2) +
  coord_equal() +
  labs(title = "all road segments")
```

![](README_files/figure-markdown_github/test%20plot%20roads-1.png)

Plotting all road types at once for a small SD area, shows that giving equal line color and thickness creates a very visually dense and overwhelming map. We need to better understand what each road type (SEGCLASS) contributes to the map to be able to choose more appealing colors and weights.

``` r
#a function to plot a specific road type (SEGCLASS) from a df
plotClass <- function(data, class) {
  #roadtypes from Sandag
  classDescr = c("1" = "Freeway/Expressway", "2" = "Highway/State Routes", "3" = "Minor Highway/Major Roads",
                 "4" = "Arterial or Collector", "5" = "Local Street", "6" = "Unpaved Road", "7" = "Private Road",
                 "8" = "Freeway Transition Ramp", "9" = "Freeway On/Off Ramp", "A" = "Alley", "H" = "Speed Hump",
                 "M" = "Military street within base", "P" = "Paper street", "Q" = "Undocumented", "W" = "Walkway",
                 "Z" = "Named private street")
  #filter data to specific road type and create plot
  data %>%
    filter(SEGCLASS == class) %>%
    ggplot() +
    geom_path(aes(x = long, y = lat, group = slotID), color = "red", size = 0.2) +
    labs(title = classDescr[as.character(class)]) +
    coord_equal()
}
```

``` r
#create plot for each road type
map(unique(sdMini$SEGCLASS), plotClass, data = sdMini)
```

<img src="README_files/figure-markdown_github/test plot roads 2-1.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-2.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-3.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-4.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-5.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-6.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-7.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-8.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-9.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-10.png" width="40%" /><img src="README_files/figure-markdown_github/test plot roads 2-11.png" width="40%" />

It looks like roads can be divided into freeways, major roads/highways, and local streets. Freeways have three categories of SEGCLASS: freeway, freeway on/off ramp, and freeway transition ramp. Freeways should be emphasized in the map, but equally weighting all three freeway SEGCLASS's may become too cluttered.

``` r
#SEGCLASS description from SANDAG
#classDescr = c("1" = "Freeway/Expressway", "2" = "Highway/State Routes", "3" = "Minor Highway/Major Roads",
#               "4" = "Arterial or Collector", "5" = "Local Street", "6" = "Unpaved Road", "7" = "Private Road",
#               "8" = "Freeway Transition Ramp", "9" = "Freeway On/Off Ramp", "A" = "Alley", "H" = "Speed Hump",
#               "M" = "Military street within base", "P" = "Paper street", "Q" = "Undocumented", "W" = "Walkway",
#               "Z" = "Named private street")

#plot freeways and ramps
sdMini %>%
  filter(SEGCLASS %in% c(1, 8, 9)) %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = slotID, color = SEGCLASS, size = SEGCLASS)) +
  scale_color_manual(values = c("1" = "black", "8" = "red", "9" = "orange")) +
  scale_size_manual(values = c("1" = 1, "8" = 0.4, "9" = 0.4)) +
  labs(title = "Freeways with ramps") +
  coord_equal()
```

![](README_files/figure-markdown_github/freeway%20plot-1.png)

As the data is currently organized, ramps plot on top of the actual freeways. I originally thought that the display order for geom\_path was controlled by the factor level order of SEGCLASS if color was set to SEGCLASS in the aesthetics. However, after testing it was obvious that this is not the case. The plotting order for geom\_path is actually controlled by the group aesthetic (slotID for this map). SlotID needs to be adjusted so that road segments to be plotted last have the highest slotID.

``` r
#reording slotID to change geom_path plotting order
#want smallest roads plotted first, largest roads plotted last (on top)

#local roads should be first, then major roads, then freeways in factor level
local = c("5","6","7","H","M","Z")
major = c("2","3", "4")
ramp = c("8","9")
freeway = c("1")

#current max slotID
max(sdRoadMap$slotID)
addSlot = 200000

#add 200000*n to each set of roads after local to reorder slotIDs but keep them unique
sdRoadMap$slotID[sdRoadMap$SEGCLASS %in% major] <- sdRoadMap$slotID[sdRoadMap$SEGCLASS %in% major] + addSlot
sdRoadMap$slotID[sdRoadMap$SEGCLASS %in% ramp] <- sdRoadMap$slotID[sdRoadMap$SEGCLASS %in% ramp] + addSlot*2
sdRoadMap$slotID[sdRoadMap$SEGCLASS %in% freeway] <- sdRoadMap$slotID[sdRoadMap$SEGCLASS %in% freeway] + addSlot*3

#new sdMini with new slotID
sdMini <- sdRoadMap %>%
  filter(long < sdBord$long[1], lat < sdBord$lat[1], long > sdBord$long[2], lat > sdBord$lat[2])

#quickplot to make sure order is working
sdMini %>%
  filter(SEGCLASS %in% c(1, 8, 9, 4, 3)) %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = slotID, color = SEGCLASS, size = SEGCLASS)) +
  coord_equal() +
  scale_color_manual(values = c("1" = "black", "8" = "red", "9" = "orange", "4" = "grey", "3" = "green")) +
  scale_size_manual(values = c("1" = 1, "8" = 0.4, "9" = 0.4, "4" = 0.1, "3" = 0.4))
```

![](README_files/figure-markdown_github/reorder%20slotID-1.png)

By reordering slotID, freeways now appear on top of all other road segments. (Freeways &gt; ramps &gt; major roads &gt; minor roads). This may not matter if decide to keep roads all one color, but better to be prepared.

``` r
#create new road class that combines segclass

#segclass groupings
local <- c("5" = "l", "6" = "l", "7" = "l", "H" = "l", "M" = "l", "Z" = "l")
major = c("2" = "m", "3" = "m", "4" = "m")
ramp = c("8" = "r", "9" = "r")
freeway = c("1" = "f")
allR = c(local, major, ramp, freeway)

#create new column
sdRoadMap <- sdRoadMap %>%
  mutate(NEWCLASS = unname(allR[SEGCLASS]))
```

``` r
#new sdMini with new slotID
sdMini <- sdRoadMap %>%
  filter(long < sdBord$long[1], lat < sdBord$lat[1], long > sdBord$long[2], lat > sdBord$lat[2])

#playing with how to display roads
sdMini %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = slotID, color = NEWCLASS, size = NEWCLASS)) +
  coord_equal() +
  scale_color_manual(values = c("l" = "grey65", "m" = "grey65", "r" = "grey47", "f" = "grey47"), guide = FALSE) +
  scale_size_manual(values = c("l" = 0.1, "m" = 0.4, "r" = 0.1, "f" = 0.5), guide = FALSE)
```

![](README_files/figure-markdown_github/road%20map%20theme-1.png)

Unified SD area map
-------------------

### Base map

``` r
#get edges for map border
sdBord = tibble(lat = c(32.55, 32.959865), long = c(-117.27, -116.931829))
sdBord <- gmapCSR(sdBord, "+init=EPSG:4326", currentCSR)

#create map plot theme
oceanCol <- "slategray2"
landCol <- "grey98"

themeMap <- theme_classic() + theme(panel.background = element_rect(fill = landCol),
                                    axis.line = element_blank(),
                                    axis.title = element_blank(),
                                    axis.ticks = element_blank(),
                                    axis.text = element_blank())

theme_set(themeMap)

#create base plot with coast outline
mapBase <- ggplot(data = pacificmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = pacificmap, color = oceanCol, aes(fill = isIsland)) +
  scale_fill_manual(values = c("TRUE" = landCol, "FALSE" = oceanCol), guide = FALSE) +
  geom_path(data = noWestCounty, color = "grey40", size = 0.5) +
  coord_equal(xlim = sdBord$long, ylim = sdBord$lat)
mapBase
```

![](README_files/figure-markdown_github/map%20base%20theme-1.png)

### Add roads

``` r
#on this zoomed out version only plotting major roads/freeways not local
#otherwise very congested/kills my computer
sdRoadMapMajor <- sdRoadMap %>%
  filter(NEWCLASS %in% c("f", "m", "r"))

roadBase <- mapBase +
  geom_path(aes(x = long, y = lat, group = slotID, color = NEWCLASS, size = NEWCLASS),
            data = sdRoadMapMajor) +
  scale_color_manual(values = c("l" = "grey65", "m" = "grey65", "r" = "grey57", "f" = "grey57"), guide = FALSE) +
  scale_size_manual(values = c("l" = 0.1, "m" = 0.1, "r" = 0.1, "f" = 0.4), guide = FALSE)
roadBase
```

![](README_files/figure-markdown_github/add%20roads-1.png)

### Add labels

``` r
#was playing around with knitr options which clears cache i think
#set this again rather than having to rerun other chunks
theme_set(themeMap)

#create this so can give SAN DIEGO different aesthetics as rest of labels
cityNTmp <- cityNTmp %>%
  mutate(isSD = NAME == "SAN DIEGO")

#testing out how to display labels without road data, since takes a long time on my computer
mapBase +
geom_text(data = cityNTmp,
          aes(label = NAME, x = long, y = lat, group = "", size = isSD),
          color = "dodgerblue3", fontface = "bold") +
  scale_size_manual(values = c("TRUE" = 3, "FALSE" = 1.5), guide = FALSE)
```

![](README_files/figure-markdown_github/add%20labels-1.png)

``` r

#decided not to use neighborhood labels after all
```

### Combine all

``` r
#was playing around with knitr options which clears cache i think
#set this again rather than having to rerun other chunks
theme_set(themeMap)

#the new scale_size_manual overrides the previous one from roadBase,
#so have to include the size specifications for the roads here as well
roadBase +
  geom_text(data = cityNTmp,
            aes(label = NAME, x = long, y = lat, group = "", size = isSD),
            color = "dodgerblue3", fontface = "bold") +
  scale_size_manual(values = c("TRUE" = 3, "FALSE" = 1.5,
                               "l" = 0.1, "m" = 0.1, "r" = 0.1, "f" = 0.4),
                    guide = FALSE)
```

![](README_files/figure-markdown_github/full%20map-1.png)

A decent-ish looking SD area plot! This will be the basis for a project to show the construciton of SD area roads over the past 30 years.
