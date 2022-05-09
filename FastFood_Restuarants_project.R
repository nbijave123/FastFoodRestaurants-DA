
# Load libraries
library(mapview)
library(rvest)
library(tidyverse)

# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
op <- options(gvis.plot.tag='chart')  # set gViz options


#Let's load shpfile of USA
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(RColorBrewer)

#Download Link:https://www2.census.gov/geo/tiger/GENZ2017/shp/ 
#We need to download zip file cb_2017_us_state_20m.zip from aforesaid link.

usa <- read_sf("C:/Users/Nilesh/Downloads/cb_2017_us_state_20m/cb_2017_us_state_20m.shp")  

usa

# check state geometry of USA
st_geometry(usa)

#load dataset
dataset <- read.csv("https://raw.githubusercontent.com/insaid2018/R/master/Projects/FastFoodRestaurants.csv")
#View(dataset)

# view 10 rows of dataset
View(head(dataset,10))

# let's see no. of rows, cols, colname in dataset 
names(dataset)
nrow(dataset)
ncol(dataset)

# CHECK - No. of countries
dataset$country[dataset$country!="US"]

# Check othe details
str(dataset)

# Let's see no of restuarants in each province of USA
table(dataset$province)
table(dataset$name)

# Let's try data correction with hunspell library
library(hunspell)

# First, make a list of correct words (TRUE/FALSE), subset the list from the 
# data, make corrections and pass the corrections back to the data frame.
# Note that 'hunspell_suggest' creates a list containing multiple suggestions 
# for each incorrect word. The first suggestion is often the best one, so I 
# extract it for my calculations
correct <- hunspell_check(dataset$name)
misspelled <- dataset[!correct, 2]
uniq_misspelled <- unique(misspelled) # Large redundancy reduction

# uniq_misspelled
# Spell checking is computationally very expensive. Use PARALLEL
# The below code creates a weird double list. The double map extracts the first 
# element of each sublist. 
library(parallel)

suggestions <- map(map(
  mclapply(uniq_misspelled, hunspell_suggest, mc.cores = 1),
  1),1)

#Now we can compare suggestions with dataset$name (list of Restaurants misspelled) 
View(suggestions)

View(table(dataset$name))

#It is observed with spell check that our suggestions are not giving right output for corrections.
#It is required some business name dictionary.
#Let's try with next library i.e. textclean

library("textclean")

Res_name<- replace_misspelling(dataset$name)
Res_name<- replace_contraction(Res_name)
Res_name<- replace_hash(Res_name)
Res_name<- replace_curly_quote(Res_name)
Res_name<- replace_grade(Res_name)
Res_name<- replace_incomplete(Res_name)
Res_name<- replace_kern(Res_name)
Res_name<- replace_names(Res_name)
Res_name<- replace_word_elongation(Res_name)
Res_name<- replace_internet_slang(Res_name)
Res_name<- replace_number(Res_name)
Res_name<- replace_ordinal(Res_name)
Res_name<- replace_tag(Res_name)
Res_name<- replace_symbol(Res_name)
Res_name<- replace_non_ascii(Res_name)
Res_name<- replace_non_ascii2(Res_name)

View(table(Res_name))
# Now it is comparable after few corrections. But not very confindent as we can check
# some KASA's Pizza beccome NASA's Pizza in new output column Res_name.
# still, we can add column to out dataset.

dataset$Res_name<- Res_name
View(dataset)

# And we can compare how textclean has changed restaurants name in dataset.

# Let's try hunspell suggest on our new column (It's for fun) as we are not expecting any
# better results with it.

words<- c(dataset$Res_name)

correct<-hunspell_check(words)
# Find suggestions for incorrect words
hunspell_suggest(words[!correct])

take_suggested<- map(map(mclapply(words[!correct], hunspell_suggest, mc.cores = 1)
                         ,1),1)

View(take_suggested)

# Now here we can see output is not acceptable for further processing of our data.(as expected)

library(RColorBrewer)
library(wordcloud)
wordcloud(table(dataset$name),
          min.freq = 1,
          max.words = 1000,
          random.order = FALSE,
          colors = brewer.pal(12, "Set3"))

# Lets try with wordcloud2.         

library(wordcloud2)
wordcloud2(table(dataset$name))
wordcloud2(table(dataset$city))

# Let's group dataset on province.
dataset %>% group_by(province) %>% add_tally() -> fastfood
View(fastfood)


# Let's plot fastfood restaurants in USA
FFplot <- theme(plot.title = element_text(family = "serif" , face = "bold", size = (10)))

background_theme <- theme(panel.background = element_blank(),
                          axis.text = element_blank(),
                          axis.title = element_blank(),
                          axis.ticks = element_blank(),
                          axis.line = element_blank())

ggplot() + geom_sf(data=usa) + geom_point(data=fastfood , aes(longitude, latitude, alpha =n, color = factor(name)), show.legend = FALSE) + 
  coord_sf(xlim = c(-130, -60), ylim = c(20, 50)) + 
  ggtitle("Fast Food Restaurants in USA") +
  FFplot + theme(plot.title = element_text(hjust = 0.5)) + background_theme



# CHECK - map type options
library(DT)
opt <- c("osm", "osm-bw","maptoolkit-topo", "waze", "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", "hillshade", "opencyclemap", "osm-transport", "osm-public-transport", "osm-bbike", "osm-bbike-german")
opt <- data.frame(opt)
datatable(opt, rownames = FALSE, options = list(pageLength = 5, scrollX=T), filter = "none")


# load package
library(leaflet)
# load library
m <- leaflet() %>% setView(lng = -90 , lat = 35 , zoom = 4.5)
# display map
m %>% addTiles()

# load packages
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# load data
world <- ne_countries(scale = "medium", returnclass = "sf")
# world map
ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)"))


library(rgeos)
library(ggspatial)
# world map
ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-130.00, -60.00), ylim = c(20.00, 50.00), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()


# extract locations
world_points<- st_centroid(world)
# extract labels
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
# generate annotated world map
worldmap<- ggplot(data = world) +
  geom_sf(fill= "gray90") +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-130.00, -60.00), ylim = c(20.00, 50.00), expand = FALSE) +
  #annotation_scale(location = "bl", width_hint = 0.5) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
   #                      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    #                     style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-130.00, -60.00), ylim = c(20.00, 50.00)) +
  theme(panel.grid.major = element_line(color = "gray60", linetype = "dashed", size = 0.25), 
        panel.background = element_rect(fill = "aliceblue")) +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "gray20", fontface = "italic", check_overlap = T, size = 3)

# inspect data
datatable(fastfood, rownames = FALSE, options = list(pageLength = 5, scrollX=T), filter = "none")

# plot data on world map
plot(worldmap, xlim = c(-130, -60), ylim = c(20, 50), 
     asp = 1, bg = "lightblue", col = "black", fill = T)
# add points
points(fastfood$longitude, fastfood$latitude, 
       col = "red", cex = .01)


# create a layer of borders
ggplot(fastfood, aes(x=longitude, y= latitude)) +   
  borders("world", colour=NA, fill="wheat1")  +
  geom_point(color="blue", alpha = .2, size = 2) +
  scale_x_continuous(name="longitude", limits=c(-130, -60)) +
  scale_y_continuous(name="latitude", limits=c(20, 50)) +
  theme(panel.background = element_rect(fill = "azure3", colour = "azure3")) +
  geom_text(aes(x=longitude, y= latitude, label= fastfood$name ),
            color = "gray20", check_overlap = T, size = 3)

# world data
# create map with density layer
ggplot(fastfood, (aes(x = longitude, y= latitude))) +   
  borders("world", colour=NA, fill="antiquewhite")  +
  stat_density2d(aes(fill = ..level..,  alpha = I(.2)),
                 size = 1, bins = 5, data = fastfood,
                 geom = "polygon") +
  geom_point(color="red", alpha = .2, size=2) +
  # define color of density polygons
  scale_fill_gradient(low = "grey50", high = "grey20") +
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # surpress legend
        legend.position = "none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_text(aes(x=longitude, y= latitude, label=fastfood$name),
            color = "gray20", fontface = "italic", check_overlap = T, size = 2,
            alpha = .3)


# load library
library(ggmap)
# define box
sbbox <- make_bbox(lon = c(-130, -60), lat = c(20, 50), f = .1)
# get map
usb = get_map(location=sbbox, zoom=4,
              #                source = "google",
              source = "osm",
              #                source = "stamen",
              color = "bw",
              #                color = "color",
              #               maptype="terrain")
              #               maptype="terrain-background")
              maptype="satellite")
#               maptype="hybrid")
#               maptype="toner")
#               maptype="hybrid")
#               maptype="terrain-labels")
#               maptype="roadmap")
# create map
usb = ggmap(usb)
# display map
usb +
  stat_density2d(data = fastfood, aes(x = longitude, y= latitude, 
                                      fill = ..level..,  alpha = I(.2)),
                 size = 1, bins = 5, geom = "polygon") +
  geom_point(data = fastfood, mapping = aes(x=longitude, y= latitude), 
             color="gray20", alpha = .2, size=2) +
  # define color of density polygons
  scale_fill_gradient(low = "grey50", high = "grey20") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "aliceblue",
                                        colour = "aliceblue"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # surpress legend
        legend.position = "none")


# define box
sbbox <- make_bbox(lon = c(-130, -60), lat = c(20, 50), f = .1)
# get map
locmax = get_map(location=sbbox, zoom=4,
                 maptype="terrain")
# create map
locmaxmap = ggmap(locmax)
# display map
locmaxmap +
  geom_point(data = fastfood, mapping = aes(x = longitude, y = latitude), 
             color = "green") +
  geom_text(data = fastfood, 
            mapping = aes(x = longitude+0.1,
                          y = latitude,
                          label = fastfood$name),
            size = 2, color = "gray20", 
            fontface = "bold", 
            check_overlap = T) 

# Lets reset options and see session info()
options(op)

sessionInfo()

# Thank you very much