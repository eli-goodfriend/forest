# do initial data exploration
library(MonetDBLite)
library(DBI)
library(vcd)
library(ggplot2)

dbfolder <- "~/Data/forest/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
tablename <- "forest"

dataset <- dbGetQuery(db, paste("SELECT * FROM", tablename))
source("~/Dropbox/Code/forest/preprocess.R")
dataset <- cleanData(dataset)
coverdata <- infoByCover(dataset)



par(mar = rep(2, 4))

#################################################
# distribution of cover types by other variables
#################################################
# what's the overall distribution of trees
# lots of Spruce/Fir and Lodgepole Pine
# not a lot of anything else
ggplot(dataset, aes(cover_type, fill = cover_type)) + 
  geom_bar()

# overall distribution by wilderness area
# most trees are Spruce/Fir and Lodgepole Pine in
# Rawah and Comanche areas
# Neota has fewer trees, but most of them are still 
# Spruce/Fir and Lodgepole Pine
# In Cache la Poudre, however, there are few of those and
# Poderosa Pine dominate
ggplot(dataset, aes(cover_type, fill = cover_type)) +
  geom_bar() +
  facet_wrap( ~ wilderness_area)

#mosaic(cover_type ~ wilderness_area, dataset)

# overall distribution by soil type
ggplot(dataset, aes(cover_type, fill = cover_type)) +
  geom_bar() +
  facet_wrap( ~ soil_type)

ggplot(coverdata, aes(x=cover_type, y=numSoilTypes, fill = cover_type)) +
  geom_bar(stat = "identity")


# for each tree, what's its elevation range?
ggplot(dataset, aes(elevation, fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")

ggplot(dataset, aes(x = cover_type, y = elevation,
                    fill = cover_type)) +
  geom_violin() 

# for each tree, what's its slope range?
ggplot(dataset, aes(slope, fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")

ggplot(dataset, aes(x = cover_type, y = slope,
                    fill = cover_type)) +
  geom_violin() 

# aspect range?
ggplot(dataset, aes(aspect, fill = cover_type)) +
  geom_histogram() +
  #coord_polar(theta = "x") + # can't use with free scale
  facet_wrap( ~ cover_type, scales = "free_y")

ggplot(dataset, aes(x = cover_type, y = aspect,
                    fill = cover_type)) +
  geom_violin()

# what's its distance to road range?
ggplot(dataset, aes(horizontal_distance_to_roadways, 
                    fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")

ggplot(dataset, aes(x = cover_type, y = horizontal_distance_to_roadways,
                    fill = cover_type)) +
  geom_violin() 

# for each tree, what's its distance to hydrology range?
ggplot(dataset, aes(horizontal_distance_to_hydrology, 
                    fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")
ggplot(dataset, aes(vertical_distance_to_hydrology, 
                    fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")

ggplot(dataset, aes(x = cover_type, y = horizontal_distance_to_hydrology,
                    fill = cover_type)) +
  geom_violin() 
ggplot(dataset, aes(x = cover_type, y = vertical_distance_to_hydrology,
                    fill = cover_type)) +
  geom_violin() 

# distance to fire line
ggplot(dataset, aes(horizontal_distance_to_fire_points, 
                    fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")

ggplot(dataset, aes(x = cover_type, y = horizontal_distance_to_fire_points,
                    fill = cover_type)) +
  geom_violin()

# hillshade
ggplot(dataset, aes(hillshade_9am, 
                    fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")
ggplot(dataset, aes(hillshade_noon, 
                    fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")
ggplot(dataset, aes(hillshade_3pm, 
                    fill = cover_type)) +
  geom_histogram() +
  facet_wrap( ~ cover_type, scales = "free_y")

ggplot(dataset, aes(x = cover_type, y = hillshade_9am,
                    fill = cover_type)) +
  geom_violin()
ggplot(dataset, aes(x = cover_type, y = hillshade_noon,
                    fill = cover_type)) +
  geom_violin()
ggplot(dataset, aes(x = cover_type, y = hillshade_3pm,
                    fill = cover_type)) +
  geom_violin()

##########################################
# what's different about Cache la Poudre?
##########################################
# --- only low elevation
ggplot(dataset, aes(elevation)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area)

# --- only close to water
ggplot(dataset, aes(horizontal_distance_to_hydrology)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area)
ggplot(dataset, aes(vertical_distance_to_hydrology)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area)

# range of slopes
ggplot(dataset, aes(slope)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area)

# somewhat more peaked aspect
ggplot(dataset, aes(aspect)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area, scales = "free_y")

# --- only close to roads
ggplot(dataset, aes(horizontal_distance_to_roadways)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area, scales = "free_y")

# --- only close to fire points
ggplot(dataset, aes(horizontal_distance_to_fire_points)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area, scales = "free_y")

# --- wider range of hillshade
ggplot(dataset, aes(hillshade_9am)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area, scales = "free_y")
ggplot(dataset, aes(hillshade_noon)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area, scales = "free_y")
ggplot(dataset, aes(hillshade_3pm)) +
  geom_histogram() +
  facet_wrap( ~ wilderness_area, scales = "free_y")





qplot(horizontal_distance_to_hydrology, 
      vertical_distance_to_hydrology,
      color = cover_type,
      shape = wilderness_area,
      data = dataset)

qplot(slope, elevation,
      color = cover_type,
      shape = wilderness_area,
      data = dataset)

qplot(aspect, hillshade_noon,
      color = cover_type,
      shape = wilderness_area,
      data = dataset) + coord_polar("x")

qplot(horizontal_distance_to_fire_points, elevation,
      color = cover_type,
      shape = wilderness_area,
      data = dataset)


mosaic(cover_type ~ wilderness_area, data = dataset)
#assoc(cover_type ~ ., data = dataset) # needs table




