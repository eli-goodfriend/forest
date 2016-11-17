cleanData <- function(dataset){

  dataset$cover_type <- factor(dataset$cover_type)
  levels(dataset$cover_type) <- c("Spruce/Fir","Lodgepole Pine",
                                  "Ponderosa Pine","Cottonwood/Willow",
                                  "Aspen","Douglas Fir","Krummholz")
  
  nWild <- 4
  dataset$wilderness_area <- 0
  for (idx in seq(nWild)){
    cmd <- paste0("dataset$wilderness_area <- dataset$wilderness_area +
                  dataset$wilderness_area",idx,"*",idx)
    eval(parse(text = cmd))
    cmd <- paste0("dataset$wilderness_area",idx,"<- NULL")
    eval(parse(text = cmd))
  }
  dataset$wilderness_area <- factor(dataset$wilderness_area)
  levels(dataset$wilderness_area) <- c("Rawah","Neota","Comanche",
                                       "Cache la Poudre")
  
  nSoil <- 40
  dataset$soil_type <- 0
  for (idx in seq(nSoil)){
    cmd <- paste0("dataset$soil_type <- dataset$soil_type +
                  dataset$soil_type",idx,"*",idx)
    eval(parse(text = cmd))
    cmd <- paste0("dataset$soil_type",idx,"<- NULL")
    eval(parse(text = cmd))
  }
  dataset$soil_type <- factor(dataset$soil_type)

  
  return(dataset)
}



infoByCover <- function(dataset){
  # number of soil types
  soil_table <- table(dataset$cover_type, dataset$soil_type)
  soildata <- as.data.frame.matrix(soil_table)
  numSoilTypes <- rowSums(soildata > 0)
  coverdata <- data.frame(numSoilTypes)
  coverdata$cover_type <- rownames(coverdata)
  
  # number of wilderness regions
  wild_table <- table(dataset$cover_type, dataset$wilderness_area)
  wilddata <- as.data.frame.matrix(wild_table)
  numWildAreas <- rowSums(wilddata > 0)
  wilddata <- data.frame(numWildAreas)
  wilddata$cover_type <- rownames(wilddata)
  coverdata <- merge(coverdata, wilddata, by = "cover_type")
  wilddata <- NULL
  
  # length of interquartile range for continuous variables
  library(data.table)
  varNames <- c("elevation","aspect","slope",
                "horizontal_distance_to_hydrology",
                "vertical_distance_to_hydrology",
                "horizontal_distance_to_roadways",
                "hillshade_9am","hillshade_noon","hillshade_3pm",
                "horizontal_distance_to_fire_points")
  for (varName in varNames){
    cmd <- paste0("IQRdata <- setDT(dataset)[,list(",varName,"IQR = IQR(",
                  varName,")), by = list(cover_type)]")
    eval(parse(text=cmd))
    coverdata <- merge(coverdata, IQRdata, by = "cover_type")
  }
  
  rightOrder <-  c("Spruce/Fir","Lodgepole Pine",
                   "Ponderosa Pine","Cottonwood/Willow",
                   "Aspen","Douglas Fir","Krummholz")
  coverdata <- coverdata[match(rightOrder,coverdata$cover_type),]
  coverdata$cover_type <- factor(coverdata$cover_type, 
                                 levels = coverdata$cover_type)
  
  return(coverdata)
}



