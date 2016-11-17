# try to predict cover type
library(MonetDBLite)
library(DBI)
library(vcd)
library(ggplot2)
library(caret)

dbfolder <- "~/Data/forest/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
tablename <- "forest"

dataset <- dbGetQuery(db, paste("SELECT * FROM", tablename))
source("~/Dropbox/Code/forest/preprocess.R")
dataset <- cleanData(dataset)
coverdata <- infoByCover(dataset)

inTrain <- createDataPartition(y = dataset$cover_type, p = .75, list = FALSE)
training <- dataset[inTrain,]
testing <- dataset[-inTrain,]

# --- generalized boosted models! ... internet seems to think these don't
# work in R for multiclass

# --- random forest! TODO wtf is that
library(randomForest)
rfFit <- randomForest(cover_type ~ .,
                      data = training,
                      ntree = 20, # should be bigger, but crashes the compy
                      importance = TRUE)

source("~/Dropbox/Code/forest/postprocess.R")
getConfusionMatrix(rfFit, testing)
plotVarImp(rfFit)


# random forest in caret, with cross validation
tc <- trainControl(method = "cv", number = 5)
rfFit <- train(cover_type ~ .,
                data = training, 
                method = "rf", ntree = 10,
                trControl = tc)





