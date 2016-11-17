getConfusionMatrix <- function(fit, testdata){
  fitClasses <- predict(fit, newdata = testdata)
  confusionMatrix(data = fitClasses, testdata$cover_type)
}

plotVarImp <- function(fit){
  varImp <- varImp(fit)
  varImp <- t(varImp)
  
  library(reshape2)
  varImp.m <- melt(varImp)
  
  ggplot(varImp.m, aes(x = Var1, y = value, fill = Var2)) +
    geom_bar(stat="identity")
}