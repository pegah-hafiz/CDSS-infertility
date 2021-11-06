cairoPlotVI <- function(button){
  
  library(randomForest)
  
  library(cairoDevice)
  
  #set.seed(2)
  
  f.address <- ""
  
  main.data<-read.csv(f.address, header=T)
  
  ivf.model <- randomForest(result ~ ., data=main.data, ntree=500,
                            keep.forest=T, importance=TRUE,
                            na.action='na.omit')

  
  imp <- importance(ivf.model, type=1, scale=T)
  
  device <- gtkDrawingArea ( )
  
  asCairoDevice ( device )
  
  plotWindowVI <- gtkWindow ( show=FALSE )
  
  plotWindowVI$setDefaultSize(700,700) 
  
  plotWindowVI["title"] <- "Variable Importance of IVF/ICSI"
  
  plotWindowVI$add ( device )
  
  plotWindowVI$showAll ( )
  
  varImpPlot(ivf.model,type=2)
  

}
