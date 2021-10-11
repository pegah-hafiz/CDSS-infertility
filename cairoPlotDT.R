cairoPlotDT <- function(button){
  
  library(rpart)
  
  library(rpart.plot)
  
  library(cairoDevice)
  
  f.address <- "C:/Users/Pegah/Desktop/R application/missing.csv"
  
  main.data<-read.csv(f.address, header=T)
  
  ivf.model <- rpart(result ~ . , data = main.data , method= "class")
  
  device <- gtkDrawingArea ( )
  
  asCairoDevice ( device )
  
  plotWindowDT <- gtkWindow ( show=FALSE )
  
  plotWindowDT$setDefaultSize(900,900) 
  
  plotWindowDT["title"] <- "Recursive Partitioning Tree for IVF/ICSI"
  
  plotWindowDT$add ( device )
  
  plotWindowDT$showAll ( )
  
  prp(x=ivf.model,type=0, extra=2, branch=1, under=T, fallen.leaves=T)
  
}