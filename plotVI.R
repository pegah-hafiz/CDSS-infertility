plotVI <- function(button){
  
  library(randomForest)
  
  #set.seed(2)
  
  filename <- "C:/Users/Pegah/Desktop/VI.pdf"
  
  f.address <- "C:/Users/Pegah/Desktop/R application/missing.csv"
  
  main.data<-read.csv(f.address, header=T)
  
  ivf.model <- randomForest(result ~ ., data=main.data, ntree=500,
                            keep.forest=T, importance=TRUE,
                            na.action='na.omit')
  pdf(filename)
  
  imp <- importance(ivf.model, type=1, scale=T)
  
  varImpPlot(ivf.model,type=2)
  
  dev.off()
  
  file.show(filename, title="Variable Importance for IVF")
}