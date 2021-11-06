plotDT <- function(button){
  
  library(rpart)
  
  library(rpart.plot)
  
  filename <- "DT.pdf"
  
  f.address <- ""
  
  main.data<-read.csv(f.address, header=T)
  
  ivf.model <- rpart(result ~ . , data = main.data , method= "class")
  
  pdf(filename)
  
  prp(x=ivf.model,type=0, extra=2, branch=1, under=T, fallen.leaves=T)
  
  dev.off()
  
  file.show(filename, title="Recursive Partitioning Tree for IVF")
  
  
}
