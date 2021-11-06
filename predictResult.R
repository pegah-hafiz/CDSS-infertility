predictResult <- function(button){
  
  
  train.address <- ""
  
  test.address <- ""
  
  data <- read.csv(train.address, header=T)
  
  train.data <- data[,-1]
  
  test.data <- read.csv(test.address, header=T)
  
  test.vec <- c()
  
  #Window to ask patient's ID
  
  pred.win <- gtkWindow()
  
  pred.win["title"] <- " "
  
  pred.win$setDefaultSize(width=50,height=30)
  
  pred.frame <- gtkFrameNew()
  
  pred.win$add(pred.frame)
  
  pred.box <- gtkVBoxNew(show=T, spacing=5)
  
  pred.box$setDirection("ltr")
  
  pred.box$setBorderWidth(50)
  
  pred.frame$add(pred.box)
  
  pred.lbl <-  gtkLabelNewWithMnemonic("Enter patient's ID:")
  
  pred.box$packStart(pred.lbl, expand=F, fill=F,padding=5)
  
  t.pid <- gtkEntryNew()
  
  t.pid$setWidthChars(10)
  
  pred.lbl$setMnemonicWidget(t.pid)
  
  pred.box$packStart(t.pid, expand=F, fill=F,padding=5)
  
  button_rem <- gtkButton("Continue")
  
  pred.box$packStart(button_rem, expand=F, fill=F,padding=5)
  
  
  gSignalConnect(button_rem, "clicked", find.sample <- function(button){
    
    
    if(is.na(as.numeric(t.pid$getText()))==T){
      
      dialog_na <- gtkMessageDialog(pred.win,"destroy-with-parent", "error", "ok", 
                                      "Empty patient ID!")
      
      if(dialog_na$run()==GtkResponseType["ok"]){
        
        dialog_na$destroy()
        
        pred.win$destroy()
        
        
      }
    }
    
    
    
    else {
      
      find.flag <- F
      
      for(i in 1:nrow(test.data)){
        
        if(as.numeric(t.pid$getText())==test.data[i, 1]){
          
          test.vec <- test.data[i,]
          
          find.flag <- T
          
          break
          
        }
        
      }
      
      if(!find.flag){
        
        dialog_find <- gtkMessageDialog(pred.win,"destroy-with-parent", "error", "ok", 
                                        "Wrong patient ID!")
        
        if(dialog_find$run()==GtkResponseType["ok"]){
          
          dialog_find$destroy()
          
          
        }
        
        
       
      }
      
      else{
        
        library(randomForest)
        
        set.seed(2)
      
        test.vec <- test.vec[,-1]
        
        if(is.na(test.vec[,18])==T & is.na(test.vec[,19])==T & is.na(test.vec[,20])==T & is.na(test.vec[,21])==T
           & is.na(test.vec[22])==T & is.na(test.vec[,23])==T & is.na(test.vec[,24])==T & is.na(test.vec[,25])==T 
           & is.na(test.vec[,26])==T & is.na(test.vec[,27])==T & is.na(test.vec[,28])==T & is.na(test.vec[,29])==T)
        {
          
          train.data <- train.data[,c(1:17,30)]
          
          test.vec <- test.vec[,1:17]
          
        }
        
        rfit <- randomForest(result ~ .,data=train.data
                             , method="classification"
                             ,mtry=5 ,ntree=500)
        
        preds <- predict(rfit, test.vec, type="prob")
        
        neg.prob <- preds[1]
        
        pos.prob <- preds[2]
        
        pid.str <- paste("<b>Patient ID:    </b>", as.numeric(t.pid$getText()))
        
        pos.str <- paste("<b>Positive probability of IVF/ICSI:    </b>", pos.prob*100, "%")
        
        neg.str <- paste("<b>Negative probability of IVF/ICSI:    </b>", neg.prob*100, "%")
        
        # Creating window to show results:
        
        res.win <- gtkWindow(show=F)
        
        res.win["title"] <- "Predicted Results:"
        
        res.win$setDefaultSize(width=40,height=40)
        
        #frame:
        
        res.frame <- gtkFrameNew()
        
        res.win$add(res.frame)
        
        #V Box
        
        res.box <- gtkVBoxNew(show=T, spacing=5)
        
        res.box$setBorderWidth(40)
        
        res.box$setDirection("ltr")
        
        res.frame$add(res.box)
        
        # Label for patient ID:
        
        patient_label <- gtkLabelNew()
        
        patient_label$setMarkup(pid.str)
        
        res.box$packStart(patient_label, expand=F, fill=F,padding=5)
        
        # Label for positive result:
        
        pos_label <- gtkLabelNew()
        
        pos_label$setMarkup(pos.str)
        
        res.box$packStart(pos_label, expand=F, fill=F,padding=5)
        
        # Label for negative result:
        
        neg_label <- gtkLabelNew()
        
        neg_label$setMarkup(neg.str)
        
        res.box$packStart(neg_label, expand=F, fill=F,padding=5)
        
        #H Box
        
        res.hbox <- gtkHBoxNew(show=T, spacing=10)
        
        res.box$packStart(child=res.hbox, expand=T, fill=F, padding=10)
        
        res.hbox$setDirection("ltr")
        
        # Button (OK)
        
        res.button <- gtkButton("OK")
        
        res.hbox$packStart(res.button, expand=T, fill=F,padding=5)
        
        res.win["visible"] <- T
        
        gSignalConnect(res.button, "clicked", close.win <- function(button) 
          {
          res.win$destroy()
          
          pred.win$destroy()
          
          }
          )
        
        
        
      }
      
    }
    
    
    
  }
)
  
}
