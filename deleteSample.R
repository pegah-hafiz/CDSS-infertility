deleteSample <- function(button){
  
  f.address <-""
  
  data <- read.csv(f.address)
  
    
  dialog2 <- gtkMessageDialog(main_window,"destroy-with-parent", "question", "yes-no", 
                              "Do you want to delete newly-entered sample?")
  
  temp.flag <- F
  
  p.row <- NA
  
  if(dialog2$run()==GtkResponseType["yes"]){
    
    dialog2$destroy()
    
    temp.win <- gtkWindow()
    
    temp.win["title"] <- " "
    
    temp.win$setDefaultSize(width=50,height=30)
    
    temp.frame <- gtkFrameNew()
    
    temp.win$add(temp.frame)
    
    temp.box <- gtkVBoxNew(show=T, spacing=5)
    
    temp.box$setDirection("ltr")
    
    temp.box$setBorderWidth(50)
    
    temp.frame$add(temp.box)
    
    temp.lbl <-  gtkLabelNewWithMnemonic("Enter patient's ID:")
    
    temp.box$packStart(temp.lbl, expand=F, fill=F,padding=5)
    
    e.pid <- gtkEntryNew()
    
    e.pid$setWidthChars(10)
    
    temp.lbl$setMnemonicWidget(e.pid)
    
    temp.box$packStart(e.pid, expand=F, fill=F,padding=5)
    
    button_temp <- gtkButton("Remove sample")
    
    temp.box$packStart(button_temp, expand=F, fill=F,padding=5)
    
    gSignalConnect(button_temp, "clicked", remove.sample <- function(button){
    
    number <- as.numeric(e.pid$getText())

    
    if(!is.na(number)){
      
      if(number%in%data[,1]){
        
        p.row <- which(data[,1]==number)
        
      }
      else{
        
        dialog5 <- gtkMessageDialog(temp.win,"destroy-with-parent", "error", "ok", 
                                    "There is no such sample!")
        
        if(dialog5$run()==GtkResponseType["ok"]){
          
          dialog5$destroy()
          
          temp.win$destroy()
          
          temp.flag <- T
          
        }
      }
  
      if(temp.flag==F){
        
        new.data <- data[-p.row, ]
        
        write.csv(new.data, file=f.address, row.names=F)
        
        dialog3 <- gtkMessageDialog(temp.win,"destroy-with-parent", "info", "close", 
                                    "Successfully deleted!")
        
        if(dialog3$run()==GtkResponseType["close"]){
          
          dialog3$destroy()
          temp.win$destroy()
          
        }
        
      }
   
      
    }
    else {
      
      dialog4 <- gtkMessageDialog(temp.win,"destroy-with-parent", "error", "ok", 
                                  "Incorrect patient ID!")
      
      if(dialog4$run()==GtkResponseType["ok"]){
        
        dialog4$destroy()
        
        temp.win$destroy()
      }
      
      
      
    }
  })
  }
  
  else {
    
    dialog2$destroy()
  }
  
}
