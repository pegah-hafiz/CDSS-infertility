enterClass <- function(button){
  
  f.address.train <-""
  
  f.address.test <-""
  
  f.address.missing <-""
  
  data <- read.csv(f.address.test)
  
  enter.win <- gtkWindow()
  
  enter.win["title"] <- " "
  
  enter.win$setDefaultSize(width=50,height=30)
  
  enter.frame <- gtkFrameNew()
  
  enter.win$add(enter.frame)
  
  enter.box <- gtkVBoxNew(show=T, spacing=5)
  
  enter.box$setDirection("ltr")
  
  enter.box$setBorderWidth(50)
  
  enter.frame$add(enter.box)
  
  enter.lbl <-  gtkLabelNewWithMnemonic("Enter patient's ID:")
  
  enter.box$packStart(enter.lbl, expand=F, fill=F,padding=5)
  
  ent.pid <- gtkEntryNew()
  
  ent.pid$setWidthChars(10)
  
  enter.lbl$setMnemonicWidget(ent.pid)
  
  enter.box$packStart(ent.pid, expand=F, fill=F,padding=5)
  
  #Radio Button for positive or negative
  
  pos <- gtkRadioButton()
  pos_l <- gtkLabelNewWithMnemonic("Positive")
  
  pos$add(pos_l)
  
  ## Create another radio button 
  neg <- gtkRadioButtonNewWithLabelFromWidget(pos, "Negative")
  
  
  ## Pack them into a box, then show all the widgets
  enter.box$packStart(pos, F, F, 2)
  
  enter.box$packStart(neg, F, F, 2)
  
  
  
  button_enter <- gtkButton("Enter")
  
  enter.box$packStart(button_enter, expand=F, fill=F,padding=5)
  
  gSignalConnect(button_enter, "clicked", enter.sample <- function(button){
    
    number <- as.numeric(ent.pid$getText())
    temp.flag <- F
    
    if(!is.na(number)){
      
      if(number%in%data[,1]){
        
        p.row <- which(data[,1]==number)
        
      }
      else{
        
        dialog_wrng_ent <- gtkMessageDialog(enter.win,"destroy-with-parent", "error", "ok", 
                                    "There is no such sample!")
        
        if(dialog_wrng_ent$run()==GtkResponseType["ok"]){
          
          dialog_wrng_ent$destroy()
          
          temp.flag <- T
          
        }
      }
      
      if(temp.flag==F){
        
        if(pos["active"]==T){
          
          class <- "positive"
        }
        else{
          
          class <- "negative"
        }
      
       
        
        if(is.na(data[p.row,ncol(data)])){
          
          vec <- data[p.row,19:ncol(data)-1]
         
          if(!is.na(all(vec))){
            
            col.names <- NA
            
            new.data <- data[-p.row, ]
            
            write.csv(new.data, file=f.address.test, row.names=F)
            
            data[p.row, ncol(data)] <- class
            
            data.vector <- data[p.row,]
            
            write.table( data.vector , file =f.address.train, sep=",", col.names=F, append=T , row.names=F)
            
            #converting some features to categorical groups to enter dataset having missings for rpart:
            data.vector.missing <- data.vector[,-1]
            
            yes.no.features <- c(4,5,6,7,8,9,11)
            
            for(iii in yes.no.features){
              
              if(data.vector.missing[,iii]==1){
                data.vector.missing[,iii] <- 'yes'
              }
              else{
                data.vector.missing[,iii] <- 'no'
              }
            }
            
            if(data.vector.missing[,25]==1){
              
              data.vector.missing[,25] <- 'icsi'
            }
            else{
              data.vector.missing[,25] <- 'IVF'
            }
            
            
            if(data.vector.missing[,26]==1){
              data.vector.missing[,26]<-'A'
            }
            else if(data.vector.missing[,26]==2){
              data.vector.missing[,26]<- 'B'
            }
            else if(data.vector.missing[,26]== 3){
              data.vector.missing[,26]<-'C'
            }
            else{
              data.vector.missing[,26]<-'D'
            }
            
            write.table( data.vector.missing , file =f.address.missing, sep=",", col.names=F, append=T , row.names=F)
            dialog_success <- gtkMessageDialog(enter.win,"destroy-with-parent", "info", "close", 
                                               "Successfully entered!")
            
            if(dialog_success$run()==GtkResponseType["close"]){
              
              dialog_success$destroy()
              
              enter.win$destroy()
            }
            
          }
          
          else {
            
            dialog_incomp <- gtkMessageDialog(enter.win,"destroy-with-parent", "error", "ok", 
                                               "The information of after ovulation induction of this patient is not entered yet!")
            
            if(dialog_incomp$run()==GtkResponseType["ok"]){
              
              dialog_incomp$destroy()
              
              enter.win$destroy()
            }
          }
          
         
        }
        else{
          
          dialog_not_null <- gtkMessageDialog(enter.win,"destroy-with-parent", "error", "ok", 
                                      "The class of this patient is entered before!")
          
          if(dialog_not_null$run()==GtkResponseType["ok"]){
            
            dialog_not_null$destroy()
            enter.win$destroy()
            
          }
          
        }
        
        
        
        
      }
      
      
    }
    
    else {
      
      dialog_null_ent <- gtkMessageDialog(enter.win,"destroy-with-parent", "warning", "ok", 
                                          "Please enter a number!")
      
      if(dialog_null_ent$run()==GtkResponseType["ok"]){
        
        dialog_null_ent$destroy()
        
        
      }
      
    }
    
    
    
  }
  )
}
