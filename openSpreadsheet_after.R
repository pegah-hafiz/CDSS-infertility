openSpreadsheet_after <- function(button){
  
  f.address <-"C:/Users/Pegah/Desktop/R application/test.csv"
  
  data <- read.csv(f.address)
  
  inner_win <- gtkWindow(show=F)
  inner_win["title"] <- "Spreadsheed for Data Entry"
  inner_win$setDefaultSize(width=400,height=400)
  inner_win$setPosition("center")
  
  #Frame
  new_frame <- gtkFrameNew()
  
  inner_win$add(new_frame)
  new_frame$setLabelAlign(xalign=10, yalign=10)
  
  #V Box
  entryBox <- gtkVBoxNew(show=T, spacing=5)
  
  entryBox$setBorderWidth(30)
  
  new_frame$add(entryBox)
  
  #H Box 1
  hbox_inner <- gtkHBoxNew(show=T, spacing=10)
  
  entryBox$packStart(child=hbox_inner, expand=F, fill=F, padding=10)
  
  hbox_inner$setDirection("ltr")
  
  #Label for patient ID
  pid_l = gtkLabelNewWithMnemonic("Patient ID:")
  pid_l$setDirection("ltr")
  hbox_inner$packStart(pid_l, expand=F, fill=F,padding=5)
  
  #Entry patient ID
  pid <- gtkEntryNew()
  
  pid$setWidthChars(10)
  
  pid$setDirection("ltr")
  
  pid_l$setMnemonicWidget(pid)
  
  hbox_inner$packStart(pid, expand=F, fill=F, 5)
  
  #Frame Laboratory test
  frame_lab <- gtkFrameNew("Laboratory Information")
  
  entryBox$add(frame_lab)
  
  frame_lab$setDirection("ltr")
  
  #H Box 4
  hbox_inner4 <- gtkHBoxNew(show=T, spacing=10)
  
  frame_lab$add(hbox_inner4)
  
  hbox_inner4$setDirection("ltr")
  
  #Label for genodotropine
  gen_l = gtkLabelNewWithMnemonic("Number of genodotropine ampules:")
  hbox_inner4$packStart(gen_l, expand=F, fill=F,padding=5)
  
  #Entry genodotropine
  gen <- gtkEntryNew()
  
  gen$setWidthChars(10)
  
  gen_l$setMnemonicWidget(gen)
  
  hbox_inner4$packStart(gen, expand=F, fill=F, 5)
  
  #Label for Follicles
  fol_l = gtkLabelNewWithMnemonic("Number of follicles:")
  hbox_inner4$packStart(fol_l, expand=F, fill=F,padding=5)
  
  #Entry Follicles
  fol <- gtkEntryNew()
  
  fol$setWidthChars(10)
  
  fol_l$setMnemonicWidget(fol)
  
  hbox_inner4$packStart(fol, expand=F, fill=F, 5)
  
  #Label for E2 level
  e2_l = gtkLabelNewWithMnemonic("Serum E2 level:")
  hbox_inner4$packStart(e2_l, expand=F, fill=F,padding=5)
  
  #Entry E2 level
  e2 <- gtkEntryNew()
  
  e2$setWidthChars(10)
  
  e2_l$setMnemonicWidget(e2)
  
  hbox_inner4$packStart(e2, expand=F, fill=F, 5)
  
  #Frame Egg Info
  frame_egg <- gtkFrameNew("Egg's Information")
  
  entryBox$add(frame_egg)
  
  frame_egg$setDirection("ltr")
  
  #H Box 5
  hbox_inner5 <- gtkHBoxNew(show=T, spacing=10)
  
  frame_egg$add(hbox_inner5)
  
  hbox_inner5$setDirection("ltr")
  
  #Label for retreived eggs
  ret_l = gtkLabelNewWithMnemonic("Number of retreived eggs:")
  hbox_inner5$packStart(ret_l, expand=F, fill=F,padding=5)
  
  #Entry retreived eggs
  ret <- gtkEntryNew()
  
  ret$setWidthChars(10)
  
  ret_l$setMnemonicWidget(ret)
  
  hbox_inner5$packStart(ret, expand=F, fill=F, 5)
  
  
  #Label for GV eggs
  gv_l = gtkLabelNewWithMnemonic("Number of GV eggs:")
  hbox_inner5$packStart(gv_l, expand=F, fill=F,padding=5)
  
  #Entry GV eggs
  gv <- gtkEntryNew()
  
  gv$setWidthChars(10)
  
  gv_l$setMnemonicWidget(gv)
  
  hbox_inner5$packStart(gv, expand=F, fill=F, 5)
  
  #Label for MI eggs
  mi_l = gtkLabelNewWithMnemonic("Number of MI eggs:")
  hbox_inner5$packStart(mi_l, expand=F, fill=F,padding=5)
  
  #Entry MI eggs
  mi <- gtkEntryNew()
  
  mi$setWidthChars(10)
  
  mi_l$setMnemonicWidget(mi)
  
  hbox_inner5$packStart(mi, expand=F, fill=F, 5)
  
  #Label for MII eggs
  mii_l = gtkLabelNewWithMnemonic("Number of MII eggs:")
  hbox_inner5$packStart(mii_l, expand=F, fill=F,padding=5)
  
  #Entry MII eggs
  mii <- gtkEntryNew()
  
  mii$setWidthChars(10)
  
  mii_l$setMnemonicWidget(mii)
  
  hbox_inner5$packStart(mii, expand=F, fill=F, 5)
  
  
  #Frame type of treatment
  frame_egg <- gtkFrameNew("Type of Treatment")
  
  entryBox$add(frame_egg)
  
  frame_egg$setDirection("ltr")
  
  #H Box 6
  hbox_inner6 <- gtkHBoxNew(show=T, spacing=10)
  
  frame_egg$add(hbox_inner6)
  
  hbox_inner6$setDirection("ltr")
  
  #Radio Button for IVF or ICSI
  
  ivf <- gtkRadioButton()
  ivf_l <- gtkLabelNewWithMnemonic("IVF")
  
  ivf$add(ivf_l)
  
  ## Create a radio button with a label
  icsi <- gtkRadioButtonNewWithLabelFromWidget(ivf, "ICSI")
  
  
  ## Pack them into a box, then show all the widgets
  hbox_inner6$packStart(ivf, F, F, 2)
  
  hbox_inner6$packStart(icsi, F, F, 2)
  
  
  #Frame embryo infp
  frame_emb <- gtkFrameNew("Embryo Information")
  
  entryBox$add(frame_emb)
  
  frame_emb$setDirection("ltr")
  
  #H Box 7
  hbox_inner7 <- gtkHBoxNew(show=T, spacing=10)
  
  frame_emb$add(hbox_inner7)
  
  hbox_inner7$setDirection("ltr")
  
  
  #Label for Embryo grade
  
  lbl_emb <- gtkLabelNewWithMnemonic("Embryo grade:")
  
  hbox_inner7$packStart(lbl_emb, expand=F, fill=F,padding=5)
  
  A <- gtkRadioButton()
  
  A_l <- gtkLabelNewWithMnemonic("A")
  
  A$add(A_l)
  
  ## Create a radio button with a label
  B <- gtkRadioButtonNewWithLabelFromWidget(A, "B")
  C <- gtkRadioButtonNewWithLabelFromWidget(B, "C")
  D <- gtkRadioButtonNewWithLabelFromWidget(C, "D")
  
  ## Pack them into a box, then show all the widgets
  hbox_inner7$packStart(A, F, F, 2)
  
  hbox_inner7$packStart(B, F, F, 2)
  
  hbox_inner7$packStart(C, F, F, 2)
  
  hbox_inner7$packStart(D, F, F, 2)
  
  #Label for Formed embryos
  femb_l = gtkLabelNewWithMnemonic("Number of formed embryos:")
  hbox_inner7$packStart(femb_l, expand=F, fill=F,padding=5)
  
  #Entry Formed embryos
  femb <- gtkEntryNew()
  
  femb$setWidthChars(10)
  
  femb_l$setMnemonicWidget(femb)
  
  hbox_inner7$packStart(femb, expand=F, fill=F, 5)
  
  #Label for EM trasfer day
  tday_l = gtkLabelNewWithMnemonic("Embryo transfer day:")
  hbox_inner7$packStart(tday_l, expand=F, fill=F,padding=5)
  
  #Entry EM transfer day
  tday <- gtkEntryNew()
  
  tday$setWidthChars(10)
  
  tday_l$setMnemonicWidget(tday)
  
  hbox_inner7$packStart(tday, expand=F, fill=F, 5)
  
  #Label for Number of transfered embryos
  temb_l = gtkLabelNewWithMnemonic("Number of transferred embryos:")
  hbox_inner7$packStart(temb_l, expand=F, fill=F,padding=5)
  
  #Entry Number of transfered embryos
  temb <- gtkEntryNew()
  
  temb$setWidthChars(10)
  
  temb_l$setMnemonicWidget(temb)
  
  hbox_inner7$packStart(temb, expand=F, fill=F, 5)
  
  #H Box 7
  hbox_inner8 <- gtkHBoxNew(show=T, spacing=10)
  
  entryBox$packStart(child=hbox_inner8, expand=F, fill=F, padding=10)
  
  hbox_inner8$setDirection("ltr")
  
  #Button Quit
  button_inner1 <- gtkButton("quit")
  
  hbox_inner8$packStart(button_inner1, expand=F, fill=F)
  
  #Button Save and continue
  button_inner2 <- gtkButton("save and continue")
  
  hbox_inner8$packStart(button_inner2, expand=F, fill=F)
  
  #Cheking for wrong inputs for each data entry:
  
  flag.null <- F
  flag.txt.aft <- F
  
  
  #End Checking
  
  inner_win["visible"] <- T
  
  gSignalConnect(button_inner2, "clicked",prompt.error<- function(button){
    
    
    #--- Checking null check buttons for factors:
  
    
    #--- Checking for non-numeric inputs:
    
    
    if(is.na(as.numeric(gen$getText()))==T || is.na(as.numeric(fol$getText()))==T
       || is.na(as.numeric(e2$getText()))==T || is.na(as.numeric(ret$getText()))==T || is.na(as.numeric(gv$getText()))==T
       || is.na(as.numeric(mi$getText()))==T || is.na(as.numeric(mii$getText()))==T || is.na(as.numeric(femb$getText()))==T
       || is.na(as.numeric(tday$getText()))==T || is.na(as.numeric(temb$getText()))==T 
       || is.na(as.numeric(pid$getText()))==T ){
            
      dialog <- gtkMessageDialog(inner_win,"destroy-with-parent", "error", "ok", 
                                 "There are empty and/or non-numeric entries in spreadsheet!")
      
      gSignalConnect(dialog, "response", gtkWidgetDestroy)
      
    }
    else{
      
        # Converting numerical features:
        
        pid_v <-  as.numeric(pid$getText())
        
          
        if(pid_v%in%data[,1]){
          
          p.row <- which(data[,1]==pid_v)
          
          aow_v <- data[p.row, 2]
          
          aom_v <- data[p.row, 3]
          
          bmi_v <- data[p.row, 4]
          
          sec_v <- data[p.row, 5]
          
          tub_v <- data[p.row, 6]
          
          pel_v <- data[p.row, 7]
          
          ovu_v <- data[p.row, 8]
          
          ute_v <- data[p.row, 9]
          
          mal_v <- data[p.row, 10]
          
          per_v <- data[p.row, 11]
          
          exp_v <- data[p.row, 12]
          
          cnt_v <- data[p.row, 13]
          
          mor_v <- data[p.row, 14]
          
          mot_v <- data[p.row, 15]
          
          fsh_v <- data[p.row, 16]
          
          amh_v <- data[p.row, 17]
          
          afc_v <- data[p.row, 18]
          
          gen_v <-  as.numeric(gen$getText())
          
          fol_v <-  as.numeric(fol$getText())
          
          e2_v <-  as.numeric(e2$getText())
          
          ret_v <-  as.numeric(ret$getText())
          
          gv_v <-  as.numeric(gv$getText())
          
          mi_v <-  as.numeric(mi$getText())
          
          mii_v <-  as.numeric(mii$getText())
          
          femb_v <-  as.numeric(femb$getText())
          
          tday_v <-  as.numeric(tday$getText())
          
          temb_v <-  as.numeric(temb$getText())
          # Converting cathegorical features:
          
          
          if(ivf["active"]==T){
            treatment <- 2
          }
          else if(icsi["active"]==T){
            treatment <- 1
          }
          
          if(A["active"]==T){
            embryo.grade <- 1
            
          }
          else if(B["active"]==T){
            embryo.grade <- 2
          }
          else if(C["active"]==T){
            embryo.grade <- 3
          }
          else {
            embryo.grade <- 4
          }
          
          
          col.names <- NA
          
          new.data <- data[-p.row, ]
          
          write.csv(new.data, file=f.address, row.names=F)
          
          data.vector <- c(pid_v, aow_v, aom_v, bmi_v, sec_v, tub_v, pel_v, ovu_v, 
                           ute_v, mal_v, per_v, exp_v, cnt_v, mor_v, mot_v, fsh_v,
                           amh_v, afc_v, gen_v,fol_v, e2_v, ret_v, gv_v, mi_v,
                           mii_v, treatment, embryo.grade, femb_v, tday_v, temb_v, 'NA')
          
          temp <- as.matrix(t(data.vector))
          
          write.table( temp , file =f.address, sep=",", col.names=F, append=T , row.names=F)
          
          
          dialog <- gtkMessageDialog(inner_win,"destroy-with-parent", "info", "close", 
                                     "Correct entries. Proceed to the next step!")
          
          if(dialog$run()==GtkResponseType["close"]){
            
            dialog$destroy()
            
            inner_win$destroy()
            
          }
          
        }
        else{
          
          dialog_wrong <- gtkMessageDialog(inner_win,"destroy-with-parent", "error", "ok", 
                                           "You haven't entered pre-ovulation induction info!")
          
          if(dialog_wrong$run()==GtkResponseType["ok"]){
            
            dialog_wrong$destroy()
            
            inner_win$destroy()
            
          }
        }
        
        
    
    }
    
  })
  
  gSignalConnect(button_inner1, "clicked",close.win <- function(button) {
    inner_win$destroy()
    
  })
  # Show window after settings
  
}