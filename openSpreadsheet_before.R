openSpreadsheet_before <- function(button){
  
  f.address <-""
  
  data <- read.csv(f.address)
  
  inner_win_bef <- gtkWindow(show=F)
  inner_win_bef["title"] <- "Spreadsheed for Data Entry"
  inner_win_bef$setDefaultSize(width=400,height=400)
  inner_win_bef$setPosition("center")
  
  data <- data[,c(1:18,ncol(data))]
  
  #main frame
  new_frame_bef <- gtkFrameNew()
  
  inner_win_bef$add(new_frame_bef)
  new_frame_bef$setLabelAlign(xalign=10, yalign=10)
  
  #V Box
  entryBox_bef <- gtkVBoxNew(show=T, spacing=5)
  
  entryBox_bef$setBorderWidth(30)
  
  new_frame_bef$add(entryBox_bef)
  
  #Frame demographic
  frame_demo_b <- gtkFrameNew("Demographic Information")
  
  entryBox_bef$add(frame_demo_b)
  
  frame_demo_b$setDirection("ltr")
  
  #H Box 1
  hbox_inner_bef <- gtkHBoxNew(show=T, spacing=10)
  
  frame_demo_b$add(hbox_inner_bef)
  
  hbox_inner_bef$setDirection("ltr")
  
  #Label for patient ID
  pid_l_bef = gtkLabelNewWithMnemonic("Patient ID:")
  pid_l_bef$setDirection("ltr")
  hbox_inner_bef$packStart(pid_l_bef, expand=F, fill=F,padding=5)
  
  #Entry patient ID
  pid_b <- gtkEntryNew()
  
  pid_b$setWidthChars(10)
  
  pid_b$setDirection("ltr")
  
  pid_l_bef$setMnemonicWidget(pid_b)
  
  hbox_inner_bef$packStart(pid_b, expand=F, fill=F, 5)
  
  #Label for age of woman
  aow_l_bef = gtkLabelNewWithMnemonic("Age of Woman:")
  hbox_inner_bef$packStart(aow_l_bef, expand=F, fill=F,padding=5)
  
  #Entry age of woman
  aow_b <- gtkEntry()
  
  aow_b$setWidthChars(10)
  
  aow_l_bef$setMnemonicWidget(aow_b)
  
  hbox_inner_bef$packStart(aow_b, expand=F, fill=F, 5)
  
  #Label for age of man
  aom_l_bef = gtkLabelNewWithMnemonic("Age of man:")
  hbox_inner_bef$packStart(aom_l_bef, expand=F, fill=F,padding=5)
  
  #Entry age of man
  aom_b <- gtkEntryNew()
  
  aom_b$setWidthChars(10)
  
  aom_l_bef$setMnemonicWidget(aom_b)
  
  hbox_inner_bef$packStart(aom_b, expand=F, fill=F, 5)
  
  #Label for height
  height_l_bef = gtkLabelNewWithMnemonic("Height of woman (in centimeter):")
  hbox_inner_bef$packStart(height_l_bef, expand=F, fill=F,padding=5)
  
  #Entry for height
  height_b <- gtkEntryNew()
  
  height_b$setWidthChars(10)
  
  height_l_bef$setMnemonicWidget(height_b)
  
  hbox_inner_bef$packStart(height_b, expand=F, fill=F, 5)
  
  #Label for weight
  weight_l_bef = gtkLabelNewWithMnemonic("Weight of woman (in kilogram):")
  hbox_inner_bef$packStart(weight_l_bef, expand=F, fill=F,padding=5)
  
  #Entry for weight
  weight_b <- gtkEntryNew()
  
  weight_b$setWidthChars(10)
  
  weight_l_bef$setMnemonicWidget(weight_b)
  
  hbox_inner_bef$packStart(weight_b, expand=F, fill=F, 5)
  
  #Frame factors
  frame_fac_b <- gtkFrameNew("Infertility period and factors")
  
  entryBox_bef$add(frame_fac_b)
  
  frame_fac_b$setDirection("ltr")
  
  #H Box 2
  hbox_inner2_bef <- gtkHBoxNew(show=T, spacing=10)
  
  frame_fac_b$add(hbox_inner2_bef)
  
  hbox_inner2_bef$setDirection("ltr")
  
  #Label for Infertility period
  per_l_bef = gtkLabelNewWithMnemonic("Infertility period:")
  hbox_inner2_bef$packStart(per_l_bef, expand=F, fill=F,padding=5)
  
  #Entry Infertility period
  per_b <- gtkEntryNew()
  
  per_b$setWidthChars(10)
  
  per_l_bef$setMnemonicWidget(per_b)
  
  hbox_inner2_bef$packStart(per_b, expand=F, fill=F, 5)
  
  #Label for Secondary factor
  sec_l_bef = gtkLabelNewWithMnemonic("Secondary factor:")
  hbox_inner2_bef$packStart(sec_l_bef, expand=F, fill=F,padding=5)
  
  #Entry Secondary factor
  sec_b <- gtkCheckButton()
  
  sec_l_bef$setMnemonicWidget(sec_b)
  
  hbox_inner2_bef$packStart(sec_b, expand=F, fill=F, 5)
  
  #Label for tubal factor
  tub_l_bef = gtkLabelNewWithMnemonic("Tubal factor:")
  hbox_inner2_bef$packStart(tub_l_bef, expand=F, fill=F,padding=5)
  
  #Entry tubal factor
  tub_b <- gtkCheckButton()
  
  tub_l_bef$setMnemonicWidget(tub_b)
  
  hbox_inner2_bef$packStart(tub_b, expand=F, fill=F, 5)
  
  #Label for pelvic factor
  pel_l_bef = gtkLabelNewWithMnemonic("Pelvic factor:")
  hbox_inner2_bef$packStart(pel_l_bef, expand=F, fill=F,padding=5)
  
  #Entry pelvic factor
  pel_b <- gtkCheckButton()
  
  pel_l_bef$setMnemonicWidget(pel_b)
  
  hbox_inner2_bef$packStart(pel_b, expand=F, fill=F, 5)
  
  #Label for ovulator factor
  ovu_l_bef = gtkLabelNewWithMnemonic("Ovulatory factor:")
  hbox_inner2_bef$packStart(ovu_l_bef, expand=F, fill=F,padding=5)
  
  #Entry ovulatory factor
  ovu_b <- gtkCheckButton()
  
  ovu_l_bef$setMnemonicWidget(ovu_b)
  
  hbox_inner2_bef$packStart(ovu_b, expand=F, fill=F, 5)
  
  #Label for uterine factor
  ute_l_bef = gtkLabelNewWithMnemonic("Uterine factor:")
  hbox_inner2_bef$packStart(ute_l_bef, expand=F, fill=F,padding=5)
  
  #Entry uterine factor
  ute_b <- gtkCheckButton()
  
  ute_l_bef$setMnemonicWidget(ute_b)
  
  hbox_inner2_bef$packStart(ute_b, expand=F, fill=F, 5)
  
  #Label for male factor
  mal_l_bef = gtkLabelNewWithMnemonic("Male factor:")
  hbox_inner2_bef$packStart(mal_l_bef, expand=F, fill=F,padding=5)
  
  #Entry male factor
  mal_b <- gtkCheckButton()
  
  mal_l_bef$setMnemonicWidget(mal_b)
  
  hbox_inner2_bef$packStart(mal_b, expand=F, fill=F, 5)
  
  #Label for IVF experience
  exp_l_bef = gtkLabelNewWithMnemonic("IVF experience:")
  hbox_inner2_bef$packStart(exp_l_bef, expand=F, fill=F,padding=5)
  
  #Entry IVF experience
  exp_b <- gtkCheckButton()
  
  exp_l_bef$setMnemonicWidget(exp_b)
  
  hbox_inner2_bef$packStart(exp_b, expand=F, fill=F, 5)
  
  
  #Frame sperm
  frame_spe_b <- gtkFrameNew("Sperm Test")
  
  entryBox_bef$add(frame_spe_b)
  
  frame_spe_b$setDirection("ltr")
  
  #H Box 3
  hbox_inner3_bef <- gtkHBoxNew(show=T, spacing=10)
  
  frame_spe_b$add(hbox_inner3_bef)
  
  hbox_inner3_bef$setDirection("ltr")
  
  #Label for sperm count
  cnt_l_bef = gtkLabelNewWithMnemonic("Sperm count (in million):")
  hbox_inner3_bef$packStart(cnt_l_bef, expand=F, fill=F,padding=5)
  
  #Entry sperm count
  cnt_b <- gtkEntryNew()
  
  cnt_b$setWidthChars(10)
  
  cnt_l_bef$setMnemonicWidget(cnt_b)
  
  hbox_inner3_bef$packStart(cnt_b, expand=F, fill=F, 5)
  
  #Label for sperm morphology
  mor_l_bef = gtkLabelNewWithMnemonic("Sperm morphology (%):")
  hbox_inner3_bef$packStart(mor_l_bef, expand=F, fill=F,padding=5)
  
  #Entry sperm morphology
  mor_b <- gtkEntryNew()
  
  mor_b$setWidthChars(10)
  
  mor_l_bef$setMnemonicWidget(mor_b)
  
  hbox_inner3_bef$packStart(mor_b, expand=F, fill=F, 5)
  
  #Label for sperm motility
  mot_l_bef = gtkLabelNewWithMnemonic("Sperm motility (%):")
  hbox_inner3_bef$packStart(mot_l_bef, expand=F, fill=F,padding=5)
  
  #Entry sperm motility
  mot_b <- gtkEntryNew()
  
  mot_b$setWidthChars(10)
  
  mot_l_bef$setMnemonicWidget(mot_b)
  
  hbox_inner3_bef$packStart(mot_b, expand=F, fill=F, 5)
  
  #Frame woman info
  frame_wom_b <- gtkFrameNew("Woman Test")
  
  entryBox_bef$add(frame_wom_b)
  
  frame_wom_b$setDirection("ltr")
  
  #H Box 4
  hbox_inner4_bef <- gtkHBoxNew(show=T, spacing=10)
  
  frame_wom_b$add(hbox_inner4_bef)
  
  hbox_inner4_bef$setDirection("ltr")
  
  #Label for FSH of woman
  fsh_l_bef = gtkLabelNewWithMnemonic("FSH of Woman:")
  hbox_inner4_bef$packStart(fsh_l_bef, expand=F, fill=F,padding=5)
  
  #Entry FSH of woman
  fsh_b <- gtkEntryNew()
  
  fsh_b$setWidthChars(10)
  
  fsh_l_bef$setMnemonicWidget(fsh_b)
  
  hbox_inner4_bef$packStart(fsh_b, expand=F, fill=F, 5)
  
  #Label for AMH
  amh_l_bef = gtkLabelNewWithMnemonic("AMH:")
  hbox_inner4_bef$packStart(amh_l_bef, expand=F, fill=F,padding=5)
  
  #Entry AMH
  amh_b <- gtkEntryNew()
  
  amh_b$setWidthChars(10)
  
  amh_l_bef$setMnemonicWidget(amh_b)
  
  hbox_inner4_bef$packStart(amh_b, expand=F, fill=F, 5)
  
  
  #Label for AFC
  afc_l_bef = gtkLabelNewWithMnemonic("AFC:")
  hbox_inner4_bef$packStart(afc_l_bef, expand=F, fill=F,padding=5)
  
  #Entry AFC
  afc_b <- gtkEntryNew()
  
  afc_b$setWidthChars(10)
  
  afc_l_bef$setMnemonicWidget(afc_b)
  
  hbox_inner4_bef$packStart(afc_b, expand=F, fill=F, 5)
  
 
  #H Box 7
  hbox_inner7_bef <- gtkHBoxNew(show=T, spacing=10)
  
  entryBox_bef$packStart(child=hbox_inner7_bef, expand=F, fill=F, padding=10)
  
  hbox_inner7_bef$setDirection("ltr")
  
  #Button Quit
  button_inner1_b <- gtkButton("quit")
  
  hbox_inner7_bef$packStart(button_inner1_b, expand=F, fill=F)
  
  #Button Save and continue
  button_inner2_b <- gtkButton("save and continue")
  
  hbox_inner7_bef$packStart(button_inner2_b, expand=F, fill=F)
  
  #Cheking for wrong inputs for each data entry:
  
  flag.null <- F
  flag.txt <- F
  
  
  #End Checking
  
  inner_win_bef["visible"] <- T
  
  gSignalConnect(button_inner2_b, "clicked",prompt.error<- function(button){
    
    
    #--- Checking null check buttons for factors:
    if(sec_b["active"]==F & tub_b["active"]==F & pel_b["active"]==F &
         ovu_b["active"]==F & ute_b["active"]==F & mal_b["active"]==F
       & exp_b["active"]==F){
      factor.flag <- T
    }
    
    
    #--- Checking for non-numeric inputs:
    if(is.na(as.numeric(aow_b$getText()))==T || is.na(as.numeric(aom_b$getText()))==T || is.na(as.numeric(height_b$getText()))==T
       || is.na(as.numeric(weight_b$getText()))==T || is.na(as.numeric(per_b$getText()))==T || is.na(as.numeric(cnt_b$getText()))==T || is.na(as.numeric(mor_b$getText()))==T
       || is.na(as.numeric(mot_b$getText()))==T || is.na(as.numeric(fsh_b$getText()))==T || is.na(as.numeric(amh_b$getText()))==T
       || is.na(as.numeric(afc_b$getText()))==T || is.na(as.numeric(pid_b$getText()))==T ){
      
      flag.txt <- T
    }
    if(flag.txt==T ){
      
      dialog_b <- gtkMessageDialog(inner_win_bef,"destroy-with-parent", "error", "ok", 
                                 "There are empty and/or non-numeric entries in spreadsheet!")
      
      gSignalConnect(dialog_b, "response", gtkWidgetDestroy)
      
    }
    if(flag.txt==F){
      
      dialog_b <- gtkMessageDialog(inner_win_bef,"destroy-with-parent", "info", "close", 
                                 "Correct entries. Proceed to the next step!")
      
      if(dialog_b$run()==GtkResponseType["close"]){
      
        # Converting numerical features:
        
        pid_v_b <-  as.numeric(pid_b$getText())
        
        aow_v_b <-  as.numeric(aow_b$getText())
        
        aom_v_b <-  as.numeric(aom_b$getText())
        
        height_v_b <-  as.numeric(height_b$getText())
        
        weight_v_b <-  as.numeric(weight_b$getText())
        
        bmi_v_b <- (weight_v_b)/((height_v_b/100)^2)
        
        per_v_b <-  as.numeric(per_b$getText())
        
        cnt_v_b <-  as.numeric(cnt_b$getText())
        
        mor_v_b <-  (as.numeric(mor_b$getText()))/100
        
        mot_v_b <-  (as.numeric(mot_b$getText()))/100
        
        fsh_v_b <-  as.numeric(fsh_b$getText())
        
        amh_v_b <-  as.numeric(amh_b$getText())
        
        afc_v_b <-  as.numeric(afc_b$getText())
        
        
        # Converting cathegorical features:
      
        
        if(sec_b["active"]==T){
          
          sec_v_b <- 1
        }
        else {
           
          sec_v_b <- 0
        }
        
        if(tub_b["active"]==T){
          
          tub_v_b <- 1
        }
        else {
          
          tub_v_b <- 0
        }
        
        if(pel_b["active"]==T){
          
          pel_v_b <- 1
        }
        else {
          
          pel_v_b <- 0
        }
        if(ovu_b["active"]==T){
          
          ovu_v_b <- 1
        }
        else {
          
          ovu_v_b <- 0
        }
        if(ute_b["active"]==T){
          
          ute_v_b <- 1
        }
        else {
          
          ute_v_b <- 0
        }
        if(mal_b["active"]==T){
          
          mal_v_b <- 1
        }
        else {
          
          mal_v_b <- 0
        }
        
        if(exp_b["active"]==T){
          
          exp_v_b <- 1
        }
        else {
          
          exp_v_b <- 0
        }
        
        
        
        col.names <- NA
        data.vector <- c(pid_v_b, aow_v_b, aom_v_b, bmi_v_b, sec_v_b, tub_v_b, pel_v_b, ovu_v_b, 
                         ute_v_b, mal_v_b, per_v_b, exp_v_b, cnt_v_b, mor_v_b, mot_v_b, fsh_v_b,
                         amh_v_b, afc_v_b,'NA', 'NA', 'NA', 'NA', 'NA', 'NA','NA',
                         'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
       temp <- as.matrix(t(data.vector))
        write.table( temp , file =f.address, sep=",", col.names=F, append=T , row.names=F)
        
       dialog_b$destroy()
       
       inner_win_bef$destroy()
     }
    }
    
    
  })
  
  gSignalConnect(button_inner1_b, "clicked",close.win <- function(button) {
    inner_win_bef$destroy()
    
  })
  
  
}
