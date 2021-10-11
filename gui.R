library(RGtk2)

library(installr)

#main window
main_window <- gtkWindow(show=F)

main_window["title"] <- " Clinical Decision Support System for In vitro Fertilization"

main_window$setDefaultSize(900,700)

#image <- gdkPixbuf(filename = imagefile("sums.gif"))

#main_window$set(icon = image[[1]], title="SUMS")

#Frame
frame <- gtkFrameNew()

main_window$add(frame)

frame$setLabel("\n Sequential steps to predict IVF/ICSI results:\n")

frame$setLabelAlign(xalign=1, yalign=1)

#frame$setLabelAlign(xalign=100, yalign=100)

#V Box
vbox <- gtkVBoxNew(show=T, spacing=0)

vbox$setBorderWidth(15)

frame$add(vbox)


#H Box 1
hbox1 <- gtkHBoxNew(show=T, spacing=0)

vbox$packStart(child=hbox1, expand=F, fill=F, padding=0)

hbox1$setDirection("ltr")

#Label for H Box 1

label_hbox1 <- gtkLabelNew()

str <- "<span size='x-large' color='dark blue'> <b>  I    Data Entry</b>  </span>"

label_hbox1$setMarkup(str)

hbox1$packStart(label_hbox1, expand=T, fill=T,padding=0)


#H Box 2
hbox2 <- gtkHBoxNew(show=T, spacing=5)

vbox$packStart(child=hbox2, expand=F, fill=F, padding=20)

hbox2$setDirection("ltr")

#Frame a
frame_a <- gtkFrameNew()

frame_a$setLabel("Remove sample")

frame_a$setLabelAlign(xalign=0, yalign=0.5)

hbox2$add(frame_a)


frame_a$setDirection("ltr")

#Frame b
frame_b <- gtkFrameNew("Spreadsheet Selection")

hbox2$add(frame_b)

frame_b$setDirection("ltr")

#V Box for button a
vbox_a <- gtkVBoxNew(show=T, spacing=20)

vbox_a$setBorderWidth(15)

frame_a$add(vbox_a)

#H Box 2-1
hbox21 <- gtkHBoxNew(show=T, spacing=30)

vbox_a$packStart(child=hbox21, expand=F, fill=F, padding=0)

hbox21$setDirection("ltr")

#V Box for button b
vbox_b <- gtkVBoxNew(show=T, spacing=20)

vbox_b$setBorderWidth(15)

frame_b$add(vbox_b)

#H Box 2-2
hbox22 <- gtkHBoxNew(show=T, spacing=30)

vbox_b$packStart(child=hbox22, expand=F, fill=F, padding=0)

hbox22$setDirection("ltr")

#Button B
button_b <- gtkButton("Delete test sample")

hbox21$packStart(button_b, expand=T, fill=F, padding=0)

#Button A1
button_a1 <- gtkButton("Before ovulation induction")

hbox22$packStart(button_a1, expand=F, fill=F, padding=0)

#Button A2
button_a2 <- gtkButton("After ovulation induction")

hbox22$packEnd(button_a2, expand=F, fill=F, padding=0)

# First Separator

vbox$packStart(gtkHSeparatorNew(show=T), expand=F, fill=F,padding= 15)

#H Box 3
hbox3 <- gtkHBoxNew(show=T, spacing=0)

vbox$packStart(child=hbox3, expand=F, fill=F, padding=0)

hbox3$setDirection("ltr")

#Label for H Box 3

label_hbox3 <- gtkLabelNew()

str <- "<span size='x-large' color='medium blue'> <b>  II    Test Sample</b>  </span>"

label_hbox3$setMarkup(str)

hbox3$packStart(label_hbox3, expand=T, fill=T,padding=0)

# Actions for first two buttons:

gSignalConnect(button_a1, "clicked", openSpreadsheet_before) 
  
gSignalConnect(button_a2, "clicked", openSpreadsheet_after) 


gSignalConnect(button_b, "clicked", deleteSample)


#H Box 4
hbox4 <- gtkHBoxNew(show=T, spacing=5)

vbox$packStart(child=hbox4, expand=F, fill=F, padding=15)

hbox4$setDirection("ltr")

#Button C
button_c <- gtkButton("See predicted result")

hbox4$packEnd(button_c, expand=T, fill=F)

# Actions for predict result buttons:
gSignalConnect(button_c, "clicked", predictResult)

# Second Separator

vbox$packStart(gtkHSeparatorNew(), expand=F, fill=T,padding= 15)

#H Box 5
hbox5 <- gtkHBoxNew(show=T, spacing=10)

vbox$packStart(child=hbox5, expand=F, fill=F, padding=0)

hbox5$setDirection("ltr")

#Label for H Box 5

label_hbox5 <- gtkLabelNew()

str <- "<span size='x-large' color='royal blue'> <b>  III     Additional Analysis</b>  </span>"

label_hbox5$setMarkup(str)

hbox5$packStart(label_hbox5, expand=T, fill=T,padding=5)

#H Box 6
hbox6 <- gtkHBoxNew(show=T, spacing=5)

vbox$packStart(child=hbox6, expand=F, fill=F, padding=20)

hbox6$setDirection("ltr")

#Frame 2
frame2 <- gtkFrameNew()

hbox6$add(frame2)


frame2$setDirection("ltr")

#Frame 3
frame3 <- gtkFrameNew()

hbox6$add(frame3)

frame3$setDirection("ltr")

#V Box 2
vbox2 <- gtkVBoxNew(show=T, spacing=0)

vbox2$setBorderWidth(15)

frame2$add(vbox2)

#Label for V Box 2

label_vbox2 <- gtkLabelNew()

str <- "<span size='large'> <b> Click to see decision tree: </b>  </span>"

label_vbox2$setMarkup(str)

vbox2$packStart(label_vbox2, expand=T, fill=T,padding=15)

#H Box 7
hbox7 <- gtkHBoxNew(show=T, spacing=10)

vbox2$packStart(child=hbox7, expand=F, fill=F, padding=0)

hbox7$setDirection("ltr")


#Button D
button_d <- gtkButton(" Open ")

hbox7$packStart(button_d, expand=T, fill=F)

gSignalConnect(button_d, "clicked", cairoPlotDT)

#V Box 3
vbox3 <- gtkVBoxNew(show=T, spacing=0)

vbox3$setBorderWidth(15)

frame3$add(vbox3)

#Label for V Box 3

label_vbox3 <- gtkLabelNew()

str <- "<span size='large' > <b> Click to see variable importance: </b>  </span>"

label_vbox3$setMarkup(str)

vbox3$packStart(label_vbox3, expand=T, fill=T,padding=5)

#H Box 8
hbox8 <- gtkHBoxNew(show=T, spacing=10)

vbox3$packStart(child=hbox8, expand=F, fill=F, padding=0)

hbox8$setDirection("ltr")

#Button E
button_e <- gtkButton(" Open ")

hbox8$packStart(button_e, expand=T, fill=F)

gSignalConnect(button_e, "clicked", cairoPlotVI)

# Thirs Separator

vbox$packStart(gtkHSeparatorNew(), expand=F, fill=T,padding= 15)

#H Box 9
hbox9 <- gtkHBoxNew(show=T, spacing=10)

vbox$packStart(child=hbox9, expand=F, fill=F, padding=0)

hbox9$setDirection("ltr")

#Label for H Box 9

label_hbox9 <- gtkLabelNew()

str <- "<span size='x-large' color='deep sky blue'> <b>  IV     Actual Class of Patient</b>  </span>"

label_hbox9$setMarkup(str)

hbox9$packStart(label_hbox9, expand=T, fill=T,padding=5)

#H Box 10
hbox10 <- gtkHBoxNew(show=T, spacing=20)

vbox$packStart(child=hbox10, expand=F, fill=F, padding=30)

hbox10$setDirection("ltr")

#Label for H Box 10

#label_hbox10 <- gtkLabelNew()

#str <- "<span size='large' > <b> Click to enter actual class of patient: </b>  </span>"

#label_hbox10$setMarkup(str)

#hbox10$packStart(label_hbox10, expand=T, fill=T,padding=20)

#Button F
button_f <- gtkButton(" Enter actual class of patient ")

hbox10$packStart(button_f, expand=T, fill=F, padding=90)

gSignalConnect(button_f, "clicked", enterClass)

# Fourth Separator

vbox$packStart(gtkHSeparatorNew(), expand=T, fill=F,padding= 15)

#H Box 11
hbox11 <- gtkHBoxNew(show=T, spacing=5)

vbox$packStart(child=hbox11, expand=F, fill=F, padding=5)

hbox11$setDirection("ltr")


#Button G
button_g <- gtkButton(" Update software ")

hbox11$packStart(button_g, expand=T, fill=F, padding=30)

gSignalConnect(button_g, "clicked", myupdate<- function(button){
  
  if(!require(installr)) {
    install.packages("installr"); require(installr)} #load / install+load installr
  
  # using the package:
  updateR() 
})

##############################

main_window["visible"] <- T

