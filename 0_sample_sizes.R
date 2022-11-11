rm(list = ls()) # clear working directory
graphics.off()

setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Data")
library(tidyverse)

plot <- read.csv("Plot_data_clean.csv")
transect <- read.csv("Transect_data_clean.csv")

unique(plot$Plot_ID)
length(unique(plot$Final_PITtag_Toeclip)) #495, 224 unique individuals


plotA <- subset(plot, Plot_ID=="A") #103,49
plotB <- subset(plot, Plot_ID=="B") #253, 118
plotC <- subset(plot, Plot_ID=="C") #86, 37
plotE <- subset(plot, Plot_ID=="E") #53, 20

length(unique(plotA$Final_PITtag_Toeclip))
length(unique(plotB$Final_PITtag_Toeclip))
length(unique(plotC$Final_PITtag_Toeclip))
length(unique(plotE$Final_PITtag_Toeclip))


length(unique(plot$Final_PITtag_Toeclip))




length(unique(transect$Plot_ID))
length(unique(transect$Survey_Number))

length(unique(transect$FINAL_Pittag_toeclip.1)) #1176, 835


tran2016 <- subset(transect, Date_num <20170000)
tran2017 <- subset(transect, Date_num >20170000)


tran0 <- subset(transect, Plot_ID=="T0")
tran3 <- subset(transect, Plot_ID=="T3") 
tran9 <- subset(transect, Plot_ID=="T9")
tran14 <- subset(transect, Plot_ID=="T14") 
tran15 <- subset(transect, Plot_ID=="T15") 
tran18 <- subset(transect, Plot_ID=="T18") 
tran20 <- subset(transect, Plot_ID=="T20") 
length(unique(tran0$FINAL_Pittag_toeclip.1)) #191, 135
length(unique(tran3$FINAL_Pittag_toeclip.1)) #163, 109
length(unique(tran9$FINAL_Pittag_toeclip.1)) #272, 206
length(unique(tran14$FINAL_Pittag_toeclip.1)) #61, 31
length(unique(tran15$FINAL_Pittag_toeclip.1)) #176, 124
length(unique(tran18$FINAL_Pittag_toeclip.1)) #310, 240
length(unique(tran20$FINAL_Pittag_toeclip.1)) #3, 2









