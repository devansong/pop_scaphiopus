#Create subset CSV for analysis

rm(list = ls()) # clear working directory
setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Data")

#

plot <- read.csv("Plot_data_clean.csv")
transect <- read.csv("Transect_data_clean.csv")
transect$Burrow_depth <- as.numeric(as.character(transect$Burrow_depth))
road <- read.csv("Road_VES_clean.csv")
road$Burrow_Depth <- as.numeric(as.character(road$Burrow_Depth))
other <- read.csv("Other_data_clean.csv")



###Extract for burrow. Only road and transect have burrow measurements 
burrowtran <- subset(transect, Burrow_depth >0)
burrowroad <- subset(road, Burrow_Depth >0)

burrowtran<- burrowtran[, c("Index", 
                         "Date",
                         "X.coord",
                         "Y.coord",
                         "Survey_Type",
                         "SVL.mm.",
                        "Mass",
                         "STAGE", 
                        "Burrow_depth")]
colnames(burrowtran) <- c("Index", 
                         "Date", 
                         "X",
                         "Y",
                         "Method",
                         "SVL_mm",
                         "Mass",
                         "STAGE", 
                         "Burrow_Depth")
burrowroad <- burrowroad[, c("Index", 
                         "Date", 
                         "X",
                         "Y",
                         "Method",
                         "SVL_mm",
                         "Mass",
                         "STAGE", 
                         "Burrow_Depth")]


burrow <- rbind(burrowtran, burrowroad)
write.csv(burrow, file="Burrow_data_clean.csv")


##Extract faint pads. Only transect, pot, and road have faint pads. 

padsroad <- subset(road, Nuptial_Pads == "FAINT")
padstran <- subset(transect, Pads == "FAINT")
padsplot <-subset(plot, Pads == "FAINT")

padsroad <- padsroad[, c("Index", 
                         "Method", 
                         "Date",
                         "SVL_mm", 
                         "Mass",
                         "X",
                         "Y")]

padsroad$ID <- ""


namescols <-c("Index", 
  "Method", 
  "Date",
  "SVL_mm", 
  "Mass",
  "X",
  "Y", 
  "ID")
padsplot <- padsplot[, c("Index", 
                         "Survey_Type", 
                         "Date",
                         "SVL.mm.", 
                         "Mass..g.",
                         "Xcoord",
                         "Ycoord", 
                        "Plot_ID_Full")]
colnames(padsplot) <- namescols

padstran <- padstran[, c("Index", 
                         "Survey_Type", 
                         "Date",
                         "SVL.mm.", 
                         "Mass",
                         "X.coord",
                         "Y.coord", 
                         "Plot_ID.1")]
colnames(padstran) <- namescols

faintpads <- rbind(padsplot, padstran, padsroad)

write.csv(faintpads, file="Faint_pads_male_clean.csv")



plot <- read.csv("Plot_data_clean.csv")
transect <- read.csv("Transect_data_clean.csv")
road <- read.csv("Road_VES_clean.csv")
other <- read.csv("Other_data_clean.csv")

#Get SVL distribution by sex, no for 2017 for transect!! 

#remove 2017 for transect 
trans16 <- subset(transect, Date_num<20170000)

#extract relevant columns to bind together

newplot <- plot[ , c(
  "Survey_Type",
  "Index",
  "Date",
  "Plot_ID",
  "SVL.mm.",
  "Mass..g.",
  "Microhabitat",
  "Pads",
  "Color",
  "Eggs",
  "Recap",
  "Unique_ID",
  "STAGE",
  "Underground"
)]

newtransect <- trans16[, c(
  "Survey_Type",
  "Index",
  "Date",
  "Plot_ID",
  "SVL.mm.",
  "Mass",
  "Microhabitat",
  "Pads",
  "Color",
  "Eggs",
  "Recap",
  "Unique_ID", 
  "STAGE"
)]
newtransect$Underground <- ""


newroad <- road[ , c(
  "Method",
  "Index",
  "Date",
  "Start.Y",
  "SVL_mm",
  "Mass",
  "Microhabitat",
  "Nuptial_Pads",
  "Color",
  "gravid"
)]

newroad$Recap <- ""
newroad$Unique_ID <- ""
newroad$STAGE<- road$STAGE
newroad$Underground<- ""



newother <- other[ , c(
  "Method",
  "No",
  "Date",
  "Location",
  "SVL.mm.",
  "Mass.g.",
  "Microhabitat",
  "Pads",
  "Color",
  "Eggs"
)]

newother$Recap <- ""
newother$Unique.ID <- ""
newother$Stage<- other$STAGE
newother$Underground<- ""

namesofcols <- c("Method", "Index", "Date", "Loc",
                 "SVL", "Mass", "Microhabitat", "Pads", "Color","Eggs", 
                 "Recap", "PIT_ID", "Stage", "Underground")

colnames(newother)<- namesofcols
colnames(newplot)<- namesofcols
colnames(newroad)<- namesofcols
colnames(newtransect)<- namesofcols


df <- rbind(newother, newplot, newroad, newtransect)
df$tran2017 <- "NO"
df$newindex <- c(1:nrow(df))

length(unique(df$newindex))

write.csv(df, file="combiWO2017.csv")

trans17 <- subset(transect, Date_num>20170000)
trans17 <- trans17[, c(
  "Survey_Type",
  "Index",
  "Date",
  "Plot_ID",
  "SVL.mm.",
  "Mass",
  "Microhabitat",
  "Pads",
  "Color",
  "Eggs",
  "Recap",
  "Unique_ID", 
  "STAGE"
)]
trans17$Underground <- ""
colnames(trans17)<- namesofcols
trans17$tran2017 <- "YES"
trans17$newindex <- c((nrow(df)+1):(nrow(df)+(nrow(trans17))))

combi2 <- rbind(df, trans17)
write.csv(combi2, file ="combiWITH2017.csv")

length(unique(combi2$newindex))





