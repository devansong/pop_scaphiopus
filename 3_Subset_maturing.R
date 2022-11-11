##Combine plot and transect data into one for maturity plots

rm(list = ls()) # clear working directory

graphics.off()
setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Data")
library(ggplot2)
library(ggpubr)

plot <- read.csv("Plot_data_clean.csv")
transect <- read.csv("Transect_data_clean.csv")

newplot <- plot[, c("Survey_Type",
                    "Index", 
                    "Date_num", 
                    "Ground_Temp..C.",
                    "Xcoord.1",
                    "Ycoord.1",
                    "Plot_ID",
                    "Recap",
                    "SVL.mm.",
                    "Mass..g.",
                    "Pads",
                    "Eggs",
                    "Unique_ID",
                    "Body.Condition",
                    "Matured_during_study",
                    "STAGE"
                    )]

colnames(newplot) <- c("Survey_type", "Index", "Date", "Temp_C", 
                       "X", "Y", "Plot_ID", "Recap", "SVL_mm", "Mass_g", 
                       "Pads", "Eggs", "Animal_ID", "Body_condition", 
                       "Matured_during_study", "Stage")

newtransect <- transect[, c("Survey_Type",
                        "Index", 
                        "Date_num", 
                        "Ground_Temp..C.",
                        "Xcoord",
                        "Ycoord",
                        "Plot_ID",
                        "Recap",
                        "SVL.mm.",
                        "Mass",
                        "Pads",
                        "Eggs",
                        "Unique_ID",
                        "Body.Condition",
                        "Matured.during.survey",
                        "STAGE"
)]

colnames(newtransect) <- c("Survey_type", "Index", "Date", "Temp_C", 
                       "X", "Y", "Plot_ID", "Recap", "SVL_mm", "Mass_g", 
                       "Pads", "Eggs", "Animal_ID", "Body_condition", 
                       "Matured_during_study", "Stage")

combined <- rbind(newplot, newtransect)
matured <- subset(combined, Matured_during_study == "Y")
write.csv(matured, file="matured_during_study.csv", row.names=FALSE)

removal <- read.csv("Remove_from_maturity.csv")

cleanmature <- merge(removal,matured, by=c("Survey_type", "Index"))
newmature <- subset(cleanmature, Remove != "remove")

newmature$SVL_mm <-as.numeric(newmature$SVL_mm)

#newmature$Animal_ID <- as.numeric(as.character(newmature$Animal_ID))
newmature <- newmature[order(newmature$Index, decreasing=FALSE),] #order by animal id
newmature <- newmature[order(newmature$Animal_ID, decreasing=TRUE),] #order by animal id

newmature <- newmature[
  order(newmature[,"Animal_ID"], newmature[,"Index"] ),
  ]

newmature$capture <- rep(c(1,2),times=50)
newmature$capture <- as.factor(newmature$capture)

write.csv(newmature, file = "matured_during_study_tidy.csv", row.names=FALSE)

submat <- subset(newmature, Date <20170000)

library(dplyr)
submat16 <- submat %>%
  group_by(Animal_ID) %>%
  filter(n() > 1)
  

######## Only look at 2016 matured 
newmature <- submat16
#newmature <- read.csv("matured_during_2016_only.csv")
newmature$Animal_ID <- as.factor(newmature$Animal_ID)
newmature$capture <- as.factor(newmature$capture)

newmature$Date <- as.character(newmature$Date)
newmature$Date <- as.Date(newmature$Date, format = "%Y%m%d")

cap1 <- subset(newmature, capture == 1)
cap2 <- subset(newmature, capture == 2)
capitan1 <- cap1[order(cap1$Animal_ID, decreasing=TRUE),] #order by animal id
capitan2 <- cap2[order(cap2$Animal_ID, decreasing=TRUE),] #order by animal id
d <- data.frame("Last capture before maturity" = capitan1$SVL_mm, "First capture after maturity" = capitan2$SVL_mm, "STAGE"= capitan2$Stage)
colnames(d) <- c("Before Maturity", "After Maturity", "Sex")

d$`After Maturity`
d <- subset(d, `After Maturity` >0)

plot <- ggpaired(d, cond1 = "Before Maturity", 
                  cond2 = "After Maturity",
                  fill = "condition", palette = "jco", ylim = c(30,60), 
                  line.size=0.2, 
                 line.color="maroon",
                  xlab = "", 
                  ylab = "SVL (mm)",
                 facet.by = "Sex") +
  theme(legend.position = "none")+
  scale_y_continuous(
    breaks = c(30, 35, 40, 45, 50, 55, 60))+
  ylab("Snout-vent Length (mm)")


plot



setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Figures")
png("Maturing_SVL.png", units="in", width=6, height=3.5, res=300)
plot
dev.off()



