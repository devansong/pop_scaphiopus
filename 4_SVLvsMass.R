rm(list = ls()) # clear working directory
graphics.off()

setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Data")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cowplot) 

df <- read.csv("combiWO2017.csv")

df$SVL <- as.numeric(as.character(df$SVL))
df$Mass <- as.numeric(as.character(df$Mass))


# find smallest female 
fem <- subset(df, Stage=="F")
#36.8 SVL, 13.5 g, Smaller ones without mass: 31.5, 35.0 
mal <- subset(df, finalstage=="MALE")
#34.3 SVL, 4.4g, probably wrong 
#40.7mm  9.7g more likely, Smalle4 ones without mass: 39, 39.8 mm 


unique(df$finalstage)

df <- df[order(df$finalstage, decreasing=TRUE),] #order by animal id
df <- subset(df, Stage != "U")

pmain <- ggplot(df, aes(x=log(SVL), y=log(Mass), color=Stage))+
  geom_point(alpha=0.25, size=2)+
  scale_color_manual(values=c("Red", "Blue", "Gold", "Grey"))+ 
  xlab("Log Snout-vent Length (mm)")+ 
  ylab("Log Mass (g)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position="none")+
  guides(color = guide_legend(title = "Sex"))+
  ggtitle("B")+
  geom_smooth(method=lm, se=TRUE)
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = df, aes(x=log(SVL), fill = Stage),
               alpha = 0.5, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Gold", "Grey"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = df, aes(x = log(Mass), fill = Stage),
               alpha = 0.5, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Gold", "Grey"))
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Figures")
png("SVLvsMASS_log.png", units="in", width=7, height=5, res=300)
ggdraw(pmain)
dev.off()


p2main <- ggplot(df, aes(x=SVL, y=Mass, color=Stage))+
  geom_point(alpha=0.4, size=2)+
  scale_color_manual(values=c("Red", "Blue", "Gold", "Grey"))+ 
  xlab("Snout-vent Length (mm)")+ 
  ylab("Mass (g)")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  guides(color = guide_legend(title = "Sex"))+
  ggtitle("A")
# Marginal densities along x axis
x2dens <- axis_canvas(p2main, axis = "x")+
  geom_density(data = df, aes(x=SVL, fill = Stage),
               alpha = 0.5, size = 0.2)+
  ggpubr::fill_palette(palette = c("Red", "Blue", "Gold", "Grey"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
y2dens <- axis_canvas(p2main, axis = "y", coord_flip = TRUE)+
  geom_density(data = df, aes(x = Mass, fill = Stage),
               alpha = 0.5, size = 0.2)+
  coord_flip() +
  ggpubr::fill_palette(palette = c("Red", "Blue", "Gold", "Grey"))
p21 <- insert_xaxis_grob(p2main, x2dens, grid::unit(.2, "null"), position = "top")
p22<- insert_yaxis_grob(p21, y2dens, grid::unit(.2, "null"), position = "right")
ggdraw(p22)

setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Figures")
png("SVLvsMASS.png", units="in", width=7, height=5, res=300)
ggdraw(p22)
dev.off()


png("SVLvsMASS_BOTH.png", units="in", width=8, height=3.5, res=300)
ggarrange(ggdraw(p22), ggdraw(pmain))
dev.off()


SNBA <- subset(df, Stage=="SNBA")
FEM <- subset(df, Stage=="F")
MAL <- subset(df, Stage=="M")

summary(lm(log(SNBA$SVL)~log(SNBA$Mass)))
#Coefficients:
#  (Intercept)  log(SNBA$Mass)  
#3.1277          0.2884 
#Multiple R-squared:  0.8099,	Adjusted R-squared:  0.8096 
#F-statistic:  3323 on 1 and 780 DF,  p-value: < 2.2e-16

summary(lm(log(FEM$SVL)~log(FEM$Mass)))
#Coefficients:
#  (Intercept)  log(FEM$Mass)  
#3.195          0.269
#Multiple R-squared:  0.6647,	Adjusted R-squared:  0.6618 
#F-statistic: 227.9 on 1 and 115 DF,  p-value: < 2.2e-16

summary(lm(log(MAL$SVL)~log(MAL$Mass)))
#Coefficients:
#  (Intercept)  log(MAL$Mass)  
#3.2070         0.2688  
#Multiple R-squared:  0.6783,	Adjusted R-squared:  0.6761 
#F-statistic: 295.3 on 1 and 140 DF,  p-value: < 2.2e-16




#Plot density of faint male 

faintm <- read.csv("faintmale.csv")

#39.0 smallest #54.4 largest
setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Figures")
png("FaintMale.png", units="in", width=3, height=2.5, res=300)
ggplot(faintm, aes(x=SVL , fill=finalstage, color=finalstage)) + 
  geom_density(alpha=0.3) + 
  ylab("Density") + 
  xlab("Snout-vent Length (mm)") + 
  scale_color_manual(values="#003f5c")+
  scale_fill_manual(values="#003f5c")+
  theme_classic()+
  theme(legend.position="none")
dev.off()


