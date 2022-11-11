rm(list = ls()) # clear working directory


setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Data")
library(ggplot2)

burrow <- read.csv("Burrow_data_clean.csv")
burrow$SVL_mm <- as.character(burrow$SVL_mm)
burrow$SVL_mm <- as.numeric(burrow$SVL_mm)

plot <-ggplot(burrow, aes(x=SVL_mm, y=Burrow_Depth, color=STAGE))+
  geom_point(alpha=0.7, size = 4)+
  scale_color_manual(values=c("Red", "Blue", "Black", "Gold", "Grey"))+ 
  ylab("Burrow Depth (mm)")+ 
  xlab("Snout-vent Length (mm)")+ 
  scale_x_continuous(breaks = round(seq(min(0), max(70), by = 5),70), limits=c(30,70))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot

setwd("~/Dropbox/Publications_Work/Spadefoot_Monograph/Figures")
png("Burrow_Depth.png", units="in", width=6, height=3.5, res=300)
plot
dev.off()
