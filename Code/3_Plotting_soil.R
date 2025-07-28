

# load libraries
library(agricolae)
library(devtools)
library(ggpubr)
library(stringr)
library(emmeans)
library(dplyr)
library(nlme)
library(plyr)
library(multcomp)
library(MuMIn)
library(car)

covercols <-c("chocolate", "goldenrod", "forestgreen", "darkcyan", "darkorchid4")
cropsyscols <- c("darkgoldenrod4", "darkolivegreen", "darkolivegreen3", "deepskyblue3")




dat2 <- read.csv("Processed-data/multifunctionality.csv")
# make factors
dat <- dat %>% mutate_each_(funs(factor(.)),c("Rep", "Depth", "Year", "Season", "Cropping.system", "Cover"))
dat$Cropping.system <- factor(dat$Cropping.system, levels(dat$Cropping.system)[c(1,4,3,2)])
dat$Cover <- factor(dat$Cover, levels(dat$Cover)[c(2,4,1,5,3)])
dat$Season <- factor(dat$Season, levels(dat$Season)[c(2,3,1)])


############### MAOC


# plot MAOC by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/MAOC_cover_cropsys_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("Mineral-associated organic C (g kg"^-1,")"))) + labs(color="Cover") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(1:5)]) +
  #xlim(c(1.5,3.5)) + #ylim(c(0,15)) +
  facet_wrap(.~Cropping.system, nrow=2) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
   scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) +
  ggrepel::geom_text_repel(
    data = testlet[which(testlet$YearN %in% c(4)),], segment.linetype = "dotted",
    aes(x = YearN, y = response, label = .group),color="black",
    size = 2.5,
    direction = "y",          # Adjusts labels only vertically
    nudge_x = 0.5,            # Nudges labels slightly to the right
    na.rm = TRUE              # Ensures it skips missing values
  )

p


# export figure
ggpubr::ggexport(p, height=1500, width=2100, filename = "Figures/_across years/_MAOC_cropsys_cover_time.png", res = 400)




############### MBC


# plot MBC by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/MBC_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Microbial biomass C (g kg"^-1,")"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  #xlim(c(1.5,3.5)) + #ylim(c(0,15)) +
  #facet_wrap(.~Cropping.system, nrow=2) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) #+
  # ggrepel::geom_text_repel(
  #   data = testlet[which(testlet$YearN %in% c(4)),], segment.linetype = "dotted",
  #   aes(x = YearN, y = response, label = .group),color="black",
  #   size = 2.5,
  #   direction = "y",          # Adjusts labels only vertically
  #   nudge_x = 0.5,            # Nudges labels slightly to the right
  #   na.rm = TRUE              # Ensures it skips missing values
  # )

p


# export figure
ggpubr::ggexport(p, height=1000, width=1800, filename = "Figures/_across years/_MBC_cropsys_time.png", res = 400)








############### GMC


# plot GMC by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/GMC_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Gravimetric moisture (g g"^-1,")"))) + labs(color="Cropping system") +
  #geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  #xlim(c(1.5,3.5)) + #ylim(c(0,15)) +
  #facet_wrap(.~Cropping.system, nrow=2) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) +
ggrepel::geom_text_repel(
  data = testlet[which(testlet$YearN %in% c(3)),], segment.linetype = "dotted",
  aes(x = YearN, y = response, label = .group,color=Cropping.system),
  size = 2.5,
  direction = "y",          # Adjusts labels only vertically
  nudge_x = 0.5,            # Nudges labels slightly to the right
  na.rm = TRUE              # Ensures it skips missing values
)

p


# export figure
ggpubr::ggexport(p, height=1000, width=2000, filename = "Figures/_across years/_GMC_cropsys_time.png", res = 400)





# plot
testlet <- read.csv("Model-output/lsmeans/_across years/GMC_time.csv")
testlet$Year <- as.factor(testlet$Year)

p <- ggplot(data=testlet, aes(x=Year, y=response, fill=Year)) +
  theme_minimal() + labs(y=expression(paste("Gravimetric moisture (g g"^-1,")"))) + labs(fill="Year") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  #scale_fill_manual(values = timecols[c(1:5)]) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank())+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.025, x=(rep(1:3,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_GMC_time.png", res = 400)







############### WAS




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/WAS_time.csv")
testlet$Year <- as.factor(testlet$Year)

p <- ggplot(data=testlet, aes(x=Year, y=response, fill=Year)) +
  theme_minimal() + labs(y=expression(paste("Aggregate stability (%)"))) + labs(fill="Year") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  #scale_fill_manual(values = timecols[c(1:5)]) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank())+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+6, x=(rep(1:3,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_WAS_time.png", res = 400)






############### WEC




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/WEC_time.csv")
testlet$Year <- as.factor(testlet$Year)

p <- ggplot(data=testlet, aes(x=Year, y=response, fill=Year)) +
  theme_minimal() + labs(y=expression(paste("Water-extractable organic C (g kg"^-1,")"))) + labs(fill="Year") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  #scale_fill_manual(values = timecols[c(1:5)]) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank())+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+6, x=(rep(1:3,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_WEC_time.png", res = 400)





############### EEA_N




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/EEA_N_time.csv")
testlet$Year <- as.factor(testlet$Year)

p <- ggplot(data=testlet, aes(x=Year, y=response, fill=Year)) +
  theme_minimal() + labs(y=expression(paste("N-cycling enzyme activity (nmol g"^-1," hr"^-1,")"))) + labs(fill="Year") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  #scale_fill_manual(values = timecols[c(1:5)]) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank())+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+6, x=(rep(1:3,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_EEA_N_time.png", res = 400)







############### PHOS




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/PHOS_time.csv")
testlet$Year <- as.factor(testlet$Year)

p <- ggplot(data=testlet, aes(x=Year, y=response, fill=Year)) +
  theme_minimal() + labs(y=expression(paste("Phosphatase activity (nmol g"^-1," hr"^-1,")"))) + labs(fill="Year") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  #scale_fill_manual(values = timecols[c(1:5)]) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank())+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+10, x=(rep(1:3,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_PHOS_time.png", res = 400)




























