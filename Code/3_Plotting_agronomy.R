

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
dat2 <- dat2 %>% mutate_each_(funs(factor(.)),c("Rep", "Depth", "Year", "Season", "Cropping.system", "Cover"))
dat2$Cropping.system <- factor(dat2$Cropping.system, levels(dat2$Cropping.system)[c(1,4,3,2)])
dat2$Cover <- factor(dat2$Cover, levels(dat2$Cover)[c(2,4,1,5,3)])
dat2$Season <- factor(dat2$Season, levels(dat2$Season)[c(2,3,1)])


############### multifunctionality_index_agronomy2


# plot multifunctionality_index_agronomy2 by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/multifunctionality_index_agronomy2_cover_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])
multifunctionality_index_agronomy2_ylim <- c(0.3,0.7)

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("Agronomic multifunctionality"))) + labs(fill="Cover") +
  scale_color_discrete() +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(1:5)], labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(multifunctionality_index_agronomy2_ylim) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10)) +#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) +
  ggrepel::geom_text_repel(
    data = testlet[which(testlet$YearN %in% c(2,3,4)),], segment.linetype = "dotted",
    aes(x = YearN, y = response, label = .group),color="black",
    size = 2.5,
    direction = "y",          # Adjusts labels only vertically
    nudge_x = 0.5,            # Nudges labels slightly to the right
    na.rm = TRUE              # Ensures it skips missing values
  )

p


# export figure
ggpubr::ggexport(p, height=800, width=1500, filename = "Figures/_across years/_multifunctionality_index_agronomy2_cover_time.png", res = 400)



# plot
testlet <- read.csv("Model-output/lsmeans/_across years/multifunctionality_index_agronomy2_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)]) # c(3,1,4,2)
multifunctionality_index_agronomy2_ylim <- c(0,0.7)

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Agronomic multifunctionality"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)], labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(multifunctionality_index_agronomy2_ylim) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 270, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.05, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_multifunctionality_index_agronomy2_cover.png", res = 400)



############### yield_scaled

# plot
testlet <- read.csv("Model-output/lsmeans/_across years/yield_scaled_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)]) # c(3,1,4,2)
yield_scaled_cover_ylim <- c(0,1)

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Scaled yield"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  scale_fill_manual(values = covercols[c(1:5)], labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(yield_scaled_cover_ylim) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 270, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.05, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_scaled_cover.png", res = 400)




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/yield_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Scaled yield"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  ylim(yield_scaled_cover_ylim) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) +
  ggrepel::geom_text_repel(
    data = testlet[which(testlet$YearN %in% c(3,4)),], segment.linetype = "dotted",
    aes(x = YearN, y = response, label = .group),color="black",
    size = 2.5,
    direction = "y",          # Adjusts labels only vertically
    nudge_x = 0.5,            # Nudges labels slightly to the right
    na.rm = TRUE              # Ensures it skips missing values
  )

p


# export figure
ggpubr::ggexport(p, height=800, width=2000, filename = "Figures/_across years/_yield_scaled_cropsys_time.png", res = 400)




############### agb


# plot
testlet <- read.csv("Model-output/lsmeans/_across years/agb_cover_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(3,1,4,2)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])
mean(testlet$response)


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("Cover crop biomass (Mg ha"^-1,"yr"^-1,")"))) + labs(color="Cover") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(2:5)], labels = c(levels(dat2$Cover)[2:4], "Five-species mix")) +
  #ylim(c(0.0,3)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 9))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) +
  ggrepel::geom_text_repel(
    data = testlet[which(testlet$YearN %in% c(2,3)),], segment.linetype = "dotted",
    aes(x = YearN, y = response, label = .group),color="black",
    size = 2.5,
    direction = "y",          # Adjusts labels only vertically
    nudge_x = 0.5,            # Nudges labels slightly to the right
    na.rm = TRUE              # Ensures it skips missing values
  )

p


# export figure
ggpubr::ggexport(p, height=800, width=1500, filename = "Figures/_across years/_agb_cover_time.png", res = 400)




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/agb_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(3,1,4,2)]) # c(2,4,1,5,3)

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Cover crop biomass (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(2:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[2:4], "Five-species mix")) +
  ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.2, x=(rep(1:4,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_agb_cover.png", res = 400)


# plot
testlet <- read.csv("Model-output/lsmeans/_across years/agb_time.csv")
testlet$Year <- as.factor(testlet$Year)

p <- ggplot(data=testlet, aes(x=Year, y=response, fill=Year)) +
  theme_minimal() + labs(y=expression(paste("Cover crop biomass (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Year") +
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
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.15, x=(rep(1:3,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_agb_time.png", res = 400)






############### inputC


# plot
testlet <- read.csv("Model-output/lsmeans/_across years/inputC_cover_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("C inputs (Mg ha"^-1,"yr"^-1,")"))) + labs(color="Cover") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(1:5)], labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  #ylim(c(0.0,3)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 9))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) +
  ggrepel::geom_text_repel(
    data = testlet[which(testlet$YearN %in% c(2,3,4)),], segment.linetype = "dotted",
    aes(x = YearN, y = response, label = .group),color="black",
    size = 2.5,
    direction = "y",          # Adjusts labels only vertically
    nudge_x = 0.5,            # Nudges labels slightly to the right
    na.rm = TRUE              # Ensures it skips missing values
  )

p


# export figure
ggpubr::ggexport(p, height=800, width=1500, filename = "Figures/_across years/_inputC_cover_time.png", res = 400)




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/inputC_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("C inputs (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.075, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_inputC_cover.png", res = 400)









############### inputN


# plot
testlet <- read.csv("Model-output/lsmeans/_across years/inputN_cover_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("N inputs (kg ha"^-1,"yr"^-1,")"))) + labs(color="Cover") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(1:5)], labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  #ylim(c(0.0,3)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 9))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(2,3,4),  labels = c( c("2021", "2022", "2023"))) +
  ggrepel::geom_text_repel(
    data = testlet[which(testlet$YearN %in% c(2,3,4)),], segment.linetype = "dotted",
    aes(x = YearN, y = response, label = .group),color="black",
    size = 2.5,
    direction = "y",          # Adjusts labels only vertically
    nudge_x = 0.5,            # Nudges labels slightly to the right
    na.rm = TRUE              # Ensures it skips missing values
  )

p


# export figure
ggpubr::ggexport(p, height=800, width=1500, filename = "Figures/_across years/_inputN_cover_time.png", res = 400)




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/inputN_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet <- testlet[order(testlet$Cover),]

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("N inputs (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+3, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_inputN_cover.png", res = 400)






############### inputCN



# plot
testlet <- read.csv("Model-output/lsmeans/_across years/inputCN_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("input CN"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+3, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_inputCN_cover.png", res = 400)





############### inputCN



# plot
testlet <- read.csv("Model-output/lsmeans/_across years/inputCN_cropsys.csv")
testlet$Cropping.system <- as.factor(testlet$Cropping.system)
testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("input CN"))) + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = cropsyscols[c(1:4)]) +
  #scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  #ylim(c(0,2.2)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+3, x=(rep(1:4,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=2100, filename = "Figures/_across years/_inputCN_cropsys.png", res = 400)





















# yield plots

maxes <- c(13,2,6)

# plot
testlet <- read.csv("Model-output/lsmeans/_across years/corn_yield_2021_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Corn yield in 2021 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(c(0,maxes[1])) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 270, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.7, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_corn_2021.png", res = 400)


# plot
testlet <- read.csv("Model-output/lsmeans/_across years/soybean_yield_2021_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Soybean yield in 2021 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(c(0,maxes[3])) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.7, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_soybean_2021.png", res = 400)



# plot
testlet <- read.csv("Model-output/lsmeans/_across years/corn_yield_2022_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Corn yield in 2022 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(c(0,maxes[1])) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.7, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_corn_2022.png", res = 400)



# plot
testlet <- read.csv("Model-output/lsmeans/_across years/cotton_yield_2022_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Cotton yield in 2022 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  ylim(c(0,maxes[2])) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.1, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_cotton_2022.png", res = 400)



# plot
testlet <- read.csv("Model-output/lsmeans/_across years/soybean_yield_2022_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Soybean yield in 2022 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(c(0,maxes[3])) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.35, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_soybean_2022.png", res = 400)






# plot
testlet <- read.csv("Model-output/lsmeans/_across years/corn_yield_2023_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Corn yield in 2023 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  ylim(c(0,maxes[1])) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.7, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_corn_2023.png", res = 400)




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/soybean_yield_2023_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])

p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Soybean yield in 2023 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  scale_x_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  ylim(c(0,maxes[3])) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 300, hjust = 0)) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.35, x=(rep(1:5,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1300, width=1400, filename = "Figures/_across years/_yield_soybean_2023.png", res = 400)









# plot
testlet <- read.csv("Model-output/lsmeans/_across years/soybean_yield_2022_cropsys.csv")
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(2,1)])

p <- ggplot(data=testlet, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Soybean yield in 2022 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = cropsyscols[c(2,3)]) +
  ylim(c(0,maxes[3])) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+1, x=(rep(1:2,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1200, width=1800, filename = "Figures/_across years/_yield_soybean_2022_cropsys.png", res = 400)






# plot
testlet <- read.csv("Model-output/lsmeans/_across years/corn_yield_2023_cropsys.csv")
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,2)])

p <- ggplot(data=testlet, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Corn yield in 2023 (Mg ha"^-1,"yr"^-1,")"))) + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = cropsyscols[c(1,3)]) +
  ylim(c(0,maxes[1])) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.7, x=(rep(1:2,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1200, width=1800, filename = "Figures/_across years/_yield_corn_2023_cropsys.png", res = 400)




# plot
testlet <- read.csv("Model-output/lsmeans/_across years/soybean_yield_2023_cropsys.csv")
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(2,1)])

p <- ggplot(data=testlet, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + 
  labs(y=expression(paste("Soybean yield in 2023 (Mg ha"^-1,"yr"^-1,")"))) + 
  labs(fill="Cropping.system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  scale_fill_manual(values = cropsyscols[c(2,4)]) +
  ylim(c(0,maxes[3])) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x = element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet, size=4,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.35, x=(rep(1:2,1)) )) 
p

# export figure
ggpubr::ggexport(p, height=1400, width=2100, filename = "Figures/_across years/_yield_soybean_2023_cropsys.png", res = 400)







########### correlation between soil and agronomic multifunctionality


tes.cor	<- cor.test(dat2$multifunctionality_index_agronomy2, dat2$multifunctionality_index_soil2)
tes.cor <- round(tes.cor$p.value,2)
cor_val <- cor(dat2$multifunctionality_index_agronomy2, dat2$multifunctionality_index_soil2)
cor_value <- round(cor_val, 2)  # Compute correlation


p <- ggplot(dat2, aes(x = multifunctionality_index_agronomy2, y = multifunctionality_index_soil2)) +
  geom_point(alpha=0.6,size = 3, color="black", aes(shape=Cropping.system, fill=Cover))+#, aes(shape=Year)) +  # Scatter points
  #facet_wrap(.~Cropping.system) +
  scale_shape_manual(values=c(21,24,22,23)) +
  scale_fill_manual(values = covercols[c(1:5)],  labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  guides(fill = guide_legend(override.aes = list(shape=22, size=3, alpha=1, color="black")) ,
         shape = guide_legend(override.aes = list(color="black"), title="Cropping system"))+
  annotate("text", x = 0.2, y = 0.68, 
           label = paste("r =", cor_value, "p =", tes.cor), 
           hjust = 0, vjust = 1, size = 5, color = "black") +  # Add correlation text
  theme_minimal() +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype="solid") +  # Add regression line
  labs(title = "", 
       x = "Agronomic multifunctionality", 
       y = "Soil multifunctionality") +
  lims(x=c(0.2,0.8), y=c(0.2,0.68))
p
ggpubr::ggexport(p, height=1300, width=2200, filename = "Figures/_across years/_multifunctionality correlation.png", res = 400)



# test correlation of indices calculated with a 50% threshold
tes.cor	<- cor.test(dat2$multifunctionality_index_agronomy, dat2$multifunctionality_index_soil)
tes.cor <- round(tes.cor$p.value,2)
cor_val <- cor(dat2$multifunctionality_index_agronomy, dat2$multifunctionality_index_soil)
cor_value <- round(cor_val, 2)  # Compute correlation


p <- ggplot(dat2, aes(x = multifunctionality_index_agronomy, y = multifunctionality_index_soil, color=Cover)) +
  geom_point( alpha = 0.6, size = 2)+#, aes(shape=Year)) +  # Scatter points
  #facet_wrap(.~Year) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype="solid") +  # Add regression line
  annotate("text", x = min(dat2$multifunctionality_index_agronomy), y = max(dat2$multifunctionality_index_soil), 
           label = paste("r =", cor_value, "p =", tes.cor), 
           hjust = 0, vjust = 1, size = 5, color = "black") +  # Add correlation text
  scale_color_manual(values = covercols[c(1:5)]) +
  theme_minimal() +
  labs(title = "", 
       x = "Agronomic multifunctionality", 
       y = "Soil multifunctionality")
p









############ plot raw yields in Mg ha-1

dat2$Yield_Mgha <- dat2$Yield
dat2$Yield_Mgha[which(dat2$Yield.crop=="Corn")] <- dat2$corn_yield[which(dat2$Yield.crop=="Corn")]
dat2$Yield_Mgha[which(dat2$Yield.crop=="Cotton")] <- dat2$cotton_yield[which(dat2$Yield.crop=="Cotton")]
dat2$Yield_Mgha[which(dat2$Yield.crop=="Soybean")] <- dat2$soybean_yield[which(dat2$Yield.crop=="Soybean")]


dat3 <-  dat2 %>%
  dplyr::group_by(Cropping.system, Year, Cover) %>%   # Group by Year and Treatment
  dplyr::summarise(response = mean(Yield_Mgha, na.rm = TRUE),
                   SE = sd(Yield_Mgha, na.rm = TRUE)/2, .groups = "drop")  # Calculate mean

dat4 <-  dat2 %>%
  dplyr::group_by(Cropping.system, Year, Cover) %>%   # Group by Year and Treatment
  dplyr::summarise(response = mean(Yield, na.rm = TRUE), .groups = "drop")  # Calculate mean



# plot
p <- ggplot(dat2, 
            aes(x=Cover, y=Yield_Mgha, fill=Cover)) +
  geom_point(alpha=0, shape=21) + scale_color_manual(values=covercols) +
  guides(fill = guide_legend(override.aes = list(alpha = 1, color="black", fill=covercols, size=3)), )+
  theme_minimal() +
  facet_grid(Cropping.system~Year) +
  labs(y=expression(paste("Cash crop yield (Mg ha"^-1,")"))) +
  scale_y_continuous(breaks = seq(0, 14, by = 2), limits = c(0,15))  +# Set intervals of 5
  scale_fill_discrete(labels = c(levels(dat2$Cover)[1:4], "Five-species mix")) +
  theme(legend.position = "bottom", axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),          
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.2), "cm"), 
        legend.key.size = unit(0.05, "cm"), # Adjust legend key size
        strip.text.y=element_text(size=7)) +
  geom_errorbar(inherit.aes=FALSE,data=dat3, width=0,
                aes(x=Cover, ymin=response-SE, ymax=response+SE)) +
  geom_point(inherit.aes=FALSE,data=dat3, x=rep(1:5, 3_4), y=dat3$response,
             fill=rep(covercols[1:5], 3_4), col = "black",size = 2,shape = 21) + 
  geom_text(inherit.aes=FALSE, x=rep(c(1:5), 3_4), y=c(dat3$response + dat3$SE+1.5), size=2.5, fontface="italic", 
            data=dat4, aes(label = round(response,0))) # yield in Mg ha-1
p


# export figure
ggpubr::ggexport(p, height=2700, width=2200, filename = "Figures/_Yield.png", res = 400)










