

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








############### tissue.Fe.ppm_scaled


# plot tissue.Fe.ppm_scaled by cropsys cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.Fe.ppm_scaled_cover_cropsys_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("Foliar Fe (scaled))"))) + labs(color="Cover") +
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
  scale_x_continuous(breaks = c(3,4),  labels = c( c( "2022", "2023"))) +
  ggrepel::geom_text_repel(
    data = testlet[which(testlet$YearN %in% c(3)),], segment.linetype = "dotted",
    aes(x = YearN, y = response, label = .group),color="black",
    size = 2.5,
    direction = "y",          # Adjusts labels only vertically
    nudge_x = 0.5,            # Nudges labels slightly to the right
    na.rm = TRUE              # Ensures it skips missing values
  )

p


# export figure
ggpubr::ggexport(p, height=1500, width=2100, filename = "Figures/_across years/_tissue.Fe.ppm_scaled_cropsys_cover_time.png", res = 400)





############### N



testlet <- read.csv("Model-output/lsmeans/_across years/tissue.N.percent_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar N (scaled)"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.N.percent_scaled_cropsys_time.png", res = 400)







############### P



testlet <- read.csv("Model-output/lsmeans/_across years/tissue.P.percent_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar P (scaled)"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.P.percent_scaled_cropsys_time.png", res = 400)



############### Mg


# plot multifunctionality_index_agronomy2 by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.Mg.percent_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar Mg (scaled)"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.Mg.percent_scaled_cropsys_time.png", res = 400)



############### B


# plot multifunctionality_index_agronomy2 by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.B.ppm_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar B (scaled)"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.B.ppm_scaled_cropsys_time.png", res = 400)





############### Fe


# plot multifunctionality_index_agronomy2 by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.Fe.ppm_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar Fe (scaled)"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.Fe.ppm_scaled_cropsys_time.png", res = 400)





############### Zn


# plot multifunctionality_index_agronomy2 by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.Zn.ppm_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar Zn (scaled)"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.Zn.ppm_scaled_cropsys_time.png", res = 400)







############### Na


# plot multifunctionality_index_agronomy2 by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.Na.ppm_scaled_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar Na (scaled)"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.Na.ppm_scaled_cropsys_time.png", res = 400)








############### multifunctionality_index_nutrition2


# plot multifunctionality_index_agronomy2 by cover and time
testlet <- read.csv("Model-output/lsmeans/_across years/multifunctionality_index_nutrition2_cropsys_time.csv")
#testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cropping.system)) +
  theme_minimal() + labs(y=expression(paste("Foliar nutrient multifunctionality"))) + labs(color="Cropping system") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cropping.system)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = cropsyscols[c(1:4)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_multifunctionality_index_nutrition2_cropsys_time.png", res = 400)












############### S


testlet <- read.csv("Model-output/lsmeans/_across years/tissue.S.percent_scaled_cover_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("Foliar S (scaled)"))) + labs(color="Cover") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(1:5)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.S.percent_scaled_cover_time.png", res = 400)











############### Fe


testlet <- read.csv("Model-output/lsmeans/_across years/tissue.Fe.ppm_scaled_cover_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("Foliar Fe (scaled)"))) + labs(color="Cover") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(1:5)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_tissue.Fe.ppm_scaled_cover_time.png", res = 400)













############### Fe


testlet <- read.csv("Model-output/lsmeans/_across years/multifunctionality_index_nutrition2_cover_time.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])

p <- ggplot(data=testlet, aes(x=YearN, y=response, color=Cover)) +
  theme_minimal() + labs(y=expression(paste("Foliar nutrient multifunctionality"))) + labs(color="Cover") +
  geom_line() +
  geom_point(size = 2, shape = 16, aes(x = YearN, y = response, color = Cover)) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width=0.1) +
  scale_color_manual(values = covercols[c(1:5)]) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 10))+#,
  #axis.text.x = element_text(angle = 300, hjust = 0)) +  
  #facet_wrap(~ Cropping.system, nrow=4) +
  scale_x_continuous(breaks = c(3,4),  labels = c( c("2022", "2023"))) +
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
ggpubr::ggexport(p, height=800, width=1600, filename = "Figures/_across years/_multifunctionality_index_nutrition2_cover_time.png", res = 400)








############### tissue.Fe.ppm_scaled


# plot tissue.Fe.ppm_scaled by cropsys and cover
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.Fe.ppm_scaled_cover_cropsys.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Foliar Fe (scaled)"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  facet_wrap(.~Cropping.system, nrow=2) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  ylim(c(0,0.5)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
  axis.text.x = element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet, size=3,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.03, x=(rep(1:5,4)) )) 

p


# export figure
ggpubr::ggexport(p, height=1500, width=2100, filename = "Figures/_across years/_tissue.Fe.ppm_scaled_cropsys_cover.png", res = 400)




############### tissue.S.ppm_scaled


# plot tissue.S.ppm_scaled by cropsys and cover
testlet <- read.csv("Model-output/lsmeans/_across years/tissue.S.percent_scaled_cover.csv")
testlet$Cover <- as.factor(testlet$Cover); testlet$Cover <- factor(testlet$Cover , levels(testlet$Cover)[c(2,4,1,5,3)])
#testlet$Cropping.system <- as.factor(testlet$Cropping.system); testlet$Cropping.system <- factor(testlet$Cropping.system , levels(testlet$Cropping.system)[c(1,4,3,2)])


p <- ggplot(data=testlet, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y=expression(paste("Foliar S (scaled)"))) + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width=0.2, position=position_dodge(0.8)) +
  #facet_wrap(.~Cropping.system, nrow=2) +
  scale_fill_manual(values = covercols[c(1:5)]) +
  ylim(c(0,0.8)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
    axis.text.x = element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet, size=3,
            aes(label=.group, y=(testlet$response+ testlet$SE)+0.04, x=(rep(1:5,1)) )) 

p


# export figure
ggpubr::ggexport(p, height=1000, width=1800, filename = "Figures/_across years/_tissue.S.percent_scaled_cover.png", res = 400)







