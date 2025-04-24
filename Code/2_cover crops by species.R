
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

##################### Import processed data #####################
dat <- read.csv("Processed-data/WTREC-CDP_processed-data-simple.csv")

# make factors
dat <- dat %>% mutate_each_(funs(factor(.)),c("Rep", "Depth", "Year", "Season", "Cropping.system", "Cover"))
dat$Cropping.system <- factor(dat$Cropping.system, levels(dat$Cropping.system)[c(1,4,3,2)])
dat$Cover <- factor(dat$Cover, levels(dat$Cover)[c(2,4,1,5,3)])
dat$Season <- factor(dat$Season, levels(dat$Season)[c(2,3,1)])

# subset to one soil depth, remove baseline
dat2020 <- dat[which(dat$Year==2020),]
dat2020_mean <- dat2020 %>%
  group_by(Depth) %>%
  summarise_if(is.numeric, mean)
write.csv(t(dat2020_mean), "Summary-stats/baseline means.csv")
dat <- dat[-which(dat$Year==2020),]
dat_depth2 <- dat[which(dat$Depth=="10-20 cm"),]
dat_depth3 <- dat[which(dat$Depth=="20-30 cm"),]
dat <- dat[which(dat$Depth=="0-10 cm"),]

dat$Season.Year <- as.factor(paste(dat$Season, dat$Year))
cbind(levels(dat$Season.Year), 1:length(levels(dat$Season.Year)))
dat$Season.Year <- factor(dat$Season.Year, levels(dat$Season.Year)[c(5,9,1,6,10,2,7,11,3,8,12,4)])

# subset to years 1/2/3
dat <- dat[which(dat$Year %in% c(2021, 2022, 2023)),]
dat_depth2 <- dat_depth2[which(dat_depth2$Year %in% c(2021, 2022, 2023)),]
dat_depth3 <- dat_depth3[which(dat_depth3$Year %in% c(2021, 2022, 2023)),]



cline <- 0.0275





################################################




################################################
### cover crop performance
################################################
################################################



### first just plot biomass outputs in each year, using barplots split by species
# convert from g m-2 to Mg ha-1
dat$Wheat0 <- dat$Wheat* (10000/1000000)
dat$Clover0 <- dat$Clover* (10000/1000000)
dat$Radish0 <- dat$Radish* (10000/1000000)
dat$Vetch0 <- dat$Vetch* (10000/1000000)
dat$Grass0 <- dat$Grass* (10000/1000000)
dat$Weeds0 <- dat$Weeds* (10000/1000000)



cdat <- dat %>%
  dplyr::group_by(Cover,Year) %>%
  dplyr::filter(Season=="Spring") %>%
  dplyr::summarize(meanw = mean(Wheat0, na.rm = TRUE),
                   meanc = mean(Clover0, na.rm = TRUE),
                   meanr = mean(Radish0, na.rm = TRUE),
                   meanv = mean(Vetch0, na.rm = TRUE),
                   meang = mean(Grass0, na.rm = TRUE),
                   meanweed = mean(Weeds0, na.rm = TRUE))
cdat2 <- data.frame(Cover=rep(cdat$Cover,6),
                    Year=rep(cdat$Year,6),
                    type=rep(c("Wheat", "Clover", "Radish", "Vetch", "Rye+Oats", "Weed"), each=15),
                    biomass=c(cdat$meanw, cdat$meanc, cdat$meanr, cdat$meanv, cdat$meang, cdat$meanweed))
cdat2$type <- as.factor(cdat2$type)
cdat2$type <- factor(cdat2$type, levels(cdat2$type)[c(6,1,2,3,4,5)])

cdat3 <- cdat2 %>%
  dplyr::group_by(type) %>%
  dplyr::filter(Cover=="SHM") %>%
  dplyr::summarize(mb = mean(biomass, na.rm = TRUE))

cdat2$Cover2 <- cdat2$Cover
levels(cdat2$Cover2) <- c(levels(cdat2$Cover2)[1:4], "Five-species mix")
cdat4 <- cdat2 %>%
  mutate(Cover2 = recode(Cover2,
                           "No cover" = "No\ncover",
                           "Wheat" = "Wheat",
                           "Clover" = "Clover",
                           "Wheat-Clover" = "Wheat-\nclover\nmix",
                         "Five-species mix" = "Five-\nspecies\nmix"))

# mean biomass by cropping system and cover
p <- ggplot(cdat4, aes(fill=type, y=biomass, x=Cover2)) +
  geom_bar(position="stack", stat="identity", color="black") +
  theme_bw() + #ylim(c(0,3)) +
  facet_wrap(.~Year, nrow=1) + theme(legend.position = "bottom") +
  labs(x="", y=expression(paste("Aboveground biomass (Mg ha"^-1,")")), fill="Biomass type") +
  scale_fill_manual(values=c("darkorange1","darkolivegreen",  "darkred", "darkslategray3", "darkviolet",  "brown1")) +
  guides(fill = guide_legend(nrow = 1))
ggpubr::ggexport(p, height=1700, width=4000, filename = "Figures/_across years/cover crop and weed biomass_stacked_mgha.png", res=400)


