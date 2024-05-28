
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



##################### MODELING APPROACH #####################
### We have a split-split block design, with 4 main plot levels (cropping system)
### nested within each of 4 blocks (Replicate), 5 subplot levels (cover) nested within 
### each main plot and sampled at multiple timepoints (Year or Season.Year)
### There are four predictor variables:
### 1. Replicate (4 levels), random effect
### 2. Cropping system (4 levels), main plot
### 3. Cover (5 levels), subplot
### 4. Year or Season.Year, random effect

# We are interested in the main effects of cover and cropping system
# across time

# lme(response ~ Cover*Cropping.system, 
#     random =~1|Year/Rep/Cropping.system), 
#     data=dat)



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
ggpubr::ggexport(p, height=1700, width=4000, filename = "Figures/*across years/cover crop and weed biomass_stacked_mgha.png", res=400)




################################################
### aboveground biomass of cover crops
################################################

dat$agb <- dat$Wheat+dat$Clover+dat$Radish+dat$Vetch+dat$Grass#+dat$Weeds
dat2 <- dat
dat2$agb[which(dat2$Cover=="No cover")] <- NA
dat2$agb0 <- dat2$agb
# convert from g m-2 to Mg ha-1
# g / m2 * 10000 m2 / ha * Mg / 1000000 g
dat2$agb <- dat2$agb* (10000/1000000) 

hist((dat2$agb)) # this is in units of t/ha


# # explore
# ggplot(dat2) + geom_boxplot(aes(y = agb, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = agb, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = agb, x = Year, color = Cover))
# ggplot(dat2, aes(y = agb, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(agb)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, 
           na.action="na.omit")
ncc <- 4 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$agb)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(agb),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold
# export model stats
  mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
  mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

  ### tukey: Cropping.system
  test1 <- emmeans(fit, ~Cropping.system)
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
  testlet_df_cropsys <- testlet
  testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))
  
  ### tukey: Cover
  test1 <- emmeans(fit, ~Cover)
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
  testlet_df_cover <- testlet
  testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))
  



# plot
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y="") + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols[c(2:5)]) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc))) 
p

# The two lines we want on the plot
line_1 <- expression(paste("Cover crop biomass (Mg ha"^-1,"yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1400, filename = "Figures/*across years/agb_.png", res = 400)
fig_agb <- fig_2


# plot
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y="") + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc))) 
p 

# The two lines we want on the plot
line_1 <- expression(paste("Cover crop biomass (Mg ha"^-1,"yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1200, filename = "Figures/*across years/agb_2.png", res = 400)
fig_agb_2 <- fig_2




# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/agb_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/agb_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/agb.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/agb_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model


testlet_df_cover_agb <- testlet_df_cover





################################################
### weed biomass 
################################################

dat$weeds <- dat$Weeds
# convert from g m-2 to Mg ha-1
# g / m2 * 10000 m2 / ha * Mg / 1000000 g
dat$weeds <- dat$weeds* (10000/1000000) 
dat2 <- dat

hist((dat2$weeds)) # this is in units of Mg/ha


# # explore
# ggplot(dat2) + geom_boxplot(aes(y = weeds, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = weeds, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = weeds, x = Year, color = Cover))
# ggplot(dat2, aes(y = weeds, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((weeds)^(1/3)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$weeds)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(weeds),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = FALSE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = FALSE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))




# plot
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
ccovy2 <- 0.07
ovyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y="") + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc))) 
p

# The two lines we want on the plot
line_1 <- expression(paste("Weed biomass (Mg ha"^-1, "yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1700, filename = "Figures/*across years/weeds_.png", res = 400)
fig_weeds <- fig_2 



# plot
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
ccovy2 <- 0.07
ovyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y="") + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) +
 geom_text(inherit.aes=FALSE, data= testlet_df_cropsys,
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs))) 
p

# The two lines we want on the plot
line_1 <- expression(paste("Weed biomass (Mg ha"^-1, "yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1200, filename = "Figures/*across years/weeds_2.png", res = 400)
fig_weeds_2 <- fig_2 



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/weeds_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/weeds_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/weeds.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/weeds_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model


testlet_df_cover_weeds <- testlet_df_cover










################################################
### C inputs from cover crops (includes weeds and cover crops)
################################################

hist(dat$inputC) # this in units of g m-2
dat$inputC0 <- dat$inputC
dat$inputC <- dat$inputC*0.01 # convert from g m-2 to t ha-1 (10,000 m2 in a hectare; 1,000,000 grams in a Mg)
hist((dat$inputC)) # this is in units of Mg ha-2
dat$inputC[which(dat$Cover=="No cover")]
dat$inputC[which(dat$Season %in% c("Summer", "Fall"))] <- NA


dat2 <- dat

hist((dat2$inputC)) # this is in units of Mg/ha


# # explore
# ggplot(dat2) + geom_boxplot(aes(y = inputC, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = inputC, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = inputC, x = Year, color = Cover))
# ggplot(dat2, aes(y = inputC, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(inputC)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$inputC)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(inputC),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y="") + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc))) 
p

# The two lines we want on the plot
line_1 <- expression(paste("C input (Mg ha"^-1, "yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1700, filename = "Figures/*across years/inputC_.png", res = 400)
fig_inputC <- fig_2



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y="") + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs))) 
p

# The two lines we want on the plot
line_1 <- expression(paste("C input (Mg ha"^-1, "yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1200, filename = "Figures/*across years/inputC_2.png", res = 400)
fig_inputC_2 <- fig_2



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/inputC_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/inputC_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/inputC.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/inputC_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model


testlet_df_cover_inputC <- testlet_df_cover











################################################
###  N inputs from cover crops (includes weeds and cover crops)
################################################

dat$inputN0 <- dat$inputN
dat$inputN <- dat$inputN * 10 # convert from g m-2 to kg ha-1
hist((dat$inputN)) # this is in units of Mg ha-1
dat$inputN[which(dat$Cover=="No cover")]
dat$inputN[which(dat$Season %in% c("Summer", "Fall"))] <- NA

dat2 <- dat

hist((dat2$inputN)) # this is in units of Mg ha-1


# # explore
# ggplot(dat2) + geom_boxplot(aes(y = inputN, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = inputN, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = inputN, x = Year, color = Cover))
# ggplot(dat2, aes(y = inputN, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(inputN)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$inputN)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(inputN),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y="") + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc))) 
p

# The two lines we want on the plot
line_1 <- expression(paste("N input (kg ha"^-1, "yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1700, filename = "Figures/*across years/inputN_.png", res = 400)
fig_inputN <- fig_2



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y="") + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))
p

# The two lines we want on the plot
line_1 <- expression(paste("N input (kg ha"^-1, "yr"^-1,")"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1200, filename = "Figures/*across years/inputN_2.png", res = 400)
fig_inputN_2 <- fig_2



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/inputN_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/inputN_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/inputN.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/inputN_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model


testlet_df_cover_inputN <- testlet_df_cover







################################################
###  CN inputs from cover crops (includes weeds and cover crops)
################################################


dat$inputCN <- dat$inputC/dat$inputN # this is ratio of c to n
hist((dat$inputCN)) 
dat$inputCN[which(dat$Cover=="No cover")]


dat2 <- dat

hist((dat2$inputCN)) # this is unitless


# # explore
# ggplot(dat2) + geom_boxplot(aes(y = inputCN, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = inputCN, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = inputCN, x = Year, color = Cover))
# ggplot(dat2, aes(y = inputCN, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(inputCN)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$inputCN)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(inputCN),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = FALSE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = FALSE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))


# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(y="") + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Input C:N"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=1700, filename = "Figures/*across years/inputCN_.png", res = 400)
fig_inputCN <- fig_2 



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(y="") + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Input C:N"))
fig_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=1200, width=2400, filename = "Figures/*across years/inputCN_2.png", res = 400)
fig_inputCN_2 <- fig_2 



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/inputCN_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/inputCN_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/inputCN.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/inputCN_rsq.csv")


testlet_df_cover_inputCN <- testlet_df_cover



# plot all cc figures together
fig_cc <- ggarrange(fig_agb, fig_weeds, fig_inputC, fig_inputN, fig_inputCN, nrow=3, ncol=2, labels="AUTO")
ggpubr::ggexport(fig_cc, height=3000, width=3000, filename = "Figures/*across years/1_cc_overall.png", res = 400)



# plot all cc figures together

fig_cc_a <- ggarrange(fig_agb_2, fig_weeds_2, ncol=2, labels="AUTO")
fig_cc_b <- ggarrange(fig_inputC_2, fig_inputN_2, ncol=2, labels=c("C", "D"))
df <- data.frame(x=1, y=1)
fig_NA <- ggplot(df,aes(x,y))+geom_blank()+theme_void()
fig_cc_c <- ggarrange(fig_inputCN_2, fig_NA, ncol=2, widths=c(1,0.17),labels=c("E", ""))
fig_cc2 <- ggarrange(fig_cc_a, fig_cc_b, fig_cc_c, nrow=3)
ggpubr::ggexport(fig_cc2, height=3000, width=2500, filename = "Figures/*across years/1_cc_overall_2.png", res = 400)


fig_cc3 <- ggarrange(fig_weeds_2, fig_inputCN_2, nrow=1, ncol=2, labels="AUTO", widths=c(0.59,1))
ggpubr::ggexport(fig_cc3, height=1200, width=3400, filename = "Figures/*across years/1_cc_overall_3.png", res = 400)





################################################



################################################
### soil health - short term indicators
################################################
################################################



################################################
### Soil inorganic (inorgN) top 10 cm
################################################

dat$inorgN0 <- dat$inorgN
hist((dat$inorgN)) # this is in units of mg/kg dry soil in 0-10 cm
hist((dat_depth2$inorgN)) # this is in units of mg/kg dry soil in 10-20 cm
dat2 <- dat

# ## note: original units are mg/kg, can convert to lb/acre by multiplying by 2 for top 6 in (15 cm), but our depth increments are 0-10, 10-20
# ### lbs of soil per acre in 0-10 cm soil = 10 cm * (4.047 * 10^7 cm 2 / acre) *(1.3 g / cm3) * (2.2 lb soil / 1000 g soil) = 1.16 * 10^6 lb / acre
# ## mg nutrient / kg soil * (2.2 lb nutrient / 10^6 mg nutrient) * (1 kg soil / 2.2 lbs soil) * ( 1.16 * 10^6 lb / acre) = 1.16
# ## lbs of soil per acre in 10-20 cm soil = 10 cm * (4.047 * 10^7 cm 2 / acre) *(1.35 g / cm3) * (2.2 lb soil / 1000 g soil) = 1.20 * 10^6 lb / acre
# # mg nutrient / kg soil * (2.2 lb nutrient / 10^6 mg nutrient) * (1 kg soil / 2.2 lbs soil) * ( 1.20 * 10^6 lb / acre ) = 1.20
# dat2$inorgN <- dat2$inorgN*1.16 #+ dat_depth2$inorgN*1.2
# #dat2$inorgN[which(dat2$Season.Year=="Summer 2022")] <- dat$inorgN[which(dat$Season.Year=="Summer 2022")]*1.16 # summer 2022 has incomplete depth 2 data

hist((dat2$inorgN)) # this is in units of mg/kg dry soil in the top 10 cm 


dat2 %>%
  dplyr::group_by(Season.Year) %>%
  dplyr::summarise(mean = mean(inorgN, na.rm=TRUE))


# # explore
# ggplot(dat2) + geom_boxplot(aes(y = inorgN, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = inorgN, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = inorgN, x = Year, color = Cover))
# ggplot(dat2, aes(y = inorgN, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(inorgN)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$inorgN)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(inorgN),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  ylab("") +   
  theme_minimal() +  labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Inorganic N (mg kg"^-1,")"))
fig_inorgN <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_inorgN, height=1200, width=1700, filename = "Figures/*across years/inorgN_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  ylab("") +   
  theme_minimal() +  labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Inorganic N (mg kg"^-1,")"))
fig_inorgN_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_inorgN_2, height=1200, width=1200, filename = "Figures/*across years/inorgN_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/inorgN_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/inorgN_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/inorgN.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/inorgN_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_inorgN <- testlet_df_cover









################################################
### moisture content (GMC)
################################################

dat <- dat
hist((dat$GMC)) # this is in units of g/g dry soil

dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = GMC, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = GMC, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = GMC, x = Year, color = Cover))
# ggplot(dat2, aes(y = GMC, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(GMC)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$GMC)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(GMC),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  ylab("") +   
  theme_minimal() + labs(fill="Cover") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Moisture (g g"^-1,")"))
fig_GMC <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_GMC, height=1200, width=1700, filename = "Figures/*across years/GMC_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  ylab("") +   
  theme_minimal() + labs(fill="Cropping system") +
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Moisture (g g"^-1,")"))
fig_GMC_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_GMC_2, height=1200, width=1200, filename = "Figures/*across years/GMC_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/GMC_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/GMC_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/GMC.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/GMC_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_GMC <- testlet_df_cover













################################################
### C-acquiring enzyme (BG)
################################################

dat <- dat
## note: units are nmol g-1 -h
hist((dat$BG)) 


  

    dat2 <- dat
  
  # # explore
  # ggplot(dat2) + geom_boxplot(aes(y = BG, x = Cover))
  # ggplot(dat2) + geom_boxplot(aes(y = BG, x = Cropping.system))
  # ggplot(dat2) + geom_boxplot(aes(y = BG, x = Year, color = Cover))
  # ggplot(dat2, aes(y = BG, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)
  
  # model
  fit <- lme(sqrt(BG)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
  ncc <- 5 # number of cover crop treatments
  ncs <- 4 # number of cropping system treatments
  
  # check for normality: if <0.05, transform the data
  resid <- residuals(fit)
  shapiro.test(resid)
  
  # check variances: if Var is >5, use a different model that allows variances to differ across groups
  resid2 = cbind(dat2[which(is.na(dat2$BG)==FALSE),],resid)
  temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(BG),funs(mean=mean(., na.rm=T),Std=sd(.)))
  max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold
  
  # export model stats
  mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
  mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)
  
 
  ### tukey: Cropping.system
  test1 <- emmeans(fit, ~Cropping.system)
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
  testlet_df_cropsys <- testlet
  testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))
  
  ### tukey: Cover
  test1 <- emmeans(fit, ~Cover)
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
  testlet_df_cover <- testlet
  testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))
  
  
  
  # plot
  testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
  testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
  maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
  posdodge <- 0.7
  maxylim <- 0.2
  covy <- 0.15
  covy2 <- 0.07
  covyline <- 0.1
  p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
    theme_minimal() + labs(fill="Cover") +
    ylab("")+
    geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
    geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                  width=0.2, position=position_dodge(posdodge)) +
    scale_fill_manual(values = covercols) +
    ylim(c(0,maxy+maxylim*maxy)) +
    theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
          plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
     geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
              aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
  p
  
  # The two lines we want on the plot
  line_1 <- expression(paste("BG activity (nmol g"^-1," h"^-1,")"))
  fig_BG <- cowplot::ggdraw(p) +
    cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
  # export figure
  ggpubr::ggexport(fig_BG, height=1200, width=1700, filename = "Figures/*across years/BG_.png", res = 400)

  

  # plot
  testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
  testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
  maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
  posdodge <- 0.7
  maxylim <- 0.2
  covy <- 0.15
  covy2 <- 0.07
  covyline <- 0.1
  p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
    theme_minimal() + labs(fill="Cropping system") +
    ylab("")+
    geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
    geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                  width=0.2, position=position_dodge(posdodge)) +
    scale_fill_manual(values = cropsyscols) +
    ylim(c(0,maxy+maxylim*maxy)) +
    theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
          plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
          axis.text.x=element_blank()) +
    geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
              aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
  p
  
  # The two lines we want on the plot
  line_1 <- expression(paste("BG activity (nmol g"^-1," h"^-1,")"))
  fig_BG_2 <- cowplot::ggdraw(p) +
    cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
  # export figure
  ggpubr::ggexport(fig_BG_2, height=1200, width=1200, filename = "Figures/*across years/BG_2.png", res = 400)
  
  
  
  # export lsmeans
  write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/BG_cropsys.csv")
  write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/BG_cover.csv")
  # export anova results
  write.csv(mod.out2, "Model-output/anova/*across years/BG.csv")
  write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/BG_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model
  
  testlet_df_cover_BG <- testlet_df_cover
  
  



################################################
### N-acquiring enzyme (NAG)
################################################

dat <- dat
## note: units are nmol g-1 -h
hist((dat$NAG)) 

   
   dat2 <- dat
 
 # # explore
 # ggplot(dat2) + geom_boxplot(aes(y = NAG, x = Cover))
 # ggplot(dat2) + geom_boxplot(aes(y = NAG, x = Cropping.system))
 # ggplot(dat2) + geom_boxplot(aes(y = NAG, x = Year, color = Cover))
 # ggplot(dat2, aes(y = NAG, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)
 
 # model
 fit <- lme(log(NAG)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
 ncc <- 5 # number of cover crop treatments
 ncs <- 4 # number of cropping system treatments
 
 # check for normality: if <0.05, transform the data
 resid <- residuals(fit)
 shapiro.test(resid)
 
 # check variances: if Var is >5, use a different model that allows variances to differ across groups
 resid2 = cbind(dat2[which(is.na(dat2$NAG)==FALSE),],resid)
 temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(NAG),funs(mean=mean(., na.rm=T),Std=sd(.)))
 max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold
 
 # export model stats
 mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
 mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)
 
 ### tukey: Cropping.system
 test1 <- emmeans(fit, ~Cropping.system)
 testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
 testlet <- testlet[order(testlet$Cropping.system),]
 testlet$.group <- gsub(" ", "", testlet$.group)
 if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
 testlet_df_cropsys <- testlet
 testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))
 
 ### tukey: Cover
 test1 <- emmeans(fit, ~Cover)
 testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
 testlet <- testlet[order(testlet$Cover),]
 testlet$.group <- gsub(" ", "", testlet$.group)
 if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
 testlet_df_cover <- testlet
 testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))
 
 
 
 # plot
 testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
 testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
 maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
 posdodge <- 0.7
 maxylim <- 0.2
 covy <- 0.15
 covy2 <- 0.07
 covyline <- 0.1
 p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
   theme_minimal() + labs(fill="Cover") +
   ylab("") +   
   geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
   geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                 width=0.2, position=position_dodge(posdodge)) +
   scale_fill_manual(values = covercols) +
   ylim(c(0,maxy+maxylim*maxy)) +
   theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
         panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
         plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
             aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
 p
 
 # The two lines we want on the plot
 line_1 <- expression(paste("NAG activity (nmol g"^-1," h"^-1,")"))
 fig_NAG <- cowplot::ggdraw(p) +
   cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
 # export figure
 ggpubr::ggexport(fig_NAG, height=1200, width=1700, filename = "Figures/*across years/NAG_.png", res = 400)

 
 
 # plot
 testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
 testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
 maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
 posdodge <- 0.7
 maxylim <- 0.2
 covy <- 0.15
 covy2 <- 0.07
 covyline <- 0.1
 p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
   theme_minimal() + labs(fill="Cropping system") +
   ylab("") +   
   geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
   geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                 width=0.2, position=position_dodge(posdodge)) +
   scale_fill_manual(values = cropsyscols) +
   ylim(c(0,maxy+maxylim*maxy)) +
   theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
         panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
         plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
         axis.text.x=element_blank()) +
   geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
             aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
 p
 
 # The two lines we want on the plot
 line_1 <- expression(paste("NAG activity (nmol g"^-1," h"^-1,")"))
 fig_NAG_2 <- cowplot::ggdraw(p) +
   cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
 # export figure
 ggpubr::ggexport(fig_NAG_2, height=1200, width=1200, filename = "Figures/*across years/NAG_2.png", res = 400)
 
 
 
 # export lsmeans
 write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/NAG_cropsys.csv")
 write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/NAG_cover.csv")
 # export anova results
 write.csv(mod.out2, "Model-output/anova/*across years/NAG.csv")
 write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/NAG_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model
 
 testlet_df_cover_NAG <- testlet_df_cover
 
 
 






################################################
### ratio of C-acquiring enzyme to N-acquiring enzyme (BG_NAG)
################################################

## note: units are nmol g-1 -h
dat$BG_NAG <- dat$BG/dat$NAG
hist((dat$BG_NAG)) 



dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = BG_NAG, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = BG_NAG, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = BG_NAG, x = Year, color = Cover))
# ggplot(dat2, aes(y = BG_NAG, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(log(BG_NAG)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$BG_NAG)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(BG_NAG),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- "C:N enzyme activity"
fig_BG_NAG <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_BG_NAG, height=1200, width=1700, filename = "Figures/*across years/BG_NAG_.png", res = 400)


# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- "C:N enzyme activity"
fig_BG_NAG_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_BG_NAG_2, height=1200, width=1200, filename = "Figures/*across years/BG_NAG_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/BG_NAG_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/BG_NAG_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/BG_NAG.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/BG_NAG_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_BG_NAG <- testlet_df_cover







################################################
### Activity of P-acquiring enzymes
################################################

dat$EEA_P <- dat$PHOS


dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = PHOS, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = PHOS, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = PHOS, x = Year, color = Cover))
# ggplot(dat2, aes(y = PHOS, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((PHOS)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$PHOS)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(PHOS),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("PHOS activity (nmol g dry soil"^-1,"hr"^-1,")"))
fig_PHOS <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_PHOS, height=1200, width=1700, filename = "Figures/*across years/PHOS_.png", res = 400)


# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("PHOS activity (nmol g dry soil"^-1,"hr"^-1,")"))
fig_PHOS_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_PHOS_2, height=1200, width=1200, filename = "Figures/*across years/PHOS_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/PHOS_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/PHOS_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/PHOS.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/PHOS_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_PHOS <- testlet_df_cover






################################################
### water extractable C (WEC)
################################################

hist((dat$WEC)) # this is in units of mg/kg dry soil



dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = WEC, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = WEC, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = WEC, x = Year, color = Cover))
# ggplot(dat2, aes(y = WEC, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((WEC)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$WEC)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(WEC),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Water-extractable C (mg kg"^-1,")"))
fig_WEC <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_WEC, height=1200, width=1700, filename = "Figures/*across years/WEC_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Water-extractable C (mg kg"^-1,")"))
fig_WEC_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_WEC_2, height=1200, width=1200, filename = "Figures/*across years/WEC_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/WEC_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/WEC_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/WEC.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/WEC_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_WEC <- testlet_df_cover







################################################
### water extractable N (WEN)
################################################

WEN <- dat
hist((WEN$WEN)) # this is in units of mg/kg dry soil


dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = WEN, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = WEN, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = WEN, x = Year, color = Cover))
# ggplot(dat2, aes(y = WEN, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(log(WEN+1)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$WEN)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(WEN),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Water-extractable N (mg kg"^-1,")"))
fig_WEN <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_WEN, height=1200, width=1700, filename = "Figures/*across years/WEN_.png", res = 400)


# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Water-extractable N (mg kg"^-1,")"))
fig_WEN_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_WEN_2, height=1200, width=2400, filename = "Figures/*across years/WEN_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/WEN_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/WEN_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/WEN.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/WEN_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_WEN <- testlet_df_cover









################################################
### microbial biomass carbon (MBC)
################################################

hist((dat$MBC)) # this is in units of mg/kg dry soil


dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = MBC, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = MBC, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = MBC, x = Year, color = Cover))
# ggplot(dat2, aes(y = MBC, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(MBC)~Cover*Cropping.system, random=~1|Season.Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$MBC)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(MBC),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Microbial biomass C (mg kg"^-1,")"))
fig_MBC <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_MBC, height=1200, width=1700, filename = "Figures/*across years/MBC_.png", res = 400)




# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
 geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Microbial biomass C (mg kg"^-1,")"))
fig_MBC_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_MBC_2, height=1200, width=1200, filename = "Figures/*across years/MBC_2.png", res = 400)





# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/MBC_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/MBC_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/MBC.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/MBC_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_MBC <- testlet_df_cover








# plot all shi figures together

fig_shi1 <- ggarrange(fig_GMC, fig_inorgN, fig_BG, 
                      fig_NAG,fig_PHOS, fig_MBC,
                      fig_WEC, fig_WEN, ncol=3, nrow=3, labels="AUTO")

# Convert to a ggplot and print
ggpubr::ggexport(fig_shi1, height=3500, width=4500, filename = "Figures/*across years/2_shi_overall.png", res = 400)





# plot all shi figures together

fig_shi1 <- ggarrange(fig_GMC_2, fig_inorgN_2, fig_BG_2, ncol=3, labels="AUTO")
fig_shi3 <- ggarrange(fig_NAG_2, fig_PHOS_2, fig_MBC_2, ncol=3, labels=c("D", "E", "F"))
df <- data.frame(x=1, y=1)
fig_NA <- ggplot(df,aes(x,y))+geom_blank()+theme_void()
fig_shi4 <- ggarrange(fig_WEC_2, fig_WEN_2, fig_NA, ncol=3, labels=c("G", "H", ""), widths=c(1, 1.6,0.4))
fig_shi4

# Convert to a ggplot and print
fig_shi_2 <- ggarrange(fig_shi1, fig_shi3, fig_shi4, nrow=3)
ggpubr::ggexport(fig_shi_2, height=3500, width=4500, filename = "Figures/*across years/2_shi_overall_2.png", res = 400)










################################################
################################################
### soil health - long term indicators
################################################
################################################



################################################
### orgC
################################################

hist((dat$orgC)) # this is in units of g kg-1

# convert to stocks 
dat$BD[which(dat$Season=="Fall")] <- rep(dat2020$BD[which(dat2020$Depth=="0-10 cm")], 3)
# megagrams of carbon per hectare:
dat$orgC1 <- dat$BD*100*10*(dat$orgC/(10*100))
hist(dat$orgC1)
# tons of carbon per hectare:
dat$orgC2 <- dat$BD*100*10*1.10231*(dat$orgC/(10*100))
hist(dat$orgC2)


dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = orgC, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = orgC, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = orgC, x = Year, color = Cover))
# ggplot(dat2, aes(y = orgC, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((orgC)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$orgC)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(orgC),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

# ### tukey: interaction
# test1 <- emmeans(fit, ~Cover, by="Cropping.system")
# testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
# testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
# testlet$.group <- gsub(" ", "", testlet$.group)
# if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc*ncs)}
# if(length(unique(testlet$.group[1:ncc]))==1) {testlet$.group[1:ncc] <- rep("",ncc)}
# if(length(unique(testlet$.group[(1:ncc)+ncc*1]))==1) {testlet$.group[(1:ncc)+ncc*1] <- rep("",ncc)}
# if(length(unique(testlet$.group[(1:ncc)+ncc*2]))==1) {testlet$.group[(1:ncc)+ncc*2] <- rep("",ncc)}
# if(length(unique(testlet$.group[(1:ncc)+ncc*3]))==1) {testlet$.group[(1:ncc)+ncc*3] <- rep("",ncc)}
# testlet_df_int <- testlet
# testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
# testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)

### tukey: interaction
test1 <- emmeans(fit, ~Cover*Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover, testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
testlet_df_int <- testlet
testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Total soil organic C (g kg"^-1,")"))
fig_orgC <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_orgC, height=1200, width=1700, filename = "Figures/*across years/orgC_.png", res = 400)


# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Total soil organic C (g kg"^-1,")"))
fig_orgC_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_orgC_2, height=1200, width=1200, filename = "Figures/*across years/orgC_2.png", res = 400)




# plot
### tukey: interaction
testlet_df_int$minys <- testlet_df_int$response-testlet_df_int$SE
testlet_df_int$minys[which(testlet_df_int$minys<0)] <- 0
maxy <- max(testlet_df_int$response+testlet_df_int$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_int, aes(x=Cropping.system, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(aes(label=.group_lower, y=covy2*(testlet_df_int$response+testlet_df_int$SE)+testlet_df_int$response+testlet_df_int$SE), 
            position = position_dodge(posdodge), size=3)+
  # geom_segment(aes(x = 1-0.3, xend = 1+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 2-0.3, xend = 2+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 3-0.3, xend = 3+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 4-0.3, xend = 4+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 5-0.3, xend = 5+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Total soil organic C (g kg"^-1,")"))
fig_orgC_3 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_orgC_3, height=1200, width=2500, filename = "Figures/*across years/orgC_3.png", res = 400)


# export lsmeans
write.csv(testlet_df_int, "Model-output/lsmeans/*across years/orgC_interactive-effect.csv")
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/orgC_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/orgC_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/orgC.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/orgC_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_orgC <- testlet_df_cover












################################################
### totN
################################################

hist((dat$totN)) # this is in units of g kg-1


dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = totN, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = totN, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = totN, x = Year, color = Cover))
# ggplot(dat2, aes(y = totN, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((totN)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$totN)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(totN),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

# ### tukey: interaction
# test1 <- emmeans(fit, ~Cover, by="Cropping.system")
# testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
# testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
# testlet$.group <- gsub(" ", "", testlet$.group)
# if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc*ncs)}
# if(length(unique(testlet$.group[1:ncc]))==1) {testlet$.group[1:ncc] <- rep("",ncc)}
# if(length(unique(testlet$.group[(1:ncc)+ncc*1]))==1) {testlet$.group[(1:ncc)+ncc*1] <- rep("",ncc)}
# if(length(unique(testlet$.group[(1:ncc)+ncc*2]))==1) {testlet$.group[(1:ncc)+ncc*2] <- rep("",ncc)}
# if(length(unique(testlet$.group[(1:ncc)+ncc*3]))==1) {testlet$.group[(1:ncc)+ncc*3] <- rep("",ncc)}
# testlet_df_int <- testlet
# testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
# testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)

### tukey: interaction
test1 <- emmeans(fit, ~Cover, by="Cropping.system")
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover, testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
testlet_df_int <- testlet
testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Total soil N (g kg"^-1,")"))
fig_totN <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_totN, height=1200, width=1700, filename = "Figures/*across years/totN_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Total soil N (g kg"^-1,")"))
fig_totN_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_totN_2, height=1200, width=1200, filename = "Figures/*across years/totN_2.png", res = 400)




# plot
### tukey: interaction
testlet_df_int$minys <- testlet_df_int$response-testlet_df_int$SE
testlet_df_int$minys[which(testlet_df_int$minys<0)] <- 0

maxy <- max(testlet_df_int$response+testlet_df_int$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_int, aes(x=Cropping.system, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(aes(label=.group_lower, y=covy2*(testlet_df_int$response+testlet_df_int$SE)+testlet_df_int$response+testlet_df_int$SE), 
            position = position_dodge(posdodge), size=3)+
  # geom_segment(aes(x = 1-0.3, xend = 1+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 2-0.3, xend = 2+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 3-0.3, xend = 3+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 4-0.3, xend = 4+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 5-0.3, xend = 5+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Total soil N (g kg"^-1,")"))
fig_totN_3 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_totN_3, height=1200, width=2500, filename = "Figures/*across years/totN_3.png", res = 400)




# export lsmeans
write.csv(testlet_df_int, "Model-output/lsmeans/*across years/totN_interactive-effect.csv")
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/totN_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/totN_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/totN.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/totN_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_totN <- testlet_df_cover








################################################
### POC
################################################

hist((dat$POC)) # this is in units of g kg-1


dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = POC, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = POC, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = POC, x = Year, color = Cover))
# ggplot(dat2, aes(y = POC, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(POC)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$POC)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(POC),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Particulate organic C (g kg"^-1,")"))
fig_POC <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_POC, height=1200, width=1700, filename = "Figures/*across years/POC_.png", res = 400)


# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Particulate organic C (g kg"^-1,")"))
fig_POC_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_POC_2, height=1200, width=1200, filename = "Figures/*across years/POC_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/POC_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/POC_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/POC.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/POC_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_POC <- testlet_df_cover









################################################
### MAOC
################################################

hist((dat$MAOC)) # this is in units of g kg-1



dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = MAOC, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = MAOC, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = MAOC, x = Year, color = Cover))
# ggplot(dat2, aes(y = MAOC, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((MAOC)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$MAOC)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(MAOC),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Mineral-associated organic C (g kg"^-1,")"))
fig_MAOC <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_MAOC, height=1200, width=1700, filename = "Figures/*across years/MAOC_.png", res = 400)




# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Mineral-associated organic C (g kg"^-1,")"))
fig_MAOC_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_MAOC_2, height=1200, width=1700, filename = "Figures/*across years/MAOC_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/MAOC_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/MAOC_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/MAOC.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/MAOC_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_MAOC <- testlet_df_cover












################################################
### Activity of C-acquiring enzymes
################################################

dat$EEA_C <- dat$AG + dat$BG + dat$CB + dat$XYL
hist((dat$EEA_C)) # cumulative measure of four enzymes



dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = EEA_C, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = EEA_C, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = EEA_C, x = Year, color = Cover))
# ggplot(dat2, aes(y = EEA_C, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((EEA_C)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$EEA_C)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(EEA_C),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: interaction
test1 <- emmeans(fit, ~Cover, by="Cropping.system")
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc*ncs)}
if(length(unique(testlet$.group[1:ncc]))==1) {testlet$.group[1:ncc] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*1]))==1) {testlet$.group[(1:ncc)+ncc*1] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*2]))==1) {testlet$.group[(1:ncc)+ncc*2] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*3]))==1) {testlet$.group[(1:ncc)+ncc*3] <- rep("",ncc)}
testlet_df_int <- testlet
testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("C-cycling enzyme activity"))
line_2 <- expression(paste("(nmol g dry soil"^-1,"hr"^-1,")"))
fig_EEA_C <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) +# use relative coordinates for positioning
  cowplot::draw_label(line_2, x = cline+0.035, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_EEA_C, height=1200, width=1700, filename = "Figures/*across years/EEA_C_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("C-cycling enzyme activity"))
line_2 <- expression(paste("(nmol g dry soil"^-1,"hr"^-1,")"))
fig_EEA_C_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) +# use relative coordinates for positioning
  cowplot::draw_label(line_2, x = cline+0.035, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_EEA_C_2, height=1200, width=1200, filename = "Figures/*across years/EEA_C_2.png", res = 400)



# plot
testlet_df_int$minys <- testlet_df_int$response-testlet_df_int$SE
#testlet_df_int$minys[which(testlet_df_int$minys<0)] <- 0
maxy <- max(testlet_df_int$response+testlet_df_int$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_int, aes(x=Cropping.system, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(aes(label=.group_lower, y=covy2*(testlet_df_int$response+testlet_df_int$SE)+testlet_df_int$response+testlet_df_int$SE),
            position = position_dodge(posdodge), size=3)+
  # geom_segment(aes(x = 1-0.3, xend = 1+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 2-0.3, xend = 2+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 3-0.3, xend = 3+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 4-0.3, xend = 4+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 5-0.3, xend = 5+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("C-cycling enzyme activity"))
line_2 <- expression(paste("(nmol g dry soil"^-1,"hr"^-1,")"))
fig_EEA_C_3 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline-0.01, y = 0.5, angle=90, size = 10) +# use relative coordinates for positioning
  cowplot::draw_label(line_2, x = cline+0.01, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_EEA_C_3, height=1200, width=2500, filename = "Figures/*across years/EEA_C_3.png", res = 400)



# export lsmeans
write.csv(testlet_df_int, "Model-output/lsmeans/*across years/EEA_C_interactive-effect.csv")
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/EEA_C_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/EEA_C_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/EEA_C.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/EEA_C_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_EEA_C <- testlet_df_cover








################################################
### Activity of N-acquiring enzymes
################################################

dat$EEA_N <- dat$NAG + dat$LAP

hist((dat$EEA_N)) # cumulative measure of two enzymes


dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = EEA_N, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = EEA_N, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = EEA_N, x = Year, color = Cover))
# ggplot(dat2, aes(y = EEA_N, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(EEA_N)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$EEA_N)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(EEA_N),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("N-cycling enzyme activity"))
line_2 <- expression(paste("(nmol g dry soil"^-1,"hr"^-1,")"))
fig_EEA_N <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) +# use relative coordinates for positioning
  cowplot::draw_label(line_2, x = cline+0.035, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_EEA_N, height=1200, width=1700, filename = "Figures/*across years/EEA_N_.png", res = 400)




# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("N-cycling enzyme activity"))
line_2 <- expression(paste("(nmol g dry soil"^-1,"hr"^-1,")"))
fig_EEA_N_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline-0.01, y = 0.5, angle=90, size = 10) +# use relative coordinates for positioning
  cowplot::draw_label(line_2, x = cline+0.02, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_EEA_N_2, height=1200, width=1200, filename = "Figures/*across years/EEA_N_2.png", res = 400)




# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/EEA_N_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/EEA_N_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/EEA_N.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/EEA_N_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_EEA_N <- testlet_df_cover











################################################
### WAS
################################################

hist((dat$WAS)) # this is in units of %



dat2 <- dat

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = WAS, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = WAS, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = WAS, x = Year, color = Cover))
# ggplot(dat2, aes(y = WAS, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme(sqrt(WAS)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$WAS)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(WAS),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
   geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Wet aggregate stability (%)"))
fig_WAS <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_WAS, height=1200, width=1700, filename = "Figures/*across years/WAS_.png", res = 400)




# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Wet aggregate stability (%)"))
fig_WAS_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_WAS_2, height=1200, width=2400, filename = "Figures/*across years/WAS_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/WAS_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/WAS_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/WAS.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/WAS_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_WAS <- testlet_df_cover











# plot all shi figures together: cover main effect
fig_shi <- ggarrange(fig_orgC,  fig_POC,  fig_MAOC, 
                     fig_totN,  fig_EEA_C,   fig_EEA_N, 
                     fig_WAS, nrow=3, ncol=3, labels="AUTO")
ggpubr::ggexport(fig_shi, height=3500, width=4500, filename = "Figures/*across years/3_shi_overall.png", res = 400)



# plot all shi figures together: cropsys main effect
fig_shi1 <- ggarrange(fig_orgC_2, 
                      fig_POC_2, 
                      fig_MAOC_2, ncol=3, labels="AUTO")
fig_shi2 <- ggarrange(fig_totN_2, 
                      fig_EEA_C_2, 
                      fig_EEA_N_2, ncol=3, labels=c("D", "E", "F"))
df <- data.frame(x=1, y=1)
fig_NA <- ggplot(df,aes(x,y))+geom_blank()+theme_void()
fig_shi3 <- ggarrange(fig_WAS_2, fig_NA, fig_NA, ncol=3, widths = c(1,0.465,0.465), labels=c("G", "", ""))
# Convert to a ggplot and print
fig_shi <- ggarrange(fig_shi1, fig_shi2, fig_shi3, nrow=3)
ggpubr::ggexport(fig_shi, height=3500, width=4800, filename = "Figures/*across years/3_shi_overall_2.png", res = 400)



# plot all shi figures together: cover x cropsys interactive effect
fig_shi <- ggarrange(fig_orgC_3,  
                     fig_orgC_3,  
                     fig_EEA_C_3,
                     nrow=3, ncol=1, labels="AUTO")
ggpubr::ggexport(fig_shi, height=2800, width=3000, filename = "Figures/*across years/3_shi_overall_3.png", res = 400)




















################################################
################################################
### main crop performance: yield and foliar nutrients
################################################
################################################




################################################
### main crop yield
################################################


Yield <- dat

Yield$Yield0 <- dat$Yield
hist((Yield$Yield0)) # this is in units of bushels/acre (corn & soybean) or lbs/acre (cotton)
hist(Yield$Yield0[which(Yield$Yield.crop %in% c("Corn", "corn"))])
hist(Yield$Yield0[which(Yield$Yield.crop %in% c("Soybean", "soybean"))])
hist(Yield$Yield0[which(Yield$Yield.crop %in% c("Cotton", "cotton"))])

Yield$Yield <- dat$Yield
Yield$Yield[which(Yield$Yield.crop %in% c("Corn", "corn"))] <- Yield$Yield[which(Yield$Yield.crop %in% c("Corn", "corn"))]*56*2.47105/2204.62
Yield$Yield[which(Yield$Yield.crop %in% c("Soybean", "soybean"))] <- Yield$Yield[which(Yield$Yield.crop %in% c("Soybean", "soybean"))]*60*2.47105/2204.62
Yield$Yield[which(Yield$Yield.crop %in% c("Cotton", "cotton"))] <- Yield$Yield[which(Yield$Yield.crop %in% c("Cotton", "cotton"))]*2.47105/2204.62
hist((Yield$Yield)) # this is in units of Mg/ha [converted from bushels/acre (corn & soybean) or lbs/acre (cotton)]


# standardize by annual average crop production in Madison County TN: 
# data from USDA national agricultural statistics service, stored in subfolder: "5_Cover crop manuscript/madison county annual yields"
# TNavg0 represents units of bu ac-1 for corn and soybean, lb ac-1 for cotton
# TNavg represents units of Mg ha-1 (converted from bu ac-1 for corn and soybean, lb ac-1 for cotton)
# 2.47105 acres in a ha
# 2204.62 lbs in a Mg
# corn: 56 lb per bu
# soybean: 60 lb per bu
yielddf <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                      Year=as.factor(rep(c(2021, 2022, 2023),4)),
                      TNavg0=c(169.9, 129.6, 177.8, # corn 2021 - corn 2022 - corn 2023
                              53.8, 50, 51.6, # soybean 2021 - soybean 2022 - soybean 2023
                              169.9, 50, 177.8, # corn 2021 - soybean 2022 - corn 2023
                              169.9, 1126, 51.6), # corn 2021 - cotton 2022 - soybean 2023
                      TNavg=c(56*2.47105*169.9/2204.62, # corn 2021 
                              56*2.47105*129.6/2204.62, # corn 2022
                              56*2.47105*177.8/2204.62, # corn 2023
                              60*2.47105*53.8/2204.62,  # soybean 2021
                              60*2.47105*50/2204.62,    # soybean 2022
                              60*2.47105*51.6/2204.62,  # soybean 2023
                              56*2.47105*169.9/2204.62, # corn 2021
                              60*2.47105*50/2204.62,    # soybean 2022 
                              56*2.47105*177.8/2204.62, # corn 2023
                              56*2.47105*169.9/2204.62, # corn 2021 
                              2.47105*1126/2204.62,  # cotton 2022 
                              60*2.47105*51.6/2204.62)) # soybean 2023
yielddf$Cropping.system <- factor(yielddf$Cropping.system, levels(yielddf$Cropping.system)[c(1,4,3,2)])
yielddf$Year
yielddf
Yield$Yield_std <- rep(NA, dim(Yield)[1])
Yield$Yield_std0 <- rep(NA, dim(Yield)[1])
Yield$TN_avg0 <- rep(NA, dim(Yield)[1])
Yield$TN_avg <- rep(NA, dim(Yield)[1])


for(i in 1:dim(yielddf)[1]){
  subset_1 <- which(Yield$Cropping.system==yielddf$Cropping.system[i] & Yield$Year %in% c(yielddf$Year[i]) & Yield$Season=="Summer") 
  Yield$Yield_std[subset_1] <- Yield$Yield[subset_1]/yielddf$TNavg[i]
  Yield$Yield_std0[subset_1] <- Yield$Yield0[subset_1]/yielddf$TNavg0[i]
  Yield$TN_avg0[subset_1] <- rep(yielddf$TNavg0[i], length(Yield$TN_avg0[subset_1]))
  Yield$TN_avg[subset_1] <- rep(yielddf$TNavg[i], length(Yield$TN_avg[subset_1]))
}
hist(Yield$Yield_std)
hist(Yield$Yield_std^2)
hist(Yield$Yield_std0)
hist(Yield$Yield_std0^2)



dat2 <- Yield

# # explore
# ggplot(dat2) + geom_boxplot(aes(y = Yield_std, x = Cover))
# ggplot(dat2) + geom_boxplot(aes(y = Yield_std, x = Cropping.system))
# ggplot(dat2) + geom_boxplot(aes(y = Yield_std, x = Year, color = Cover))
# ggplot(dat2, aes(y = Yield_std, x = Year, color = Cover, group = Cover)) + geom_line() + facet_wrap(~Cropping.system)

# model
fit <- lme((Yield_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$Yield_std)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(Yield_std),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized yield"))
fig_Yield_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_Yield_std, height=1200, width=1700, filename = "Figures/*across years/Yield_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "right",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized yield"))
fig_Yield_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_Yield_std_2, height=1200, width=2400, filename = "Figures/*across years/Yield_std_2.png", res = 400)




# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/Yield_std_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/Yield_std_cover.csv")
write.csv(yielddf, "Model-output/lsmeans/*across years/yield annual county averages.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/Yield_std.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/Yield_std_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model
testlet_df_cover_Yield_std <- testlet_df_cover









################################################
### main crop yield within each year
################################################




# Look at yield across reps 
Yield %>%
  dplyr::group_by(Rep, Year) %>%
  dplyr::summarise(meanyield=mean(Yield_std, na.rm=TRUE)) 
# considered tossing rep 1 because there was a lot of herbivore damage, 
# however including rep as random effect in the model should take care of this





## Create dataframes to store model output:
testlet_df_int <- data.frame(Cropping.system=NA, Cover=NA, response=NA, .group=NA, SE=NA)
testlet_df_cropsys <- data.frame(Cropping.system=NA, response=NA, .group=NA, SE=NA)
testlet_df_cover <- data.frame(Cover=NA, response=NA, .group=NA, SE=NA)
options("scipen"=100, "digits"=4)
n <- 3
mod.effects <- data.frame(Year=rep(NA, n), 
                          Response=rep(NA, n), 
                          Shapiro.test=rep(NA, n), 
                          Var=rep(NA,n),
                          Cover.effect=rep(NA, n),
                          Cropsys.effect=rep(NA, n), 
                          Interaction.effect=rep(NA, n),
                          R2m=rep(NA, n), 
                          R2c=rep(NA, n))
# Yield$Cropping.system.Year <- as.factor(paste0(Yield$Cropping.system," ", Yield$Year))
# Yield$Cropping.system.Year <- factor(Yield$Cropping.system.Year, levels(Yield$Cropping.system.Year)[c(1,10,7,4, c(1,10,7,4)+1, c(1,10,7,4)+2)])
tt_yield <- levels(Yield$Year)[c(1:n)+1]

mods_Yield_std <- list(rep(NA, n))

# models
for(i in 1:length(tt_yield)) {
  #mod.effects$Cropping.system[i] <- str_split(tt_yield," ")[[i]][1]
  #mod.effects$Season[i] <- str_split(tt_yield," ")[[i]][1]
  mod.effects$Year[i] <- str_split(tt_yield," ")[[i]][1]
  mod.effects$Response[i] <- "Yield"
  dat_subset <- Yield[which(Yield$Year==tt_yield[i] & Yield$Season=="Summer"), ]
  fit <- lme((Yield_std) ~ Cropping.system*Cover, random=~1|Rep/Cropping.system, 
             data = dat_subset,  
             na.action="na.omit")
  
  # check for normality
  resid <- residuals(fit)
  shapiro.test.values <- shapiro.test(resid)
  mod.effects$Shapiro.test[i] <- shapiro.test.values$p.value
  # # if residuals are non-normal remove outliers
  # if(mod.effects$Shapiro.test[i]<0.05){
  #   dat_subset <- dat_subset[-which(abs(resid) > 0.2),]
  #   fit <- lme((Yield_std) ~ Cropping.system*Cover, random=~1|Rep/Cropping.system, 
  #              data = dat_subset,  
  #              na.action="na.omit")
  #   # check again for normality
  #   resid <- residuals(fit)
  #   shapiro.test.values <- shapiro.test(resid)
  #   mod.effects$Shapiro.test[i] <- shapiro.test.values$p.value
  # }
  
  # check variances
  resid2 = cbind(dat_subset[which(is.na(dat_subset$Yield_std)==FALSE),],resid)
  temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(Yield_std),funs(mean=mean(., na.rm=T),Std=sd(.)))
  mod.effects$Var[i] <-  max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold
  # # if Var is >5, use a different model that allows variances to differ across groups
  # if(mod.effects$Var[i]>5){
  # fit <- lme((Yield_std) ~ Cropping.system*Cover, random=~1|Rep/Cropping.system, 
  #                   data = dat_subset,  
  #                   na.action="na.omit",
  #            weights = varIdent(form= ~ 1 | Cropping.system*Cover))}
  
  # export model stats
  mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
  mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)
  mod.effects$Cover.effect[i] <- paste0("Chisq=", mod.out2$`Chisq`[1], ", Df=", mod.out2$`Df`[1], ", p=", mod.out2$`Pr(>Chisq)`[1])   
  mod.effects$Cropsys.effect[i] <- paste0("Chisq=", mod.out2$`Chisq`[2], ", Df=", mod.out2$`Df`[2], ", p=", mod.out2$`Pr(>Chisq)`[2])   
  mod.effects$Interaction.effect[i] <- paste0("Chisq=", mod.out2$`Chisq`[3], ", Df=", mod.out2$`Df`[3], ", p=", mod.out2$`Pr(>Chisq)`[3])
  mod.effects$R2m[i] <- round(r.squaredGLMM(fit),2)[1] # marginal: explained by fixed effects; conditional: explained by entire model
  mod.effects$R2c[i] <- round(r.squaredGLMM(fit),2)[2] # marginal: explained by fixed effects; conditional: explained by entire model
  
  ### tukey & visualize: interaction
  # compute least-squares means
  test1 <- emmeans(fit, ~Cover, by="Cropping.system")
  # compact letter display
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",20)}
  if(length(unique(testlet$.group[1:5]))==1) {testlet$.group[1:5] <- rep("",5)}
  if(length(unique(testlet$.group[6:10]))==1) {testlet$.group[6:10] <- rep("",5)}
  if(length(unique(testlet$.group[11:15]))==1) {testlet$.group[11:15] <- rep("",5)}
  if(length(unique(testlet$.group[16:20]))==1) {testlet$.group[16:20] <- rep("",5)}
  testlet_df_int <- rbind(testlet_df_int, testlet[,c("Cropping.system","Cover", "response", ".group", "SE")])
  
  ### tukey & visualize: Cropping.system
  # compute least-squares means
  test1 <- emmeans(fit, ~Cropping.system)
  # compact letter display
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",4)}
  testlet_df_cropsys <- rbind(testlet_df_cropsys, testlet[,c("Cropping.system", "response", ".group", "SE")])
  
  ### tukey & visualize: Cover
  # compute least-squares means
  test1 <- emmeans(fit, ~Cover)
  # compact letter display
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",5)}
  testlet_df_cover <- rbind(testlet_df_cover,testlet[,c("Cover", "response", ".group", "SE")])
  
  mods_Yield_std[[i]] <- fit
}

# check
mods_Yield_std[[1]][["call"]]
mods_Yield_std[[2]][["call"]]
mods_Yield_std[[3]][["call"]]


testlet_df_int <- testlet_df_int[-1,]
testlet_df_int$Year <- rep(tt_yield, each=20)
testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Year",  "Cover", "Cropping.system"))
testlet_df_int$Cropping.system <- factor(testlet_df_int$Cropping.system, levels(testlet_df_int$Cropping.system)[c(1,4,3,2)])
testlet_df_int$Cover <- factor(testlet_df_int$Cover, levels(testlet_df_int$Cover)[c(2,4,1,5,3)])
testlet_df_int$Year <- factor(testlet_df_int$Year, levels(testlet_df_int$Year))

testlet_df_cropsys <- testlet_df_cropsys[-1,]
testlet_df_cropsys$Year <- rep(tt_yield, each=4)
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Year", "Cropping.system"))
testlet_df_cropsys$Cropping.system <- factor(testlet_df_cropsys$Cropping.system, levels(testlet_df_cropsys$Cropping.system)[c(1,4,3,2)])
testlet_df_cropsys$Year <- factor(testlet_df_cropsys$Year, levels(testlet_df_cropsys$Year))


testlet_df_cover <- testlet_df_cover[-1,]
testlet_df_cover$Year <- rep(tt_yield, each=5)
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Year", "Cover"))
testlet_df_int$Cover <- factor(testlet_df_int$Cover, levels(testlet_df_int$Cover)[c(2,4,1,5,3)])
testlet_df_cover$Year <- factor(testlet_df_cover$Year, levels(testlet_df_cover$Year))


# back-transform average standardized yield to bushels/ha
testlet_df_int$response_bt <- rep(NA, dim(testlet_df_int)[1])
testlet_df_int$TNavg <- rep(NA, dim(testlet_df_int)[1])
testlet_df_int$TNavg0 <- rep(NA, dim(testlet_df_int)[1])
testlet_df_int$maincrop <- c(rep(c("corn", "soy", "corn", "corn"), each=5),
                             rep(c("corn", "soy", "soy", "cotton"), each=5),
                             rep(c("corn", "soy", "corn", "soy"), each=5))
testlet_df_int$response_bt0 <- rep(NA, dim(testlet_df_int)[1])
for(i in 1:dim(testlet_df_int)[1]){
  testlet_df_int$response_bt[i] <- testlet_df_int$response[i] * yielddf$TNavg[which(yielddf$Year==testlet_df_int$Year[i] & yielddf$Cropping.system==testlet_df_int$Cropping.system[i])]
  testlet_df_int$TNavg[i] <- yielddf$TNavg[which(yielddf$Year==testlet_df_int$Year[i] & yielddf$Cropping.system==testlet_df_int$Cropping.system[i])]
  testlet_df_int$TNavg0[i] <- yielddf$TNavg0[which(yielddf$Year==testlet_df_int$Year[i] & yielddf$Cropping.system==testlet_df_int$Cropping.system[i])]
}
testlet_df_int$response_bt0[which(testlet_df_int$maincrop=="corn")] <- testlet_df_int$response_bt[which(testlet_df_int$maincrop=="corn")]*2204.62/(56*2.47105)
testlet_df_int$response_bt0[which(testlet_df_int$maincrop=="soy")] <- testlet_df_int$response_bt[which(testlet_df_int$maincrop=="soy")]*2204.62/(60*2.47105)
testlet_df_int$response_bt0[which(testlet_df_int$maincrop=="cotton")] <- testlet_df_int$response_bt[which(testlet_df_int$maincrop=="cotton")]*2204.62/(2.47105)



testlet_df_cropsys$response_bt <- rep(NA, dim(testlet_df_cropsys)[1])
testlet_df_cropsys$TNavg <- rep(NA, dim(testlet_df_cropsys)[1])
testlet_df_cropsys$TNavg0 <- rep(NA, dim(testlet_df_cropsys)[1])
testlet_df_cropsys$maincrop <- c(rep(c("corn", "soy", "corn", "corn"), each=1),
                             rep(c("corn", "soy", "soy", "cotton"), each=1),
                             rep(c("corn", "soy", "corn", "soy"), each=1))
testlet_df_cropsys$response_bt0 <- rep(NA, dim(testlet_df_cropsys)[1])
for(i in 1:dim(testlet_df_cropsys)[1]){
  testlet_df_cropsys$response_bt[i] <- testlet_df_cropsys$response[i] * yielddf$TNavg[which(yielddf$Year==testlet_df_cropsys$Year[i] & yielddf$Cropping.system==testlet_df_cropsys$Cropping.system[i])]
  testlet_df_cropsys$TNavg[i] <- yielddf$TNavg[which(yielddf$Year==testlet_df_cropsys$Year[i] & yielddf$Cropping.system==testlet_df_cropsys$Cropping.system[i])]
  testlet_df_cropsys$TNavg0[i] <- yielddf$TNavg0[which(yielddf$Year==testlet_df_cropsys$Year[i] & yielddf$Cropping.system==testlet_df_cropsys$Cropping.system[i])]
}
testlet_df_cropsys$response_bt0[which(testlet_df_cropsys$maincrop=="corn")] <- testlet_df_cropsys$response_bt[which(testlet_df_cropsys$maincrop=="corn")]*2204.62/(56*2.47105)
testlet_df_cropsys$response_bt0[which(testlet_df_cropsys$maincrop=="soy")] <- testlet_df_cropsys$response_bt[which(testlet_df_cropsys$maincrop=="soy")]*2204.62/(60*2.47105)
testlet_df_cropsys$response_bt0[which(testlet_df_cropsys$maincrop=="cotton")] <- testlet_df_cropsys$response_bt[which(testlet_df_cropsys$maincrop=="cotton")]*2204.62/(2.47105)



# plot
p <- ggplot(data=Yield[which(Yield$Season=="Summer"),], 
            aes(x=Cover, y=Yield_std, fill=Cover)) +
  geom_hline(yintercept=1, linetype="dashed", alpha=0.5) +
  #geom_bar(position = "dodge", stat = "summary",fun = "mean")+
  theme_minimal() +
  facet_grid(Cropping.system~Year) +
  labs(y="") +
  scale_x_continuous(limits=c(0.5,5.5)) + ylim(c(0.5,1.55)) + 
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),          
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.6), "cm")) +
  geom_errorbar(inherit.aes=FALSE,data=testlet_df_int, width=0.2,
                aes(x=rep(c(1:5), n*4), ymin=response-SE, ymax=response+SE)) +
  geom_point(inherit.aes=FALSE,data=testlet_df_int, x=rep(1:5, n*4), y=testlet_df_int$response,
             fill=rep(covercols[1:5], n*4), col = "black",size = 2,shape = 21)
fig <- p + 
  geom_text(x=rep(c(1:5), n*4)+0.2, y=c(testlet_df_int$response), hjust=0, size=3, data=testlet_df_int, aes(label = .group)) + 
  #geom_text(x=rep(c(1:5), n*4), y=c(testlet_df_int$response - testlet_df_int$SE-0.05), size=2.5, fontface="italic", data=testlet_df_int, aes(label = round(response_bt0,0))) + # yield in bu ac-1 (corn/soy) and lb ac-1 (cotton)
  geom_text(x=rep(c(1:5), n*4), y=c(testlet_df_int$response + testlet_df_int$SE+0.085), size=2.5, fontface="italic", data=testlet_df_int, aes(label = round(response_bt,2))) # yield in Mg ha-1

# The two lines we want on the plot
line_1 <- expression(paste("Standardized yield (", over("Actual",  "County average"), ")"))
# Or avoid paste() (is not actually needed)
# line_2 <- expression("of illustrating my point" [reported])
# Call cowplot::draw_label two times to plot two lines of text
fig_2 <- cowplot::ggdraw(fig) + 
  cowplot::draw_label(line_1, x = 0.035, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning

# export figure
ggpubr::ggexport(fig_2, height=2700, width=2500, filename = "Figures/Yield.png", res = 400)







# plot year for each year separately with actual yield

dat_actual <- Yield %>%
  dplyr::group_by(Year, Cropping.system, Cover) %>%
  dplyr::summarise(meanyield=mean(Yield, na.rm=TRUE), 
                   seyield=sd(Yield, na.rm=TRUE)/sqrt(4),
                   TN_avg=mean(TN_avg, na.rm=TRUE),
                   meanyield0=mean(Yield0, na.rm=TRUE), 
                   seyield0=sd(Yield0, na.rm=TRUE)/sqrt(4),
                   TN_avg0=mean(TN_avg0, na.rm=TRUE)) 
dat_actual <- as.data.frame(dat_actual)

n <- 1

dat_actual$max <- dat_actual$meanyield + dat_actual$seyield
dat_actual$max_year <- c(rep(max(dat_actual$max[c(1:5, 11:20)]*1.1), 5),
                         rep(max(dat_actual$max[6:10]*1.1), 5),
                         rep(max(dat_actual$max[c(1:5, 11:20)]*1.1), 5),
                         rep(max(dat_actual$max[c(1:5, 11:20)]*1.1), 5),
                         rep(max(dat_actual$max[21:25]*1.1), 5),
                         rep(max(dat_actual$max[26:35]*1.1), 5),
                         rep(max(dat_actual$max[26:35]*1.1), 5),
                         rep(max(dat_actual$max[36:40]*1.1), 5),
                         rep(max(dat_actual$max[c(41:45,51:55)]*1.1), 5),
                         rep(max(dat_actual$max[c(46:50,56:60)]*1.1), 5),
                         rep(max(dat_actual$max[c(41:45,51:55)]*1.1), 5),
                         rep(max(dat_actual$max[c(46:50,56:60)]*1.1), 5))
dat_actual$max_crop <- c(rep(max(dat_actual$max[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(max(dat_actual$max[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(max(dat_actual$max[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(max(dat_actual$max[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(max(dat_actual$max[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(max(dat_actual$max[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(max(dat_actual$max[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(max(dat_actual$max[36:40]), 5),
                         rep(max(dat_actual$max[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(max(dat_actual$max[c(c(6:10, 26:35, 46:50,56:60))]), 5),
                         rep(max(dat_actual$max[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(max(dat_actual$max[c(c(6:10, 26:35, 46:50,56:60))]), 5))
dat_actual$min <- dat_actual$meanyield - dat_actual$seyield
dat_actual$min_year <- c(rep(min(dat_actual$min[c(1:5, 11:20)]), 5),
                         rep(min(dat_actual$min[6:10]), 5),
                         rep(min(dat_actual$min[c(1:5, 11:20)]), 5),
                         rep(min(dat_actual$min[c(1:5, 11:20)]), 5),
                         rep(min(dat_actual$min[21:25]), 5),
                         rep(min(dat_actual$min[26:35]), 5),
                         rep(min(dat_actual$min[26:35]), 5),
                         rep(min(dat_actual$min[36:40]), 5),
                         rep(min(dat_actual$min[c(41:45,51:55)]), 5),
                         rep(min(dat_actual$min[c(46:50,56:60)]), 5),
                         rep(min(dat_actual$min[c(41:45,51:55)]), 5),
                         rep(min(dat_actual$min[c(46:50,56:60)]), 5))
dat_actual$min_crop <- c(rep(min(dat_actual$min[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(min(dat_actual$min[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(min(dat_actual$min[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(min(dat_actual$min[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(min(dat_actual$min[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(min(dat_actual$min[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(min(dat_actual$min[c(6:10, 26:35, 46:50,56:60)]), 5),
                         rep(min(dat_actual$min[36:40]), 5),
                         rep(min(dat_actual$min[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(min(dat_actual$min[c(c(6:10, 26:35, 46:50,56:60))]), 5),
                         rep(min(dat_actual$min[c(1:5, 11:20, 21:25, 41:45,51:55)]), 5),
                         rep(min(dat_actual$min[c(c(6:10, 26:35, 46:50,56:60))]), 5))


# 2021
p <- ggplot(data=dat_actual[which(dat_actual$Year==2021),], 
            aes(x=Cover, y=meanyield, fill=Cover)) + ggtitle("2021") +
  geom_point(data=dat_actual[which(dat_actual$Year==2021),], aes(y=min_year),alpha = 0) +
  geom_point(data=dat_actual[which(dat_actual$Year==2021),], aes(y=max_year),alpha = 0) +
  geom_hline(aes(yintercept=TN_avg), linetype="dashed", alpha=0.5) +
  #geom_bar(position = "dodge", stat = "summary",fun = "mean")+
  theme_minimal() +
  facet_wrap(.~Cropping.system, scales="free", ncol=1) +
  labs(y= expression(paste("Yield (Mg ha"^-1,")"))) +
  #scale_x_discrete(limits=c(0.5,5.5)) + #ylim(c(0.55,1.55)) + 
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),          
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.3), "cm"),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(data=dat_actual[which(dat_actual$Year==2021),], width=0.2,
                aes(x=Cover,  ymin=meanyield-seyield, ymax=meanyield+seyield)) +
  geom_point(data=dat_actual[which(dat_actual$Year==2021),],
             aes(x=Cover, y=meanyield, fill=Cover), col = "black",size = 2,shape = 21) +
  scale_fill_manual(values=covercols)  
fig1 <- p + 
  # geom_text(inherit.aes = FALSE, x=rep(c(1:5), n*4)+0.2, y=dat_actual$meanyield[which(dat_actual$Year==2021)], 
  #           hjust=0, size=3, data=testlet_df_int[which(testlet_df_int$Year==2021),], aes(label = .group)) +
  geom_text(inherit.aes = FALSE, x=rep(c(1:5), n*4), y=dat_actual$meanyield[which(dat_actual$Year==2021)]+dat_actual$seyield[which(dat_actual$Year==2021)]+0.04*dat_actual$meanyield[which(dat_actual$Year==2021)], 
            hjust=0.5, size=3, data=testlet_df_int[which(testlet_df_int$Year==2021),], aes(label = round(response_bt0,0))) 


# 2022
p <- ggplot(data=dat_actual[which(dat_actual$Year==2022),], 
            aes(x=Cover, y=meanyield, fill=Cover)) + ggtitle("2022") +
  geom_point(data=dat_actual[which(dat_actual$Year==2022),], aes(y=min_year),alpha = 0) +
  geom_point(data=dat_actual[which(dat_actual$Year==2022),], aes(y=max_year),alpha = 0) +
  geom_hline(aes(yintercept=TN_avg), linetype="dashed", alpha=0.5) +
  #geom_bar(position = "dodge", stat = "summary",fun = "mean")+
  theme_minimal() +
  facet_wrap(.~Cropping.system, scales="free", ncol=1) +
  labs(y="") +
  #scale_x_discrete(limits=c(0.5,5.5)) + #ylim(c(0.55,1.55)) + 
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),          
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.3), "cm"),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(data=dat_actual[which(dat_actual$Year==2022),], width=0.2,
                aes(x=Cover,  ymin=meanyield-seyield, ymax=meanyield+seyield)) +
  geom_point(data=dat_actual[which(dat_actual$Year==2022),],
             aes(x=Cover, y=meanyield, fill=Cover), col = "black",size = 2,shape = 21) +
  scale_fill_manual(values=covercols)
fig2 <- p + 
  # geom_text(inherit.aes = FALSE, x=rep(c(1:5), n*4)+0.2, y=dat_actual$meanyield[which(dat_actual$Year==2022)], 
  #           hjust=0, size=3, data=testlet_df_int[which(testlet_df_int$Year==2022),], aes(label = .group)) +
  geom_text(inherit.aes = FALSE, x=rep(c(1:5), n*4), y=dat_actual$meanyield[which(dat_actual$Year==2022)]+dat_actual$seyield[which(dat_actual$Year==2022)]+0.04*dat_actual$meanyield[which(dat_actual$Year==2022)], 
            hjust=0.5, size=3, data=testlet_df_int[which(testlet_df_int$Year==2022),], aes(label = round(response_bt0,0))) 



# 2023
p <- ggplot(data=dat_actual[which(dat_actual$Year==2023),], 
            aes(x=Cover, y=meanyield, fill=Cover)) + ggtitle("2023") +
  geom_point(data=dat_actual[which(dat_actual$Year==2023),], aes(y=min_year),alpha = 0) +
  geom_point(data=dat_actual[which(dat_actual$Year==2023),], aes(y=max_year),alpha = 0) +
  geom_hline(aes(yintercept=TN_avg), linetype="dashed", alpha=0.5) +
  #geom_bar(position = "dodge", stat = "summary",fun = "mean")+
  theme_minimal() +
  facet_wrap(.~Cropping.system, scales="free", ncol=1,strip.position="right") +
  labs(y="") +
  #scale_x_discrete(limits=c(0.5,5.5)) + #ylim(c(0.55,1.55)) + 
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),          
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.3), "cm"),
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(data=dat_actual[which(dat_actual$Year==2023),], width=0.2,
                aes(x=Cover,  ymin=meanyield-seyield, ymax=meanyield+seyield)) +
  geom_point(data=dat_actual[which(dat_actual$Year==2023),],
             aes(x=Cover, y=meanyield, fill=Cover), col = "black",size = 2,shape = 21) +
  scale_fill_manual(values=covercols)
fig3 <- p + 
  # geom_text(inherit.aes = FALSE, x=rep(c(1:5), n*4)+0.2, y=dat_actual$meanyield[which(dat_actual$Year==2023)], 
  #           hjust=0, size=3, data=testlet_df_int[which(testlet_df_int$Year==2023),], aes(label = .group)) +
  geom_text(inherit.aes = FALSE, x=rep(c(1:5), n*4), y=dat_actual$meanyield[which(dat_actual$Year==2023)]+dat_actual$seyield[which(dat_actual$Year==2023)]+0.04*dat_actual$meanyield[which(dat_actual$Year==2023)], 
            hjust=0.5, size=3, data=testlet_df_int[which(testlet_df_int$Year==2023),], aes(label = round(response_bt0,0))) 


# export figure
fig_yield_actual <- ggarrange(fig1, fig2, fig3, ncol=3, widths=c(1,1,1))

ggpubr::ggexport(fig_yield_actual, height=3000, width=3000, filename = "Figures/*across years/Yield_actual.png", res = 400)






# export lsmeans 
write.csv(testlet_df_int, "Model-output/lsmeans/Yield_std_interactive-effect.csv")
write.csv(testlet_df_cropsys, "Model-output/lsmeans/Yield_std_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/Yield_std_cover.csv")
# export anova results 
write.csv(mod.effects, "Model-output/anova/Yield_std.csv")

fig_2











################################################
### main crop foliar nutrients: tissue.N.percent
################################################

hist((dat$tissue.N.percent)) 

dat2 <- dat

# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                      Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.N.percent_lower=c(2.8, 2.8, 2.8, 
                            3.25, 3.25, 3.25,
                            2.8, 3.25, 2.8, 
                            2.8, 3, 3.25)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.N.percent_std <- rep(NA, dim(dat2)[1])
dat2$tissue.N.percent_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.N.percent_std[subset_1] <- dat2$tissue.N.percent[subset_1]/df$tissue.N.percent_lower[i]
  dat2$tissue.N.percent_lower[subset_1] <- rep(df$tissue.N.percent_lower[i], length(dat2$tissue.N.percent_std[subset_1]))
}
hist(dat2$tissue.N.percent_std)

# model
fit <- lme(log(tissue.N.percent_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.N.percent)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.N.percent),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))


# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar N"))
fig_tissue.N.percent_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.N.percent_std, height=1200, width=1700, filename = "Figures/*across years/tissue.N.percent_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar N"))
fig_tissue.N.percent_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.N.percent_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.N.percent_std_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.N.percent_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.N.percent_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.N.percent.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.N.percent_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.N.percent <- testlet_df_cover






################################################
### main crop foliar nutrients: tissue.P.percent
################################################

hist((dat$tissue.P.percent)) 


dat2 <- dat

# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.P.percent_lower=c(0.25, 0.25, 0.25, 
                                          0.3, 0.3, 0.3,
                                          0.25, 0.3, 0.25, 
                                          0.25, 0.2, 0.3)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.P.percent_std <- rep(NA, dim(dat2)[1])
dat2$tissue.P.percent_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.P.percent_std[subset_1] <- dat2$tissue.P.percent[subset_1]/df$tissue.P.percent_lower[i]
  dat2$tissue.P.percent_lower[subset_1] <- rep(df$tissue.P.percent_lower[i], length(dat2$tissue.P.percent_std[subset_1]))
}
hist(dat2$tissue.P.percent_std)

# model
fit <- lme((tissue.P.percent_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.P.percent)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.P.percent),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))


# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar P"))
fig_tissue.P.percent_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.P.percent_std, height=1200, width=1700, filename = "Figures/*across years/tissue.P.percent_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar P"))
fig_tissue.P.percent_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.P.percent_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.P.percent_std_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.P.percent_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.P.percent_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.P.percent.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.P.percent_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.P.percent <- testlet_df_cover













################################################
### main crop foliar nutrients: tissue.K.percent
################################################

hist((dat$tissue.K.percent)) 

dat2 <- dat

# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.K.percent_lower=c(1.8, 1.8, 1.8, 
                                          1.5, 1.5, 1.5,
                                          1.8, 1.5, 1.8, 
                                          1.8, 1.5, 1.5)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.K.percent_std <- rep(NA, dim(dat2)[1])
dat2$tissue.K.percent_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.K.percent_std[subset_1] <- dat2$tissue.K.percent[subset_1]/df$tissue.K.percent_lower[i]
  dat2$tissue.K.percent_lower[subset_1] <- rep(df$tissue.K.percent_lower[i], length(dat2$tissue.K.percent_std[subset_1]))
}
hist(dat2$tissue.K.percent_std)

# model
fit <- lme((tissue.K.percent_std)~Cover*Cropping.system, random=~1|Year/Rep, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.K.percent)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.K.percent),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))


# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar K"))
fig_tissue.K.percent_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.K.percent_std, height=1200, width=1700, filename = "Figures/*across years/tissue.K.percent_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar K"))
fig_tissue.K.percent_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.K.percent_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.K.percent_std_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.K.percent_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.K.percent_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.K.percent.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.K.percent_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.K.percent <- testlet_df_cover









################################################
### main crop foliar nutrients: tissue.Ca.percent
################################################

hist((dat$tissue.Ca.percent)) 

# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.Ca.percent_lower=c(0.25, 0.25, 0.25, 
                                          0.8, 0.8, 0.8,
                                          0.25, 0.8, 0.25, 
                                          0.25, 2, 0.8)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.Ca.percent_std <- rep(NA, dim(dat2)[1])
dat2$tissue.Ca.percent_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.Ca.percent_std[subset_1] <- dat2$tissue.Ca.percent[subset_1]/df$tissue.Ca.percent_lower[i]
  dat2$tissue.Ca.percent_lower[subset_1] <- rep(df$tissue.Ca.percent_lower[i], length(dat2$tissue.Ca.percent_std[subset_1]))
}
hist(dat2$tissue.Ca.percent_std)

# model
fit <- lme((tissue.Ca.percent_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.Ca.percent)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.Ca.percent),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))


# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Ca"))
fig_tissue.Ca.percent_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Ca.percent_std, height=1200, width=1700, filename = "Figures/*across years/tissue.Ca.percent_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Ca"))
fig_tissue.Ca.percent_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Ca.percent_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.Ca.percent_std_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.Ca.percent_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.Ca.percent_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.Ca.percent.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.Ca.percent_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.Ca.percent <- testlet_df_cover









################################################
### main crop foliar nutrients: tissue.Mg.percent
################################################

hist((dat$tissue.Mg.percent)) 

# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.Mg.percent_lower=c(0.15, 0.15, 0.15, 
                                           0.25, 0.25, 0.25,
                                           0.15, 0.25, 0.15, 
                                           0.15, 0.3, 0.25)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.Mg.percent_std <- rep(NA, dim(dat2)[1])
dat2$tissue.Mg.percent_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.Mg.percent_std[subset_1] <- dat2$tissue.Mg.percent[subset_1]/df$tissue.Mg.percent_lower[i]
  dat2$tissue.Mg.percent_lower[subset_1] <- rep(df$tissue.Mg.percent_lower[i], length(dat2$tissue.Mg.percent_std[subset_1]))
}
hist(dat2$tissue.Mg.percent_std)

# model
fit <- lme(log(tissue.Mg.percent_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.Mg.percent)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.Mg.percent),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))


# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Mg"))
fig_tissue.Mg.percent_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Mg.percent_std, height=1200, width=1700, filename = "Figures/*across years/tissue.Mg.percent_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Mg"))
fig_tissue.Mg.percent_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Mg.percent_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.Mg.percent_std_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.Mg.percent_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.Mg.percent_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.Mg.percent.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.Mg.percent_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.Mg.percent <- testlet_df_cover







################################################
### main crop foliar nutrients: tissue.S.percent
################################################

hist((dat$tissue.S.percent)) 


# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.S.percent_lower=c(0.15, 0.15, 0.15, 
                                           0.25, 0.25, 0.25,
                                           0.15, 0.25, 0.15, 
                                           0.15, 0.25, 0.25)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.S.percent_std <- rep(NA, dim(dat2)[1])
dat2$tissue.S.percent_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.S.percent_std[subset_1] <- dat2$tissue.S.percent[subset_1]/df$tissue.S.percent_lower[i]
  dat2$tissue.S.percent_lower[subset_1] <- rep(df$tissue.S.percent_lower[i], length(dat2$tissue.S.percent_std[subset_1]))
}
hist(dat2$tissue.S.percent_std)

# model
fit <- lme((tissue.S.percent_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.S.percent)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.S.percent),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: interaction
test1 <- emmeans(fit, ~Cover, by="Cropping.system")
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc*ncs)}
if(length(unique(testlet$.group[1:ncc]))==1) {testlet$.group[1:ncc] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*1]))==1) {testlet$.group[(1:ncc)+ncc*1] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*2]))==1) {testlet$.group[(1:ncc)+ncc*2] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*3]))==1) {testlet$.group[(1:ncc)+ncc*3] <- rep("",ncc)}
testlet_df_int <- testlet
testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar S"))
fig_tissue.S.percent_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.S.percent_std, height=1200, width=1700, filename = "Figures/*across years/tissue.S.percent_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar S"))
fig_tissue.S.percent_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.S.percent_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.S.percent_std_2.png", res = 400)





# plot
testlet_df_int$minys <- testlet_df_int$response-testlet_df_int$SE
maxy <- max(testlet_df_int$response+testlet_df_int$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_int, aes(x=Cropping.system, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(aes(label=.group_lower, y=covy2*(testlet_df_int$response+testlet_df_int$SE)+testlet_df_int$response+testlet_df_int$SE),
            position = position_dodge(posdodge), size=3)+
  # geom_segment(aes(x = 1-0.3, xend = 1+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 2-0.3, xend = 2+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 3-0.3, xend = 3+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 4-0.3, xend = 4+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 5-0.3, xend = 5+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar S"))
fig_tissue.S.percent_3 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.S.percent_3, height=1200, width=2500, filename = "Figures/*across years/tissue.S.percent_3.png", res = 400)


# export lsmeans
write.csv(testlet_df_int, "Model-output/lsmeans/*across years/tissue.S.percent_interactive-effect.csv")
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.S.percent_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.S.percent_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.S.percent.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.S.percent_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.S.percent <- testlet_df_cover










################################################
### main crop foliar nutrients: tissue.B.ppm
################################################

hist((dat$tissue.B.ppm)) 



# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.B.ppm_lower=c(5, 5, 5, 
                                          20, 20, 20,
                                          5, 20, 5, 
                                          5, 20, 20)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.B.ppm_std <- rep(NA, dim(dat2)[1])
dat2$tissue.B.ppm_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.B.ppm_std[subset_1] <- dat2$tissue.B.ppm[subset_1]/df$tissue.B.ppm_lower[i]
  dat2$tissue.B.ppm_lower[subset_1] <- rep(df$tissue.B.ppm_lower[i], length(dat2$tissue.B.ppm_std[subset_1]))
}
hist(dat2$tissue.B.ppm_std)

# model
fit <- lme((tissue.B.ppm_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.B.ppm)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.B.ppm),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)


### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar B"))
fig_tissue.B.ppm_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.B.ppm_std, height=1200, width=1700, filename = "Figures/*across years/tissue.B.ppm_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar B"))
fig_tissue.B.ppm_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.B.ppm_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.B.ppm_std_2.png", res = 400)




# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.B.ppm_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.B.ppm_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.B.ppm.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.B.ppm_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.B.ppm <- testlet_df_cover









################################################
### main crop foliar nutrients: tissue.Cu.ppm
################################################

hist((dat$tissue.Cu.ppm)) 



# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.Cu.ppm_lower=c(5, 5, 5, 
                                      4, 4, 4,
                                      5, 4, 5, 
                                      5, 5, 4)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.Cu.ppm_std <- rep(NA, dim(dat2)[1])
dat2$tissue.Cu.ppm_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.Cu.ppm_std[subset_1] <- dat2$tissue.Cu.ppm[subset_1]/df$tissue.Cu.ppm_lower[i]
  dat2$tissue.Cu.ppm_lower[subset_1] <- rep(df$tissue.Cu.ppm_lower[i], length(dat2$tissue.Cu.ppm_std[subset_1]))
}
hist(dat2$tissue.Cu.ppm_std)

# model
fit <- lme((tissue.Cu.ppm_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.Cu.ppm)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.Cu.ppm),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))




# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Cu"))
fig_tissue.Cu.ppm_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Cu.ppm_std, height=1200, width=1700, filename = "Figures/*across years/tissue.Cu.ppm_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Cu"))
fig_tissue.Cu.ppm_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Cu.ppm_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.Cu.ppm_std_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.Cu.ppm_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.Cu.ppm_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.Cu.ppm.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.Cu.ppm_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.Cu.ppm <- testlet_df_cover









################################################
### main crop foliar nutrients: tissue.Fe.ppm
################################################

hist((dat$tissue.Fe.ppm)) 
dat$tissue.Fe.ppm[which(dat$tissue.Fe.ppm>300)] <- NA
dat2<-dat


# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.Fe.ppm_lower=c(30, 30, 30, 
                                       25, 25, 25,
                                       30, 25, 30, 
                                       30, 50, 25)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.Fe.ppm_std <- rep(NA, dim(dat2)[1])
dat2$tissue.Fe.ppm_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.Fe.ppm_std[subset_1] <- dat2$tissue.Fe.ppm[subset_1]/df$tissue.Fe.ppm_lower[i]
  dat2$tissue.Fe.ppm_lower[subset_1] <- rep(df$tissue.Fe.ppm_lower[i], length(dat2$tissue.Fe.ppm_std[subset_1]))
}
hist(dat2$tissue.Fe.ppm_std)

# model
fit <- lme(log(tissue.Fe.ppm_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.Fe.ppm)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.Fe.ppm),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: interaction
test1 <- emmeans(fit, ~Cover, by="Cropping.system")
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc*ncs)}
if(length(unique(testlet$.group[1:ncc]))==1) {testlet$.group[1:ncc] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*1]))==1) {testlet$.group[(1:ncc)+ncc*1] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*2]))==1) {testlet$.group[(1:ncc)+ncc*2] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*3]))==1) {testlet$.group[(1:ncc)+ncc*3] <- rep("",ncc)}
testlet_df_int <- testlet
testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))



# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Fe"))
fig_tissue.Fe.ppm_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Fe.ppm_std, height=1200, width=1700, filename = "Figures/*across years/tissue.Fe.ppm_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Fe"))
fig_tissue.Fe.ppm_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Fe.ppm_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.Fe.ppm_std_2.png", res = 400)





# plot
testlet_df_int$minys <- testlet_df_int$response-testlet_df_int$SE
maxy <- max(testlet_df_int$response+testlet_df_int$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_int, aes(x=Cropping.system, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(aes(label=.group_lower, y=covy2*(testlet_df_int$response+testlet_df_int$SE)+testlet_df_int$response+testlet_df_int$SE),
            position = position_dodge(posdodge), size=3)+
  geom_segment(aes(x = 1-0.3, xend = 1+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_segment(aes(x = 2-0.3, xend = 2+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_segment(aes(x = 3-0.3, xend = 3+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_segment(aes(x = 4-0.3, xend = 4+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 5-0.3, xend = 5+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Fe"))
fig_tissue.Fe.ppm_3 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Fe.ppm_3, height=1200, width=2500, filename = "Figures/*across years/tissue.Fe.ppm_3.png", res = 400)


# export lsmeans
write.csv(testlet_df_int, "Model-output/lsmeans/*across years/tissue.Fe.ppm_interactive-effect.csv")
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.Fe.ppm_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.Fe.ppm_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.Fe.ppm.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.Fe.ppm_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.Fe.ppm <- testlet_df_cover








################################################
### main crop foliar nutrients: tissue.Mn.ppm
################################################

hist((dat$tissue.Mn.ppm)) 
dat2<-dat


# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.Mn.ppm_lower=c(15, 15, 15, 
                                       17, 17, 17,
                                       15, 17, 15, 
                                       15, 25, 17)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.Mn.ppm_std <- rep(NA, dim(dat2)[1])
dat2$tissue.Mn.ppm_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.Mn.ppm_std[subset_1] <- dat2$tissue.Mn.ppm[subset_1]/df$tissue.Mn.ppm_lower[i]
  dat2$tissue.Mn.ppm_lower[subset_1] <- rep(df$tissue.Mn.ppm_lower[i], length(dat2$tissue.Mn.ppm_std[subset_1]))
}
hist(dat2$tissue.Mn.ppm_std)

# model
fit <- lme(log(tissue.Mn.ppm_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.Mn.ppm)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.Mn.ppm),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: interaction
test1 <- emmeans(fit, ~Cover, by="Cropping.system")
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc*ncs)}
if(length(unique(testlet$.group[1:ncc]))==1) {testlet$.group[1:ncc] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*1]))==1) {testlet$.group[(1:ncc)+ncc*1] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*2]))==1) {testlet$.group[(1:ncc)+ncc*2] <- rep("",ncc)}
if(length(unique(testlet$.group[(1:ncc)+ncc*3]))==1) {testlet$.group[(1:ncc)+ncc*3] <- rep("",ncc)}
testlet_df_int <- testlet
testlet_df_int <- testlet_df_int %>% mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
testlet_df_int$.group_lower <- tolower(testlet_df_int$.group)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))




# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Mn"))
fig_tissue.Mn.ppm_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Mn.ppm_std, height=1200, width=1700, filename = "Figures/*across years/tissue.Mn.ppm_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Mn"))
fig_tissue.Mn.ppm_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Mn.ppm_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.Mn.ppm_std_2.png", res = 400)





# plot
testlet_df_int$minys <- testlet_df_int$response-testlet_df_int$SE
maxy <- max(testlet_df_int$response+testlet_df_int$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_int, aes(x=Cropping.system, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(aes(label=.group_lower, y=covy2*(testlet_df_int$response+testlet_df_int$SE)+testlet_df_int$response+testlet_df_int$SE),
            position = position_dodge(posdodge), size=3)+
  geom_segment(aes(x = 1-0.3, xend = 1+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_segment(aes(x = 2-0.3, xend = 2+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_segment(aes(x = 3-0.3, xend = 3+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_segment(aes(x = 4-0.3, xend = 4+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  # geom_segment(aes(x = 5-0.3, xend = 5+0.3, y = maxy+covyline*maxy, yend = maxy+covyline*maxy)) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Mn"))
fig_tissue.Mn.ppm_3 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Mn.ppm_3, height=1200, width=2500, filename = "Figures/*across years/tissue.Mn.ppm_3.png", res = 400)


# export lsmeans
write.csv(testlet_df_int, "Model-output/lsmeans/*across years/tissue.Mn.ppm_interactive-effect.csv")
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.Mn.ppm_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.Mn.ppm_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.Mn.ppm.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.Mn.ppm_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.Mn.ppm <- testlet_df_cover






################################################
### main crop foliar nutrients: tissue.Zn.ppm
################################################

hist((dat$tissue.Zn.ppm)) 
dat$tissue.Zn.ppm[which(dat$tissue.Zn.ppm>150)] <- NA

dat2<-dat


# standardize by : 
# data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
                 Year=as.factor(rep(c(2021, 2022, 2023),4)),
                 tissue.Zn.ppm_lower=c(20, 20, 20, 
                                       21, 21, 21,
                                       20, 21, 20, 
                                       20, 20, 21)) 
df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
df$Year
df
dat2$tissue.Zn.ppm_std <- rep(NA, dim(dat2)[1])
dat2$tissue.Zn.ppm_lower <- rep(NA, dim(dat2)[1])


for(i in 1:dim(df)[1]){
  subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
  dat2$tissue.Zn.ppm_std[subset_1] <- dat2$tissue.Zn.ppm[subset_1]/df$tissue.Zn.ppm_lower[i]
  dat2$tissue.Zn.ppm_lower[subset_1] <- rep(df$tissue.Zn.ppm_lower[i], length(dat2$tissue.Zn.ppm_std[subset_1]))
}
hist(dat2$tissue.Zn.ppm_std)

# model
fit <- lme(log(tissue.Zn.ppm_std)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.Zn.ppm)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.Zn.ppm),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))





# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Zn"))
fig_tissue.Zn.ppm_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Zn.ppm_std, height=1200, width=1700, filename = "Figures/*across years/tissue.Zn.ppm_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Standardized foliar Zn"))
fig_tissue.Zn.ppm_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.Zn.ppm_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.Zn.ppm_std_2.png", res = 400)



# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.Zn.ppm_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.Zn.ppm_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.Zn.ppm.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.Zn.ppm_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.Zn.ppm <- testlet_df_cover







################################################
### main crop foliar nutrients: tissue.Na.ppm
################################################

hist((dat$tissue.Na.ppm)) 
dat$tissue.Na.ppm[which(dat$tissue.Na.ppm>500)] <- NA

dat2<-dat


# # standardize by : 
# # data from W976 Adotey et al. extension publication: Visual Symptoms: A Handy Tool in Identifying Nutrient Deficiency in Corn, Cotton and Soybean
# # no data for sodium in this paper 
# df <- data.frame(Cropping.system=as.factor(rep(levels(dat$Cropping.system), each=3)),
#                  Year=as.factor(rep(c(2021, 2022, 2023),4)),
#                  tissue.Na.ppm_lower=c(20, 20, 20, 
#                                        21, 21, 21,
#                                        20, 21, 20, 
#                                        20, 20, 21)) 
# df$Cropping.system <- factor(df$Cropping.system, levels(df$Cropping.system)[c(1,4,3,2)])
# df$Year
# df
# dat2$tissue.Na.ppm_std <- rep(NA, dim(dat2)[1])
# dat2$tissue.Na.ppm_lower <- rep(NA, dim(dat2)[1])
# 
# 
# for(i in 1:dim(df)[1]){
#   subset_1 <- which(dat2$Cropping.system==df$Cropping.system[i] & dat2$Year %in% c(df$Year[i]) & dat2$Season=="Summer") 
#   dat2$tissue.Na.ppm_std[subset_1] <- dat2$tissue.Na.ppm[subset_1]/df$tissue.Na.ppm_lower[i]
#   dat2$tissue.Na.ppm_lower[subset_1] <- rep(df$tissue.Na.ppm_lower[i], length(dat2$tissue.Na.ppm_std[subset_1]))
# }
# hist(dat2$tissue.Na.ppm_std)

# model
fit <- lme((tissue.Na.ppm)~Cover*Cropping.system, random=~1|Year/Rep/Cropping.system, dat2, na.action="na.omit")
ncc <- 5 # number of cover crop treatments
ncs <- 4 # number of cropping system treatments

# check for normality: if <0.05, transform the data
resid <- residuals(fit)
shapiro.test(resid)

# check variances: if Var is >5, use a different model that allows variances to differ across groups
resid2 = cbind(dat2[which(is.na(dat2$tissue.Na.ppm)==FALSE),],resid)
temp = resid2%>% group_by(Cropping.system,Cover)%>% summarise_at(vars(tissue.Na.ppm),funs(mean=mean(., na.rm=T),Std=sd(.)))
max(temp$Std)/min(temp$Std) # check that max/min variation across groups is less than five-fold

# export model stats
mod.out <- as.data.frame(car::Anova(fit)) # default SS for Anova is type-III
mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)

### tukey: Cropping.system
test1 <- emmeans(fit, ~Cropping.system)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cropping.system),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncs)}
testlet_df_cropsys <- testlet
testlet_df_cropsys <- testlet_df_cropsys %>% mutate_each_(funs(factor(.)),c("Cropping.system"))

### tukey: Cover
test1 <- emmeans(fit, ~Cover)
testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
testlet <- testlet[order(testlet$Cover),]
testlet$.group <- gsub(" ", "", testlet$.group)
if(length(unique(testlet$.group))==1) {testlet$.group <- rep("",ncc)}
testlet_df_cover <- testlet
testlet_df_cover <- testlet_df_cover %>% mutate_each_(funs(factor(.)),c("Cover"))





# plot
testlet_df_cover$minys <- testlet_df_cover$response-testlet_df_cover$SE
#testlet_df_cover$minys[which(testlet_df_cover$minys<0)] <- 0
maxy <- max(testlet_df_cover$response+testlet_df_cover$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cover, aes(x=Cover, y=response, fill=Cover)) +
  theme_minimal() + labs(fill="Cover") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = covercols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cover, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncc), x=(1:ncc)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Foliar Na (ppm)"))
fig_tissue.na.ppm_std <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.na.ppm_std, height=1200, width=1700, filename = "Figures/*across years/tissue.na.ppm_std_.png", res = 400)



# plot
testlet_df_cropsys$minys <- testlet_df_cropsys$response-testlet_df_cropsys$SE
#testlet_df_cropsys$minys[which(testlet_df_cropsys$minys<0)] <- 0
maxy <- max(testlet_df_cropsys$response+testlet_df_cropsys$SE)
posdodge <- 0.7
maxylim <- 0.2
covy <- 0.15
covy2 <- 0.07
covyline <- 0.1
p <- ggplot(data=testlet_df_cropsys, aes(x=Cropping.system, y=response, fill=Cropping.system)) +
  theme_minimal() + labs(fill="Cropping system") +
  ylab("") +   
  geom_bar(stat = "identity", position = position_dodge(), color="black", width=0.7) + 
  geom_errorbar(aes(ymin = minys, ymax = response+SE), 
                width=0.2, position=position_dodge(posdodge)) +
  scale_fill_manual(values = cropsyscols) +
  ylim(c(0,maxy+maxylim*maxy)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.ticks.y=element_line(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.minor = element_blank(), panel.grid.major.x=element_blank(),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.4), "cm"), axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_text(inherit.aes=FALSE, data= testlet_df_cropsys, 
            aes(label=.group, y=rep(maxy+covy*maxy, ncs), x=(1:ncs)))  
p

# The two lines we want on the plot
line_1 <- expression(paste("Foliar Na (ppm)"))
fig_tissue.na.ppm_std_2 <- cowplot::ggdraw(p) +
  cowplot::draw_label(line_1, x = cline, y = 0.5, angle=90, size = 10) # use relative coordinates for positioning
# export figure
ggpubr::ggexport(fig_tissue.na.ppm_std_2, height=1200, width=1200, filename = "Figures/*across years/tissue.na.ppm_std_2.png", res = 400)


# export lsmeans
write.csv(testlet_df_cropsys, "Model-output/lsmeans/*across years/tissue.Na.ppm_cropsys.csv")
write.csv(testlet_df_cover, "Model-output/lsmeans/*across years/tissue.Na.ppm_cover.csv")
# export anova results
write.csv(mod.out2, "Model-output/anova/*across years/tissue.Na.ppm.csv")
write.csv(round(r.squaredGLMM(fit),2), "Model-output/anova/*across years/tissue.Na.ppm_rsq.csv") # marginal: explained by fixed effects; conditional: explained by entire model

testlet_df_cover_tissue.Na.ppm <- testlet_df_cover






# plot all cash crop performance figures together: main effect of cover crop

fig_cash1 <- ggarrange(fig_tissue.N.percent_std, fig_tissue.P.percent_std, fig_tissue.K.percent_std,
                       fig_tissue.Ca.percent_std, fig_tissue.Mg.percent_std, fig_tissue.S.percent_std,
                       fig_tissue.B.ppm_std, fig_tissue.Fe.ppm_std, fig_tissue.Mn.ppm_std, 
                       fig_tissue.Cu.ppm_std, fig_tissue.Zn.ppm_std, fig_Yield_std,
                       ncol=3, nrow=4, labels="AUTO")
ggpubr::ggexport(fig_cash1, height=3500, width=5000, filename = "Figures/*across years/4_cash_overall.png", res = 400)


# plot all cash crop performance figures together: main effect of cropping system

fig_cash1 <- ggarrange(fig_tissue.N.percent_std_2, fig_tissue.P.percent_std_2, fig_tissue.K.percent_std_2,
                       fig_tissue.Ca.percent_std_2, fig_tissue.Mg.percent_std_2, fig_tissue.S.percent_std_2,
                       fig_tissue.B.ppm_std_2, fig_tissue.Fe.ppm_std_2, fig_tissue.Mn.ppm_std_2, 
                       ncol=3, nrow=3, labels="AUTO")
df <- data.frame(x=1, y=1)
fig_NA <- ggplot(df,aes(x,y))+geom_blank()+theme_void()
fig_cash2 <- ggarrange(fig_cash1, fig_NA,
                       ncol=2, nrow=1, widths=c(1, 0.22))
fig_cash3 <- ggarrange(fig_tissue.Cu.ppm_std_2, fig_tissue.Zn.ppm_std_2, fig_Yield_std_2,
                       ncol=3, nrow=1, labels=c("J", "K", "L"), widths=c(0.6, 0.6, 1))

fig_cash4 <- ggarrange(fig_cash2, fig_cash3,
                       ncol=1, nrow=2, heights=c(1, 0.34))


ggpubr::ggexport(fig_cash4, height=3500, width=5000, filename = "Figures/*across years/4_cash_overall_2.png", res = 400)





# plot all cash crop performance figures together: interactions

fig_cash1 <- ggarrange(fig_tissue.S.percent_3, 
                       fig_tissue.Fe.ppm_3, 
                       fig_tissue.Mn.ppm_3,
                       ncol=1, nrow=3, labels="AUTO")
ggpubr::ggexport(fig_cash1, height=3500, width=2700, filename = "Figures/*across years/4_cash_overall_3.png", res = 400)




################################################




################################################
### all done
################################################
################################################



