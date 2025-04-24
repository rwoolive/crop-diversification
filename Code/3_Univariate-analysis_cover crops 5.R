
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

# condense data across seasons
dat0 <- dat[which(dat$Season=="Fall"),]
dat0$Weeds <- dat$Weeds[which(dat$Season=="Spring")]
dat0$Wheat <- dat$Wheat[which(dat$Season=="Spring")]
dat0$Clover <- dat$Clover[which(dat$Season=="Spring")]
dat0$Radish <- dat$Radish[which(dat$Season=="Spring")]
dat0$Vetch <- dat$Vetch[which(dat$Season=="Spring")]
dat0$Grass <- dat$Grass[which(dat$Season=="Spring")]
dat0$inputC <- dat$inputC[which(dat$Season=="Spring")]
dat0$inputN <- dat$inputN[which(dat$Season=="Spring")]
dat0$inputCN <- dat$inputCN[which(dat$Season=="Spring")]

dat0$Yield <- dat$Yield[which(dat$Season=="Summer")]
dat0$Yield.crop <- dat$Yield.crop[which(dat$Season=="Summer")]
dat0$tissue.N.percent <- dat$tissue.N.percent[which(dat$Season=="Summer")]
dat0$tissue.P.percent <- dat$tissue.P.percent[which(dat$Season=="Summer")]
dat0$tissue.K.percent <- dat$tissue.K.percent[which(dat$Season=="Summer")]
dat0$tissue.Ca.percent <- dat$tissue.Ca.percent[which(dat$Season=="Summer")]
dat0$tissue.Mg.percent <- dat$tissue.Mg.percent[which(dat$Season=="Summer")]
dat0$tissue.S.percent <- dat$tissue.S.percent[which(dat$Season=="Summer")]
dat0$tissue.B.ppm <- dat$tissue.B.ppm[which(dat$Season=="Summer")]
dat0$tissue.Cu.ppm <- dat$tissue.Cu.ppm[which(dat$Season=="Summer")]
dat0$tissue.Fe.ppm <- dat$tissue.Fe.ppm[which(dat$Season=="Summer")]
dat0$tissue.Mn.ppm <- dat$tissue.Mn.ppm[which(dat$Season=="Summer")]
dat0$tissue.Zn.ppm <- dat$tissue.Zn.ppm[which(dat$Season=="Summer")]
dat0$tissue.Na.ppm <- dat$tissue.Na.ppm[which(dat$Season=="Summer")]

hist(dat0$tissue.N.percent)
hist(dat0$tissue.P.percent)
hist(dat0$tissue.K.percent)
hist(dat0$tissue.Ca.percent)
hist(dat0$tissue.Mg.percent)
hist(dat0$tissue.S.percent)
hist(dat0$tissue.B.ppm)
hist(dat0$tissue.Cu.ppm)
hist(dat0$tissue.Fe.ppm)
dat0$tissue.Fe.ppm[which(dat0$tissue.Fe.ppm>300)] <- NA
hist(dat0$tissue.Mn.ppm)
hist(dat0$tissue.Zn.ppm)
dat0$tissue.Zn.ppm[which(dat0$tissue.Zn.ppm>150)] <- NA
hist(dat0$tissue.Na.ppm)

# dat0 %>%
#   dplyr::group_by(Season) %>%   
#   dplyr::summarise(Average_Yield = mean(Yield, na.rm = TRUE), .groups = "drop")





##################### MODELING APPROACH #####################
### We have a split-plot block design, 
### with 4 main plot levels (cropping system)
### and 5 subplot levels (cover)  
### with each treatment combo represented within each of 4 blocks (Replicate)
### and sampled at multiple timepoints (Year or Season.Year)
### There are four predictor variables:
### 1. Replicate (4 levels), random effect
### 2. Cropping system (4 levels), main plot
### 3. Cover (5 levels), subplot
### 4. Year or Season.Year, random effect

# We are interested in the main effects of cover and cropping system
# across time on the following responses: 
dat2 <- dat0


### agb
dat2$agb <- dat2$Wheat+dat2$Clover+dat2$Radish+dat2$Vetch+dat2$Grass#+dat2$Weeds
dat2$agb[which(dat2$Cover=="No cover")] <- NA
dat2$agb0 <- dat2$agb
# convert from g m-2 to Mg ha-1
# g / m2 * 10000 m2 / ha * Mg / 1000000 g
dat2$agb <- dat2$agb* (10000/1000000) 
hist((dat2$agb)) # this is in units of t/ha

### weeds
dat2$weeds <- dat2$Weeds
# convert from g m-2 to Mg ha-1
# g / m2 * 10000 m2 / ha * Mg / 1000000 g
dat2$weeds <- dat2$weeds* (10000/1000000) 
hist((dat2$weeds)) # this is in units of Mg/ha

### inputC
hist(dat2$inputC) # this in units of g m-2
dat2$inputC0 <- dat2$inputC
dat2$inputC <- dat2$inputC*0.01 # convert from g m-2 to t ha-1 (10,000 m2 in a hectare; 1,000,000 grams in a Mg)
hist((dat2$inputC)) # this is in units of Mg ha-2
dat2$inputC[which(dat2$Cover=="No cover")]
#dat2$inputC[which(dat2$Season %in% c("Summer", "Fall"))] <- NA
hist((dat2$inputC)) # this is in units of Mg/ha

### inputN
hist(dat2$inputN) # this in units of g m-2
dat2$inputN0 <- dat2$inputN
dat2$inputN <- dat2$inputN*10 # convert from g m-2 to t ha-1 (10,000 m2 in a hectare; 1,000 grams in a kg)
hist((dat2$inputN)) # this is in units of kg ha-2
dat2$inputN[which(dat2$Cover=="No cover")]
#dat2$inputN[which(dat2$Season %in% c("Summer", "Fall"))] <- NA
hist((dat2$inputN)) # this is in units of kg/ha

### inputCN
dat2$inputCN <- dat2$inputC/(dat2$inputN/1000) # this is ratio of c to n
hist((dat2$inputCN)) 
dat2$inputCN[which(dat2$Cover=="No cover")]
hist((dat2$inputCN)) # this is unitless

### inorgN
dat2$inorgN0 <- dat2$inorgN
hist((dat2$inorgN)) # this is in units of mg/kg dry soil in 0-10 cm

### GMC
hist((dat2$GMC)) # this is in units of g/g dry soil

### BG
hist((dat2$BG)) # this is in units of nmol g-1 -h

### NAG
hist((dat2$NAG)) # this is in units of nmol g-1 -h

### PHOS
hist((dat2$PHOS)) # this is in units of nmol g-1 -h

### WEC
hist((dat2$WEC)) # this is in units of mg/kg dry soil

### WEN
hist((dat2$WEN)) # this is in units of mg/kg dry soil

### MBC
hist((dat2$MBC)) # this is in units of mg/kg dry soil

### orgC
hist((dat2$orgC)) # this is in units of g/kg dry soil

### totN
hist((dat2$totN)) # this is in units of g/kg dry soil

### POC
hist((dat2$POC)) # this is in units of g/kg dry soil

### MAOC
hist((dat2$MAOC)) # this is in units of g/kg dry soil

### EEA_C (activity of c-acquiring enzymes)
dat2$EEA_C <- dat2$AG + dat2$BG + dat2$CB + dat2$XYL
hist((dat2$EEA_C)) # cumulative measure of four enzymes

### EEA_C (activity of n-acquiring enzymes)
dat2$EEA_N <- dat2$NAG + dat2$LAP
hist((dat2$EEA_N)) # cumulative measure of two enzymes

### WAS
hist((dat2$WAS)) # this is in units of %


###  corn_yield, soybean_yield, and cotton_yield 
`%notin%` <- Negate(`%in%`)
# convert from bu/ac (corn/soy) or lb/ac (cotton) to Mg/ha
dat2$corn_yield <- dat2$Yield * 62.77/1000 # 1 bushel per acre = 62.77 (63) kilograms per hectare
dat2$corn_yield[which(dat2$Yield.crop %notin% c("Corn", "corn"))] <- NA
hist(dat2$corn_yield)
dat2 %>%
  dplyr::group_by(Year, Cropping.system) %>%   
  dplyr::summarise(Average_Yield = mean(corn_yield, na.rm = TRUE), .groups = "drop")

dat2$soybean_yield <- dat2$Yield * 67.25/1000 # 1 bushel per acre = 67.25 (67) kilograms per hectare
dat2$soybean_yield[which(dat2$Yield.crop %notin% c("Soybean", "soybean"))] <- NA
hist(dat2$soybean_yield)
dat2 %>%
  dplyr::group_by(Year, Cropping.system) %>%   # Group by Year and Treatment
  dplyr::summarise(Average_Yield = mean(soybean_yield, na.rm = TRUE), .groups = "drop")  # Calculate mean

dat2$cotton_yield <- dat2$Yield * 1.1209/1000 # 1 lb per acre =  1.1209 Kilograms Per Hectare
dat2$cotton_yield[which(dat2$Yield.crop %notin% c("Cotton", "cotton"))] <- NA
hist(dat2$cotton_yield)
dat2 %>%
  dplyr::group_by(Year, Cropping.system) %>%   # Group by Year and Treatment
  dplyr::summarise(Average_Yield = mean(cotton_yield, na.rm = TRUE), .groups = "drop")  # Calculate mean

# create a column of yield data scaled within each crop
dat2$Yield.crop[which(dat2$Yield.crop=="corn")] <- "Corn"
dat2$Yield.crop[which(dat2$Yield.crop=="soybean")] <- "Soybean"
unique(dat2$Yield.crop)
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(Yield_scaled = (Yield - min(Yield, na.rm=T)) / (max(Yield, na.rm=T) - min(Yield, na.rm=T)),
                Yield_min = min(Yield, na.rm=T),
                Yield_max = max(Yield, na.rm=T))  
dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::summarise(Average = mean(Yield_scaled, na.rm = TRUE), .groups = "drop")  # Calculate mean
dat3 <- dat2 %>%
  dplyr::group_by(Cover) %>%   # Group by Year and Treatment
  dplyr::summarise(Average = mean(Yield_scaled, na.rm = TRUE), .groups = "drop")  # Calculate mean



# make new columns for yield of each crop in each year:
dat2$corn_yield_2021 <- dat2$corn_yield
dat2$corn_yield_2021[which(dat2$Year %notin% c("2021"))] <- NA
dat2 %>% dplyr::group_by(Year, Cropping.system) %>% dplyr::summarise(Average_Yield = mean(corn_yield_2021, na.rm = TRUE), .groups = "drop")  # Calculate mean
dat2$corn_yield_2022 <- dat2$corn_yield
dat2$corn_yield_2022[which(dat2$Year %notin% c("2022"))] <- NA
dat2 %>% dplyr::group_by(Year, Cropping.system) %>%  dplyr::summarise(Average_Yield = mean(corn_yield_2022, na.rm = TRUE), .groups = "drop")  # Calculate mean
dat2$corn_yield_2023 <- dat2$corn_yield
dat2$corn_yield_2023[which(dat2$Year %notin% c("2023"))] <- NA
dat2 %>% dplyr::group_by(Year, Cropping.system) %>% dplyr::summarise(Average_Yield = mean(corn_yield_2023, na.rm = TRUE), .groups = "drop")  # Calculate mean
dat2$cotton_yield_2022 <- dat2$cotton_yield
dat2$cotton_yield_2022[which(dat2$Year %notin% c("2022"))] <- NA
dat2 %>% dplyr::group_by(Year, Cropping.system) %>%   dplyr::summarise(Average_Yield = mean(cotton_yield_2022, na.rm = TRUE), .groups = "drop")  # Calculate mean
dat2$soybean_yield_2021 <- dat2$soybean_yield
dat2$soybean_yield_2021[which(dat2$Year %notin% c("2021"))] <- NA
dat2 %>% dplyr::group_by(Year, Cropping.system) %>% dplyr::summarise(Average_Yield = mean(soybean_yield_2021, na.rm = TRUE), .groups = "drop")  # Calculate mean
dat2$soybean_yield_2022 <- dat2$soybean_yield
dat2$soybean_yield_2022[which(dat2$Year %notin% c("2022"))] <- NA
dat2 %>% dplyr::group_by(Year, Cropping.system) %>%  dplyr::summarise(Average_Yield = mean(soybean_yield_2022, na.rm = TRUE), .groups = "drop")  # Calculate mean
dat2$soybean_yield_2023 <- dat2$soybean_yield
dat2$soybean_yield_2023[which(dat2$Year %notin% c("2023"))] <- NA
dat2 %>% dplyr::group_by(Year, Cropping.system) %>% dplyr::summarise(Average_Yield = mean(soybean_yield_2023, na.rm = TRUE), .groups = "drop")  # Calculate mean






### multifunctionality index
# first, calculate an index for soil properties
soil_properties <- c(#"agb", "weeds", "inputC", "inputN", "inputCN", 
               "inorgN", "GMC",  "EEA_C", "EEA_N", "PHOS", "WEC", "WEN", "MBC",#"BG", "NAG",
               "orgC", "totN", "POC", "MAOC",  "WAS")
# scale everything between 0 and 1
dat_scaled <- as.data.frame(apply(dat2[, soil_properties], 2, function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))))
# add to dataframe
threshold <- 0.5  # Example: 50th percentile
dat2$multifunctionality_index_soil <- rowMeans(dat_scaled > threshold, na.rm = TRUE)
dat2$multifunctionality_index_soil2 <- rowMeans(dat_scaled, na.rm = TRUE)

# second, calculate an index for agronomic properties:
ag_properties <- c("agb", "weeds", "inputC", "inputN", "inputCN",
                     "Yield_scaled"
                     #"inorgN", "GMC", "BG", "NAG", "PHOS", "WEC", "WEN", "MBC"#,
                     #"orgC", "totN", "POC", "MAOC", "EEA_C", "EEA_N", "WAS"
                     )
# scale everything between 0 and 1
dat_scaled <- as.data.frame(apply(dat2[, ag_properties], 2, function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))))
# reverse weeds and C:N
dat_scaled$weeds <- 1-dat_scaled$weeds
dat_scaled$inputCN <- 1-dat_scaled$inputCN
# add to dataframe
threshold <- 0.5  # Example: 50th percentile
dat2$multifunctionality_index_agronomy <- rowMeans(dat_scaled > threshold, na.rm = TRUE)
dat2$multifunctionality_index_agronomy2 <- rowMeans(dat_scaled, na.rm = TRUE)


# second, calculate an index for yield properties only:
yield_properties <- c(#"agb", "weeds", "inputC", "inputN", "inputCN",
                     "corn_yield_2021", "soybean_yield_2021",
                     "corn_yield_2022", "cotton_yield_2022", "soybean_yield_2022",
                     "corn_yield_2023", "soybean_yield_2023"
                     #"inorgN", "GMC", "BG", "NAG", "PHOS", "WEC", "WEN", "MBC"#,
                     #"orgC", "totN", "POC", "MAOC", "EEA_C", "EEA_N", "WAS"
)
# scale everything between 0 and 1
dat_scaled <- as.data.frame(apply(dat2[, yield_properties], 2, function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))))
# add to dataframe
threshold <- 0.5  # Example: 50th percentile
dat2$multifunctionality_index_yield<- rowMeans(dat_scaled > threshold, na.rm = TRUE)
dat2$multifunctionality_index_yield2 <- rowMeans(dat_scaled, na.rm = TRUE)


foliarcols <- colnames(dat2)[grep("tissue", colnames(dat2))]
foliarcols_scaled <- paste0(foliarcols, "_scaled")
# create a columns of foliar nutrient data scaled within each crop
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.N.percent_scaled = (tissue.N.percent - min(tissue.N.percent, na.rm=T)) / (max(tissue.N.percent, na.rm=T) - min(tissue.N.percent, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.P.percent_scaled = (tissue.P.percent - min(tissue.P.percent, na.rm=T)) / (max(tissue.P.percent, na.rm=T) - min(tissue.P.percent, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.K.percent_scaled = (tissue.K.percent - min(tissue.K.percent, na.rm=T)) / (max(tissue.K.percent, na.rm=T) - min(tissue.K.percent, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.Ca.percent_scaled = (tissue.Ca.percent - min(tissue.Ca.percent, na.rm=T)) / (max(tissue.Ca.percent, na.rm=T) - min(tissue.Ca.percent, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.Mg.percent_scaled = (tissue.Mg.percent - min(tissue.Mg.percent, na.rm=T)) / (max(tissue.Mg.percent, na.rm=T) - min(tissue.Mg.percent, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.S.percent_scaled = (tissue.S.percent - min(tissue.S.percent, na.rm=T)) / (max(tissue.S.percent, na.rm=T) - min(tissue.S.percent, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.B.ppm_scaled = (tissue.B.ppm - min(tissue.B.ppm, na.rm=T)) / (max(tissue.B.ppm, na.rm=T) - min(tissue.B.ppm, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.Cu.ppm_scaled = (tissue.Cu.ppm - min(tissue.Cu.ppm, na.rm=T)) / (max(tissue.Cu.ppm, na.rm=T) - min(tissue.Cu.ppm, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.Fe.ppm_scaled = (tissue.Fe.ppm - min(tissue.Fe.ppm, na.rm=T)) / (max(tissue.Fe.ppm, na.rm=T) - min(tissue.Fe.ppm, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.Mn.ppm_scaled = (tissue.Mn.ppm - min(tissue.Mn.ppm, na.rm=T)) / (max(tissue.Mn.ppm, na.rm=T) - min(tissue.Mn.ppm, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.Zn.ppm_scaled = (tissue.Zn.ppm - min(tissue.Zn.ppm, na.rm=T)) / (max(tissue.Zn.ppm, na.rm=T) - min(tissue.Zn.ppm, na.rm=T)))  
dat2 <- dat2 %>%
  dplyr::group_by(Yield.crop) %>%   # Group by Year and Treatment
  dplyr::mutate(tissue.Na.ppm_scaled = (tissue.Na.ppm - min(tissue.Na.ppm, na.rm=T)) / (max(tissue.Na.ppm, na.rm=T) - min(tissue.Na.ppm, na.rm=T)))  

# third, calculate an index for cash crop nutrition:
ag_properties <- c(foliarcols_scaled)
# scale everything between 0 and 1
dat_scaled <- as.data.frame(apply(dat2[, ag_properties], 2, function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))))
# add to dataframe
threshold <- 0.5  # Example: 50th percentile
dat2$multifunctionality_index_nutrition <- rowMeans(dat_scaled > threshold, na.rm = TRUE)
dat2$multifunctionality_index_nutrition2 <- rowMeans(dat_scaled, na.rm = TRUE)


# ### test correlation structures
# 
# # Make sure Year is numeric
# dat2$YearN <- as.numeric(dat2$Year)
# 
# 
# # Make data frame with responses and repeated measures (time)
# responses <- c("agb", "weeds", "inputC", "inputN", "inputCN", "Yield_scaled",
#                "multifunctionality_index_agronomy", "multifunctionality_index_agronomy2",
#                "multifunctionality_index_yield", "multifunctionality_index_yield2",
#                "inorgN", "GMC",  "EEA_C", "EEA_N", "PHOS", "WEC", "WEN", "MBC", # "BG", "NAG",
#                "orgC", "totN", "POC", "MAOC", "WAS",
#                "multifunctionality_index_soil",  "multifunctionality_index_soil2")
# mod_comp <- data.frame(resp = responses,
#                    AIC_fit = rep(NA, length(responses)),
#                    AIC_fit_corAR1 = rep(NA, length(responses)),
#                    AIC_fit_corCompSymm = rep(NA, length(responses)),
#                    bestmod = rep(NA, length(responses)))
# mod_comp
# 
# dat2$Plot <- as.factor(dat2$Plot)
# 
# # models
# # Loop through responses and fit models
# for (i in 1:nrow(mod_comp)) { # nrow(resp)
#   response_var <- mod_comp$resp[i]  # Current response variable
#   time_var <- "YearN"    # Current time variable
#   if(response_var != "multifunctionality_index_soil2")
# {  fit <- lme(
#     fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
#     random = ~ 1 | Rep/Cropping.system,
#     #correlation = corAR1(form =as.formula(paste("~", time_var, "| Rep/Cropping.system/Cover"))),
#     weights = varIdent(form = ~ 1 | Cover),
#     data = dat2,
#     na.action = na.omit)
#   fit_corAR1 <- lme(
#     fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
#     random = ~ 1 | Rep/Cropping.system/Cover,
#     correlation = corAR1(form =as.formula(paste("~", time_var, "| Rep/Cropping.system/Cover"))),
#     weights = varIdent(form = ~ 1 | Cover),
#     data = dat2,
#     na.action = na.omit)
#   fit_corCompSymm <- lme(
#     fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
#     random = ~ 1 | Rep/Cropping.system/Cover,
#     correlation = corCompSymm(form =as.formula(paste("~", time_var, "| Rep/Cropping.system/Cover"))),
#     weights = varIdent(form = ~ 1 | Cover),
#     data = dat2,
#     na.action = na.omit)
#   comparison <- AIC(fit, fit_corAR1, fit_corCompSymm)
#   mod_comp$AIC_fit[i] <- comparison$AIC[1]
#   mod_comp$AIC_fit_corAR1[i] <- comparison$AIC[2]
#   mod_comp$AIC_fit_corCompSymm[i] <- comparison$AIC[3]
# 
#   mod_comp$bestmod[i] <- rownames(comparison)[which(comparison$AIC==min(comparison$AIC))]
#   }
# 
#   if(response_var == "multifunctionality_index_soil2")
#   {  fit <- lme(
#     fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
#     random = ~ 1 | Rep/Cropping.system,
#     #correlation = corAR1(form =as.formula(paste("~", time_var, "| Rep/Cropping.system/Cover"))),
#     weights = varIdent(form = ~ 1 | Cover),
#     data = dat2,
#     na.action = na.omit)
#   fit_corAR1 <- lme(
#     fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
#     random = ~ 1 | Rep/Cropping.system/Cover,
#     correlation = corAR1(form =as.formula(paste("~", time_var, "| Rep/Cropping.system/Cover"))),
#     weights = varIdent(form = ~ 1 | Cover),
#     data = dat2,
#     na.action = na.omit)
#   comparison <- AIC(fit, fit_corAR1)
#   mod_comp$AIC_fit[i] <- comparison$AIC[1]
#   mod_comp$AIC_fit_corAR1[i] <- comparison$AIC[2]
#   mod_comp$AIC_fit_corCompSymm[i] <- NA
# 
#   mod_comp$bestmod[i] <- rownames(comparison)[which(comparison$AIC==min(comparison$AIC))]
#   }
# 
# }
# mod_comp
# write.csv(mod_comp, paste0("Model-output/anova/_model-comparison.csv"))





# # Our model for each response variable will look like this:
# # with time as the repeated measure
# fit <- lme(# Fixed effects
#             resp ~ Cover * Cropping.system * factor(time),         
#             # Random intercepts for Cover nested within Cropping.system within Rep  
#             random = ~ 1 | Rep/Cropping.system/Cover,  
#             # Temporal correlation for repeated measures each sampling point
#            correlation = corCompSymm(form = ~ time | Rep/Cropping.system/Cover),  
#            # Allow unequal variances across levels of Cover
#            weights = varIdent(form = ~ 1 | Cover),   
#            # Dataset
#            data = dat2,   
#            # Handle missing values
#            na.action = na.omit                          
# )



################################################

mod_comp <- read.csv(paste0("Model-output/anova/_model-comparison.csv"))
resp_compsym <- mod_comp$resp[which(mod_comp$bestmod=="fit_corCompSymm")] # responses that require correlation structure
resp_normal <- mod_comp$resp[which(mod_comp$bestmod=="fit")] # responses that do not require correlation structure


# Make sure Year is numeric
dat2$YearN <- as.numeric(dat2$Year)


# Make data frame with responses and repeated measures (time)
responses <- c("agb", "weeds", "inputC", "inputN", "inputCN", "Yield_scaled",
               #"multifunctionality_index_agronomy", 
               "multifunctionality_index_agronomy2",
               #"multifunctionality_index_yield", 
               #"multifunctionality_index_yield2",
               "inorgN", "GMC", "BG", "NAG", "PHOS", "WEC", "WEN", "MBC",
               "orgC", "totN", "POC", "MAOC", "EEA_C", "EEA_N", "WAS",  
               #"multifunctionality_index_soil", 
               "multifunctionality_index_soil2",
               foliarcols_scaled,
               "multifunctionality_index_nutrition2")

resp <- data.frame(resp = responses,
                   normality = rep(NA, length(responses)),
                   cover = rep(NA, length(responses)),
                   cropsys = rep(NA, length(responses)),
                   time = rep(NA, length(responses)),
                   cover.cropsys = rep(NA, length(responses)),
                   cover.time = rep(NA, length(responses)),
                   cropsys.time = rep(NA, length(responses)),
                   cover.cropsys.time = rep(NA, length(responses)),
                   rsq = rep(NA, length(responses)))
resp
 
dat2$Plot <- as.factor(dat2$Plot)

# model
# Loop through responses and fit models
for (i in 1:nrow(resp)) { # nrow(resp)
  response_var <- resp$resp[i]  # Current response variable
  time_var <- "YearN"    # Current time variable
  
  # Fit the model
  if(response_var %in% c(resp_normal))
      {  fit <- lme(
          fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
          random = ~ 1 | Rep/Cropping.system,
          weights = varIdent(form = ~ 1 | Cover),
          data = dat2,
          na.action = na.omit)
        }
  if(response_var %in% c(foliarcols_scaled, "multifunctionality_index_nutrition2"))
  {  fit <- lme(
    fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
    random = ~ 1 | Rep/Cropping.system,
    weights = varIdent(form = ~ 1 | Cropping.system),
    data = dat2,
    na.action = na.omit)
  }
  if(response_var %in% c("weeds"))
        {  fit <- lme(
          fixed = as.formula(paste("(", response_var, ")^(1/3) ~ Cover * Cropping.system * factor(", time_var, ")")),
          random = ~ 1 | Rep/Cropping.system,
          weights = varIdent(form = ~ 1 | Cover),
          data = dat2,
          na.action = na.omit)
         }  
  if(response_var %in% resp_compsym)
        {  fit <- lme(
          fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system * factor(", time_var, ")")),
          random = ~ 1 | Rep/Cropping.system/Cover,
          correlation = corCompSymm(form =as.formula(paste("~", time_var, "| Rep/Cropping.system/Cover"))),
          weights = varIdent(form = ~ 1 | Cover),
          data = dat2,
          na.action = na.omit)
        }
  # check for normality
  resid <- residuals(fit)
  resp$normality[i] <- shapiro.test(resid)$p.value
  
# export model stats
  mod.out <- as.data.frame(anova(fit, type="marginal")) 
  mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)
  #write.csv(mod.out2, paste0("Model-output/anova/_across years/",response_var,".csv"))
  resp[i,c(3:9)] <- paste0("F=", round(mod.out2$`F-value`,2), ", p=", mod.out2$`p-value`)[2:8]
  #write.csv(round(r.squaredGLMM(fit),2), paste0("Model-output/anova/_across years/",response_var,"_rsq.csv")) # marginal: explained by fixed effects; conditional: explained by entire model
  resp$rsq[i] <- paste0(round(r.squaredGLMM(fit)[1],2), ", ", round(r.squaredGLMM(fit)[2],2))
  

  
    ### tukey: Cover * Cropping system * time
  test1 <- emmeans(fit, ~ Cover*Cropping.system, by=c(time_var))
  testlet <- cld(test1, type = "response", Letters = "ABCDEFGH", reversed = TRUE)
  testlet <- testlet[order(testlet[[time_var]], testlet$Cropping.system, testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cover_cropsys_time <- testlet
  testlet_df_cover_cropsys_time <- testlet_df_cover_cropsys_time %>% dplyr::mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
  write.csv(testlet_df_cover_cropsys_time, paste0("Model-output/lsmeans/_across years/",response_var,"_cover_cropsys_time.csv"))
 
  ### tukey: Cover * Cropping system
  test1 <- emmeans(fit, ~ Cover, by=c("Cropping.system"))
  testlet <- cld(test1, type = "response", Letters = "ABCDEFGH", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cover_cropsys <- testlet
  testlet_df_cover_cropsys <- testlet_df_cover_cropsys %>% dplyr::mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
  write.csv(testlet_df_cover_cropsys, paste0("Model-output/lsmeans/_across years/",response_var,"_cover_cropsys.csv"))
  
  ### tukey: Cropping.system * time
  test1 <- emmeans(fit, ~ Cropping.system, by=time_var)
  testlet <- cld(test1, type = "response", Letters = "ABCDEF", reversed = TRUE)
  testlet <- testlet[order(testlet[[time_var]], testlet$Cropping.system),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cropsys_time <- testlet
  testlet_df_cropsys_time <- testlet_df_cropsys_time %>% dplyr::mutate_each_(funs(factor(.)),c("Cropping.system"))
  
  ### tukey: Cover * time
  test1 <- emmeans(fit, ~ Cover, by=time_var)
  testlet <- cld(test1, type = "response", Letters = "ABCDEF", reversed = TRUE)
  testlet <- testlet[order(testlet[[time_var]], testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cover_time <- testlet
  testlet_df_cover_time <- testlet_df_cover_time %>% dplyr::mutate_each_(funs(factor(.)),c("Cover"))
 
  ### tukey: Cropping.system 
  test1 <- emmeans(fit, ~ Cropping.system)
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cropsys <- testlet
  testlet_df_cropsys <- testlet_df_cropsys %>% dplyr::mutate_each_(funs(factor(.)),c("Cropping.system"))
  
  ### tukey: Cover 
  test1 <- emmeans(fit, ~ Cover)
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cover <- testlet
  testlet_df_cover <- testlet_df_cover %>% dplyr::mutate_each_(funs(factor(.)),c("Cover"))
  
  ### tukey: YearN 
  test1 <- emmeans(fit, ~ YearN)
  testlet <- cld(test1, type = "response", Letters = "ABCDE", reversed = TRUE)
  testlet <- testlet[order(testlet$YearN),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  if (response_var %notin% c(foliarcols_scaled, "multifunctionality_index_nutrition2")){ testlet$Year <- c(2021,2022,2023)}
  if (response_var %in% c(foliarcols_scaled, "multifunctionality_index_nutrition2")){ testlet$Year <- c(2022,2023)}
  testlet_df_time <- testlet
  testlet_df_time <- testlet_df_time %>% dplyr::mutate_each_(funs(factor(.)),c("Year"))
  
  # export lsmeans
  write.csv(testlet_df_cropsys_time, paste0("Model-output/lsmeans/_across years/",response_var,"_cropsys_time.csv"))
  write.csv(testlet_df_cover_time, paste0("Model-output/lsmeans/_across years/",response_var,"_cover_time.csv"))
  write.csv(testlet_df_cropsys, paste0("Model-output/lsmeans/_across years/",response_var,"_cropsys.csv"))
  write.csv(testlet_df_cover, paste0("Model-output/lsmeans/_across years/",response_var,"_cover.csv"))
  write.csv(testlet_df_time, paste0("Model-output/lsmeans/_across years/",response_var,"_time.csv"))
  
}
resp
write.csv(resp, paste0("Model-output/anova/_across years/_all.csv"))









### models for yield
# because there is only one phase represented in a rotation per year, 
# the above model does not work for corn, cotton, and soybean yield




################################################


# Make sure Year and Season.Year are numeric
dat2$YearN <- as.numeric(dat2$Year)


# Make data frame with responses and repeated measures (time)
responses <- c("corn_yield_2021", "soybean_yield_2021",
               "corn_yield_2022", "cotton_yield_2022", "soybean_yield_2022",
               "corn_yield_2023", "soybean_yield_2023")

resp <- data.frame(resp = responses,
                   time = c(rep("YearN", length(responses))),
                   normality = rep(NA, length(responses)),
                   cropsys.included = c("no","no", 
                                        "no","no","yes",
                                        "yes","yes"),
                   cover = rep(NA, length(responses)),
                   cropsys = rep(NA, length(responses)),
                   cover.cropsys = rep(NA, length(responses)),
                   rsq = rep(NA, length(responses)))
resp


# model
# Loop through responses and fit models
for (i in 1:nrow(resp)) { # nrow(resp)
  response_var <- resp$resp[i]  # Current response variable

  if (resp$cropsys.included[i]=="no")  {# Fit the model for Cover 
    fit <- lme(
      fixed = as.formula(paste("(", response_var, ") ~ Cover")),
      random = ~ 1 | Rep,
      #correlation = corAR1(form = as.formula(paste("~ 1| Rep"))),
      #weights = varIdent(form = ~ 1 | Cover),
      data = dat2,
      na.action = na.omit
    )
  # check for normality: if <0.05, transform the data
  resid <- residuals(fit)
  resp$normality[i] <- shapiro.test(resid)$p.value
  if(resp$normality[i]<0.05)  
  {fit <- lme(
    fixed = as.formula(paste("log(", response_var, ") ~ Cover)")),
    random = ~ 1 | Rep,
    #correlation = corAR1(form = as.formula(paste("~1 | Rep"))),
    weights = varIdent(form = ~ 1 | Cover),
    data = dat2,
    na.action = na.omit
  )
  }
  # check for normality
  resid <- residuals(fit)
  resp$normality[i] <- shapiro.test(resid)$p.value
  
  # export model stats
  mod.out <- as.data.frame(anova(fit, type="marginal")) 
  mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)
  #write.csv(mod.out2, paste0("Model-output/anova/_across years/",response_var,".csv"))
  resp[i,c(5)] <- paste0("F=", round(mod.out2$`F-value`,2), ", p=", mod.out2$`p-value`)[2]
  #write.csv(round(r.squaredGLMM(fit),2), paste0("Model-output/anova/_across years/",response_var,"_rsq.csv")) # marginal: explained by fixed effects; conditional: explained by entire model
  resp$rsq[i] <- paste0(round(r.squaredGLMM(fit)[1],2), ", ", round(r.squaredGLMM(fit)[2],2))
  
  ### tukey: Cover 
  test1 <- emmeans(fit, ~ Cover)
  testlet <- cld(test1, type = "response", Letters = "ABCD", reversed = TRUE)
  testlet <- testlet[order(testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cover <- testlet
  testlet_df_cover <- testlet_df_cover %>% dplyr::mutate_each_(funs(factor(.)),c("Cover"))
  
  # export lsmeans
  write.csv(testlet_df_cover, paste0("Model-output/lsmeans/_across years/",response_var,"_cover.csv"))
  }

  

  if (resp$cropsys.included[i]=="yes") { # Fit the model for Cover and Cropping.system
  fit <- lme(
    fixed = as.formula(paste("(", response_var, ") ~ Cover * Cropping.system")),
    random = ~ 1 | Rep,
    #correlation = corAR1(form = as.formula(paste("~ 1| Rep/Cropping.system"))),
    weights = varIdent(form = ~ 1 | Cover),
    data = dat2,
    na.action = na.omit
  )
  # check for normality: if <0.05, transform the data
  resid <- residuals(fit)
  resp$normality[i] <- shapiro.test(resid)$p.value
  if(resp$normality[i]<0.05)  
    {fit <- lme(
    fixed = as.formula(paste("log(", response_var, "+1) ~ Cover * Cropping.system")),
    random = ~ 1 | Rep,
    #correlation = corAR1(form = as.formula(paste("~", time_var, "| Rep/Cropping.system"))),
    weights = varIdent(form = ~ 1 | Cover),
    data = dat2,
    na.action = na.omit
  )
  }
  # check for normality
  resid <- residuals(fit)
  resp$normality[i] <- shapiro.test(resid)$p.value
  
  # export model stats
  mod.out <- as.data.frame(anova(fit, type="marginal")) 
  mod.out2 <- mod.out %>% mutate_if(is.numeric, round, digits=3)
  #write.csv(mod.out2, paste0("Model-output/anova/_across years/",response_var,".csv"))
  resp[i,c(5:7)] <- paste0("F=", round(mod.out2$`F-value`,2), ", p=", mod.out2$`p-value`)[2:4]
  #write.csv(round(r.squaredGLMM(fit),2), paste0("Model-output/anova/_across years/",response_var,"_rsq.csv")) # marginal: explained by fixed effects; conditional: explained by entire model
  resp$rsq[i] <- paste0(round(r.squaredGLMM(fit)[1],2), ", ", round(r.squaredGLMM(fit)[2],2))
  
  ### tukey: Cover * Cropping.system
  test1 <- emmeans(fit, ~ Cover*Cropping.system)
  testlet <- cld(test1, type = "response", Letters = "ABCDEFG", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system, testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cover <- testlet
  testlet_df_cover_cropsys <- testlet_df_cover %>% dplyr::mutate_each_(funs(factor(.)),c("Cover", "Cropping.system"))
  
  ### tukey: Cropping.system 
  test1 <- emmeans(fit, ~ Cropping.system)
  testlet <- cld(test1, type = "response", Letters = "ABCD", reversed = TRUE)
  testlet <- testlet[order(testlet$Cropping.system),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cropsys <- testlet
  testlet_df_cropsys <- testlet_df_cropsys %>% dplyr::mutate_each_(funs(factor(.)),c("Cropping.system"))
  
  ### tukey: Cover 
  test1 <- emmeans(fit, ~ Cover)
  testlet <- cld(test1, type = "response", Letters = "ABCD", reversed = TRUE)
  testlet <- testlet[order(testlet$Cover),]
  testlet$.group <- gsub(" ", "", testlet$.group)
  testlet_df_cover <- testlet
  testlet_df_cover <- testlet_df_cover %>% dplyr::mutate_each_(funs(factor(.)),c("Cover"))
  
  # export lsmeans
  write.csv(testlet_df_cover_cropsys, paste0("Model-output/lsmeans/_across years/",response_var,"_cover_cropsys.csv"))
  write.csv(testlet_df_cropsys, paste0("Model-output/lsmeans/_across years/",response_var,"_cropsys.csv"))
  write.csv(testlet_df_cover, paste0("Model-output/lsmeans/_across years/",response_var,"_cover.csv"))
  }
}
resp
write.csv(resp, paste0("Model-output/anova/_across years/_all_yield.csv"))





write.csv(dat2, "Processed-data/multifunctionality.csv")


