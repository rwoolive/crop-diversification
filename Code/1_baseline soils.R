
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

##################### Import processed data #####################
dat <- read.csv("Processed-data/WTREC-CDP_processed-data-simple.csv")
dat_p <- read.csv("Processed-data/WTREC-CDP_processedpooled-data-simple.csv")




# make factors
dat <- dat %>% mutate_each_(funs(factor(.)),c("Rep", "Depth", "Year", "Season", "Cropping.system", "Cover"))
dat_p <- dat_p %>% mutate_each_(funs(factor(.)),c("Rep", "Depth", "Year", "Season", "Cropping.system"))

# subset to one soil depth, baseline
dat2020 <- dat[which(dat$Year==2020),]
dat2020_1 <- dat2020[which(dat2020$Depth=="0-10 cm"),]

dat_p_1 <- dat_p[which(dat_p$Depth=="0-10 cm"),]


# calculate mean and sd
dat2020_1_avgs <- dat2020_1 %>%
  dplyr::summarise(across(
    .cols = is.numeric, 
    .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

write.csv(round(t(dat2020_1_avgs), 3), "Processed-data/baseline_averages.csv")


dat_p_1_avgs <- dat_p_1 %>%
  dplyr::summarise(across(
    .cols = is.numeric, 
    .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

write.csv(round(t(dat_p_1_avgs), 3), "Processed-data/baseline_averages_pooled.csv")

