
library(ggplot2)

dat2 <- read.csv("Processed-data/multifunctionality.csv")

############
# benefits of cover crop diversification
############


# import raw anova output
agb <- read.csv("Model-output/lsmeans/*across years/agb_cover.csv")
weeds <- read.csv("Model-output/lsmeans/*across years/weeds_cover.csv")
inputC <- read.csv("Model-output/lsmeans/*across years/inputC_cover.csv")
inputN <- read.csv("Model-output/lsmeans/*across years/inputN_cover.csv")
inputCN <- read.csv("Model-output/lsmeans/*across years/inputCN_cover.csv")
Yield_scaled <-  read.csv("Model-output/lsmeans/*across years/Yield_scaled_cover.csv")
multifunctionality_index_agronomy2 <-  read.csv("Model-output/lsmeans/*across years/multifunctionality_index_agronomy2_cover.csv")

inorgN <- read.csv("Model-output/lsmeans/*across years/inorgN_cover.csv")
GMC <- read.csv("Model-output/lsmeans/*across years/GMC_cover.csv")
EEA_C <- read.csv("Model-output/lsmeans/*across years/EEA_C_cover.csv")
EEA_N <- read.csv("Model-output/lsmeans/*across years/EEA_N_cover.csv")
PHOS <- read.csv("Model-output/lsmeans/*across years/PHOS_cover.csv")
WEC <- read.csv("Model-output/lsmeans/*across years/WEC_cover.csv")
WEN <- read.csv("Model-output/lsmeans/*across years/WEN_cover.csv")
MBC <- read.csv("Model-output/lsmeans/*across years/MBC_cover.csv")
orgC <- read.csv("Model-output/lsmeans/*across years/orgC_cover.csv")
totN <- read.csv("Model-output/lsmeans/*across years/totN_cover.csv")
POC <- read.csv("Model-output/lsmeans/*across years/POC_cover.csv")
MAOC <- read.csv("Model-output/lsmeans/*across years/MAOC_cover.csv")
WAS <- read.csv("Model-output/lsmeans/*across years/WAS_cover.csv")
multifunctionality_index_soil2 <- read.csv("Model-output/lsmeans/*across years/multifunctionality_index_soil2_cover.csv")

tissue.N.percent_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.N.percent_scaled_cover.csv")
tissue.P.percent_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.P.percent_scaled_cover.csv")
tissue.K.percent_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.K.percent_scaled_cover.csv")
tissue.Ca.percent_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.Ca.percent_scaled_cover.csv")
tissue.Mg.percent_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.Mg.percent_scaled_cover.csv")
tissue.S.percent_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.S.percent_scaled_cover.csv")
tissue.B.ppm_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.B.ppm_scaled_cover.csv")
tissue.Cu.ppm_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.Cu.ppm_scaled_cover.csv")
tissue.Fe.ppm_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.Fe.ppm_scaled_cover.csv")
tissue.Mn.ppm_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.Mn.ppm_scaled_cover.csv")
tissue.Zn.ppm_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.Zn.ppm_scaled_cover.csv")
tissue.Na.ppm_scaled <- read.csv("Model-output/lsmeans/*across years/tissue.Na.ppm_scaled_cover.csv")



# back-transform scaled yield to get yield for corn/cotton/soy

max_ <- max(dat2$Yield[which(dat2$Yield.crop=="Corn")], na.rm = T)* 62.77/1000 
min_ <- min(dat2$Yield[which(dat2$Yield.crop=="Corn")], na.rm = T)* 62.77/1000 
Yield_scaled$response_Corn <- Yield_scaled$response*(max_ - min_) + min_
Yield_scaled$SE_Corn <- Yield_scaled$SE*(max_ - min_) + min_

max_ <- max(dat2$Yield[which(dat2$Yield.crop=="Cotton")], na.rm = T)* 1.1209/1000
min_ <- min(dat2$Yield[which(dat2$Yield.crop=="Cotton")], na.rm = T)* 1.1209/1000
Yield_scaled$response_Cotton <- Yield_scaled$response*(max_ - min_) + min_
Yield_scaled$SE_Cotton <- Yield_scaled$SE*(max_ - min_) + min_

max_ <- max(dat2$Yield[which(dat2$Yield.crop=="Soybean")], na.rm = T)* 67.25/1000
min_ <- min(dat2$Yield[which(dat2$Yield.crop=="Soybean")], na.rm = T)* 67.25/1000
Yield_scaled$response_Soybean <- Yield_scaled$response*(max_ - min_) + min_
Yield_scaled$SE_Soybean <- Yield_scaled$SE*(max_ - min_) + min_




# extract means 
agb$resp_se <- paste0(round(agb$response, 2), " ± ", round(agb$SE, 2), " ", gsub(" ", "", agb$.group))
weeds$resp_se <- paste0(round(weeds$response, 2), " ± ", round(weeds$SE, 2))#, " ", gsub(" ", "", weeds$.group))
inputC$resp_se <- paste0(round(inputC$response, 2), " ± ", round(inputC$SE, 2), " ", gsub(" ", "", inputC$.group))
inputN$resp_se <- paste0(round(inputN$response, 2), " ± ", round(inputN$SE, 2), " ", gsub(" ", "", inputN$.group))
inputCN$resp_se <- paste0(round(inputCN$response, 2), " ± ", round(inputCN$SE, 2), " ", gsub(" ", "", inputCN$.group))
Yield_scaled$resp_se <- paste0(round(Yield_scaled$response, 2), " ± ", round(Yield_scaled$SE, 2), " ", gsub(" ", "", Yield_scaled$.group))
Yield_scaled$Corn_se <- paste0(round(Yield_scaled$response_Corn, 2), " ± ", round(Yield_scaled$SE_Corn, 2))#, " ", gsub(" ", "", Yield_scaled$.group))
Yield_scaled$Cotton_se <- paste0(round(Yield_scaled$response_Cotton, 2), " ± ", round(Yield_scaled$SE_Cotton, 2))#, " ", gsub(" ", "", Yield_scaled$.group))
Yield_scaled$Soybean_se <- paste0(round(Yield_scaled$response_Soybean, 2), " ± ", round(Yield_scaled$SE_Soybean, 2))#, " ", gsub(" ", "", Yield_scaled$.group))
multifunctionality_index_agronomy2$resp_se <- paste0(round(multifunctionality_index_agronomy2$response, 2), " ± ", round(multifunctionality_index_agronomy2$SE, 2), " ", gsub(" ", "", multifunctionality_index_agronomy2$.group))

inorgN$resp_se <- paste0(round(inorgN$response, 2), " ± ", round(inorgN$SE, 2))#, " ", gsub(" ", "", inorgN$.group))
GMC$resp_se <- paste0(round(GMC$response, 2), " ± ", round(GMC$SE, 2))#, " ", gsub(" ", "", GMC$.group))
EEA_C$resp_se <- paste0(round(EEA_C$response, 2), " ± ", round(EEA_C$SE, 2))#, " ", gsub(" ", "", EEA_C$.group))
EEA_N$resp_se <- paste0(round(EEA_N$response, 2), " ± ", round(EEA_N$SE, 2))#, " ", gsub(" ", "", EEA_N$.group))
PHOS$resp_se <- paste0(round(PHOS$response, 2), " ± ", round(PHOS$SE, 2))#, " ", gsub(" ", "", PHOS$.group))
WEC$resp_se <- paste0(round(WEC$response, 2), " ± ", round(WEC$SE, 2))#, " ", gsub(" ", "", WEC$.group))
WEN$resp_se <- paste0(round(WEN$response, 2), " ± ", round(WEN$SE, 2))#, " ", gsub(" ", "", WEN$.group))
MBC$resp_se <- paste0(round(MBC$response, 2), " ± ", round(MBC$SE, 2))#, " ", gsub(" ", "", MBC$.group))
orgC$resp_se <- paste0(round(orgC$response, 2), " ± ", round(orgC$SE, 2))#, " ", gsub(" ", "", orgC$.group))
totN$resp_se <- paste0(round(totN$response, 2), " ± ", round(totN$SE, 2))#, " ", gsub(" ", "", totN$.group))
POC$resp_se <- paste0(round(POC$response, 2), " ± ", round(POC$SE, 2))#, " ", gsub(" ", "", POC$.group))
MAOC$resp_se <- paste0(round(MAOC$response, 2), " ± ", round(MAOC$SE, 2))#, " ", gsub(" ", "", MAOC$.group))
WAS$resp_se <- paste0(round(WAS$response, 2), " ± ", round(WAS$SE, 2))#, " ", gsub(" ", "", WAS$.group))
multifunctionality_index_soil2$resp_se <- paste0(round(multifunctionality_index_soil2$response, 2), " ± ", round(multifunctionality_index_soil2$SE, 2))#, " ", gsub(" ", "", multifunctionality_index_soil2$.group))

tissue.N.percent_scaled$resp_se <- paste0(round(tissue.N.percent_scaled$response, 2), " ± ", round(tissue.N.percent_scaled$SE, 2))#, " ", gsub(" ", "", tissue.N.percent_scaled$.group))
tissue.P.percent_scaled$resp_se <- paste0(round(tissue.P.percent_scaled$response, 2), " ± ", round(tissue.P.percent_scaled$SE, 2))#, " ", gsub(" ", "", tissue.P.percent_scaled$.group))
tissue.K.percent_scaled$resp_se <- paste0(round(tissue.K.percent_scaled$response, 2), " ± ", round(tissue.K.percent_scaled$SE, 2))#, " ", gsub(" ", "", tissue.K.percent_scaled$.group))
tissue.Ca.percent_scaled$resp_se <- paste0(round(tissue.Ca.percent_scaled$response, 2), " ± ", round(tissue.Ca.percent_scaled$SE, 2))#, " ", gsub(" ", "", tissue.Ca.percent_scaled$.group))
tissue.Mg.percent_scaled$resp_se <- paste0(round(tissue.Mg.percent_scaled$response, 2), " ± ", round(tissue.Mg.percent_scaled$SE, 2))#, " ", gsub(" ", "", tissue.Mg.percent_scaled$.group))
tissue.S.percent_scaled$resp_se <- paste0(round(tissue.S.percent_scaled$response, 2), " ± ", round(tissue.S.percent_scaled$SE, 2))#, " ", gsub(" ", "", tissue.S.percent_scaled$.group))
tissue.B.ppm_scaled$resp_se <- paste0(round(tissue.B.ppm_scaled$response, 2), " ± ", round(tissue.B.ppm_scaled$SE, 2))#, " ", gsub(" ", "", tissue.B.ppm_scaled$.group))
tissue.Cu.ppm_scaled$resp_se <- paste0(round(tissue.Cu.ppm_scaled$response, 2), " ± ", round(tissue.Cu.ppm_scaled$SE, 2))#, " ", gsub(" ", "", tissue.Cu.ppm_scaled$.group))
tissue.Fe.ppm_scaled$resp_se <- paste0(round(tissue.Fe.ppm_scaled$response, 2), " ± ", round(tissue.Fe.ppm_scaled$SE, 2))#, " ", gsub(" ", "", tissue.Fe.ppm_scaled$.group))
tissue.Mn.ppm_scaled$resp_se <- paste0(round(tissue.Mn.ppm_scaled$response, 2), " ± ", round(tissue.Mn.ppm_scaled$SE, 2))#, " ", gsub(" ", "", tissue.Mn.ppm_scaled$.group))
tissue.Zn.ppm_scaled$resp_se <- paste0(round(tissue.Zn.ppm_scaled$response, 2), " ± ", round(tissue.Zn.ppm_scaled$SE, 2))#, " ", gsub(" ", "", tissue.Zn.ppm_scaled$.group))
tissue.Na.ppm_scaled$resp_se <- paste0(round(tissue.Na.ppm_scaled$response, 2), " ± ", round(tissue.Na.ppm_scaled$SE, 2))#, " ", gsub(" ", "", tissue.NA.ppm_scaled$.group))


# combine normalized means
agron_table_means <- data.frame(agb = c(0,agb$resp_se), 
                          weeds = weeds$resp_se, 
                          inputC = inputC$resp_se, 
                          inputN = inputN$resp_se, 
                          inputCN = inputCN$resp_se, 
                          Yield_scaled = Yield_scaled$resp_se,
                          Yield_Corn = Yield_scaled$Corn_se,
                          Yield_Cotton = Yield_scaled$Cotton_se,
                          Yield_Soybean = Yield_scaled$Soybean_se,
                          multifunctionality_index_agronomy2 = multifunctionality_index_agronomy2$resp_se, 
                          inorgN = inorgN$resp_se, 
                          GMC = GMC$resp_se, 
                          EEA_C = EEA_C$resp_se, 
                          EEA_N = EEA_N$resp_se, 
                          PHOS = PHOS$resp_se, 
                          WEC = WEC$resp_se, 
                          WEN = WEN$resp_se, 
                          MBC = MBC$resp_se, 
                          orgC = orgC$resp_se, 
                          totN = totN$resp_se, 
                          POC = POC$resp_se, 
                          MAOC = MAOC$resp_se, 
                          WAS = WAS$resp_se, 
                          multifunctionality_index_soil2 = multifunctionality_index_soil2$resp_se, 
                          tissue.N.percent_scaled = tissue.N.percent_scaled$resp_se, 
                          tissue.P.percent_scaled = tissue.P.percent_scaled$resp_se, 
                          tissue.K.percent_scaled = tissue.K.percent_scaled$resp_se, 
                          tissue.Ca.percent_scaled = tissue.Ca.percent_scaled$resp_se, 
                          tissue.Mg.percent_scaled = tissue.Mg.percent_scaled$resp_se, 
                          tissue.S.percent_scaled = tissue.S.percent_scaled$resp_se, 
                          tissue.B.ppm_scaled = tissue.B.ppm_scaled$resp_se, 
                          tissue.Cu.ppm_scaled = tissue.Cu.ppm_scaled$resp_se, 
                          tissue.Fe.ppm_scaled = tissue.Fe.ppm_scaled$resp_se, 
                          tissue.Mn.ppm_scaled = tissue.Mn.ppm_scaled$resp_se, 
                          tissue.Zn.ppm_scaled = tissue.Zn.ppm_scaled$resp_se, 
                          tissue.Na.ppm_scaled = tissue.Na.ppm_scaled$resp_se)
write.csv(t(agron_table_means), "Tables/means-cover.csv")






############
# benefits of cropping system diversification
############


# import raw anova output
agb <- read.csv("Model-output/lsmeans/*across years/agb_cropsys.csv")
weeds <- read.csv("Model-output/lsmeans/*across years/weeds_cropsys.csv")
inputC <- read.csv("Model-output/lsmeans/*across years/inputC_cropsys.csv")
inputN <- read.csv("Model-output/lsmeans/*across years/inputN_cropsys.csv")
inputCN <- read.csv("Model-output/lsmeans/*across years/inputCN_cropsys.csv")
Yield_scaled <-  read.csv("Model-output/lsmeans/*across years/Yield_scaled_cropsys.csv")
multifunctionality_index_agronomy2 <-  read.csv("Model-output/lsmeans/*across years/multifunctionality_index_agronomy2_cropsys.csv")

inorgN <- read.csv("Model-output/lsmeans/*across years/inorgN_cropsys.csv")
GMC <- read.csv("Model-output/lsmeans/*across years/GMC_cropsys.csv")
EEA_C <- read.csv("Model-output/lsmeans/*across years/EEA_C_cropsys.csv")
EEA_N <- read.csv("Model-output/lsmeans/*across years/EEA_N_cropsys.csv")
PHOS <- read.csv("Model-output/lsmeans/*across years/PHOS_cropsys.csv")
WEC <- read.csv("Model-output/lsmeans/*across years/WEC_cropsys.csv")
WEN <- read.csv("Model-output/lsmeans/*across years/WEN_cropsys.csv")
MBC <- read.csv("Model-output/lsmeans/*across years/MBC_cropsys.csv")
orgC <- read.csv("Model-output/lsmeans/*across years/orgC_cropsys.csv")
totN <- read.csv("Model-output/lsmeans/*across years/totN_cropsys.csv")
POC <- read.csv("Model-output/lsmeans/*across years/POC_cropsys.csv")
MAOC <- read.csv("Model-output/lsmeans/*across years/MAOC_cropsys.csv")
WAS <- read.csv("Model-output/lsmeans/*across years/WAS_cropsys.csv")
multifunctionality_index_soil2 <- read.csv("Model-output/lsmeans/*across years/multifunctionality_index_soil2_cropsys.csv")

# back-transform scaled yield to get yield for corn/cotton/soy

max_ <- max(dat2$Yield[which(dat2$Yield.crop=="Corn")], na.rm = T)* 62.77/1000 
min_ <- min(dat2$Yield[which(dat2$Yield.crop=="Corn")], na.rm = T)* 62.77/1000 
Yield_scaled$response_Corn <- Yield_scaled$response*(max_ - min_) + min_
Yield_scaled$SE_Corn <- Yield_scaled$SE*(max_ - min_) + min_

max_ <- max(dat2$Yield[which(dat2$Yield.crop=="Cotton")], na.rm = T)* 1.1209/1000
min_ <- min(dat2$Yield[which(dat2$Yield.crop=="Cotton")], na.rm = T)* 1.1209/1000
Yield_scaled$response_Cotton <- Yield_scaled$response*(max_ - min_) + min_
Yield_scaled$SE_Cotton <- Yield_scaled$SE*(max_ - min_) + min_

max_ <- max(dat2$Yield[which(dat2$Yield.crop=="Soybean")], na.rm = T)* 67.25/1000
min_ <- min(dat2$Yield[which(dat2$Yield.crop=="Soybean")], na.rm = T)* 67.25/1000
Yield_scaled$response_Soybean <- Yield_scaled$response*(max_ - min_) + min_
Yield_scaled$SE_Soybean <- Yield_scaled$SE*(max_ - min_) + min_




# extract means 
agb$resp_se <- paste0(round(agb$response, 2), " ± ", round(agb$SE, 2))#, " ", gsub(" ", "", agb$.group))
weeds$resp_se <- paste0(round(weeds$response, 2), " ± ", round(weeds$SE, 2))#, " ", gsub(" ", "", weeds$.group))
inputC$resp_se <- paste0(round(inputC$response, 2), " ± ", round(inputC$SE, 2))#, " ", gsub(" ", "", inputC$.group))
inputN$resp_se <- paste0(round(inputN$response, 2), " ± ", round(inputN$SE, 2))#, " ", gsub(" ", "", inputN$.group))
inputCN$resp_se <- paste0(round(inputCN$response, 2), " ± ", round(inputCN$SE, 2))#, " ", gsub(" ", "", inputCN$.group))
Yield_scaled$resp_se <- paste0(round(Yield_scaled$response, 2), " ± ", round(Yield_scaled$SE, 2))#, " ", gsub(" ", "", Yield_scaled$.group))
Yield_scaled$Corn_se <- paste0(round(Yield_scaled$response_Corn, 2), " ± ", round(Yield_scaled$SE_Corn, 2))#, " ", gsub(" ", "", Yield_scaled$.group))
Yield_scaled$Cotton_se <- paste0(round(Yield_scaled$response_Cotton, 2), " ± ", round(Yield_scaled$SE_Cotton, 2))#, " ", gsub(" ", "", Yield_scaled$.group))
Yield_scaled$Soybean_se <- paste0(round(Yield_scaled$response_Soybean, 2), " ± ", round(Yield_scaled$SE_Soybean, 2))#, " ", gsub(" ", "", Yield_scaled$.group))
multifunctionality_index_agronomy2$resp_se <- paste0(round(multifunctionality_index_agronomy2$response, 2), " ± ", round(multifunctionality_index_agronomy2$SE, 2))#, " ", gsub(" ", "", multifunctionality_index_agronomy2$.group))

inorgN$resp_se <- paste0(round(inorgN$response, 2), " ± ", round(inorgN$SE, 2))#, " ", gsub(" ", "", inorgN$.group))
GMC$resp_se <- paste0(round(GMC$response, 2), " ± ", round(GMC$SE, 2))#, " ", gsub(" ", "", GMC$.group))
EEA_C$resp_se <- paste0(round(EEA_C$response, 2), " ± ", round(EEA_C$SE, 2))#, " ", gsub(" ", "", EEA_C$.group))
EEA_N$resp_se <- paste0(round(EEA_N$response, 2), " ± ", round(EEA_N$SE, 2))#, " ", gsub(" ", "", EEA_N$.group))
PHOS$resp_se <- paste0(round(PHOS$response, 2), " ± ", round(PHOS$SE, 2))#, " ", gsub(" ", "", PHOS$.group))
WEC$resp_se <- paste0(round(WEC$response, 2), " ± ", round(WEC$SE, 2))#, " ", gsub(" ", "", WEC$.group))
WEN$resp_se <- paste0(round(WEN$response, 2), " ± ", round(WEN$SE, 2))#, " ", gsub(" ", "", WEN$.group))
MBC$resp_se <- paste0(round(MBC$response, 2), " ± ", round(MBC$SE, 2))#, " ", gsub(" ", "", MBC$.group))
orgC$resp_se <- paste0(round(orgC$response, 2), " ± ", round(orgC$SE, 2))#, " ", gsub(" ", "", orgC$.group))
totN$resp_se <- paste0(round(totN$response, 2), " ± ", round(totN$SE, 2))#, " ", gsub(" ", "", totN$.group))
POC$resp_se <- paste0(round(POC$response, 2), " ± ", round(POC$SE, 2))#, " ", gsub(" ", "", POC$.group))
MAOC$resp_se <- paste0(round(MAOC$response, 2), " ± ", round(MAOC$SE, 2))#, " ", gsub(" ", "", MAOC$.group))
WAS$resp_se <- paste0(round(WAS$response, 2), " ± ", round(WAS$SE, 2))#, " ", gsub(" ", "", WAS$.group))
multifunctionality_index_soil2$resp_se <- paste0(round(multifunctionality_index_soil2$response, 2), " ± ", round(multifunctionality_index_soil2$SE, 2))#, " ", gsub(" ", "", multifunctionality_index_soil2$.group))


# combine normalized means
agron_table_means <- data.frame(agb = c(agb$resp_se), 
                                weeds = weeds$resp_se, 
                                inputC = inputC$resp_se, 
                                inputN = inputN$resp_se, 
                                inputCN = inputCN$resp_se, 
                                Yield_scaled = Yield_scaled$resp_se,
                                Yield_Corn = Yield_scaled$Corn_se,
                                Yield_Cotton = Yield_scaled$Cotton_se,
                                Yield_Soybean = Yield_scaled$Soybean_se,
                                multifunctionality_index_agronomy2 = multifunctionality_index_agronomy2$resp_se, 
                                inorgN = inorgN$resp_se, 
                                GMC = GMC$resp_se, 
                                EEA_C = EEA_C$resp_se, 
                                EEA_N = EEA_N$resp_se, 
                                PHOS = PHOS$resp_se, 
                                WEC = WEC$resp_se, 
                                WEN = WEN$resp_se, 
                                MBC = MBC$resp_se, 
                                orgC = orgC$resp_se, 
                                totN = totN$resp_se, 
                                POC = POC$resp_se, 
                                MAOC = MAOC$resp_se, 
                                WAS = WAS$resp_se, 
                                multifunctionality_index_soil2 = multifunctionality_index_soil2$resp_se)
write.csv(t(agron_table_means), "Tables/means-cropsys.csv")














#### make a heatmaps of standardized means

# agronomy response to cover crops
h_agron_cover <- as.matrix(t(agron_table[,c(1:6)]))
colnames(h_agron_cover) <- c("No cover", "Wheat", "Clover", "Wheat-clover mix", "Five-species mix")
hh_agron_cover <- data.frame(value=as.vector(h_agron_cover),
                             Cover=as.factor(rep(c("No cover", "Wheat", "Clover", "Wheat-clover mix", "Five-species mix"), each=6)),
                             Response=as.factor(rep(rownames(h_agron_cover), 5)))
hh_agron_cover$Cover <- factor(hh_agron_cover$Cover, levels(hh_agron_cover$Cover)[c(3,1,4,5,2)])
hh_agron_cover$Response <- factor(hh_agron_cover$Response, levels(hh_agron_cover$Response)[c(1,5,2,4,3,6)])

heat_agron_cover <- ggplot(hh_agron_cover, aes(x=Cover, y=Response, fill=value, label=value))+
  geom_tile()+ xlab("")+ ylab("Agronomic responses") + geom_text() +
  scale_fill_gradientn(colors=c("indianred3","gray90","steelblue1"), na.value="white")+
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(hh_agron_cover$Response)), 
                   labels=c("agb" = expression(paste("Cover crop biomass")), 
                            "weeds" =  expression(paste("Weed biomass")), 
                            "inputC" = expression(paste("C inputs")), 
                            "inputN" = expression(paste("N inputs")), 
                            "inputCN" = expression(paste("Biomass quality")),
                            "Yield" = "Standardized yield")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
heat_agron_cover

# soil response to cover crops
h_soil_table <- as.matrix(t(soil_table[,c(1:10)]))
colnames(h_soil_table) <- c("No cover", "Wheat", "Clover", "Wheat-clover mix", "Five-species mix")
hh_soil_table <- data.frame(value=as.vector(h_soil_table),
                             Cover=as.factor(rep(c("No cover", "Wheat", "Clover", "Wheat-clover mix", "Five-species mix"), each=10)),
                             Response=as.factor(rep(rownames(h_soil_table), 5)))
hh_soil_table$Cover <- factor(hh_soil_table$Cover, levels(hh_soil_table$Cover)[c(3,1,4,5,2)])
hh_soil_table$Response <- factor(hh_soil_table$Response, levels(hh_soil_table$Response)[c(3,2,1,5,7,10,6,9,8,4)])

heat_soil_table_cover <- ggplot(hh_soil_table, aes(x=Cover, y=Response, fill=value, label=value))+
  geom_tile()+ xlab("")+ ylab("Soil health responses") + geom_text() +
  scale_fill_gradientn(colors=c("indianred3","gray90","steelblue1"), na.value="white")+
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(hh_soil_table$Response)), 
                   labels=c("inorgN" = expression(paste("Inorganic N")), 
                            "GMC" =  expression(paste("Moisture")), 
                            "BG" = expression(paste("β-glucosidase activity")), 
                            "NAG" = expression(paste("N-acetyl-β-glucosaminidase activity")), 
                            "PHOS" = expression(paste("Phosphatase activity")),
                            "WEC" = expression(paste("Water-extractable organic C")), 
                            "orgC" =  expression(paste("Soil organic C")), 
                            "totN" = expression(paste("Total N")), 
                            "POC" = expression(paste("Particulate organic C")), 
                            "MAOC" = expression(paste("Mineral-associated organic C")))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
heat_soil_table_cover


### show correlation between agronomy and soil benefits across cover crop treatments
sum_tab <- data.frame(agron = agron_table$sum1, 
                      soil = soil_table$sum2, 
                      Cover = as.factor(c("No cover", "Clover", "Wheat", "Wheat-clover", "Five-species mix")))
sum_tab$Cover <- factor(sum_tab$Cover, levels(sum_tab$Cover)[c(3,1,4,5,2)])
covercols <-c("chocolate", "goldenrod", "forestgreen", "darkcyan", "darkorchid4")
rsq_cover <- paste("R^2 == ", round(cor(sum_tab$agron, sum_tab$soil),2))

cor_cover <- ggplot(sum_tab, aes(x=soil, y=agron, color=Cover)) +
  theme_minimal() + labs(y="Sum of agronomic benefits", x="Sum of soil health benefits") + labs(color=NA) +
  scale_x_continuous(breaks=seq(0,10,2), limits=c(0,10))+
  scale_y_continuous(breaks=seq(0,6,2), limits=c(0,6))+
  geom_smooth(method="lm",  color="black", fill=NA, size=0.5) +
  geom_point(size=4, shape=19) +
  scale_color_manual(values = covercols) + 
  ggrepel::geom_text_repel(label=sum_tab$Cover, force_pull=10) +
  theme(legend.position = "none", plot.margin=margin(15,25,15,25),
        panel.background = element_rect()) +
  annotate(geom="text", x=1, y=5.5, label=rsq_cover, parse = TRUE)
cor_cover
  


############
# agronomic benefits of cropping systems
############


# import raw anova output
weeds <- read.csv("Model-output/lsmeans/*across years/weeds_cropsys.csv")
inputCN <- read.csv("Model-output/lsmeans/*across years/inputCN_cropsys.csv")
#foliarN <-  read.csv("Model-output/lsmeans/*across years/tissue.N.percent_cropsys.csv")
#foliarP <-  read.csv("Model-output/lsmeans/*across years/tissue.P.percent_cropsys.csv")
#foliarK <-  read.csv("Model-output/lsmeans/*across years/tissue.K.percent_cropsys.csv")
#foliarCa <-  read.csv("Model-output/lsmeans/*across years/tissue.Ca.percent_cropsys.csv")
#foliarMg <-  read.csv("Model-output/lsmeans/*across years/tissue.Mg.percent_cropsys.csv")
#foliarB <-  read.csv("Model-output/lsmeans/*across years/tissue.B.ppm_cropsys.csv")
#foliarFe <-  read.csv("Model-output/lsmeans/*across years/tissue.Fe.ppm_cropsys.csv")
#foliarMn <-  read.csv("Model-output/lsmeans/*across years/tissue.Mn.ppm_cropsys.csv")
#foliarZn <-  read.csv("Model-output/lsmeans/*across years/tissue.Zn.ppm_cropsys.csv")
Yield <-  read.csv("Model-output/lsmeans/*across years/Yield_std_cropsys.csv")

# normalize means between 0 and 1
weeds$response_std <- (weeds$response-max(weeds$response))/(min(weeds$response-max(weeds$response)))
inputCN$response_std <- (inputCN$response-max(inputCN$response))/(min(inputCN$response-max(inputCN$response)))
#foliarN$response_std <- (foliarN$response-min(foliarN$response))/(max(foliarN$response-min(foliarN$response)))
#foliarP$response_std <- (foliarP$response-min(foliarP$response))/(max(foliarP$response-min(foliarP$response)))
#foliarK$response_std <- (foliarK$response-min(foliarK$response))/(max(foliarK$response-min(foliarK$response)))
# foliarCa$response_std <- (foliarCa$response-min(foliarCa$response))/(max(foliarCa$response-min(foliarCa$response)))
# foliarMg$response_std <- (foliarMg$response-min(foliarMg$response))/(max(foliarMg$response-min(foliarMg$response)))
# foliarB$response_std <- (foliarB$response-min(foliarB$response))/(max(foliarB$response-min(foliarB$response)))
# foliarFe$response_std <- (foliarFe$response-min(foliarFe$response))/(max(foliarFe$response-min(foliarFe$response)))
# foliarMn$response_std <- (foliarMn$response-min(foliarMn$response))/(max(foliarMn$response-min(foliarMn$response)))
# foliarZn$response_std <- (foliarZn$response-min(foliarZn$response))/(max(foliarZn$response-min(foliarZn$response)))
Yield$response_std <- (Yield$response-min(Yield$response))/(max(Yield$response-min(Yield$response)))


# combine normalized means
agron_table <- data.frame(weeds = weeds$response_std,
                          inputCN = inputCN$response_std,
                          #foliarN = foliarN$response_std,
                          # foliarP = foliarP$response_std,
                          # foliarK = foliarK$response_std,
                          # foliarCa = foliarCa$response_std,
                          # foliarMg = foliarMg$response_std,
                          # foliarB = foliarB$response_std,
                          # foliarFe = foliarFe$response_std,
                          # foliarMn = foliarMn$response_std,
                          # foliarZn = foliarZn$response_std,
                          Yield = Yield$response_std)
agron_table$sum1 <- rowSums(agron_table, na.rm = TRUE)
agron_table <- round(agron_table, 2)



# extract means 
weeds$resp_se <- paste0(round(weeds$response, 2), " ± ", round(weeds$SE, 2), " ", gsub(" ", "", weeds$.group))
inputCN$resp_se <- paste0(round(inputCN$response, 2), " ± ", round(inputCN$SE, 2), " ", gsub(" ", "", inputCN$.group))
Yield$resp_se <- paste0(round(Yield$response, 2), " ± ", round(Yield$SE, 2), " ", gsub(" ", "", Yield$.group))



# combine normalized means
agron_table_means <- data.frame(weeds = weeds$resp_se, 
                                inputCN = inputCN$resp_se, 
                                Yield = Yield$resp_se
                                
)
write.csv(t(agron_table_means), "Tables/means-agron-cropsys.csv")


Magron_table_means <- as.matrix(agron_table_means)
Magron_table <- as.matrix(as.data.frame(agron_table)[,-4])
Magron_table_merged <- matrix( paste0(Magron_table_means, " (", Magron_table, ")"), 
                              nrow=nrow(Magron_table_means), dimnames = dimnames(Magron_table_means))

Magron_table_merged2 <- cbind(Magron_table_merged, as.data.frame(agron_table)[,4])





############
# soil benefits of cropping systems
############


# import raw anova output
#inorgN <- read.csv("Model-output/lsmeans/*across years/inorgN_cropsys.csv")
NAG <- read.csv("Model-output/lsmeans/*across years/NAG_cropsys.csv")
WEC <- read.csv("Model-output/lsmeans/*across years/WEC_cropsys.csv")
WEN <- read.csv("Model-output/lsmeans/*across years/WEN_cropsys.csv")
#MAOC <- read.csv("Model-output/lsmeans/*across years/MAOC_cropsys.csv")
#EEA_C <- read.csv("Model-output/lsmeans/*across years/EEA_C_cropsys.csv")
WAS <- read.csv("Model-output/lsmeans/*across years/WAS_cropsys.csv")


# normalize means between 0 and 1
#inorgN$response_std <- (inorgN$response-min(inorgN$response))/(max(inorgN$response-min(inorgN$response)))
NAG$response_std <- (NAG$response-min(NAG$response))/(max(NAG$response-min(NAG$response)))
WEC$response_std <- (WEC$response-min(WEC$response))/(max(WEC$response-min(WEC$response)))
WEN$response_std <- (WEN$response-min(WEN$response))/(max(WEN$response-min(WEN$response)))
#MAOC$response_std <- (MAOC$response-min(MAOC$response))/(max(MAOC$response-min(MAOC$response)))
#EEA_C$response_std <- (EEA_C$response-min(EEA_C$response))/(max(EEA_C$response-min(EEA_C$response)))
WAS$response_std <- (WAS$response-min(WAS$response))/(max(WAS$response-min(WAS$response)))


# combine normalized means
soil_table <- data.frame(#inorgN = c(inorgN$response_std),
                         NAG = c(NAG$response_std),
                         WEC = c(WEC$response_std),
                         WEN = c(WEN$response_std),
                         #MAOC = c(MAOC$response_std),
                         #EEA_C = c(EEA_C$response_std),
                         WAS = c(WAS$response_std))
soil_table$sum2 <- rowSums(soil_table, na.rm = TRUE)
soil_table <- round(soil_table, 2)






# extract means 
NAG$resp_se <- paste0(round(NAG$response, 2), " ± ", round(NAG$SE, 2), " ", gsub(" ", "", NAG$.group))
WEC$resp_se <- paste0(round(WEC$response, 2), " ± ", round(WEC$SE, 2), " ", gsub(" ", "", WEC$.group))
WEN$resp_se <- paste0(round(WEN$response, 2), " ± ", round(WEN$SE, 2), " ", gsub(" ", "", WEN$.group))
WAS$resp_se <- paste0(round(WAS$response, 2), " ± ", round(WAS$SE, 2), " ", gsub(" ", "", WAS$.group))



# combine normalized means
soil_table_means <- data.frame(NAG = NAG$resp_se,
                               WEC = WEC$resp_se,
                               WEN = WEN$resp_se,
                               WAS = WAS$resp_se
                                
)
write.csv(t(soil_table_means), "Tables/means-soil-cropsys.csv")


Msoil_table_means <- as.matrix(soil_table_means)
Msoil_table <- as.matrix(as.data.frame(soil_table)[,-5])
Msoil_table_merged <- matrix( paste0(Msoil_table_means, " (", Msoil_table, ")"), 
                               nrow=nrow(Msoil_table_means), dimnames = dimnames(Msoil_table_means))

Msoil_table_merged2 <- cbind(Msoil_table_merged, as.data.frame(soil_table)[,5])




# merge tables

cropsys_table <- cbind(Magron_table_merged2, Msoil_table_merged2)
cropsys_table <- as.data.frame(cropsys_table)
cropsys_table$overallsum <- as.numeric(cropsys_table$V4) + as.numeric(cropsys_table$V9)
cropsys_table <- t(cropsys_table)



# export
write.csv(cropsys_table, "Tables/benefits-cropsys.csv")




#### make a heatmaps of standardized means

# agronomy response to cropping systems
h_agron_cropsys <- as.matrix(t(agron_table[,c(1:3)]))
colnames(h_agron_cropsys) <- c("Continuous corn", "Continuous soybean", "Corn-soybean rotation", "Corn-cotton-soybean rotation")
hh_agron_cropsys <- data.frame(value=as.vector(h_agron_cropsys),
                             Cropsys=as.factor(rep(c("Continuous corn", "Continuous soybean", "Corn-soybean rotation", "Corn-cotton-soybean rotation"), each=3)),
                             Response=as.factor(rep(rownames(h_agron_cropsys), 4)))
hh_agron_cropsys$Cropsys <- factor(hh_agron_cropsys$Cropsys, levels(hh_agron_cropsys$Cropsys)[c(1,2,4,3)])
hh_agron_cropsys$Response <- factor(hh_agron_cropsys$Response, levels(hh_agron_cropsys$Response)[c(2,1,3)])

heat_agron_cropsys <- ggplot(hh_agron_cropsys, aes(x=Cropsys, y=Response, fill=value, label=value))+
  geom_tile()+ xlab("")+ ylab("Agronomic responses") + geom_text() +
  scale_fill_gradientn(colors=c("indianred3","gray90","steelblue1"), na.value="white")+
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(hh_agron_cropsys$Response)), 
                   labels=c("weeds" =  expression(paste("Weed biomass")), 
                            "inputCN" = expression(paste("Biomass quality")),
                            "Yield" = "Standardized yield")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
heat_agron_cropsys

# soil response to cropping systems
h_soil_table <- as.matrix(t(soil_table[,c(1:4)]))
colnames(h_soil_table) <- c("Continuous corn", "Continuous soybean", "Corn-soybean rotation", "Corn-cotton-soybean rotation")
hh_soil_table <- data.frame(value=as.vector(h_soil_table),
                            Cropsys=as.factor(rep(c("Continuous corn", "Continuous soybean", "Corn-soybean rotation", "Corn-cotton-soybean rotation"), each=4)),
                            Response=as.factor(rep(rownames(h_soil_table), 4)))
hh_soil_table$Cropsys <- factor(hh_soil_table$Cropsys, levels(hh_soil_table$Cropsys)[c(1,2,4,3)])
hh_soil_table$Response <- factor(hh_soil_table$Response, levels(hh_soil_table$Response)[c(1,3,4,2)])

heat_soil_table_cropsys <- ggplot(hh_soil_table, aes(x=Cropsys, y=Response, fill=value, label=value))+
  geom_tile()+ xlab("")+ ylab("Soil health responses") + geom_text() +
  scale_fill_gradientn(colors=c("indianred3","gray90","steelblue1"), na.value="white")+
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(hh_soil_table$Response)), 
                   labels=c("NAG" = expression(paste("N-acetyl-β-glucosaminidase activity")), 
                            "WEC" = expression(paste("Water-extractable organic C")), 
                            "WEN" = expression(paste("Water-extractable N")), 
                            "WAS" = expression(paste("Aggregate stability")))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
heat_soil_table_cropsys



### show correlation between agronomy and soil benefits across cropping system treatments
sum_tab <- data.frame(agron = agron_table$sum1, 
                      soil = soil_table$sum2, 
                      Cropping.system = as.factor(c("Continuous corn", "Continuous soybean", "Corn-soybean rotation", "Corn-cotton-soybean rotation")))
sum_tab$Cropping.system <- factor(sum_tab$Cropping.system, levels(sum_tab$Cropping.system)[c(1,2,4,3)])
cropsyscols <- c("darkgoldenrod4", "darkolivegreen", "darkolivegreen3", "deepskyblue3")
rsq_cropsys<- paste("R^2 == ", round(cor(sum_tab$agron, sum_tab$soil),2))


cor_cropsys <- ggplot(sum_tab, aes(x=soil, y=agron, color=Cropping.system)) +
  theme_minimal() + labs(y="Sum of agronomic benefits", x="Sum of soil health benefits") + labs(color=NA) +
  geom_smooth(method="lm",  color="black", fill=NA, size=0.5) +
  geom_point(size=4, shape=19) +
  scale_color_manual(values = cropsyscols) + 
  ggrepel::geom_text_repel(label=sum_tab$Cropping.system, force_pull=10) +
  scale_x_continuous(breaks=seq(0,10,2), limits=c(0,10))+
  scale_y_continuous(breaks=seq(0,6,2), limits=c(0,6))+
  theme(legend.position = "none", plot.margin=margin(15,25,15,25),
        panel.background = element_rect()) +  
  annotate(geom="text", x=1, y=5.5, label=rsq_cropsys, parse = TRUE)
cor_cropsys


#### combine heatmaps and correlations
heat_agron_cover2 <- heat_agron_cover + theme(legend.position="none")
heat_soil_table_cover2 <- heat_soil_table_cover + theme(legend.position="none")
heat_soil_table_cropsys2 <- heat_soil_table_cropsys + theme(legend.position="none", plot.margin = margin(0,60,0,0))


fig_1 <- ggpubr::ggarrange(heat_agron_cover2, heat_soil_table_cover2, cor_cover, nrow=3, ncol=1, labels=c("A", "C", "E"))
fig_2 <- ggpubr::ggarrange(heat_agron_cropsys, heat_soil_table_cropsys2, cor_cropsys, nrow=3, ncol=1, labels=c("B", "D", "F"))

fig_benefits <- ggpubr::ggarrange(fig_1, fig_2, nrow=1, ncol=2)
ggpubr::ggexport(fig_benefits, height=5000, width=4200, filename = "Figures/*across years/5_benefits_1.png", res = 400)





############
# agronomic benefits of cover cropping within cropping systems
############


# import raw anova output
# foliarS <-  read.csv("Model-output/lsmeans/*across years/tissue.S.percent_interactive-effect.csv")
# foliarFe <-  read.csv("Model-output/lsmeans/*across years/tissue.Fe.ppm_interactive-effect.csv")
# foliarMn <-  read.csv("Model-output/lsmeans/*across years/tissue.Mn.ppm_interactive-effect.csv")


# split between cropping systems
# foliarS_1 <- foliarS[which(foliarS$Cropping.system=="Corn-Corn-Corn"),]
# foliarS_2 <- foliarS[which(foliarS$Cropping.system=="Soybean-Soybean-Soybean"),]
# foliarS_3 <- foliarS[which(foliarS$Cropping.system=="Corn-Soybean-Corn"),]
# foliarS_4 <- foliarS[which(foliarS$Cropping.system=="Corn-Cotton-Soybean"),]
# foliarFe_1 <- foliarFe[which(foliarFe$Cropping.system=="Corn-Corn-Corn"),]
# foliarFe_2 <- foliarFe[which(foliarFe$Cropping.system=="Soybean-Soybean-Soybean"),]
# foliarFe_3 <- foliarFe[which(foliarFe$Cropping.system=="Corn-Soybean-Corn"),]
# foliarFe_4 <- foliarFe[which(foliarFe$Cropping.system=="Corn-Cotton-Soybean"),]
# foliarMn_1 <- foliarMn[which(foliarMn$Cropping.system=="Corn-Corn-Corn"),]
# foliarMn_2 <- foliarMn[which(foliarMn$Cropping.system=="Soybean-Soybean-Soybean"),]
# foliarMn_3 <- foliarMn[which(foliarMn$Cropping.system=="Corn-Soybean-Corn"),]
# foliarMn_4 <- foliarMn[which(foliarMn$Cropping.system=="Corn-Cotton-Soybean"),]



# normalize means between 0 and 1
# foliarS_1$response_std <- rep(NA, 5); if(length(unique(foliarS_1$.group))>1){foliarS_1$response_std <- (foliarS_1$response-min(foliarS_1$response))/(max(foliarS_1$response-min(foliarS_1$response)))}
# foliarS_2$response_std <- rep(NA, 5); if(length(unique(foliarS_2$.group))>1){foliarS_2$response_std <- (foliarS_2$response-min(foliarS_2$response))/(max(foliarS_2$response-min(foliarS_2$response)))}
# foliarS_3$response_std <- rep(NA, 5); if(length(unique(foliarS_3$.group))>1){foliarS_3$response_std <- (foliarS_3$response-min(foliarS_3$response))/(max(foliarS_3$response-min(foliarS_3$response)))}
# foliarS_4$response_std <- rep(NA, 5); if(length(unique(foliarS_4$.group))>1){foliarS_4$response_std <- (foliarS_4$response-min(foliarS_4$response))/(max(foliarS_4$response-min(foliarS_4$response)))}
# foliarFe_1$response_std <- rep(NA, 5); if(length(unique(foliarFe_1$.group))>1){foliarFe_1$response_std <- (foliarFe_1$response-min(foliarFe_1$response))/(max(foliarFe_1$response-min(foliarFe_1$response)))}
# foliarFe_2$response_std <- rep(NA, 5); if(length(unique(foliarFe_2$.group))>1){foliarFe_2$response_std <- (foliarFe_2$response-min(foliarFe_2$response))/(max(foliarFe_2$response-min(foliarFe_2$response)))}
# foliarFe_3$response_std <- rep(NA, 5); if(length(unique(foliarFe_3$.group))>1){foliarFe_3$response_std <- (foliarFe_3$response-min(foliarFe_3$response))/(max(foliarFe_3$response-min(foliarFe_3$response)))}
# foliarFe_4$response_std <- rep(NA, 5); if(length(unique(foliarFe_4$.group))>1){foliarFe_4$response_std <- (foliarFe_4$response-min(foliarFe_4$response))/(max(foliarFe_4$response-min(foliarFe_4$response)))}
# foliarMn_1$response_std <- rep(NA, 5); if(length(unique(foliarMn_1$.group))>1){foliarMn_1$response_std <- (foliarMn_1$response-min(foliarMn_1$response))/(max(foliarMn_1$response-min(foliarMn_1$response)))}
# foliarMn_2$response_std <- rep(NA, 5); if(length(unique(foliarMn_2$.group))>1){foliarMn_2$response_std <- (foliarMn_2$response-min(foliarMn_2$response))/(max(foliarMn_2$response-min(foliarMn_2$response)))}
# foliarMn_3$response_std <- rep(NA, 5); if(length(unique(foliarMn_3$.group))>1){foliarMn_3$response_std <- (foliarMn_3$response-min(foliarMn_3$response))/(max(foliarMn_3$response-min(foliarMn_3$response)))}
# foliarMn_4$response_std <- rep(NA, 5); if(length(unique(foliarMn_4$.group))>1){foliarMn_4$response_std <- (foliarMn_4$response-min(foliarMn_4$response))/(max(foliarMn_4$response-min(foliarMn_4$response)))}


# combine normalized means
# agron_table <- data.frame(foliarS_1 = foliarS_1$response_std, 
#                           foliarFe_1 = foliarFe_1$response_std, 
#                           foliarMn_1 = foliarMn_1$response_std, 
#                           sum1_1 = rep(NA, 5),
#                           foliarS_2 = foliarS_2$response_std, 
#                           foliarFe_2 = foliarFe_2$response_std, 
#                           foliarMn_2 = foliarMn_2$response_std, 
#                           sum1_2 = rep(NA, 5),
#                           foliarS_3 = foliarS_3$response_std, 
#                           foliarFe_3 = foliarFe_3$response_std, 
#                           foliarMn_3 = foliarMn_3$response_std, 
#                           sum1_3 = rep(NA, 5),
#                           foliarS_4 = foliarS_4$response_std,
#                           foliarFe_4 = foliarFe_4$response_std,
#                           foliarMn_4 = foliarMn_4$response_std,
#                           sum1_4 = rep(NA, 5))
# agron_table$sum1_1 <- rowSums(agron_table[,1:3], na.rm = TRUE)
# agron_table$sum1_2 <- rowSums(agron_table[,5:7], na.rm = TRUE)
# agron_table$sum1_3 <- rowSums(agron_table[,9:11], na.rm = TRUE)
# agron_table$sum1_4 <- rowSums(agron_table[,13:15], na.rm = TRUE)
# agron_table <- round(agron_table, 2)




############
# soil benefits of cover cropping within cropping systems
############


# import raw anova output
orgC <-  read.csv("Model-output/lsmeans/*across years/orgC_interactive-effect.csv")
totN <-  read.csv("Model-output/lsmeans/*across years/totN_interactive-effect.csv")
#EEA_C <-  read.csv("Model-output/lsmeans/*across years/EEA_C_interactive-effect.csv")


# split between cropping systems
orgC_1 <- orgC[which(orgC$Cropping.system=="Corn-Corn-Corn"),]
orgC_2 <- orgC[which(orgC$Cropping.system=="Soybean-Soybean-Soybean"),]
orgC_3 <- orgC[which(orgC$Cropping.system=="Corn-Soybean-Corn"),]
orgC_4 <- orgC[which(orgC$Cropping.system=="Corn-Cotton-Soybean"),]
totN_1 <- totN[which(totN$Cropping.system=="Corn-Corn-Corn"),]
totN_2 <- totN[which(totN$Cropping.system=="Soybean-Soybean-Soybean"),]
totN_3 <- totN[which(totN$Cropping.system=="Corn-Soybean-Corn"),]
totN_4 <- totN[which(totN$Cropping.system=="Corn-Cotton-Soybean"),]
# EEA_C_1 <- EEA_C[which(EEA_C$Cropping.system=="Corn-Corn-Corn"),]
# EEA_C_2 <- EEA_C[which(EEA_C$Cropping.system=="Soybean-Soybean-Soybean"),]
# EEA_C_3 <- EEA_C[which(EEA_C$Cropping.system=="Corn-Soybean-Corn"),]
# EEA_C_4 <- EEA_C[which(EEA_C$Cropping.system=="Corn-Cotton-Soybean"),]


# normalize means between 0 and 1
orgC_1$response_std <- rep(NA, 5); if(length(unique(orgC_1$.group))>1){orgC_1$response_std <- (orgC_1$response-min(orgC_1$response))/(max(orgC_1$response-min(orgC_1$response)))}
orgC_2$response_std <- rep(NA, 5); if(length(unique(orgC_2$.group))>1){orgC_2$response_std <- (orgC_2$response-min(orgC_2$response))/(max(orgC_2$response-min(orgC_2$response)))}
orgC_3$response_std <- rep(NA, 5); if(length(unique(orgC_3$.group))>1){orgC_3$response_std <- (orgC_3$response-min(orgC_3$response))/(max(orgC_3$response-min(orgC_3$response)))}
orgC_4$response_std <- rep(NA, 5); if(length(unique(orgC_4$.group))>1){orgC_4$response_std <- (orgC_4$response-min(orgC_4$response))/(max(orgC_4$response-min(orgC_4$response)))}
totN_1$response_std <- rep(NA, 5); if(length(unique(totN_1$.group))>1){totN_1$response_std <- (totN_1$response-min(totN_1$response))/(max(totN_1$response-min(totN_1$response)))}
totN_2$response_std <- rep(NA, 5); if(length(unique(totN_2$.group))>1){totN_2$response_std <- (totN_2$response-min(totN_2$response))/(max(totN_2$response-min(totN_2$response)))}
totN_3$response_std <- rep(NA, 5); if(length(unique(totN_3$.group))>1){totN_3$response_std <- (totN_3$response-min(totN_3$response))/(max(totN_3$response-min(totN_3$response)))}
totN_4$response_std <- rep(NA, 5); if(length(unique(totN_4$.group))>1){totN_4$response_std <- (totN_4$response-min(totN_4$response))/(max(totN_4$response-min(totN_4$response)))}
# EEA_C_1$response_std <- rep(NA, 5); if(length(unique(EEA_C_1$.group))>1){EEA_C_1$response_std <- (EEA_C_1$response-min(EEA_C_1$response))/(max(EEA_C_1$response-min(EEA_C_1$response)))}
# EEA_C_2$response_std <- rep(NA, 5); if(length(unique(EEA_C_2$.group))>1){EEA_C_2$response_std <- (EEA_C_2$response-min(EEA_C_2$response))/(max(EEA_C_2$response-min(EEA_C_2$response)))}
# EEA_C_3$response_std <- rep(NA, 5); if(length(unique(EEA_C_3$.group))>1){EEA_C_3$response_std <- (EEA_C_3$response-min(EEA_C_3$response))/(max(EEA_C_3$response-min(EEA_C_3$response)))}
# EEA_C_4$response_std <- rep(NA, 5); if(length(unique(EEA_C_4$.group))>1){EEA_C_4$response_std <- (EEA_C_4$response-min(EEA_C_4$response))/(max(EEA_C_4$response-min(EEA_C_4$response)))}


# combine normalized means
soil_table <- data.frame(orgC_1 = orgC_1$response_std, 
                         totN_1 = totN_1$response_std, 
                         #EEA_C_1 = EEA_C_1$response_std, 
                         sum2_1 = rep(NA, 5),
                         orgC_2 = orgC_2$response_std, 
                         totN_2 = totN_2$response_std, 
                         #EEA_C_2 = EEA_C_2$response_std, 
                         sum2_2 = rep(NA, 5),
                         orgC_3 = orgC_3$response_std, 
                         totN_3 = totN_3$response_std, 
                         #EEA_C_3 = EEA_C_3$response_std, 
                         sum2_3 = rep(NA, 5),
                         orgC_4 = orgC_4$response_std,
                         totN_4 = totN_4$response_std,
                         #EEA_C_4 = EEA_C_4$response_std,
                         sum2_4 = rep(NA, 5))
soil_table$sum2_1 <- rowSums(soil_table[,1:2], na.rm = TRUE)
soil_table$sum2_2 <- rowSums(soil_table[,4:5], na.rm = TRUE)
soil_table$sum2_3 <- rowSums(soil_table[,7:8], na.rm = TRUE)
soil_table$sum2_4 <- rowSums(soil_table[,10:11], na.rm = TRUE)
soil_table <- round(soil_table, 2)




# extract means 
orgC_1$resp_se <- paste0(round(orgC_1$response, 2), " ± ", round(orgC_1$SE, 2), " ", gsub(" ", "", orgC_1$.group))
orgC_2$resp_se <- paste0(round(orgC_2$response, 2), " ± ", round(orgC_2$SE, 2), " ", gsub(" ", "", orgC_2$.group))
orgC_3$resp_se <- paste0(round(orgC_3$response, 2), " ± ", round(orgC_3$SE, 2), " ", gsub(" ", "", orgC_3$.group))
orgC_4$resp_se <- paste0(round(orgC_4$response, 2), " ± ", round(orgC_4$SE, 2), " ", gsub(" ", "", orgC_4$.group))

totN_1$resp_se <- paste0(round(totN_1$response, 2), " ± ", round(totN_1$SE, 2), " ", gsub(" ", "", totN_1$.group))
totN_2$resp_se <- paste0(round(totN_2$response, 2), " ± ", round(totN_2$SE, 2), " ", gsub(" ", "", totN_2$.group))
totN_3$resp_se <- paste0(round(totN_3$response, 2), " ± ", round(totN_3$SE, 2), " ", gsub(" ", "", totN_3$.group))
totN_4$resp_se <- paste0(round(totN_4$response, 2), " ± ", round(totN_4$SE, 2), " ", gsub(" ", "", totN_4$.group))


# combine normalized means
soil_table_means <- data.frame(orgC_1 = orgC_1$resp_se,
                               orgC_2 = orgC_2$resp_se,
                               orgC_3 = orgC_3$resp_se,
                               orgC_4 = orgC_4$resp_se,
                               totN_1 = totN_1$resp_se,
                               totN_2 = totN_2$resp_se,
                               totN_3 = totN_3$resp_se,
                               totN_4 = totN_4$resp_se                              
)
write.csv(t(soil_table_means), "Tables/means-soil-cover-cropsys.csv")


Msoil_table_means <- as.matrix(soil_table_means)
Msoil_table <- as.matrix(as.data.frame(soil_table)[,c(1,4,7,10,2,5,8,11)])
Msoil_table_merged <- matrix( paste0(Msoil_table_means, " (", Msoil_table, ")"), 
                              nrow=nrow(Msoil_table_means), dimnames = dimnames(Msoil_table_means))

Msoil_table_merged <- as.data.frame(Msoil_table_merged)
Msoil_table_merged2 <- data.frame(orgC = c(Msoil_table_merged$orgC_1, Msoil_table_merged$orgC_2, Msoil_table_merged$orgC_3, Msoil_table_merged$orgC_4),
                                  totN = c(Msoil_table_merged$totN_1, Msoil_table_merged$totN_2, Msoil_table_merged$totN_3, Msoil_table_merged$totN_4), 
                                  sums = c(soil_table$sum2_1, soil_table$sum2_2, soil_table$sum2_3, soil_table$sum2_4))




# merge tables

cropsys_table <- t(Msoil_table_merged2)


# export
write.csv(cropsys_table, "Tables/benefits-cover-cropsys.csv")





####################
# foliar nutrient response to cover cropping
############


# import raw anova output
foliarP <-  read.csv("Model-output/lsmeans/*across years/tissue.P.percent_cover.csv")
foliarB <-  read.csv("Model-output/lsmeans/*across years/tissue.B.ppm_cover.csv")
foliarCu <-  read.csv("Model-output/lsmeans/*across years/tissue.Cu.ppm_cover.csv")
foliarZn <-  read.csv("Model-output/lsmeans/*across years/tissue.Zn.ppm_cover.csv")





# extract means 
foliarP$resp_se <- paste0(round(foliarP$response, 2), " ± ", round(foliarP$SE, 2), " ", gsub(" ", "", foliarP$.group))
foliarB$resp_se <- paste0(round(foliarB$response, 2), " ± ", round(foliarB$SE, 2), " ", gsub(" ", "", foliarB$.group))
foliarCu$resp_se <- paste0(round(foliarCu$response, 2), " ± ", round(foliarCu$SE, 2), " ", gsub(" ", "", foliarCu$.group))
foliarZn$resp_se <- paste0(round(foliarZn$response, 2), " ± ", round(foliarZn$SE, 2), " ", gsub(" ", "", foliarZn$.group))


# combine normalized means
agron_table_means <- data.frame(foliarP = foliarP$resp_se, 
                                foliarB = foliarB$resp_se, 
                                foliarCu = foliarCu$resp_se, 
                                foliarZn = foliarZn$resp_se)
write.csv(t(agron_table_means), "Tables/means-foliar nutrients-cover.csv")



####################
# foliar nutrient response to cropping system
############


# import raw anova output
foliarN <-  read.csv("Model-output/lsmeans/*across years/tissue.N.percent_cropsys.csv")
foliarP <-  read.csv("Model-output/lsmeans/*across years/tissue.P.percent_cropsys.csv")
foliarK <-  read.csv("Model-output/lsmeans/*across years/tissue.K.percent_cropsys.csv")
foliarCa <-  read.csv("Model-output/lsmeans/*across years/tissue.Ca.percent_cropsys.csv")
foliarMg <-  read.csv("Model-output/lsmeans/*across years/tissue.Mg.percent_cropsys.csv")
foliarB <-  read.csv("Model-output/lsmeans/*across years/tissue.B.ppm_cropsys.csv")
foliarFe <-  read.csv("Model-output/lsmeans/*across years/tissue.Fe.ppm_cropsys.csv")
foliarMn <-  read.csv("Model-output/lsmeans/*across years/tissue.Mn.ppm_cropsys.csv")
foliarZn <-  read.csv("Model-output/lsmeans/*across years/tissue.Zn.ppm_cropsys.csv")


# extract means 
foliarN$resp_se <- paste0(round(foliarN$response, 2), " ± ", round(foliarN$SE, 2), " ", gsub(" ", "", foliarN$.group))
foliarP$resp_se <- paste0(round(foliarP$response, 2), " ± ", round(foliarP$SE, 2), " ", gsub(" ", "", foliarP$.group))
foliarK$resp_se <- paste0(round(foliarK$response, 2), " ± ", round(foliarK$SE, 2), " ", gsub(" ", "", foliarK$.group))
foliarCa$resp_se <- paste0(round(foliarCa$response, 2), " ± ", round(foliarCa$SE, 2), " ", gsub(" ", "", foliarCa$.group))
foliarMg$resp_se <- paste0(round(foliarMg$response, 2), " ± ", round(foliarMg$SE, 2), " ", gsub(" ", "", foliarMg$.group))
foliarB$resp_se <- paste0(round(foliarB$response, 2), " ± ", round(foliarB$SE, 2), " ", gsub(" ", "", foliarB$.group))
foliarFe$resp_se <- paste0(round(foliarFe$response, 2), " ± ", round(foliarFe$SE, 2), " ", gsub(" ", "", foliarFe$.group))
foliarMn$resp_se <- paste0(round(foliarMn$response, 2), " ± ", round(foliarMn$SE, 2), " ", gsub(" ", "", foliarMn$.group))
foliarZn$resp_se <- paste0(round(foliarZn$response, 2), " ± ", round(foliarZn$SE, 2), " ", gsub(" ", "", foliarZn$.group))


# combine normalized means
agron_table_means <- data.frame(foliarN = foliarN$resp_se, 
                                foliarP = foliarP$resp_se, 
                                foliarK = foliarK$resp_se, 
                                foliarCa = foliarCa$resp_se, 
                                foliarMg = foliarMg$resp_se, 
                                foliarB = foliarB$resp_se, 
                                foliarFe = foliarFe$resp_se, 
                                foliarMn = foliarMn$resp_se, 
                                foliarZn = foliarZn$resp_se)
write.csv(t(agron_table_means), "Tables/means-foliar nutrients-cropsys.csv")

