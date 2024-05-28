
library(dplyr)
library(tidyr)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



############
# cover crop performance
############


# import raw anova output
agb <- read.csv("Model-output/anova/*across years/agb.csv")
weeds <- read.csv("Model-output/anova/*across years/weeds.csv")
inputC <- read.csv("Model-output/anova/*across years/inputC.csv")
inputN <- read.csv("Model-output/anova/*across years/inputN.csv")
inputCN <- read.csv("Model-output/anova/*across years/inputCN.csv")


# create names for anova output
agb$name <- rep("agb", dim(agb)[1])
weeds$name <- rep("weeds", dim(weeds)[1])
inputC$name <- rep("inputC", dim(inputC)[1])
inputN$name <- rep("inputN", dim(inputN)[1])
inputCN$name <- rep("inputCN", dim(inputCN)[1])

# combine anova output
agron_table <- dplyr::bind_rows(agb, weeds, inputC, inputN, inputCN)
agron_table$stat <- paste0("X2=", agron_table$Chisq, ", Df=", agron_table$Df, ", P=", agron_table$Pr..Chisq.)
agron_table$stat2 <- paste0("P=", agron_table$Pr..Chisq.)

# replace P=0 with P<0.001
agron_table$p_indic <- substrRight(agron_table$stat, 3)
agron_table$stat[which(agron_table$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=agron_table$stat[which(agron_table$p_indic=="P=0")])
agron_table$stat2[which(agron_table$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=agron_table$stat2[which(agron_table$p_indic=="P=0")])

# reshape
agron_table_a <- bind_cols(agron_table[which(agron_table$X=="Cover"),],
                           agron_table[which(agron_table$X=="Cropping.system"),], 
                           agron_table[which(agron_table$X=="Cover:Cropping.system"),])
agron_table_b <- data.frame(Cover=agron_table_a$stat...6, 
                            Cropping.system=agron_table_a$stat...14,
                            Cover.Cropping.system=agron_table_a$stat...22)
agron_table_c <- data.frame(Cover=agron_table_a$stat2...7, 
                            Cropping.system=agron_table_a$stat2...15,
                            Cover.Cropping.system=agron_table_a$stat2...23)

rownames(agron_table_b) <- c("agb", "weeds", "inputN", "inputC", "inputCN")
rownames(agron_table_c) <- c("agb", "weeds", "inputN", "inputC", "inputCN")

# r squares
agb_R <- read.csv("Model-output/anova/*across years/agb_rsq.csv")
weeds_R <- read.csv("Model-output/anova/*across years/weeds_rsq.csv")
inputC_R <- read.csv("Model-output/anova/*across years/inputC_rsq.csv")
inputN_R <- read.csv("Model-output/anova/*across years/inputN_rsq.csv")
inputCN_R <- read.csv("Model-output/anova/*across years/inputCN_rsq.csv")

agron_table_b$R2 <- c(paste0(agb_R[2], ", ", agb_R[3]),
                      paste0(weeds_R[2], ", ", weeds_R[3]),
                      paste0(inputC_R[2], ", ", inputC_R[3]),
                      paste0(inputN_R[2], ", ", inputN_R[3]),
                      paste0(inputCN_R[2], ", ", inputCN_R[3]))
agron_table_c$R2 <- c(paste0(agb_R[2], ", ", agb_R[3]),
                      paste0(weeds_R[2], ", ", weeds_R[3]),
                      paste0(inputC_R[2], ", ", inputC_R[3]),
                      paste0(inputN_R[2], ", ", inputN_R[3]),
                      paste0(inputCN_R[2], ", ", inputCN_R[3]))
# export
write.csv(agron_table_b, "Tables/agronomy_anova.csv")
write.csv(agron_table_c, "Tables/agronomy_anova2.csv")





############
# cash crop performance
############


# import raw anova output
tissue.N.percent <- read.csv("Model-output/anova/*across years/tissue.N.percent.csv")
tissue.P.percent <- read.csv("Model-output/anova/*across years/tissue.P.percent.csv")
tissue.K.percent <- read.csv("Model-output/anova/*across years/tissue.K.percent.csv")
tissue.Ca.percent <- read.csv("Model-output/anova/*across years/tissue.Ca.percent.csv")
tissue.Mg.percent <- read.csv("Model-output/anova/*across years/tissue.Mg.percent.csv")
tissue.S.percent <- read.csv("Model-output/anova/*across years/tissue.S.percent.csv")
tissue.B.ppm <- read.csv("Model-output/anova/*across years/tissue.B.ppm.csv")
tissue.Fe.ppm <- read.csv("Model-output/anova/*across years/tissue.Fe.ppm.csv")
tissue.Mn.ppm <- read.csv("Model-output/anova/*across years/tissue.Mn.ppm.csv")
tissue.Cu.ppm <- read.csv("Model-output/anova/*across years/tissue.Cu.ppm.csv")
tissue.Zn.ppm <- read.csv("Model-output/anova/*across years/tissue.Zn.ppm.csv")
#tissue.Na.ppm <- read.csv("Model-output/anova/*across years/tissue.Na.ppm.csv")
Yield <- read.csv("Model-output/anova/*across years/Yield_std.csv")

# create names for anova output
tissue.N.percent$name <- rep("tissue.N.percent", dim(tissue.N.percent)[1])
tissue.P.percent$name <- rep("tissue.P.percent ", dim(tissue.P.percent )[1])
tissue.K.percent$name <- rep("tissue.K.percent ", dim(tissue.K.percent )[1])
tissue.Ca.percent$name <- rep("tissue.Ca.percent ", dim(tissue.Ca.percent )[1])
tissue.Mg.percent$name <- rep("tissue.Mg.percent ", dim(tissue.Mg.percent )[1])
tissue.S.percent$name <- rep("tissue.S.percent ", dim(tissue.S.percent )[1])
tissue.B.ppm$name <- rep("tissue.B.ppm ", dim(tissue.B.ppm )[1])
tissue.Fe.ppm$name <- rep("tissue.Fe.ppm ", dim(tissue.Fe.ppm )[1])
tissue.Mn.ppm$name <- rep("tissue.Mn.ppm ", dim(tissue.Mn.ppm )[1])
tissue.Cu.ppm$name <- rep("tissue.Cu.ppm ", dim(tissue.Cu.ppm )[1])
tissue.Zn.ppm$name <- rep("tissue.Zn.ppm ", dim(tissue.Zn.ppm )[1])
#tissue.Na.ppm$name <- rep("tissue.Na.ppm ", dim(tissue.Na.ppm )[1])
Yield$name <- rep("Yield", dim(Yield)[1])


# combine anova output
agron_table2 <- dplyr::bind_rows(tissue.N.percent, tissue.P.percent, tissue.K.percent, 
                                 tissue.Ca.percent, tissue.Mg.percent, tissue.S.percent, 
                                 tissue.B.ppm, tissue.Fe.ppm, tissue.Mn.ppm, tissue.Cu.ppm,
                                 tissue.Zn.ppm, Yield)
agron_table2$stat <- paste0("X2=", agron_table2$Chisq, ", Df=", agron_table2$Df, ", P=", agron_table2$Pr..Chisq.)
agron_table2$stat2 <- paste0("P=", agron_table2$Pr..Chisq.)

# replace P=0 with P<0.001
agron_table2$p_indic <- substrRight(agron_table2$stat, 3)
agron_table2$stat[which(agron_table2$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=agron_table2$stat[which(agron_table2$p_indic=="P=0")])
agron_table2$stat2[which(agron_table2$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=agron_table2$stat2[which(agron_table2$p_indic=="P=0")])

# reshape
agron_table2_a <- bind_cols(agron_table2[which(agron_table2$X=="Cover"),],
                           agron_table2[which(agron_table2$X=="Cropping.system"),], 
                           agron_table2[which(agron_table2$X=="Cover:Cropping.system"),])
agron_table2_b <- data.frame(Cover=agron_table2_a$stat...6, 
                            Cropping.system=agron_table2_a$stat...14,
                            Cover.Cropping.system=agron_table2_a$stat...22)
agron_table2_c <- data.frame(Cover=agron_table2_a$stat2...7, 
                             Cropping.system=agron_table2_a$stat2...15,
                             Cover.Cropping.system=agron_table2_a$stat2...23)


rownames(agron_table2_b) <- c("N", "P", "K", "Ca", "Mg","S", 
                              "B", "Fe", "Mn", "Cu", "Zn", "Yield")
rownames(agron_table2_c) <- c("N", "P", "K", "Ca", "Mg","S", 
                              "B", "Fe", "Mn", "Cu", "Zn", "Yield")

# r squares
tissue.N.percent_R <- read.csv("Model-output/anova/*across years/tissue.N.percent_rsq.csv")
tissue.P.percent_R <- read.csv("Model-output/anova/*across years/tissue.P.percent_rsq.csv")
tissue.K.percent_R <- read.csv("Model-output/anova/*across years/tissue.K.percent_rsq.csv")
tissue.Ca.percent_R <- read.csv("Model-output/anova/*across years/tissue.Ca.percent_rsq.csv")
tissue.Mg.percent_R <- read.csv("Model-output/anova/*across years/tissue.Mg.percent_rsq.csv")
tissue.S.percent_R <- read.csv("Model-output/anova/*across years/tissue.S.percent_rsq.csv")
tissue.B.ppm_R <- read.csv("Model-output/anova/*across years/tissue.B.ppm_rsq.csv")
tissue.Fe.ppm_R <- read.csv("Model-output/anova/*across years/tissue.Fe.ppm_rsq.csv")
tissue.Mn.ppm_R <- read.csv("Model-output/anova/*across years/tissue.Mn.ppm_rsq.csv")
tissue.Cu.ppm_R <- read.csv("Model-output/anova/*across years/tissue.Cu.ppm_rsq.csv")
tissue.Zn.ppm_R <- read.csv("Model-output/anova/*across years/tissue.Zn.ppm_rsq.csv")
Yield_std_R <- read.csv("Model-output/anova/*across years/Yield_std_rsq.csv")

agron_table2_b$R2 <- c(paste0(tissue.N.percent_R[2], ", ", tissue.N.percent_R[3]),
                      paste0(tissue.P.percent_R[2], ", ", tissue.P.percent_R[3]),
                      paste0(tissue.K.percent_R[2], ", ", tissue.K.percent_R[3]),
                      paste0(tissue.Ca.percent_R[2], ", ", tissue.Ca.percent_R[3]),
                      paste0(tissue.Mg.percent_R[2], ", ", tissue.Mg.percent_R[3]),
                      paste0(tissue.S.percent_R[2], ", ", tissue.S.percent_R[3]),
                      paste0(tissue.B.ppm_R[2], ", ", tissue.B.ppm_R[3]),
                      paste0(tissue.Fe.ppm_R[2], ", ", tissue.Fe.ppm_R[3]),
                      paste0(tissue.Mn.ppm_R[2], ", ", tissue.Mn.ppm_R[3]),
                      paste0(tissue.Cu.ppm_R[2], ", ", tissue.Cu.ppm_R[3]),
                      paste0(tissue.Zn.ppm_R[2], ", ", tissue.Zn.ppm_R[3]),
                      paste0(Yield_std_R[2], ", ", Yield_std_R[3]))
agron_table2_c$R2 <- c(paste0(tissue.N.percent_R[2], ", ", tissue.N.percent_R[3]),
                       paste0(tissue.P.percent_R[2], ", ", tissue.P.percent_R[3]),
                       paste0(tissue.K.percent_R[2], ", ", tissue.K.percent_R[3]),
                       paste0(tissue.Ca.percent_R[2], ", ", tissue.Ca.percent_R[3]),
                       paste0(tissue.Mg.percent_R[2], ", ", tissue.Mg.percent_R[3]),
                       paste0(tissue.S.percent_R[2], ", ", tissue.S.percent_R[3]),
                       paste0(tissue.B.ppm_R[2], ", ", tissue.B.ppm_R[3]),
                       paste0(tissue.Fe.ppm_R[2], ", ", tissue.Fe.ppm_R[3]),
                       paste0(tissue.Mn.ppm_R[2], ", ", tissue.Mn.ppm_R[3]),
                       paste0(tissue.Cu.ppm_R[2], ", ", tissue.Cu.ppm_R[3]),
                       paste0(tissue.Zn.ppm_R[2], ", ", tissue.Zn.ppm_R[3]),
                       paste0(Yield_std_R[2], ", ", Yield_std_R[3]))

# export
write.csv(agron_table2_b, "Tables/agronomy2_anova.csv")
write.csv(agron_table2_c, "Tables/agronomy2_anova2.csv")






############
# short term soil health
############


# import raw anova output
inorgN <- read.csv("Model-output/anova/*across years/inorgN.csv")
GMC <- read.csv("Model-output/anova/*across years/GMC.csv")
BG <- read.csv("Model-output/anova/*across years/BG.csv")
NAG <- read.csv("Model-output/anova/*across years/NAG.csv")
BG_NAG <- read.csv("Model-output/anova/*across years/BG_NAG.csv")
PHOS <- read.csv("Model-output/anova/*across years/PHOS.csv")
WEC <- read.csv("Model-output/anova/*across years/WEC.csv")
WEN <- read.csv("Model-output/anova/*across years/WEN.csv")
MBC <- read.csv("Model-output/anova/*across years/MBC.csv")

# create names for anova output
inorgN$name <- rep("inorgN", dim(inorgN)[1])
GMC$name <- rep("GMC", dim(GMC)[1])
BG$name <- rep("BG", dim(BG)[1])
NAG$name <- rep("NAG", dim(NAG)[1])
PHOS$name <- rep("PHOS", dim(PHOS)[1])
WEC$name <- rep("WEC", dim(WEC)[1])
WEN$name <- rep("WEN", dim(WEN)[1])
MBC$name <- rep("MBC", dim(MBC)[1])


# combine anova output
shi_table <- dplyr::bind_rows(inorgN, GMC, BG, 
                              NAG, PHOS, WEC, 
                              WEN, MBC)
shi_table$stat <- paste0("X2=", shi_table$Chisq, ", Df=", shi_table$Df, ", P=", shi_table$Pr..Chisq.)
shi_table$stat2 <- paste0("P=", shi_table$Pr..Chisq.)

# replace P=0 with P<0.001
shi_table$p_indic <- substrRight(shi_table$stat, 3)
shi_table$stat[which(shi_table$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=shi_table$stat[which(shi_table$p_indic=="P=0")])
shi_table$stat2[which(shi_table$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=shi_table$stat2[which(shi_table$p_indic=="P=0")])

# reshape
shi_table_a <- bind_cols(shi_table[which(shi_table$X=="Cover"),],
                            shi_table[which(shi_table$X=="Cropping.system"),], 
                            shi_table[which(shi_table$X=="Cover:Cropping.system"),])
shi_table_b <- data.frame(Cover=shi_table_a$stat...6, 
                             Cropping.system=shi_table_a$stat...14,
                             Cover.Cropping.system=shi_table_a$stat...22)
shi_table_c <- data.frame(Cover=shi_table_a$stat2...7, 
                          Cropping.system=shi_table_a$stat2...15,
                          Cover.Cropping.system=shi_table_a$stat2...23)

rownames(shi_table_b) <- c("inorgN", "GMC", "BG", "NAG", "PHOS", "WEC", "WEN", "MBC")
rownames(shi_table_c) <- c("inorgN", "GMC", "BG", "NAG", "PHOS", "WEC", "WEN", "MBC")

# r squares
inorgN_R <- read.csv("Model-output/anova/*across years/inorgN_rsq.csv")
GMC_R <- read.csv("Model-output/anova/*across years/GMC_rsq.csv")
BG_R <- read.csv("Model-output/anova/*across years/BG_rsq.csv")
NAG_R <- read.csv("Model-output/anova/*across years/NAG_rsq.csv")
PHOS_R <- read.csv("Model-output/anova/*across years/PHOS_rsq.csv")
WEC_R <- read.csv("Model-output/anova/*across years/WEC_rsq.csv")
WEN_R <- read.csv("Model-output/anova/*across years/WEN_rsq.csv")
MBC_R <- read.csv("Model-output/anova/*across years/MBC_rsq.csv")


shi_table_b$R2 <- c(paste0(inorgN_R[2], ", ", inorgN_R[3]),
                    paste0(GMC_R[2], ", ", GMC_R[3]),
                    paste0(BG_R[2], ", ", BG_R[3]),
                    paste0(NAG_R[2], ", ", NAG_R[3]),
                    paste0(PHOS_R[2], ", ", PHOS_R[3]),
                    paste0(WEC_R[2], ", ", WEC_R[3]),
                    paste0(WEN_R[2], ", ", WEN_R[3]),
                    paste0(MBC_R[2], ", ", MBC_R[3]))
shi_table_c$R2 <- c(paste0(inorgN_R[2], ", ", inorgN_R[3]),
                    paste0(GMC_R[2], ", ", GMC_R[3]),
                    paste0(BG_R[2], ", ", BG_R[3]),
                    paste0(NAG_R[2], ", ", NAG_R[3]),
                    paste0(PHOS_R[2], ", ", PHOS_R[3]),
                    paste0(WEC_R[2], ", ", WEC_R[3]),
                    paste0(WEN_R[2], ", ", WEN_R[3]),
                    paste0(MBC_R[2], ", ", MBC_R[3]))
# export
write.csv(shi_table_b, "Tables/shi1_anova.csv")
write.csv(shi_table_c, "Tables/shi1_anova2.csv")












############
# long term soil health
############


# import raw anova output
orgC <- read.csv("Model-output/anova/*across years/orgC.csv")
totN <- read.csv("Model-output/anova/*across years/totN.csv")
POC <- read.csv("Model-output/anova/*across years/POC.csv")
MAOC <- read.csv("Model-output/anova/*across years/MAOC.csv")
EEA_C <- read.csv("Model-output/anova/*across years/EEA_C.csv")
EEA_N <- read.csv("Model-output/anova/*across years/EEA_N.csv")
WAS <- read.csv("Model-output/anova/*across years/WAS.csv")


# create names for anova output
orgC$name <- rep("orgC", dim(orgC)[1])
totN$name <- rep("totN", dim(totN)[1])
POC$name <- rep("POC", dim(POC)[1])
MAOC$name <- rep("MAOC", dim(MAOC)[1])
EEA_C$name <- rep("EEA_C", dim(EEA_C)[1])
EEA_N$name <- rep("EEA_N", dim(EEA_N)[1])
WAS$name <- rep("WAS", dim(WAS)[1])


# combine anova output
shi_table2 <- dplyr::bind_rows(orgC, totN, POC, 
                              MAOC, EEA_C, EEA_N, 
                              WAS)
shi_table2$stat <- paste0("X2=", shi_table2$Chisq, ", Df=", shi_table2$Df, ", P=", shi_table2$Pr..Chisq.)
shi_table2$stat2 <- paste0("P=", shi_table2$Pr..Chisq.)

# replace P=0 with P<0.001
shi_table2$p_indic <- substrRight(shi_table2$stat, 3)
shi_table2$stat[which(shi_table2$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=shi_table2$stat[which(shi_table2$p_indic=="P=0")])
shi_table2$stat2[which(shi_table2$p_indic=="P=0")] <- gsub(pattern="P=0", replacement="P<0.001",x=shi_table2$stat2[which(shi_table2$p_indic=="P=0")])

# reshape
shi_table2_a <- bind_cols(shi_table2[which(shi_table2$X=="Cover"),],
                         shi_table2[which(shi_table2$X=="Cropping.system"),], 
                         shi_table2[which(shi_table2$X=="Cover:Cropping.system"),])
shi_table2_b <- data.frame(Cover=shi_table2_a$stat...6, 
                          Cropping.system=shi_table2_a$stat...14,
                          Cover.Cropping.system=shi_table2_a$stat...22)
shi_table2_c <- data.frame(Cover=shi_table2_a$stat2...7, 
                           Cropping.system=shi_table2_a$stat2...15,
                           Cover.Cropping.system=shi_table2_a$stat2...23)


rownames(shi_table2_b) <- c("orgC", "totN", "POC", "MAOC", "EEA_C", "EEA_N", "WAS")
rownames(shi_table2_c) <- c("orgC", "totN", "POC", "MAOC", "EEA_C", "EEA_N", "WAS")

# r squares
orgC_R <- read.csv("Model-output/anova/*across years/orgC_rsq.csv")
totN_R <- read.csv("Model-output/anova/*across years/totN_rsq.csv")
POC_R <- read.csv("Model-output/anova/*across years/POC_rsq.csv")
MAOC_R <- read.csv("Model-output/anova/*across years/MAOC_rsq.csv")
EEA_C_R <- read.csv("Model-output/anova/*across years/EEA_C_rsq.csv")
EEA_N_R <- read.csv("Model-output/anova/*across years/EEA_N_rsq.csv")
WAS_R <- read.csv("Model-output/anova/*across years/WAS_rsq.csv")



shi_table2_b$R2 <- c(paste0(orgC_R[2], ", ", orgC_R[3]),
                     paste0(totN_R[2], ", ", totN_R[3]),
                     paste0(POC_R[2], ", ", POC_R[3]),
                     paste0(MAOC_R[2], ", ", MAOC_R[3]),
                     paste0(EEA_C_R[2], ", ", EEA_C_R[3]),
                     paste0(EEA_N_R[2], ", ", EEA_N_R[3]),
                     paste0(WAS_R[2], ", ", WAS_R[3]))
shi_table2_c$R2 <- c(paste0(orgC_R[2], ", ", orgC_R[3]),
                     paste0(totN_R[2], ", ", totN_R[3]),
                     paste0(POC_R[2], ", ", POC_R[3]),
                     paste0(MAOC_R[2], ", ", MAOC_R[3]),
                     paste0(EEA_C_R[2], ", ", EEA_C_R[3]),
                     paste0(EEA_N_R[2], ", ", EEA_N_R[3]),
                     paste0(WAS_R[2], ", ", WAS_R[3]))

# export
write.csv(shi_table2_b, "Tables/shi2_anova.csv")
write.csv(shi_table2_c, "Tables/shi2_anova2.csv")

