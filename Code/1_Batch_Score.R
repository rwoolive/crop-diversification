# use SHAPE to determine if SOC values could be much higher given inherent site properties 
# (climate and soil class) Nunes et al. 2021, https://github.com/paparker/SHAPE/tree/main/R 


library(readr)
library(ggplot2)
library(dplyr)

### This script will batch score soil SOC data using the SHAPE procedure
  ## The input data should be changed to generate your own scores

dataIn0 <- read_csv('Processed-data/dataIn.csv') ## Read in your own data with same format
  ## Columns should be:
    # MATC: (numeric) mean annual temp. in degrees Celsius
    # MAPmm: (numeric) mean annual precipitation in milimeters
    # Texture: (character/text) Texture category...one of (T1, T2, T3, T4, T5)
    # Suborder: (character/text) Suborder category...one of (S2, S3, S4, S5)
        # Note that S1 is not part of the initial SHAPE modelling approach...to be done separately
    # SOC: (numeric) soil organic carbon as a percentage (e.g. 3% -> 3)
dataIn <- dataIn0[,2:6]
descriptions <- as.data.frame(dataIn0[,1])[,1]
##########################################
##### Leave the remaining code as is #####
##########################################

## Read in model output
mod <- readr::read_rds('Processed-data/logitMod.rds')

## Lookups
if(mod$Transform=="logit"){ soc <- qlogis(dataIn$SOC/100)} else soc <- dataIn$SOC
grp <- dataIn %>% left_join(mod$Groups, by=c("Texture", "Suborder")) %>% select(ID) %>% unlist()
xint <- mod$GroupXmat[grp,]

## Make Predictions
## Note that this creates a posterior distribution of predicitons...one for each iteration of the model fit
xt <- cbind(xint, dataIn$MATC, dataIn$MAPmm)
mu <- mod$Beta%*%t(xt)
sig <- sqrt(mod$Variance[,grp])

## Contruct output file 
tmp <- pnorm(soc, mean=t(mu), sd=t(sig))  ## This is the CDF evaluated for input data
outTab <- data.frame("Score_2.5%"=apply(tmp, 1, quantile, probs=0.025),
                     "Score_Mean"=apply(tmp, 1, mean),
                     "Score_97.5%"=apply(tmp, 1, quantile, probs=0.975), check.names = F)
dataOut <- cbind(dataIn, outTab)

dataOut$description<- descriptions
write_csv(dataOut, 'Processed-data/Scored_Data.csv', )


