
library(ggplot2)
library(dplyr)
library(tidyr)


climdat <- read.csv("Raw-data/noaa historical climate data for jackson research station.csv")

hist(climdat$PRCP) # daily precipitation in inches
hist(climdat$TMAX) # daily maximum temperature in Fahrenheit
hist(climdat$TMIN) # daily maximum temperature in Fahrenheit

climdat1 <- climdat
# create new column for date
for(i in 1:dim(climdat1)[1]){
  if(nchar(climdat1$date[i])==1){climdat1$date[i] <- paste0(0,climdat1$date[i])}
}
climdat1$DATE2 <- paste0(climdat1$year,"-",climdat1$month,"-",climdat1$date)
climdat1$DATE2 <- as.Date(climdat1$DATE2)

# calculate cumulative precipitation for each year
climdat2 <- climdat1 %>%                             
  dplyr::group_by(year) %>%
  dplyr::mutate(PRCP_C = cumsum(replace_na(PRCP,0))) 
range(na.omit(climdat2$PRCP_C))

# look at historical averages
climdat_avg <- climdat2 %>%                             
  group_by(year) %>%
  dplyr::summarise(TMIN_avg = mean(TMIN, na.rm=TRUE),
                   TMAX_avg = mean(TMAX, na.rm=TRUE),
                   PRCP_C = max(PRCP_C, na.rm=TRUE))
climdat_avgs <- data.frame(hist30_TMIN = min(climdat_avg$TMIN_avg[which(climdat_avg$year %in% c(1989:2019))]),
                           hist30_TMAX = max(climdat_avg$TMAX_avg[which(climdat_avg$year %in% c(1989:2019))]),
                           hist30_PRCP_C = mean(climdat_avg$PRCP_C[which(climdat_avg$year %in% c(1989:2019))]), 
                           hist30_TAV = mean((climdat_avg$TMIN_avg[which(climdat_avg$year %in% c(1989:2019))]+climdat_avg$TMAX_avg[which(climdat_avg$year %in% c(1989:2019))]))/2)
climdat_avgs <- t(climdat_avgs)
colnames(climdat_avgs) <- "histav"
write.csv(climdat_avgs, "Raw-data/Historical averages.csv")

# subset to 2020-present
climdat_fig <- climdat2[which(climdat2$year>2019),]   

climdat3 <- climdat2[,c("year", "PRCP", "PRCP_C")]
mid_year <- range(climdat2$year) %>% {(.[2] - .[1]) / 2 + .[1]}

# convert f to c
climdat_fig$TMINc <- (climdat_fig$TMIN-32)*(5/9)
climdat_fig$TMAXc <- (climdat_fig$TMAX-32)*(5/9)
climdat_avgs <- as.data.frame(climdat_avgs)
climdat_avgs$converted <- rep(NA,4)
climdat_avgs$converted[c(1,2,4)] <- (climdat_avgs$histav[c(1,2,4)] -32)*(5/9)

# convert in to mm
climdat_fig$PRCP_Cc <-  climdat_fig$PRCP_C * 25.4
climdat_avgs$converted[c(3)] <- climdat_avgs$histav[3] * 25.4

climfig <- ggplot(climdat_fig, aes(x=DATE2, y=TMAXc+20))+
  geom_line(color="red", alpha=0.6) + 
  scale_x_date(date_breaks = "months", date_labels = "%b \n '%y", limits = as.Date(c("2020-01-01", "2023-11-01"))) +
  theme_bw() + theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), legend.position="bottom") +
  labs( y=expression(paste("Temperature (°C)"))) +
  # add PRCP
  scale_y_continuous(limits = c(0,60), breaks=seq(0,60,by=10), labels = seq(-20,40,by=10),
    sec.axis = sec_axis(~ . * 75, name = "Cumulative precipitation (mm)")) +
  geom_area(inherit.aes=FALSE,data=climdat_fig, aes(y=(PRCP_Cc-0)/75, x=DATE2), #convert precip to temperature scale
            fill="deepskyblue", color="black", alpha=0.4, linetype="dashed") +
  geom_hline(yintercept=(climdat_avgs$converted[3]-0)/75, alpha=1, linetype="dashed") + # historical precip
  # add TMIN
  geom_line(inherit.aes=FALSE,data=climdat_fig, aes(y=TMINc+20, x=DATE2), col="blue", alpha=0.6) +
  # historical mean temperature
  geom_hline(yintercept=climdat_avgs$converted[4]+20, alpha=1, linetype="solid") +
  # indicate when field trial started and ended
  annotate("text", label="*",y=0, x= as.Date(c("2020-09-30", "2023-09-30")), size=6)
  
climfig
# export figure
ggpubr::ggexport(climfig, height=1500, width=5500, filename = "Figures/0_clim.png", res = 400)



# look at minimum temp extremes

dat4 <- data.frame(TMINc=(climdat_fig$TMINc),
                      DATE2=(climdat_fig$DATE2))
