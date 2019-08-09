# Check equilibration PAP from 2017-2018 campaign
# seawater CO2 from keel: .co2
library(lubridate)
pap2017 <- read.table("/Users/rpr061/Dropbox/BCDC_Projects/ICOS/ICOS_Labelling_work_folder/Reports/10_PAP/20180911_OriginalData/PAP_Apr2017.co2",
                   header=TRUE, sep="", stringsAsFactors = FALSE)

pap2017$datetime <- date_decimal(pap2017$TS_jday/365 + pap2017$TS_yr)


azpcchange <- c(pap2017$AZPC[1:length(pap2017$AZPC)-1] 
                - pap2017$AZPC[2:length(pap2017$AZPC)], 0)
azpcchange[abs(azpcchange)==0] <- NA
azpcindex <- which(!is.na(azpcchange))
azpcindex <- c(azpcindex, length(azpcchange)) # Position of last value of sequence

co2change <- c(pap2017$CONC[1:length(pap2017$CONC)-1] 
                         - pap2017$CONC[2:length(pap2017$CONC)], 0)

# Condition for equilibration: last 3 changes are below 1 uatm? 5?






plot(pap2017$datetime[(azpcindex[50]-5):(azpcindex[50])],
     co2change[(azpcindex[50]-5):(azpcindex[50])])



for (as in 20:23){#length(azpcindex)-1) {#length(azpcsequences)) {
plot(pap2017$datetime[azpcindex[as]+1:azpcindex[as+1]],pap2017$CONC[azpcindex[as]+1:azpcindex[as+1]], 
      ylim=c(100,600), xlab="date", ylab="xCO2")
}

plot(pap2017$CONC[200:250]-pap2017$CONC[199:249], col=pap2017$AZPC[200:250],ylim=c(-1,30),
     xlab="index", ylab="xCO2 difference")
lines(c(0,32),c(0,0))



