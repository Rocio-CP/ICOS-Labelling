
workdir <- "/Users/rpr061/Documents/DATAMANAGEMENT/Projects/ICOS/test/Nuka_raw_data/workspace"
setwd(workdir)

eqpress_original="equ.press"
atmpress_original="atm.press"
date_original="PC.Date"
time_original="PC.Time"
	
Nuka_raw <- read.table("AllNuka2017.txt", sep="\t", header = TRUE, stringsAsFactors = FALSE)

# Fix equilibrium pressure
if (max(Nuka_raw[eqpress_original], na.rm=TRUE) < 800.) {
  # eq press is given as difference
  Nuka_raw[eqpress_original]=Nuka_raw[atmpress_original]+Nuka_raw[eqpress_original]
}

# Order chronologically
newdate <- as.POSIXct(paste(Nuka_raw[[date_original]],Nuka_raw[[time_original]]), format="%d/%m/%y %H:%M:%S")
Nuka_raw <- Nuka_raw[order(newdate),]

# remove -DRAIN messages
Nuka_raw$Type <- gsub("-DRAIN", "", Nuka_raw$Type, fixed = TRUE)

# save in textfile
write.table(Nuka_raw,"AllNuka2017_RCP_QC.txt", sep = "\t", row.names = FALSE, quote = FALSE)
