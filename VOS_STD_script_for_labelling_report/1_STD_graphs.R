########################################################################################
### Function for creating STD anomaly plots to be used for the labellign step 2 reports
###########################
### FOR VOS only



### Run the script in a directory which contains an input folder and an output folder.
### The Input folder needs to contain raw data file(s) (they need to have the same format and standards).
### Output folder will one plot (per file) showing the standard measurements anomalies.


#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# Input params to be assigned:

if (!input_from_main) {

date_col <-1
time_col <- 1
dt_format <- "%d/%m/%Y %H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
run_type_col <- 3
CO2_col <- 12
CO2_name <- "CO2_PHYS"
std_val_col <- 10
std_val_name <- "STD"
std_names <- c("STD1","STD2","STD3","STD4")

}

##-------------
# Do not change these:
ymin <- -2
ymax <- 2

good_min <- -2
good_max <- 2
questionable_min <- -5
questionable_max <- 5


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Remove old figure produced by this script from the output directory
image <- paste("output/",list.files("output", pattern="time.jpg$",), sep="")
file.remove(image)


Sys.setlocale("LC_ALL", "English"); 

# Write output to file
table_info_file <- paste("output", "/", "STD_graphs_table_info.txt", sep="")
sink(table_info_file)

  input_dir<- "input"
  output_dir<-"output"
  
  # List all files in input directory
  input_files <- list.files(input_dir)
 
  
  # Loop through the input files
  	for (file_loop in 1:length(input_files)) {
     
	    # Get the path to file and read the data 
	    in_file <- paste(input_dir, "/", input_files[file_loop], sep="")
	    data <- read.table(in_file,header=T, sep = "\t", strip.white=TRUE, fileEncoding="UTF8")
	    
	    # Identify date and time
	    # (The if statement is related to whether there are one or two date/time columns in raw file)
	    if (date_col == time_col) {
	        date.time <- as.POSIXct(data[,date_col], tz="UTC", format=dt_format)
	    } else {
	        date.time <- as.POSIXct(paste(data[,date_col], data[,time_col]), tz="UTC", format=dt_format)          
	    }
	    
	    # Add the date time column to the data frame
	    data$date.time <- date.time
	    
	    ## Make the plots as output
	    out_file <- paste(output_dir, "/", input_files[file_loop], sep="")
	    
	    # Make output path and filename 
	    filename <- paste((paste((sub("^([^.]*).*", "\\1", out_file)),"time", sep="_")), "png", sep=".")
	      
	    # Make the plot:
	    png(filename)#, width=1000)
          
        
        # Extract data needed for plotting (those in cols) from each standard and store as separate list elements 
        df_list <- list()
        cols <- c(ncol(data), run_type_col, CO2_col, std_val_col)
        
        for (i in 1:length(std_names)) {                                                                                              
	      
	        type <- std_names[i]
	        data_sub <- data[data[,run_type_col]==type,cols]   
	        data_sub$diff <- data_sub[[CO2_name]] - data_sub[[std_val_name]]
          data_sub$seconds <- as.numeric(data_sub$date.time)                   # Needed when making linear model to consider trend
	        df_list[[i]] <- data_sub
	     
	    }
	             

      # Make plots 
	    par(mfrow=c(4,1))
      par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
      par(mar = c(2, 2, 1, 1)) # make the plots be closer together
    	    
	    #plot_name <-paste((sub("^([^.]*).*", "\\1", input_files[file_loop])), sep="_")
	    color <- c("green", "blue", "red", "purple")
      zeros <- rep(0, length(data$date.time))
  
      

      
    for (k in 1:length(std_names)) {
      plot (df_list[[k]]$date.time, df_list[[k]]$diff, col=color[k], ylab="", xlab="", type="p", ylim = c(ymin,ymax))
      abline(h=0)
      #lines(data$date.time, zeros, col="black", lwd=1) 
      legend("topleft", legend = std_names[k])
      good_df <- subset(df_list[[k]], df_list[[k]]$diff < ymax & df_list[[k]]$diff > ymin)
      abline(lm(diff~seconds, good_df), col="red")
		  }
      mtext('Time', side = 1, outer = TRUE, line = 2)
      mtext('Calibration anomaly [ppm]', side = 2, outer = TRUE, line = 2)
    
	    #legend("topleft", legend = c("STD1", "STD2", "STD3", "STD4", "Base line"), pch, col=c(color,"black"), lwd=c(2,2,2,2,2))
	    dev.off()
    
    

    # Calculate how many measurements are outside the range
    outlier_count <- rep(0,length(std_names))
    for (l in 1:length(std_names)) {
      # Absolute number
      high <- sum(df_list[[l]]$diff > ymax)
      low  <- sum(df_list[[l]]$diff < ymin)
      outliers <- high + low
      
      # In percentage:
      total_meas <- length(df_list[[l]]$diff)
      percent <- round((outliers/total_meas)*100, 2)
      
      # Write to screnn
      cat("Number of outliers for ", std_names[l], " is ", outliers, " (", percent , "%).", "\n", sep="")    
      outlier_count[l]<-outliers
      
    }
    #cat("\n", "The total number of outliers are ", sum(outlier_count),"\n", sep="")
    
    
    
    
    # Make linear model for trend lines and find out if trends are significant
    cat("\n")
    slope <- rep(0, length(std_names))
    p_val <- rep(0, length(std_names))
    total_change <- rep(0, length(std_names))
    
    for (m in 1:length(std_names)) { 
      good_df <- subset(df_list[[m]], df_list[[m]]$diff < ymax & df_list[[m]]$diff > ymin)
      lin_mod <- lm(diff~seconds, good_df)
      slope[m] <- summary(lin_mod)$coefficients[2,1]
      p_val[m] <- summary(lin_mod)$coefficients[2,4]
      
      total_seconds <- (good_df$seconds[length(good_df$seconds)]) - (good_df$seconds[1])
      total_change[m] <- slope[m]*total_seconds
      
      if(p_val[m] < 0.05) {
        cat(std_names[m], " changes by ", round(total_change[m],2), " ppm (significant - ", round(p_val[m],3),").","\n", sep ="")
      } else {
        cat(std_names[m], " changes by ", round(total_change[m],2), " ppm (not significant - ", round(p_val[m],3),").","\n", sep="")
      }
    }
    
    
 
    
    # loop though the list (each STD) and could how many values are ouside the questionable and bad range
    cat("\n")
    good <- rep(0, length(std_names))
    questionable <- rep(0, length(std_names))
    bad <- rep(0, length(std_names)) 
    total <- rep(0,length(std_names))
    
    for (n in 1:length(std_names)){
    good[n] <- sum(df_list[[n]]$diff > good_min & df_list[[n]]$diff < good_max)
    bad[n] <- sum(df_list[[n]]$diff < questionable_min, df_list[[n]]$diff > questionable_max)
    questionable[n] <- sum(df_list[[n]]$diff < good_min & df_list[[n]]$diff > questionable_min, df_list[[n]]$diff > good_max & df_list[[n]]$diff < questionable_max)
    total[n] <- length(df_list[[n]]$diff)
    
    cat(std_names[n]," - G: ", good[n], " (", round((good[n]/total[n])*100,2), "%), Q: ", questionable[n], " (", round((questionable[n]/total[n])*100,2), "%), B: ", bad[n], " (", round((bad[n]/total[n])*100,2), "%).", "\n", sep="")                  
    
    }
    
    
    
    
    
    
  	}



sink()


# TESTING While making linear models and calculating trend lines 
#plot (df_list[[1]]$date.time, df_list[[1]]$diff, ylim = c(ymin,ymax))
#good_df <- subset(df_list[[1]], df_list[[1]]$diff < 5 & df_list[[1]]$diff > -5)
#abline(lm(diff~seconds, good_df), col="red")

#x <- c(1:10)
#y <- c(4,3,5,6,21,20,21,22,23,25)
#test_df <- data.frame(x,y)
#plot(test_df$x,test_df$y)
#abline(lm(y~x,test_df), col="red")

#lin_mod <- lm(y~x, test_df)

#summary(lin_mod)

#slope <- summary(lin_mod)$coefficients[2,1]
#p_val <- summary(lin_mod)$coefficients[2,4]

#if(p_val<0.05) {
#cat("STD1 has slope ", slope, " (significant)")
#} else {
#  cat("STD1 has slope ", slope, " (not significant)")
#}



