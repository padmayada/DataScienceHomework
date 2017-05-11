##--------------------------------------------
##
## Chicago Diabetes Homework (Lecture 4)
## Date: 5/10/2017
## Name: Padma Yada
## Class: PCE Data Science Methods Class
##
##--------------------------------------------


##-----Load Libraries-----
require(logging)

# Declare the loading data function
load_data = function(datafile, logger=NA){
  data = read.csv(datafile, stringsAsFactors=FALSE)
  loginfo("Loaded Data.", logger="data_logger")
  
  # Check if any data was loaded
  if(nrow(data)==0){
    logwarn("No Data Loaded", logger="data_logger")
  }
  return(data)
}

# Get the log file name that has a date-time in the name
get_log_filename = function(){
  log_file_name = format(Sys.time(), format="HW4_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}

## Get the slope and intercept
slope_intercept = function(lm_model){
 lm_model_intercept = summary(lm_model)$coefficients[1]
 lm_model_slope = summary(lm_model)$coefficients[2]
 lm_model_data = c(lm_model_intercept, lm_model_slope)
 return(lm_model_data)
}

test_slope_intercept = function(){
  test_data_x = c(1,2,3)
  test_data_y = c(3,5,7)
  test_output = slope_intercept(lm(test_data_y ~ test_data_x))
  expected_output = c(1, 2)
  stopifnot(test_output == expected_output)
}

if (interactive()){
  
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
    # Run unit test:
   test_slope_intercept
  
  # Set working director and load data
  loginfo("Setting wd and loading data.", logger="data_logger")
  setwd('C:/Machine learning/UW - Data Science/')
  data = read.csv('ChicagoDiabetesData.csv', stringsAsFactors = FALSE)
  
  # Number of Hospitalizations vs. Crude Admittance Rate.
  data_means = apply(data[-1],2,mean)
  
  hospitalizations = data_means[grepl('Hospitalizations', names(data[-1]))]
  admit_rate = data_means[grepl('Crude.Rate.[0-9]+$', names(data[-1]), perl = TRUE)]
   
  # Plot hospitalizations vs admit_rate
  plot(hospitalizations, admit_rate)
  grid()
  best_line = lm(admit_rate ~ hospitalizations)
  
  # Add a straight line to the plot
  
  abline(best_line, col="gray23", lty=2)
  
  # Calculate the Slope and the Intercept 
  a1 <- slope_intercept(best_line)
  
  loginfo((paste('For Number of Hospitalizations vs. Crude Admittance Rate, The intercept is', a1[1],'and the slope is',a1[2])))
  
  # Calculate SSE
  y1 = a1[2]*hospitalizations + a1[1]
  SSE1 = sum((y1-admit_rate)^2)
  
  # Calculate SST
   y_avg1 = rep(mean(admit_rate), length(hospitalizations))
   SST1 = sum((y1 - y_avg1)^2)  # Sum of Squares Total
   
  # Calculate R^2
   
  R21 <- 1 - SSE1/SST1
  
  loginfo((paste('For Number of Hospitalizations vs. Crude Admittance Rate, R^2 is', R21)))
  
  # Change in Number of Hospitalizations vs. Change in Crude Admittance Rate
  
  hospitalizations_diff = diff(hospitalizations)
  admit_rate_diff = diff(admit_rate)
  
  plot(hospitalizations_diff, admit_rate_diff)
  grid()
  best_line_diff = lm(admit_rate_diff ~ hospitalizations_diff)
  abline(best_line_diff, col="gray23", lty=2)
  
  a2 <- slope_intercept(best_line_diff)
  loginfo((paste('For Change in Number of Hospitalizations vs. Change in Crude Admittance Rate, The intercept is', a2[1],'and the slope is',a2[2])))
  
  # Calculate SSE
  y2 = a2[2]*hospitalizations_diff + a2[1]
  SSE2 = sum((y2-admit_rate_diff)^2)
  
  # Calculate SST
  y_avg2 = rep(mean(admit_rate_diff), length(hospitalizations_diff))
  SST2 = sum((y2 - y_avg2)^2)  # Sum of Squares Total
  
  # Calculate R^2
  
  R22 <- 1 - SSE2/SST2
  
  loginfo((paste('For Change in Number of Hospitalizations vs. Change in Crude Admittance Rate, R^2 is', R22)))
  
}
