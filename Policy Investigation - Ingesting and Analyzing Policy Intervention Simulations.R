
library(readxl)
library(tidyr)
library(dplyr)


pull_sim <- function(data_file_path){
  
  # Define the file path
  file_path <- data_file_path
  
  # Read the first sheet of the Excel file
  df <- read_excel(file_path, sheet = 1, col_names = FALSE)
  
  # Assign column names (first row contains variable names)
  colnames(df) <- df[1, ]
  df <- df[-1, ]  # Remove the first row after setting column names
  
  # Rename the first column as "Variable"
  colnames(df)[1] <- "Variable"
  
  # Pivot the data to long format
  df_long <- pivot_longer(df, cols = -Variable, names_to = "Year", values_to = "Value")
  
  # Pivot the data back to wide format with years as row names
  df_wide <- pivot_wider(df_long, names_from = "Variable", values_from = "Value")
  
  # Convert Year column to row names
  #df_wide$Year <- rownames(df_wide$Year)
  df <- as.data.frame(df_wide)
  
  # Return final data frame 
  return(df)

}

calc_indexes <- function(sim_results, baseline_results) { 
  
  # Extract the variable name dynamically
  variable_name <- deparse(substitute(sim_results))
  
  # Convert data frames to matrixes
  mtx <- as.matrix(sim_results[,-1])
  mtx_baseline <- as.matrix(baseline_results[,-1])
  
  # Calculate the difference between the simulation and baseline values 
  mtx_diff <- (mtx - mtx_baseline)/(mtx_baseline+0.000000001)
  #mtx_diff <- (mtx - mtx_baseline)
  
  
  # Save as a data frame
  df <- as.data.frame(mtx_diff)
  
  # Calculate the relevant indexes
  df$Index_SocJustice <- (
    df$`Residents BIPOC (%)`+ 
      -1 * (
        df$`Below High School Education (%)` + 
          df$`Below Poverty (%)` + 
          df$`Uninsured (%)` + 
          df$`Mean Housing Affordability (% of Income Spent on Housing)` + 
          df$`Unemployment Rate (%)`) 
  )
  
  df$Index_Hazard <- -1 * (
    df$`% of Housing Units Built 1959 or Earlier` + 
      df$AQI + 
      df$`Polluting Facilities Release: Other Contamination (lbs)` + 
      df$`Polluting Facilities Release: Pb Release (Air Only) (lbs)` + 
      df$`Traffic (VMT)`
  )
  
  df$Index_PbAdults <- df$`Rate or % of People with Above Safe Levels of Pb[Adults]` * -1
  
  df$Index_PbChild <- df$`Rate or % of People with Above Safe Levels of Pb[Children 10]` * -1
  
  
  # Define a function to plot with colored points
  plot_colored_scatter <- function(y, title) {
    colors <- ifelse(y >= 0, "darkgreen", "red")  # Green if positive, red if negative
    plot(y, col = colors, pch = 16, cex = 1.5, main = title, ylim = c(-0.15, 0.15))  # pch=16 for solid circles
    lines(smooth.spline(y), col = "grey", lwd = 2)  # Add a smooth trend line in grey
  }
  
  par(mfrow = c(2, 2), oma = c(2, 2, 2, 2))  # Adjust outer margins
  
  # Plot the index values over time with colored points
  plot_colored_scatter(df$Index_PbAdults, "Lead (Adult)")
  plot_colored_scatter(df$Index_PbChild, "Lead (Children)")
  plot_colored_scatter(df$Index_SocJustice, "Index (SocJus)")
  plot_colored_scatter(df$Index_Hazard, "Index (Haz)")
  
  # Overlay a text label in the middle of the plotting area
  par(mfrow = c(1, 1), new = TRUE)  # Allow adding a new plot without erasing existing plots
  plot.new()  # Create an empty plot to overlay text
  text(0.5, 0.5, variable_name, cex = 1.5, font = 2)  # Centered text
  
  
  # Save the results to the Indexes data frame
  Indexes <- rbind(data.frame(
    Variable = variable_name,
    EBLL_Adults = df$Index_PbAdults[11],
    EBLL_Children = df$Index_PbChild[11],
    SocialJustice = df$Index_SocJustice[11],
    Hazard = df$Index_Hazard[11]
  ), 
  stringsAsFactors = FALSE)
  
  par(mfrow = c(1, 1))
  
  return(Indexes)
  
}

################################################################################
### GET BASELINE DATA SET

# Set the path for the dynamic datasheet which contains the simulation's current results
data_file_path <- "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #4 - Policy and Governance/SIMULATIONS/Stella Results (Dynamic).xlsx"

# Import Baseline Simulation results (be sure to first run the baseline simulation first)
#BASELINE <- pull_sim(data_file_path)

# Create an empty data frame for storing the simulation index results 
Indexes <- data.frame(Variable = character(), EBLL_Adults = numeric(), EBLL_Children = numeric(), SocialJustice = numeric(), Hazard = numeric())

################################################################################
### EX. UNINSURED

## SET UP - if necessary, run the simulation in Stella then import (format is variable_speed_stim%)
#UNINSURED_none_95 <- pull_sim(data_file_path)

## PROCESS - calculate the indexes from the simulation results 
#sim_index <- calc_indexes(UNINSURED_none_95, BASELINE)

## SAVE - Bind the final results to the Indexes data frame 
#Indexes <- rbind(Indexes, sim_index)

################################################################################

### UNINSURED
#UNINSURED_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNINSURED_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#UNINSURED_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNINSURED_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#UNINSURED_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNINSURED_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

### UNEMPLOYED 
#UNEMPLOYED_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNEMPLOYED_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#UNEMPLOYED_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNEMPLOYED_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#UNEMPLOYED_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNEMPLOYED_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

### EDUCATION
#EDUCATION_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(EDUCATION_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#EDUCATION_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(EDUCATION_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#EDUCATION_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(EDUCATION_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

### HOUSINGAFFORD
#HOUSINGAFFORD_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(HOUSINGAFFORD_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#HOUSINGAFFORD_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(HOUSINGAFFORD_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#HOUSINGAFFORD_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(HOUSINGAFFORD_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

### SNAP
#SNAP_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_none_95, BASELINE) # 1.005
Indexes <- rbind(Indexes, sim_index)

#SNAP_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_none_90, BASELINE) # 1.01
Indexes <- rbind(Indexes, sim_index)

#SNAP_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_none_85, BASELINE) # 1.015
Indexes <- rbind(Indexes, sim_index)

### POVERTY
#POVERTY_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POVERTY_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#POVERTY_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POVERTY_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#POVERTY_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POVERTY_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)



### AQI
#AQI_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(AQI_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#AQI_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(AQI_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#AQI_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(AQI_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

### HOUSING1959

#HOUSING1959_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(HOUSING1959_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#HOUSING1959_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(HOUSING1959_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#HOUSING1959_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(HOUSING1959_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

### POLLUTOTHER
#POLLUTOTHER_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHER_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHER_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHER_none_90, BASELINE) #0.99
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHER_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHER_none_85, BASELINE) #0.985
Indexes <- rbind(Indexes, sim_index)

### POLLUTPB
#POLLUTPB_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTPB_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#POLLUTPB_none_90 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTPB_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#POLLUTPB_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTPB_none_85, BASELINE)  #0.985
Indexes <- rbind(Indexes, sim_index)

### TRAFFIC
#TRAFFIC_none_95 <- pull_sim(data_file_path)
sim_index <- calc_indexes(TRAFFIC_none_95, BASELINE) # 0.995
Indexes <- rbind(Indexes, sim_index)

#TRAFFIC_none_90 <- pull_sim(data_file_path) 
sim_index <- calc_indexes(TRAFFIC_none_90, BASELINE) # 0.99
Indexes <- rbind(Indexes, sim_index)

#TRAFFIC_none_85 <- pull_sim(data_file_path)
sim_index <- calc_indexes(TRAFFIC_none_85, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#write.csv(Indexes, file = "Indexes.csv")

################################################################################
### POLICY APPROACHES (ALL ARE 85 LEVEL OF INTENSITY)


### Direct time triggered 

#POLLUTOTHR_POLLUTPB_HOUS59_85_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_HOUS59_85_7YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_HOUS59_85_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_HOUS59_85_5YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_HOUS59_85_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_HOUS59_85_3YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_7YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_5YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_3YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)


### Capabilities time triggered

#SNAP_UNEMPLOYED_HOUSAFORD_UNINSURED_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_HOUSAFORD_UNINSURED_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_HOUSAFORD_UNINSURED_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_HOUSAFORD_UNINSURED_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_HOUSAFORD_UNINSURED_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_HOUSAFORD_UNINSURED_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_HOUSAFORD_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_HOUSAFORD_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_UNINSURED_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_UNINSURED_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_UNINSURED_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_UNINSURED_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_UNINSURED_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_UNINSURED_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#UNEMPLOYED_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNEMPLOYED_HOUSAFORD_7YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#UNEMPLOYED_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNEMPLOYED_HOUSAFORD_5YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#UNEMPLOYED_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(UNEMPLOYED_HOUSAFORD_3YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_HOUSAFORD_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_HOUSAFORD_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

### Both approaches 

# Comprehensive approach 
#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# No Housing Affordability ################################################################################
#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# No SNAP ################################################################################
#POLLUTOTHR_POLLUTPB_UNEMPLOYED_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_HOUSAFORD_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNEMPLOYED_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_HOUSAFORD_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNEMPLOYED_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# No Unemployment ################################################################################
#POLLUTOTHR_POLLUTPB_SNAP_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_HOUSAFORD_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_HOUSAFORD_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# No SNAP or Unemployment ################################################################################
#POLLUTOTHR_POLLUTPB_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_HOUSAFORD_7YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_HOUSAFORD_5YEARS, BASELINE) # 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_HOUSAFORD_3YEARS, BASELINE) #  0.985
Indexes <- rbind(Indexes, sim_index)

# No Housing Afford or Unemployment ################################################################################
#POLLUTOTHR_POLLUTPB_SNAP_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# No Housing Afford or SNAP ################################################################################
#POLLUTOTHR_POLLUTPB_UNEMPLOYED_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNEMPLOYED_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNEMPLOYED_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# Uninsured and Pollutants Only ################################################################################

#POLLUTOTHR_POLLUTPB_UNINSURED_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNINSURED_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNINSURED_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNINSURED_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNINSURED_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNINSURED_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

################################################################################################################################################################
# New Comprehensive(with uninsured) ################################################################################

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_HOUSAFORD_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_HOUSAFORD_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# No Housing Afford ################################################################################

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_UNINSURED_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

# No SNAP ################################################################################

#POLLUTOTHR_POLLUTPB_UNEMPLOYED_UNINSURED_HOUSAFORD_7YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_UNINSURED_HOUSAFORD_7YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNEMPLOYED_UNINSURED_HOUSAFORD_5YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_UNINSURED_HOUSAFORD_5YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#POLLUTOTHR_POLLUTPB_UNEMPLOYED_UNINSURED_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_UNEMPLOYED_UNINSURED_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)



write.csv(Indexes, file = "Indexes.csv")







### EXPERIMENTAL RUNS  



#POLLUTOTHR_POLLUTPB_SNAP_UNINSURED_HOUSAFORD_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNINSURED_HOUSAFORD_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_90_none <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_90_none, BASELINE) # 1.01 AND 0.99
Indexes <- rbind(Indexes, sim_index)

#SNAP_UNEMPLOYED_85_none <- pull_sim(data_file_path)
sim_index <- calc_indexes(SNAP_UNEMPLOYED_85_none, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

scatter.smooth(BASELINE$`Rate or % of People with Above Safe Levels of Pb[Adults]`)
scatter.smooth(BASELINE$`Rate or % of People with Above Safe Levels of Pb[Children 10]`)
scatter.smooth(BASELINE$`Google Trends Index`)


#POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_HOUS59_3YEARS <- pull_sim(data_file_path)
sim_index <- calc_indexes(POLLUTOTHR_POLLUTPB_SNAP_UNEMPLOYED_HOUSAFORD_HOUS59_3YEARS, BASELINE) # 1.015 AND 0.985
Indexes <- rbind(Indexes, sim_index)

#BASELINE_SIGDIGITS <- pull_sim(data_file_path)
sim_index <- calc_indexes(BASELINE_SIGDIGITS, BASELINE) 
Indexes <- rbind(Indexes, sim_index)

#HOUSING1959_none_85_SIGDIG <- pull_sim(data_file_path)
sim_index <- calc_indexes(HOUSING1959_none_85_SIGDIG, BASELINE_SIGDIGITS) # 0.985
Indexes <- rbind(Indexes, sim_index)









################################################################################
### SCRATCH 

# Convert data frames to matrixes
mtx <- as.matrix(UNINSURED[,-1])
mtx_baseline <- as.matrix(BASELINE[,-1])

# Calculate the difference between the simulation and baseline values 
mtx_diff <- mtx - mtx_baseline

# Save as a data frame
df <- as.data.frame(mtx_diff)

# Calculate the relevant indexes
df$Index_SocJustice <- (
  df$`Residents BIPOC (%)`+ 
    -1 * (
      df$`Below High School Education (%)` + 
        df$`Below Poverty (%)` + 
        df$`Uninsured (%)` + 
        df$`Mean Housing Affordability (% of Income Spent on Housing)` + 
        df$`Unemployment Rate (%)`) 
)

df$Index_Hazard <- -1 * (
  df$`% of Housing Units Built 1959 or Earlier` + 
    df$AQI + 
    df$`Polluting Facilities Release: Other Contamination (lbs)` + 
    df$`Polluting Facilities Release: Pb Release (Air Only) (lbs)` + 
    df$`Traffic (VMT)`
)

# Plot the index values over time
scatter.smooth(df$`Rate or % of People with Above Safe Levels of Pb[Adults]`)
scatter.smooth(df$`Rate or % of People with Above Safe Levels of Pb[Children 10]`)
scatter.smooth(df$Index_SocJustice) 
scatter.smooth(df$Index_Hazard)

# Save the results to the Indexes data frame
Indexes <- rbind(Indexes, data.frame(
  Variable = sim_name,
  EBLL_Adults = df$`Rate or % of People with Above Safe Levels of Pb[Adults]`[11],
  EBLL_Children = df$`Rate or % of People with Above Safe Levels of Pb[Children 10]`[11],
  SocialJustice = df$Index_SocJustice[11],
  Hazard = df$Index_Hazard[11]
), 
stringsAsFactors = FALSE)

print(Indexes[nrow(Indexes),])



