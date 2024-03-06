
# log1p()
# sqrt()
# var + var^2
# #cubic
# 1/var
# boxcox 

#natural log (log)
#exponential 
#inverse square root 
#bounded exponential (s-shaped)

# no quad, cubic, or title expressions when possible
# poly for 


write.csv(mined_relationships, file = "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/MinedRelationships_27Feb2024.csv")

################################################################################
### SET UP
library(dplyr)
library(stats)
library(zoo)
library(GreyModel)

# Import data
alldata <- read.csv(file = "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/MASTER SD Data 02.27.24 (Downloaded from Drive and Edited).csv", row.names = "Year")

# Interpolation 

alldata$EBLL_Adults_Prcnt[22:23] <- fcast_grey(alldata$EBLL_Adults_Prcnt[1:21], 2)
#alldata$EBLL_Chldrn_Prcnt[23] <- fcast_grey(alldata$EBLL_Chldrn_Prcnt[6:22], 1)
alldata$EBLL_Chldrn_5_Prcnt[23] <- fcast_grey(alldata$EBLL_Chldrn_5_Prcnt[13:22], 1)
alldata$Urban_LULC_Prcnt[13] <- fcast_grey(alldata$Urban_LULC_Prcnt[1:12], 1)
#alldata$DrnkngWtr_PbLvls_Avg_MG_L <- zoo::na.spline(alldata$DrnkngWtr_PbLvls_Avg_MG_L)
#alldata$SldWste_Dspsd_Tons <- zoo::na.approx(alldata$SldWste_Dspsd_Tons)

par(mfrow = c(2, 2))
scatter.smooth(alldata$EBLL_Adults_Prcnt)
scatter.smooth(alldata$DrnkngWtr_PbLvls_Avg_MG_L)
scatter.smooth(alldata$EBLL_Chldrn_Prcnt)
scatter.smooth(alldata$Urban_LULC_Prcnt)
#scatter.smooth(alldata$SldWste_Dspsd_Tons)
par()

# Limit Data to eliminate validation set
#alldata <- alldata[1:18,]

# Limit Data to 2010 forward 
#alldata <- alldata[11:23,]


# FUNCTION: Create simple plotting function for various transformations
plotting_scatters <- function(x, y, x_name){
  
  # Set up plots 
  par(mfrow = c(2, 3))
  
  # Plot Y alone
  scatter.smooth(y, lpars = c(col = "red"), main = paste("Dep Var Alone"))
  
  # Plot no transformation
  scatter.smooth(y ~ x, lpars = c(col = "red"), main = paste("No Transformation"))
  
  # Plot log transformation
  scatter.smooth(y~ log(x), lpars = c(col = "red"), main = paste("log Transformation"))
  
  # Plot Square root transformation
  scatter.smooth(y~ sqrt(x), lpars = c(col = "red"), main = paste("Sqrt Transformation"))
  
  # Plot Inverse square root transformation
  scatter.smooth(y~ 1/sqrt(x), lpars = c(col = "red"), main = paste("Invserse Sqrt Transformation"))
  
  # Plot Quadratic model
  scatter.smooth(y ~ x + x^2, lpars = c(col = "red"), main = paste("Quad Transformation"))

  # Plot exponential model
  #scatter.smooth(y, exp(x), lpars = c(col = "red"), main = paste("Exponential Transformation"))
  
  # Add general title
  main_title <- paste(x_name)
  mtext(main_title, side = 1, line = -2, outer = TRUE, col = "red", font = 2, cex = 1.5)
  
  # Reset plots 
  par(mfrow = c(1, 1))
  
  return("Plots Below")
}

# FUNCTION: Create a simple covariation mining function, recording r-squared from lms using various transformations
covariation_mining <- function(x, y, x_name, data) {
  
  # Create a data frame to store results
  results <- data.frame(
    Variable = character(),
    Transformation = character(), 
    R_squared = numeric(), 
    stringsAsFactors = FALSE
  )
  
  # No transformation
  lm_model <- lm(y ~ x, data = data)
  results <- rbind(results, c(x_name, "None", round(summary(lm_model)$r.squared, 5)))
  
  # log transformation
  lm_model_log <- lm(y ~ log(x), data = data)
  results <- rbind(results, c(x_name, "log", round(summary(lm_model_log)$r.squared, 5)))
  
  # Square root transformation
  lm_model_sqrt <- lm(y ~ sqrt(x), data = data)
  results <- rbind(results, c(x_name, "Square Root", round(summary(lm_model_sqrt)$r.squared, 5)))
  
  # Inverse square root transformation
  lm_model_sqrt <- lm(y ~ 1/sqrt(x), data = data)
  results <- rbind(results, c(x_name, "Inverse Square Root", round(summary(lm_model_sqrt)$r.squared, 5)))
  
  # Quadratic model
  lm_model_quad <- lm(y ~ poly(x, 2), data = data)
  results <- rbind(results, c(x_name, "Quadratic", round(summary(lm_model_quad)$r.squared, 5)))
  
  # Exponential model 
  #lm_model_quad <- lm(y ~ exp(x), data = data)
  #results <- rbind(results, c(x_name, "Exponential", round(summary(lm_model_quad)$r.squared, 5)))
  
  
  # Set column names
  colnames(results) <- c("Variable", "Transformation", "R_Squared")
  
  return(results)
}

#FUNCTION: Combining the above two into a single function for plotting and checking r-squared for all transformations
phase_one <- function(indep_variables, dep_variable){
  
  ## PLOTTING
  # Plot for normality of dep variable 
  plot(stats::density(mydata[[dep_variable]], na.rm = TRUE))
  
  # Plot scatter plots for all indep variables 
  for (indep_var in indep_variables) {
    plotting_scatters(x = mydata[[indep_var]], 
                      y = mydata[[dep_variable]], 
                      x_name = indep_var)
  }
  
  ## COVARIATION MINING
  
  # Create empty dataframe
  final_results <- data.frame(
    Variable = character(),
    Transformation = character(), 
    R_squared = numeric(), 
    stringsAsFactors = FALSE
  )
  
  # Pull r-squared for EACH linear transformation
  for (indep_var in indep_variables) {
    results <- covariation_mining(
      y = mydata[[dep_variable]], 
      x = mydata[[indep_var]], 
      x_name = indep_var, 
      data = mydata)
    
    final_results <- rbind(
      final_results, 
      results
    )
  }
  
  # Print results
  print(final_results)
  
  ## SELECT MODEL 
  # Generate the model with no transofrmations 
  #paste(dep_variable, "~", paste(indep_variables, collapse = " + "))
  
  return(paste(dep_variable, "~", paste(indep_variables, collapse = " + ")))
}

# FUNCTION: Add regression outputs to a data frame
store_lm_outputs <- function(lm_object, x, y) {
  # Capture current date and time
  timestamp <- Sys.time()
  
  # Extract relevant information
  coefficients <- coef(lm_object)[-1]
  p_values <- summary(lm_object)$coefficients[-1, 4]
  r_squared <- summary(lm_object)$r.squared
  intrcpt <- summary(lm_object)$coefficients[1, 1]
  
  # Initialize an empty data frame
  result_df <- data.frame(
    Dependent_Variable = character(),
    Independent_Variable = character(),
    Coefficient = numeric(),
    P_Value = numeric(),
    R_Squared = numeric(),
    Intercept = numeric(),
    Model = character(),
    Date_Time = character(),
    stringsAsFactors = FALSE
  )
  
  # Use a for loop to populate the data frame
  for (i in seq_along(x)) {
    result_df <- rbind(result_df, data.frame(
      Dependent_Variable = y,
      Independent_Variable = x[i],
      Coefficient = coefficients[i],
      P_Value = p_values[i],
      R_Squared = r_squared,
      Intercept = intrcpt,
      Model = paste(lm_object[10]),
      Date_Time = timestamp
    ))
  }
  
  return(result_df)
}

#FUNCTION: Removing rows where column is equal to "fieldvalue"
trim_df <- function(df, fieldname, fieldvalue){
  df <- 
    df[!grepl(
      fieldvalue,
      df[[fieldname]])
      , , drop = FALSE]
}



###################################################################AQI_AvgAnnual###################

### AQI_AvgAnnual

## SET UP
# Assign variables 
indep_variables <- c(                                # ASSIGN VARIABLES
  "FuelUsage_ThousGals", 
  "Release_OthrCntmts_PoltnFclts_Lbs",                
  "Release_Pb_PoltnFclts_Lbs"                           
)

dep_variable <- "AQI_AvgAnnual"   

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  AQI_AvgAnnual ~
    FuelUsage_ThousGals + log(Release_OthrCntmts_PoltnFclts_Lbs) + log(Release_Pb_PoltnFclts_Lbs), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)


## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- output_data
#mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed # SET COLUMN NAME & FIELD VALUE
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")                        



###################################################################BlwPvrty_Prcnt####################

### BlwPvrty_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Educ_BlwHghSchl_Prcnt", "Unmplmnt_Prcnt"
)

dep_variable <- "BlwPvrty_Prcnt" 

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(BlwPvrty_Prcnt ~ log(Educ_BlwHghSchl_Prcnt) + log(Unmplmnt_Prcnt),
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships,
#   fieldname = "Date_Time",                                    # SET COLUMN NAME
#   fieldvalue = "2024-02-16 15:42:04")                         # SET FIELD VALUE


# ###############################################################RMVD BrthRt_PrcntofWmn##################
# 
# ### BrthRt_PrcntofWmn
# 
# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "MedIncm_HshldAnnual"
# )
# 
# dep_variable <- "BrthRt_PrcntofWmn"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   BrthRt_PrcntofWmn ~ poly(MedIncm_HshldAnnual, 2), 
#   data = mydata
# )
# 
# summary(lm_model)
# 
# # Recapture independent variables (captures quadratic if necessary)
# indep_variables_updated <- names(lm_model$coefficients[-1])
# 
# #Run function to store model outputs
# output_data <- store_lm_outputs(
#   lm_object = lm_model,
#   x = indep_variables_updated,
#   y = dep_variable)
# 
# #View(output_data)
# 
# ## SAVE RESULTS 
# # Bind to mined relationships data frame 
# mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    # SET COLUMN NAME
#   fieldvalue = "2024-02-08 16:33:22")                         # SET FIELD VALUE

################################################Budget_EnvrnmtlHlth_Fed_St_Grnts#############################################

### Budget_EnvrnmtlHlth_Fed_St_Grnts

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "GrnWtr_PBCntm_Area"
)

dep_variable <- "Budget_EnvrnmtlHlth_Fed_St_Grnts"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata$GrnWtr_PBCntm_Area <- mydata$GrnWtr_PBCntm_Area+0.0000001
mydata <- mydata[4:16,] # Cut down to 2010 forward to remove outliers 

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Budget_EnvrnmtlHlth_Fed_St_Grnts  ~
    log(GrnWtr_PBCntm_Area), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    # SET COLUMN NAME
#   fieldvalue = "2024-02-08 16:33:22")                         # SET FIELD VALUE


#########################################################Budget_Housing_Ecnm_Dev################################

### Budget_Housing_Ecnm_Dev

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Pop_Ttl"
)

dep_variable <- "Budget_Housing_Ecnm_Dev"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Budget_Housing_Ecnm_Dev ~ poly(Pop_Ttl, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")                         

########################################################Budget_PbPrjctsAndPrgrms########################

### Budget_PbPrjctsAndPrgrms

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Budget_EnvrnmtlHlth_Fed_St_Grnts", "Budget_Housing_Ecnm_Dev", "Schlrly_Actv_TtlArtcls"
)

dep_variable <- "Budget_PbPrjctsAndPrgrms"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Budget_PbPrjctsAndPrgrms ~ poly(Budget_EnvrnmtlHlth_Fed_St_Grnts, 2) + 
    poly(Budget_Housing_Ecnm_Dev, 2) + 
    log(Schlrly_Actv_TtlArtcls), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships,
#   fieldname = "Date_Time",
#   fieldvalue = "2024-02-16 16:10:53")

################################################################ RMVD Demolished_Units#################

# ### Demolished_Units
# 
# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "Pop_Ttl"
# )
# 
# dep_variable <- "Demolished_Units"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[3:13,]
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Demolished_Units ~ log(Pop_Ttl), 
#   data = mydata
# )
# 
# summary(lm_model)
# 
# # Recapture independent variables (captures quadratic if necessary)
# indep_variables_updated <- names(lm_model$coefficients[-1])
# 
# #Run function to store model outputs
# output_data <- store_lm_outputs(
#   lm_object = lm_model,
#   x = indep_variables_updated,
#   y = dep_variable)
# 
# #View(output_data)
# 
# ## SAVE RESULTS 
# # Bind to mined relationships data frame 
# mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

################################################################Disability_Prcnt###############

### Disability_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt", "EBLL_Chldrn_5_Prcnt"
)

dep_variable <- "Disability_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
#mydata <- mydata[-1,] #Remove outlier

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Disability_Prcnt ~ 
    EBLL_Adults_Prcnt + 
    EBLL_Chldrn_Prcnt + 
    poly(EBLL_Chldrn_5_Prcnt,2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

#######################################################DrnkngWtr_PbLvls_Avg_MG_L#####################

### DrnkngWtr_PbLvls_Avg_MG_L

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "GrnWtr_PBCntm_Area", "Budget_Housing_Ecnm_Dev"
)

dep_variable <- "DrnkngWtr_PbLvls_Avg_MG_L"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]

mydata$GrnWtr_PBCntm_Area[3] <- mean(mydata$GrnWtr_PBCntm_Area[3:5])
mydata$GrnWtr_PBCntm_Area[4:5] <- NA
mydata$GrnWtr_PBCntm_Area[6] <- mean(mydata$GrnWtr_PBCntm_Area[6:8])
mydata$GrnWtr_PBCntm_Area[7:8] <- NA
mydata$GrnWtr_PBCntm_Area[9] <- mean(mydata$GrnWtr_PBCntm_Area[9:10])
mydata$GrnWtr_PBCntm_Area[10] <- NA
mydata$GrnWtr_PBCntm_Area[11] <- mean(mydata$GrnWtr_PBCntm_Area[11:13])
mydata$GrnWtr_PBCntm_Area[12:13] <- NA
mydata$GrnWtr_PBCntm_Area[14] <- mean(mydata$GrnWtr_PBCntm_Area[14:16])
mydata$GrnWtr_PBCntm_Area[15:16] <- NA

mydata <- mydata[complete.cases(mydata), ]
#mydata$GrnWtr_PBCntm_Area <- mydata$GrnWtr_PBCntm_Area + 0.000000000001

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  DrnkngWtr_PbLvls_Avg_MG_L ~ poly(GrnWtr_PBCntm_Area, 2) + poly(Budget_Housing_Ecnm_Dev, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

###############################################################EBLL_Adults_Prcnt#################

### EBLL_Adults_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Pre1980_Owned_Prcnt", 
  "Pre1980_Rented_Prcnt", 
  "NonWhite_Prcnt",
  "GoogleTrendsIndex"
)

dep_variable <- "EBLL_Adults_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  EBLL_Adults_Prcnt ~ 
    Pre1980_Owned_Prcnt + 
    Pre1980_Rented_Prcnt + 
    log(NonWhite_Prcnt) + 
    log(GoogleTrendsIndex), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

###############################################################EBLL_Chldrn_Prcnt#################

### EBLL_Chldrn_Prcnt

## SET UP
# Assign a variable
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Pre1980_Owned_Prcnt", 
  "Pre1980_Rented_Prcnt", 
  "NonWhite_Prcnt",
  "GoogleTrendsIndex"
)

dep_variable <- "EBLL_Chldrn_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
#mydata <- mydata[-23,] # Remove interpolated outlier
mydata <- mydata[complete.cases(mydata), ]

#mydata$EBLL_Chldrn_Prcnt <- log(mydata$EBLL_Chldrn_Prcnt) # Making dep variable log transformed

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  EBLL_Chldrn_Prcnt ~ 
    Pre1980_Owned_Prcnt + 
    Pre1980_Rented_Prcnt + 
    log(NonWhite_Prcnt) + 
    log(GoogleTrendsIndex), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

###############################################################EBLL_Chldrn_5_Prcnt#################

### EBLL_Chldrn_5_Prcnt

## SET UP
# Assign a variable
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Pre1980_Owned_Prcnt", 
  "Pre1980_Rented_Prcnt", 
  "NonWhite_Prcnt",
  "GoogleTrendsIndex"
)

dep_variable <- "EBLL_Chldrn_5_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]


## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  EBLL_Chldrn_5_Prcnt ~ 
    log(Pre1980_Owned_Prcnt) + 
    Pre1980_Rented_Prcnt + 
    log(NonWhite_Prcnt) + 
    log(GoogleTrendsIndex), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

###########################################################Educ_BlwHghSchl_Prcnt####################

### Educ_BlwHghSchl_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt", "EBLL_Chldrn_5_Prcnt"
)

dep_variable <- "Educ_BlwHghSchl_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Educ_BlwHghSchl_Prcnt ~ 
    sqrt(EBLL_Adults_Prcnt) + 
    poly(EBLL_Chldrn_Prcnt, 2) + 
    EBLL_Chldrn_5_Prcnt, 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

#############################################################FuelUsage_ThousGals#####################

### FuelUsage_ThousGals

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "VMT"
)

dep_variable <- "FuelUsage_ThousGals"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
#mydata$VMT <- log(mydata$VMT)

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  FuelUsage_ThousGals ~ poly(VMT, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

###############################################################GoogleTrendsIndex##################

### GoogleTrendsIndex

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Educ_BlwHghSchl_Prcnt", "DrnkngWtr_PbLvls_Avg_MG_L"
)

dep_variable <- "GoogleTrendsIndex"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]

# GoogleTrendsIndex
mydata$GoogleTrendsIndex[3] <- mean(mydata$GoogleTrendsIndex[3:5])
mydata$GoogleTrendsIndex[4:5] <- NA
mydata$GoogleTrendsIndex[6] <- mean(mydata$GoogleTrendsIndex[6:8])
mydata$GoogleTrendsIndex[7:8] <- NA
mydata$GoogleTrendsIndex[9] <- mean(mydata$GoogleTrendsIndex[9:10])
mydata$GoogleTrendsIndex[10] <- NA
mydata$GoogleTrendsIndex[11] <- mean(mydata$GoogleTrendsIndex[11:13])
mydata$GoogleTrendsIndex[12:13] <- NA
mydata$GoogleTrendsIndex[14] <- mean(mydata$GoogleTrendsIndex[14:16])
mydata$GoogleTrendsIndex[15:16] <- NA

# Educ_BlwHghSchl_Prcnt
mydata$Educ_BlwHghSchl_Prcnt[3] <- mean(mydata$Educ_BlwHghSchl_Prcnt[3:5])
mydata$Educ_BlwHghSchl_Prcnt[4:5] <- NA
mydata$Educ_BlwHghSchl_Prcnt[6] <- mean(mydata$Educ_BlwHghSchl_Prcnt[6:8])
mydata$Educ_BlwHghSchl_Prcnt[7:8] <- NA
mydata$Educ_BlwHghSchl_Prcnt[9] <- mean(mydata$Educ_BlwHghSchl_Prcnt[9:10])
mydata$Educ_BlwHghSchl_Prcnt[10] <- NA
mydata$Educ_BlwHghSchl_Prcnt[11] <- mean(mydata$Educ_BlwHghSchl_Prcnt[11:13])
mydata$Educ_BlwHghSchl_Prcnt[12:13] <- NA
mydata$Educ_BlwHghSchl_Prcnt[14] <- mean(mydata$Educ_BlwHghSchl_Prcnt[14:16])
mydata$Educ_BlwHghSchl_Prcnt[15:16] <- NA

mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  GoogleTrendsIndex ~ 
    log(Educ_BlwHghSchl_Prcnt) + 
    poly(DrnkngWtr_PbLvls_Avg_MG_L, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

#########################################################GRAPI_35PctOrMore_Prcnt######################

### GRAPI_35PctOrMore_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "BlwPvrty_Prcnt"
)

dep_variable <- "GRAPI_35PctOrMore_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  GRAPI_35PctOrMore_Prcnt ~ poly(BlwPvrty_Prcnt, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     


##############################################################GrnWtr_PBCntm_Area####################

### GrnWtr_PBCntm_Area

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "AQI_AvgAnnual", "SldWste_Dspsd_Tons", "Urban_LULC_Prcnt"
)

dep_variable <- "GrnWtr_PBCntm_Area"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[-1,] #Remove 2000, extreme outlier 

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  GrnWtr_PBCntm_Area ~ AQI_AvgAnnual + SldWste_Dspsd_Tons + Urban_LULC_Prcnt, 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    


##############################################################HousingAfrd_Prcnt####################

### HousingAfrd_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "BlwPvrty_Prcnt"
)

dep_variable <- "HousingAfrd_Prcnt" 

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  HousingAfrd_Prcnt  ~ log(BlwPvrty_Prcnt), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")

########################################### RMVD Langu_EnglshLssThanWell_Hshlds_Prcnt#############################################################
# 
# 
# ### Langu_EnglshLssThanWell_Hshlds_Prcnt
# 
# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "Educ_BlwHghSchl_Prcnt"
# )
# 
# dep_variable <- "Langu_EnglshLssThanWell_Hshlds_Prcnt"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Langu_EnglshLssThanWell_Hshlds_Prcnt ~ log(Educ_BlwHghSchl_Prcnt), 
#   data = mydata
# )
# 
# summary(lm_model)
# 
# # Recapture independent variables (captures quadratic if necessary)
# indep_variables_updated <- names(lm_model$coefficients[-1])
# 
# #Run function to store model outputs
# output_data <- store_lm_outputs(
#   lm_object = lm_model,
#   x = indep_variables_updated,
#   y = dep_variable)
# 
# #View(output_data)
# 
# ## SAVE RESULTS 
# # Bind to mined relationships data frame 
# mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")   

#############################################################MedIncm_HshldAnnual####################

### MedIncm_HshldAnnual

## SET UP
# Assign variables 

indep_variables <- c(                                    # ASSIGN VARIABLES
  "HousingAfrd_Prcnt"
)

dep_variable <- "MedIncm_HshldAnnual"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  MedIncm_HshldAnnual ~ 
    HousingAfrd_Prcnt, 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

##################################################################NonWhite_Prcnt##################

### NonWhite_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "BlwPvrty_Prcnt"
)

dep_variable <- "NonWhite_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  NonWhite_Prcnt ~ log(BlwPvrty_Prcnt), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    



#########################################################################Pop_Ttl#########

### Pop_Ttl

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "MedIncm_HshldAnnual"
)

dep_variable <- "Pop_Ttl"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Pop_Ttl ~ MedIncm_HshldAnnual, 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

#############################################################Pre1980_Owned_Prcnt##################

### Pre1980_Owned_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Urban_LULC_Prcnt"
)

dep_variable <- "Pre1980_Owned_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Pre1980_Owned_Prcnt ~ Urban_LULC_Prcnt, 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

############################################################Pre1980_Rented_Prcnt###################

### Pre1980_Rented_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Urban_LULC_Prcnt"
)

dep_variable <- "Pre1980_Rented_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Pre1980_Rented_Prcnt ~ Urban_LULC_Prcnt, 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

###############################################Release_OthrCntmts_PoltnFclts_Lbs################################

### Release_OthrCntmts_PoltnFclts_Lbs

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Unmplmnt_Prcnt"
)

dep_variable <- "Release_OthrCntmts_PoltnFclts_Lbs"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[-1,] # Remove outlier for the year 2000

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Release_OthrCntmts_PoltnFclts_Lbs ~ 
    log(Unmplmnt_Prcnt), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

#######################################################Release_Pb_PoltnFclts_Lbs###########################

### Release_Pb_PoltnFclts_Lbs

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
   "Unmplmnt_Prcnt"
)

dep_variable <- "Release_Pb_PoltnFclts_Lbs"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[-1,] # Remove outlier for the year 2000
mydata <- mydata[-10,] # Remove outlier for the year 2019
mydata <- mydata[-10,] # Remove outlier for the year 2020


## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Release_Pb_PoltnFclts_Lbs ~ 
    sqrt(Unmplmnt_Prcnt), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    



##########################################################Schlrly_Actv_TtlArtcls######################

### Schlrly_Actv_TtlArtcls

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt", "EBLL_Chldrn_5_Prcnt"
)

dep_variable <- "Schlrly_Actv_TtlArtcls"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Schlrly_Actv_TtlArtcls ~ 
    poly(EBLL_Adults_Prcnt, 2) + 
    poly(EBLL_Chldrn_Prcnt, 2) + 
    poly(EBLL_Chldrn_5_Prcnt, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

##############################################################SldWste_Dspsd_Tons#####################

### SldWste_Dspsd_Tons

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Pop_Ttl"
)

dep_variable <- "SldWste_Dspsd_Tons"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  SldWste_Dspsd_Tons ~ 
    poly(Pop_Ttl, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")    

#################################################SMOCAPI_wMrtg_35PctOrMore_Prcnt##############################

### SMOCAPI_wMrtg_35PctOrMore_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "BlwPvrty_Prcnt"
)

dep_variable <- "SMOCAPI_wMrtg_35PctOrMore_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  SMOCAPI_wMrtg_35PctOrMore_Prcnt ~ 
    BlwPvrty_Prcnt, 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")   

###############################################SMOCAPI_wNoMrtg_35PctOrMore_Prcnt###############################

### SMOCAPI_wNoMrtg_35PctOrMore_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "BlwPvrty_Prcnt"
)

dep_variable <- "SMOCAPI_wNoMrtg_35PctOrMore_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[-2,] # Remove 2011 outlier 

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  SMOCAPI_wNoMrtg_35PctOrMore_Prcnt ~ 
    log(BlwPvrty_Prcnt), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")   

######################################################SNAP_ReceivingHshlds_Prcnt#################################################################

### SNAP_ReceivingHshlds_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Unmplmnt_Prcnt"
)

dep_variable <- "SNAP_ReceivingHshlds_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  SNAP_ReceivingHshlds_Prcnt ~ 
    poly(Unmplmnt_Prcnt, 2), 
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")  

#################################################################Uninsured_Prcnt#################

### Uninsured_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Unmplmnt_Prcnt"
)

dep_variable <- "Uninsured_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Uninsured_Prcnt ~ 
    Unmplmnt_Prcnt,  
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")   

##################################################################Unmplmnt_Prcnt##############

### Unmplmnt_Prcnt


## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Disability_Prcnt"
)

dep_variable <- "Unmplmnt_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[-1,] #Remove 2010 outlier

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Unmplmnt_Prcnt ~ 
    log(Disability_Prcnt),  
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22") 

################################################################Urban_LULC_Prcnt##################

### Urban_LULC_Prcnt


## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Pop_Ttl"
)

dep_variable <- "Urban_LULC_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]


## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Urban_LULC_Prcnt ~ 
    poly(Pop_Ttl, 2),  
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22") 

#############################################################################VMT######

### VMT


## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Pop_Ttl"
)

dep_variable <- "VMT"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[-1,] # Removed year 2000 
mydata <- mydata[-11,]# Removed year 2020

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  VMT ~ 
    poly(Pop_Ttl, 2),  
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22") 


##################################################################Outflow#################

### EBLL_Chldrn_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Budget_PbPrjctsAndPrgrms", "Budget_Housing_Ecnm_Dev", "SNAP_ReceivingHshlds_Prcnt", "Uninsured_Prcnt"
)

dep_variable <- "EBLL_Chldrn_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata$Budget_PbPrjctsAndPrgrms <- mydata$Budget_PbPrjctsAndPrgrms+0.000001

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  EBLL_Chldrn_Prcnt ~ 
    Budget_PbPrjctsAndPrgrms + 
    Budget_Housing_Ecnm_Dev + 
    log(SNAP_ReceivingHshlds_Prcnt) + 
    Uninsured_Prcnt,  
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

##View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22") 


### EBLL_Chldrn_5_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Budget_PbPrjctsAndPrgrms", "Budget_Housing_Ecnm_Dev", "SNAP_ReceivingHshlds_Prcnt", "Uninsured_Prcnt"
)

dep_variable <- "EBLL_Chldrn_5_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata$Budget_PbPrjctsAndPrgrms <- mydata$Budget_PbPrjctsAndPrgrms+0.000001

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  EBLL_Chldrn_5_Prcnt ~ 
    Budget_PbPrjctsAndPrgrms + 
    poly(Budget_Housing_Ecnm_Dev, 2) + 
    log(SNAP_ReceivingHshlds_Prcnt) + 
    Uninsured_Prcnt,  
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22") 



### EBLL_Adults_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Budget_PbPrjctsAndPrgrms", "Budget_Housing_Ecnm_Dev", "SNAP_ReceivingHshlds_Prcnt", "Uninsured_Prcnt"
)

dep_variable <- "EBLL_Adults_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata$Budget_PbPrjctsAndPrgrms <- mydata$Budget_PbPrjctsAndPrgrms+0.000001

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  EBLL_Adults_Prcnt ~ 
    Budget_PbPrjctsAndPrgrms + 
    poly(Budget_Housing_Ecnm_Dev, 2) + 
    log(SNAP_ReceivingHshlds_Prcnt) + 
    Uninsured_Prcnt,  
  data = mydata
)

summary(lm_model)

# Recapture independent variables (captures quadratic if necessary)
indep_variables_updated <- names(lm_model$coefficients[-1])

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables_updated,
  y = dep_variable)

#View(output_data)

## SAVE RESULTS 
# Bind to mined relationships data frame 
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22") 


####### SCRATCH ######

### ORIGINAL


## SET UP
# Assign variables 
indep_variables <- c(
  "FuelUsage_ThousGals", 
  "Release_OthrCntmts_PoltnFclts_Lbs",                
  "Release_Pb_PoltnFclts_Lbs"                           
)

dep_variable <- "AQI_AvgAnnual"   

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## PLOTTING
# Plot for normality of dep variable 
plot(stats::density(mydata[[dep_variable]]))

# Plot scatter plots for all indep variables 
for (indep_var in indep_variables) {
  plotting_scatters(x = mydata[[indep_var]], 
                    y = mydata[[dep_variable]], 
                    x_name = indep_var)
}

## COVARIATION MINING

# Create empty dataframe
final_results <- data.frame(
  Variable = character(),
  Transformation = character(), 
  R_squared = numeric(), 
  stringsAsFactors = FALSE
)

# Pull r-squared for EACH linear transformation
for (indep_var in indep_variables) {
  results <- covariation_mining(
    y = mydata[[dep_variable]], 
    x = mydata[[indep_var]], 
    x_name = indep_var, 
    data = mydata)
  
  final_results <- rbind(
    final_results, 
    results
  )
}

# Print results
print(final_results)

## SELECT MODEL 
# Generate the model with no transofrmations 
paste(dep_variable, "~", paste(indep_variables, collapse = " + "))

# Manually copy and paste that model and add transformations 
lm_model <- lm( 
  AQI_AvgAnnual ~
    FuelUsage_ThousGals + 
    log(Release_OthrCntmts_PoltnFclts_Lbs) +         
    log(Release_Pb_PoltnFclts_Lbs), 
  data = mydata
)

summary(lm_model)

#Run function to store model outputs
output_data <- store_lm_outputs(
  lm_object = lm_model,
  x = indep_variables,
  y = dep_variable
)

#View(output_data)


## SAVE RESULTS 
# Bind to mined relationships data frame 
#mined_relationships <- output_data
mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed # SET COLUMN NAME & FIELD VALUE
mined_relationships <- trim_df(
  df = mined_relationships, 
  fieldname = "Date_Time",                                    
  fieldvalue = "2024-02-08 16:33:22")                        

