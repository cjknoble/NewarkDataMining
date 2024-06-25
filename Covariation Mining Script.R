
################################################################################
### LOAD REQUIRED PACKAGES, FUNCTIONS, AND DATA

## LOAD PACKAGES 

library(stats)
library(GreyModel)

## IMPORT STANDARDIZED MASTER DATASET
alldata <- read.csv(
  file = "C:/...Data.csv", 
  row.names = "Years")

## CREATE ALL CUSTOM FUNCTIONS FOR COVARIATION MINING
# FUNCTION: Greymodel creation
greymodeling <- function(y){
  for (i in 2:11){
    greymodelfit <- GM(mydata[, y])
    
    a <- greymodelfit[["a"]]
    
    if (sqrt(a^2) > 0.3) {
      print("Value for a too large")
    }
    
    else {
      b <- greymodelfit[["b"]]
      x <- mydata[, y][1]
      print(
        (((b-0.5*a)/(b+0.5*a))^(i-2))*
          ((b-a*x)/(b+0.5*a))
      )
    }
  }
}

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
  adjr_squared <- summary(lm_object)$adj.r.squared
  intrcpt <- summary(lm_object)$coefficients[1, 1]
  
  gmdata <- alldata[ , y, drop = F]
  gmdata <- gmdata[complete.cases(gmdata), ]
  greymodelfit <- GM(gmdata)
  a <- greymodelfit[["a"]]
  b <- greymodelfit[["b"]]
  first <- mydata[, y][1]

  # Create model string
  model_string <- paste("y ~", paste0(intrcpt, " + ", paste(coefficients, "*", x, collapse = " + ")), collapse = " + ")
  
  # Create grey model string 
  if (sqrt(a^2) > 0.3) {
    grey_string <- paste("value for 'a' is", paste0(a, ", which is above 0.3 or below -0.3"))
  }
  
  else {
    grey_string <- paste(
      "(((", 
      paste0(b, "-0.5*", 
             paste0(a, ")/("), 
             paste0(b, "+0.5*"), 
             paste0(a, "))^(TIME-2))*(("), 
             paste0(b,"-"), 
             paste0(a, "*"), 
             paste0(first), ")/("), 
      paste0(b, "+0.5*"),
      paste0(a,"))"), 
      collapse = "")
  }
  
  # Initialize an empty data frame
  result_df <- data.frame(
    Dependent_Variable = character(),
    Independent_Variable = character(),
    Coefficient = numeric(),
    P_Value = numeric(),
    R_Squared = numeric(),
    AdjR_Squared = numeric(),
    Intercept = numeric(),
    Model = character(),
    Date_Time = character(),
    GreyModel = character(),
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
      AdjR_Squared = adjr_squared,
      Intercept = intrcpt,
      Model = model_string,
      Date_Time = timestamp,
      GreyModel = grey_string
    ))
  }
  
  print(result_df$Model[1])
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


################################################AQI_AvgAnnual###################

### AQI_AvgAnnual

## SET UP
# Assign variables 
indep_variables <- c(                                # ASSIGN VARIABLES
  "VMT", 
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
    VMT + 
    log(Release_OthrCntmts_PoltnFclts_Lbs) + 
    log(Release_Pb_PoltnFclts_Lbs), 
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
# Bind to mined relationships data frame (in this case, creaing data frame for the first time)
mined_relationships <- output_data
#mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed # SET COLUMN NAME & FIELD VALUE
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")                        


################################################BlwPvrty_Prcnt####################

### BlwPvrty_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Unmplmnt_Prcnt"
)

dep_variable <- "BlwPvrty_Prcnt" 

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

mydata <- mydata[rownames(mydata) != "2021", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(BlwPvrty_Prcnt ~ 
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
#   fieldname = "Date_Time",                                    # SET COLUMN NAME
#   fieldvalue = "2024-02-16 15:42:04")                         # SET FIELD VALUE



################################################Budget_HousingEcon_PBPrjcts_Mean2########################

### Budget_HousingEcon_PBPrjcts_Mean2

## SET UP
# Assign variables 
indep_variables <- c(                                  # ASSIGN VARIABLES
  "MedIncm_HshldAnnual", "NonWhite_Prcnt", "AQI_AvgAnnual"
)

dep_variable <- "Budget_HousingEcon_PBPrjcts_Mean2"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

mydata <- mydata[rownames(mydata) != "2011", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2017", , drop = FALSE] # OUTLIER


## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Budget_HousingEcon_PBPrjcts_Mean2 ~ 
    log(MedIncm_HshldAnnual) + 
    NonWhite_Prcnt+ 
    poly(AQI_AvgAnnual, 2), 
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


################################################EBLL_Adults_Prcnt#################

### EBLL_Adults_Prcnt

## SET UP
#Assign variables

indep_variables <- c(
  "BlwPvrty_Prcnt", "SNAP_ReceivingHshlds_Prcnt", "Uninsured_Prcnt"
)

dep_variable <- "EBLL_Adults_Prcnt"

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS
  EBLL_Adults_Prcnt ~
    log(BlwPvrty_Prcnt) + 
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

################################################EBLL_Chldrn_Prcnt#################

### EBLL_Chldrn_Prcnt

## SET UP

indep_variables <- c(
  "BlwPvrty_Prcnt", "SNAP_ReceivingHshlds_Prcnt", "Uninsured_Prcnt"
)

dep_variable <- "EBLL_Chldrn_Prcnt"

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
  EBLL_Chldrn_Prcnt ~
    BlwPvrty_Prcnt + 
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


################################################Educ_BlwHghSchl_Prcnt####################

### Educ_BlwHghSchl_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt", "Unmplmnt_Prcnt"
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
    EBLL_Adults_Prcnt + 
    log(EBLL_Chldrn_Prcnt) + 
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

################################################GoogleTrendsIndex##################

### GoogleTrendsIndex

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
    "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt"
    )

dep_variable <- "GoogleTrendsIndex"  

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
  GoogleTrendsIndex ~ 
    log(EBLL_Adults_Prcnt) + 
    log(EBLL_Chldrn_Prcnt), 
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
#   fieldvalue = "2024-03-29 11:30:09")

################################################HousingAfrd_Prcnt####################

### HousingAfrd_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "BlwPvrty_Prcnt", "SNAP_ReceivingHshlds_Prcnt", "GoogleTrendsIndex"
)

dep_variable <- "HousingAfrd_Prcnt" 

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

mydata <- mydata[rownames(mydata) != "2010", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2011", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2012", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  HousingAfrd_Prcnt ~ 
    log(BlwPvrty_Prcnt) + 
    log(SNAP_ReceivingHshlds_Prcnt) + 
    poly(GoogleTrendsIndex, 2), 
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

################################################HsngBlt_1959orBefore##################

### HsngBlt_1959orBefore

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "MedIncm_HshldAnnual", "Budget_HousingEcon_PBPrjcts_Mean2"
)

dep_variable <- "HsngBlt_1959orBefore"  

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
  HsngBlt_1959orBefore ~ 
    log(MedIncm_HshldAnnual) + 
    poly(Budget_HousingEcon_PBPrjcts_Mean2, 2), 
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

################################################MedIncm_HshldAnnual####################

### MedIncm_HshldAnnual

## SET UP
# Assign variables 

indep_variables <- c(                                    # ASSIGN VARIABLES
  "HousingAfrd_Prcnt", "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt"
)

dep_variable <- "MedIncm_HshldAnnual"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[rownames(mydata) != "2020", , drop = FALSE] # OUTLIER
 

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  MedIncm_HshldAnnual ~ 
    HousingAfrd_Prcnt + 
    log(EBLL_Adults_Prcnt) + 
    poly(EBLL_Chldrn_Prcnt, 2), 
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

################################################NonWhite_Prcnt##################

### NonWhite_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "MedIncm_HshldAnnual", "AQI_AvgAnnual"
)

dep_variable <- "NonWhite_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

mydata <- mydata[rownames(mydata) != "2000", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2016", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2019", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  NonWhite_Prcnt ~ 
    log(MedIncm_HshldAnnual) + 
    log(AQI_AvgAnnual), 
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



################################################Release_OthrCntmts_PoltnFclts_Lbs################################

### Release_OthrCntmts_PoltnFclts_Lbs

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Unmplmnt_Prcnt", "HousingAfrd_Prcnt"
)

dep_variable <- "Release_OthrCntmts_PoltnFclts_Lbs"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[rownames(mydata) != "2000", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2014", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Release_OthrCntmts_PoltnFclts_Lbs ~ 
    Unmplmnt_Prcnt + 
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

################################################Release_Pb_PoltnFclts_Lbs###########################

### Release_Pb_PoltnFclts_Lbs

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
   "Unmplmnt_Prcnt", "HousingAfrd_Prcnt"
)

dep_variable <- "Release_Pb_PoltnFclts_Lbs"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[rownames(mydata) != "2000", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2019", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2020", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  Release_Pb_PoltnFclts_Lbs ~ 
    sqrt(Unmplmnt_Prcnt) + 
    sqrt(HousingAfrd_Prcnt), 
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

################################################SNAP_ReceivingHshlds_Prcnt#################################################################

### SNAP_ReceivingHshlds_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Uninsured_Prcnt", "BlwPvrty_Prcnt"
)

dep_variable <- "SNAP_ReceivingHshlds_Prcnt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[rownames(mydata) != "2010", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  SNAP_ReceivingHshlds_Prcnt ~ 
    Uninsured_Prcnt + 
    poly(BlwPvrty_Prcnt, 2), 
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

################################################Uninsured_Prcnt#################

### Uninsured_Prcnt

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "Educ_BlwHghSchl_Prcnt", "Unmplmnt_Prcnt"
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
    Unmplmnt_Prcnt + 
    Educ_BlwHghSchl_Prcnt,  
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

################################################Unmplmnt_Prcnt##############

### Unmplmnt_Prcnt


## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt"
)

dep_variable <- "Unmplmnt_Prcnt"  

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
  Unmplmnt_Prcnt ~ 
    EBLL_Adults_Prcnt + 
    EBLL_Chldrn_Prcnt,  
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



################################################VMT######

### VMT

## SET UP
# Assign variables 
indep_variables <- c(                                    # ASSIGN VARIABLES
  "MedIncm_HshldAnnual", "BlwPvrty_Prcnt"
)

dep_variable <- "VMT"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[rownames(mydata) != "2000", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2020", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2021", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2022", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
  VMT ~ 
    poly(MedIncm_HshldAnnual, 2) + 
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


################################################Change Rate (Outflow) - Children #################

### EBLL_Chldrn_ChngeRt Outflow

## SET UP
# Assign variables 

indep_variables <- c(                                    # ASSIGN VARIABLES
  "AQI_AvgAnnual", "HsngBlt_1959orBefore","GoogleTrendsIndex", "Budget_HousingEcon_PBPrjcts_Mean2"
)

dep_variable <- "EBLL_Chldrn_ChngeRt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]

mydata <- mydata[rownames(mydata) != "2019", , drop = FALSE] # OUTLIER
mydata <- mydata[rownames(mydata) != "2012", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS 
  EBLL_Chldrn_ChngeRt ~ 
    poly(AQI_AvgAnnual, 2) + #Can be removed with little impact
    log(HsngBlt_1959orBefore) + 
    poly(GoogleTrendsIndex, 2) + 
    poly(Budget_HousingEcon_PBPrjcts_Mean2, 2),
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


################################################Change Rate (Outflow) - Adults #################

### EBLL_Adults_ChngeRt

## SET UP
# Assign variables 

indep_variables <- c(                                    # ASSIGN VARIABLES
  "AQI_AvgAnnual", "HsngBlt_1959orBefore","GoogleTrendsIndex", "Budget_HousingEcon_PBPrjcts_Mean2"
)

dep_variable <- "EBLL_Adults_ChngeRt"  

# Drop rows with nulls
all_vars <- c(indep_variables, dep_variable)

mydata <- alldata[, all_vars, drop = FALSE]
mydata <- mydata[complete.cases(mydata), ]
mydata <- mydata[rownames(mydata) != "2015", , drop = FALSE] # OUTLIER

## RUN FUNCTION FOR PHASE 1
# Function
phase_one(indep_variables = indep_variables, dep_variable = dep_variable)

## SELECT MODEL FOR PHASE 2
# Manually copy and paste that model and add transformations 

lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS 
  EBLL_Adults_ChngeRt ~ 
    poly(AQI_AvgAnnual, 2) + 
    log(HsngBlt_1959orBefore) + 
    poly(GoogleTrendsIndex, 2)
    + poly(Budget_HousingEcon_PBPrjcts_Mean2, 2),
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


####### EXPORT RESULTING RELATIONSHIPS ######

# After Running Whole Script, Export mined_relationships
write.csv(mined_relationships, file = "C:/...Final.csv")

###################################################################################
### TESTED AND REMOVED PRESERVED BELOW ###
###################################################################################

################################################################RMVD BrthRt_PrcntofWmn##################

### BrthRt_PrcntofWmn

## SET UP
# Assign variables
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "Educ_BlwHghSchl_Prcnt"
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
# # mydata <- mydata[rownames(mydata) != "2017", , drop = FALSE] #Remove 2015 outlier
# # mydata <- mydata[rownames(mydata) != "2018", , drop = FALSE] #Remove 2015 outlier
# mydata <- mydata[rownames(mydata) != "2022", , drop = FALSE] #Remove 2015 outlier
# 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS
#   BrthRt_PrcntofWmn ~ 
#     poly(Educ_BlwHghSchl_Prcnt, 2),
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

################################################RMVD Budget_EnvrnmtlHlth_Fed_St_Grnts#############################################

### Budget_EnvrnmtlHlth_Fed_St_Grnts

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "AQI_AvgAnnual", "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt"
# )
# 
# dep_variable <- "Budget_EnvrnmtlHlth_Fed_St_Grnts"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# 
# # mydataavg <- mydata
# # mydataavg$AQI_AvgAnnual[8] <- mean(mydataavg$AQI_AvgAnnual[8:9])
# # mydataavg <- mydataavg[-9,]
# # mydataavg$AQI_AvgAnnual[9] <- mean(mydataavg$AQI_AvgAnnual[9:10])
# # mydataavg <- mydataavg[-10,]
# # mydataavg$AQI_AvgAnnual[10] <- mean(mydataavg$AQI_AvgAnnual[10:11])
# # mydataavg <- mydataavg[-11,]
# # mydataavg$AQI_AvgAnnual[11] <- mean(mydataavg$AQI_AvgAnnual[11:12])
# # mydataavg <- mydataavg[-12,]
# # mydataavg$AQI_AvgAnnual[12] <- mean(mydataavg$AQI_AvgAnnual[12:13])
# # mydataavg <- mydataavg[-13,]
# # mydataavg$AQI_AvgAnnual[13] <- mean(mydataavg$AQI_AvgAnnual[13:14])
# # mydataavg <- mydataavg[-14,]
# # mydataavg$AQI_AvgAnnual[14] <- mean(mydataavg$AQI_AvgAnnual[14:15])
# # mydataavg <- mydataavg[-15,]
# # mydataavg$AQI_AvgAnnual[15] <- mean(mydataavg$AQI_AvgAnnual[15:16])
# # mydataavg <- mydataavg[-16,]
# # mydataavg <- mydataavg[complete.cases(mydataavg), ]
# # mydatatest <- mydataavg["AQI_AvgAnnual"]
# # 
# # mydataavg <- mydata
# # mydataavg$EBLL_Adults_Prcnt[8] <- mean(mydataavg$EBLL_Adults_Prcnt[8:9])
# # mydataavg <- mydataavg[-9,]
# # mydataavg$EBLL_Adults_Prcnt[9] <- mean(mydataavg$EBLL_Adults_Prcnt[9:10])
# # mydataavg <- mydataavg[-10,]
# # mydataavg$EBLL_Adults_Prcnt[10] <- mean(mydataavg$EBLL_Adults_Prcnt[10:11])
# # mydataavg <- mydataavg[-11,]
# # mydataavg$EBLL_Adults_Prcnt[11] <- mean(mydataavg$EBLL_Adults_Prcnt[11:12])
# # mydataavg <- mydataavg[-12,]
# # mydataavg$EBLL_Adults_Prcnt[12] <- mean(mydataavg$EBLL_Adults_Prcnt[12:13])
# # mydataavg <- mydataavg[-13,]
# # mydataavg$EBLL_Adults_Prcnt[13] <- mean(mydataavg$EBLL_Adults_Prcnt[13:14])
# # mydataavg <- mydataavg[-14,]
# # mydataavg$EBLL_Adults_Prcnt[14] <- mean(mydataavg$EBLL_Adults_Prcnt[14:15])
# # mydataavg <- mydataavg[-15,]
# # mydataavg$EBLL_Adults_Prcnt[15] <- mean(mydataavg$EBLL_Adults_Prcnt[15:16])
# # mydataavg <- mydataavg[-16,]
# # mydataavg <- mydataavg[complete.cases(mydataavg), ]
# # mydataavg <- mydataavg["EBLL_Adults_Prcnt"]
# # mydatatest <- cbind(mydatatest, mydataavg)
# # 
# # mydataavg <- mydata
# # mydataavg$EBLL_Chldrn_Prcnt[8] <- mean(mydataavg$EBLL_Chldrn_Prcnt[8:9])
# # mydataavg <- mydataavg[-9,]
# # mydataavg$EBLL_Chldrn_Prcnt[9] <- mean(mydataavg$EBLL_Chldrn_Prcnt[9:10])
# # mydataavg <- mydataavg[-10,]
# # mydataavg$EBLL_Chldrn_Prcnt[10] <- mean(mydataavg$EBLL_Chldrn_Prcnt[10:11])
# # mydataavg <- mydataavg[-11,]
# # mydataavg$EBLL_Chldrn_Prcnt[11] <- mean(mydataavg$EBLL_Chldrn_Prcnt[11:12])
# # mydataavg <- mydataavg[-12,]
# # mydataavg$EBLL_Chldrn_Prcnt[12] <- mean(mydataavg$EBLL_Chldrn_Prcnt[12:13])
# # mydataavg <- mydataavg[-13,]
# # mydataavg$EBLL_Chldrn_Prcnt[13] <- mean(mydataavg$EBLL_Chldrn_Prcnt[13:14])
# # mydataavg <- mydataavg[-14,]
# # mydataavg$EBLL_Chldrn_Prcnt[14] <- mean(mydataavg$EBLL_Chldrn_Prcnt[14:15])
# # mydataavg <- mydataavg[-15,]
# # mydataavg <- mydataavg[complete.cases(mydataavg), ]
# # mydataavg <- mydataavg["EBLL_Chldrn_Prcnt"]
# # mydatatest <- cbind(mydatatest, mydataavg)
# # 
# # # mydata$GrnWtr_PBCntm_Area[8] <- mean(mydata$GrnWtr_PBCntm_Area[8:9])
# # # mydata <- mydata[-9,]
# # # mydata$GrnWtr_PBCntm_Area[9] <- mean(mydata$GrnWtr_PBCntm_Area[9:10])
# # # mydata <- mydata[-10,]
# # # mydata$GrnWtr_PBCntm_Area[10] <- mean(mydata$GrnWtr_PBCntm_Area[10:11])
# # # mydata <- mydata[-11,]
# # # mydata$GrnWtr_PBCntm_Area[11] <- mean(mydata$GrnWtr_PBCntm_Area[11:12])
# # # mydata <- mydata[-12,]
# # # mydata$GrnWtr_PBCntm_Area[12] <- mean(mydata$GrnWtr_PBCntm_Area[12:13])
# # # mydata <- mydata[-13,]
# # # mydata$GrnWtr_PBCntm_Area[13] <- mean(mydata$GrnWtr_PBCntm_Area[13:14])
# # # mydata <- mydata[-14,]
# # # mydata$GrnWtr_PBCntm_Area[14] <- mean(mydata$GrnWtr_PBCntm_Area[14:15])
# # # mydata <- mydata[-15,]
# # # mydata$GrnWtr_PBCntm_Area[15] <- mean(mydata$GrnWtr_PBCntm_Area[15:16])
# # # mydata <- mydata[-16,]
# # 
# # mydata <- mydata[complete.cases(mydata), ]
# # mydata <- mydata["Budget_EnvrnmtlHlth_Fed_St_Grnts_Avg"]
# # mydata <- cbind(mydatatest, mydata)
# 
# mydata <- mydata[complete.cases(mydata), ] # Removing outliers just makes it progressively worse
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Budget_EnvrnmtlHlth_Fed_St_Grnts  ~
#     poly(AQI_AvgAnnual, 2) + 
#     poly(EBLL_Adults_Prcnt, 2) + 
#     poly(EBLL_Chldrn_Prcnt, 2), 
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


#########################################################RMVD Budget_Housing_Ecnm_Dev################################

### Budget_Housing_Ecnm_Dev

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "MedIncm_HshldAnnual", "SNAP_ReceivingHshlds_Prcnt"
# )
# 
# dep_variable <- "Budget_Housing_Ecnm_Dev"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[rownames(mydata) != "2020", , drop = FALSE]
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Budget_Housing_Ecnm_Dev ~ 
#     poly(MedIncm_HshldAnnual, 2) + 
#     SNAP_ReceivingHshlds_Prcnt, 
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

########################################################RMVD Budget_PbPrjctsAndPrgrms########################

### Budget_PbPrjctsAndPrgrms

## SET UP
# Assign variables 
# indep_variables <- c(                                  # ASSIGN VARIABLES
#   "Budget_Housing_Ecnm_Dev", "NonWhite_Prcnt", "AQI_AvgAnnual"
#   )
# 
# dep_variable <- "Budget_PbPrjctsAndPrgrms"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# 
# mydata <- mydata[rownames(mydata) != "2011", , drop = FALSE]
# mydata <- mydata[rownames(mydata) != "2017", , drop = FALSE]
# mydata <- mydata[rownames(mydata) != "2020", , drop = FALSE]
# 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Budget_PbPrjctsAndPrgrms ~ 
#     Budget_Housing_Ecnm_Dev + 
#     NonWhite_Prcnt + 
#     poly(AQI_AvgAnnual, 2), 
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

################################################################ RMVD Disability_Prcnt###############

### Disability_Prcnt

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "SNAP_ReceivingHshlds_Prcnt", "Uninsured_Prcnt"
# )
# 
# dep_variable <- "Disability_Prcnt"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# #mydata <- mydata[-7,] #Remove outlier
# #mydata <- mydata[-1,]
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Disability_Prcnt ~ 
#     poly(SNAP_ReceivingHshlds_Prcnt, 2) + 
#     poly(Uninsured_Prcnt, 2), 
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

####################################################### RMVD DrnkngWtr_PbLvls_Avg_MG_L#####################

### DrnkngWtr_PbLvls_Avg_MG_L

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#    "NonWhite_Prcnt", "Budget_Housing_Ecnm_Dev"
# )
# 
# # indep_variables <- c(                                    # ASSIGN VARIABLES
# #   "GrnWtr_PBCntm_Area", "Budget_Housing_Ecnm_Dev", "NonWhite_Prcnt"
# # )
# 
# dep_variable <- "DrnkngWtr_PbLvls_Avg_MG_L"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# 
# # mydata$GrnWtr_PBCntm_Area[3] <- mean(mydata$GrnWtr_PBCntm_Area[3:5])
# # mydata$GrnWtr_PBCntm_Area[4:5] <- NA
# # mydata$GrnWtr_PBCntm_Area[6] <- mean(mydata$GrnWtr_PBCntm_Area[6:8])
# # mydata$GrnWtr_PBCntm_Area[7:8] <- NA
# # mydata$GrnWtr_PBCntm_Area[9] <- mean(mydata$GrnWtr_PBCntm_Area[9:10])
# # mydata$GrnWtr_PBCntm_Area[10] <- NA
# # mydata$GrnWtr_PBCntm_Area[11] <- mean(mydata$GrnWtr_PBCntm_Area[11:13])
# # mydata$GrnWtr_PBCntm_Area[12:13] <- NA
# # mydata$GrnWtr_PBCntm_Area[14] <- mean(mydata$GrnWtr_PBCntm_Area[14:16])
# # mydata$GrnWtr_PBCntm_Area[15:16] <- NA
# 
# mydata$Budget_Housing_Ecnm_Dev[3] <- mean(mydata$Budget_Housing_Ecnm_Dev[3:5])
# mydata$Budget_Housing_Ecnm_Dev[4:5] <- NA
# mydata$Budget_Housing_Ecnm_Dev[6] <- mean(mydata$Budget_Housing_Ecnm_Dev[6:8])
# mydata$Budget_Housing_Ecnm_Dev[7:8] <- NA
# mydata$Budget_Housing_Ecnm_Dev[9] <- mean(mydata$Budget_Housing_Ecnm_Dev[9:10])
# mydata$Budget_Housing_Ecnm_Dev[10] <- NA
# mydata$Budget_Housing_Ecnm_Dev[11] <- mean(mydata$Budget_Housing_Ecnm_Dev[11:13])
# mydata$Budget_Housing_Ecnm_Dev[12:13] <- NA
# mydata$Budget_Housing_Ecnm_Dev[14] <- mean(mydata$Budget_Housing_Ecnm_Dev[14:16])
# mydata$Budget_Housing_Ecnm_Dev[15:16] <- NA
# 
# # mydata$Urban_LULC_Prcnt[3] <- mean(mydata$Urban_LULC_Prcnt[3:5])
# # mydata$Urban_LULC_Prcnt[4:5] <- NA
# # mydata$Urban_LULC_Prcnt[6] <- mean(mydata$Urban_LULC_Prcnt[6:8])
# # mydata$Urban_LULC_Prcnt[7:8] <- NA
# # mydata$Urban_LULC_Prcnt[9] <- mean(mydata$Urban_LULC_Prcnt[9:10])
# # mydata$Urban_LULC_Prcnt[10] <- NA
# # mydata$Urban_LULC_Prcnt[11] <- mean(mydata$Urban_LULC_Prcnt[11:13])
# # mydata$Urban_LULC_Prcnt[12:13] <- NA
# # mydata$Urban_LULC_Prcnt[14] <- mean(mydata$Urban_LULC_Prcnt[14:16])
# # mydata$Urban_LULC_Prcnt[15:16] <- NA
# # 
# # mydata$SldWste_Dspsd_Tons[3] <- mean(mydata$SldWste_Dspsd_Tons[3:5])
# # mydata$SldWste_Dspsd_Tons[4:5] <- NA
# # mydata$SldWste_Dspsd_Tons[6] <- mean(mydata$SldWste_Dspsd_Tons[6:8])
# # mydata$SldWste_Dspsd_Tons[7:8] <- NA
# # mydata$SldWste_Dspsd_Tons[9] <- mean(mydata$SldWste_Dspsd_Tons[9:10])
# # mydata$SldWste_Dspsd_Tons[10] <- NA
# # mydata$SldWste_Dspsd_Tons[11] <- mean(mydata$SldWste_Dspsd_Tons[11:13])
# # mydata$SldWste_Dspsd_Tons[12:13] <- NA
# # mydata$SldWste_Dspsd_Tons[14] <- mean(mydata$SldWste_Dspsd_Tons[14:16])
# # mydata$SldWste_Dspsd_Tons[15:16] <- NA
# # # 
# # mydata$AQI_AvgAnnual[3] <- mean(mydata$AQI_AvgAnnual[3:5])
# # mydata$AQI_AvgAnnual[4:5] <- NA
# # mydata$AQI_AvgAnnual[6] <- mean(mydata$AQI_AvgAnnual[6:8])
# # mydata$AQI_AvgAnnual[7:8] <- NA
# # mydata$AQI_AvgAnnual[9] <- mean(mydata$AQI_AvgAnnual[9:10])
# # mydata$AQI_AvgAnnual[10] <- NA
# # mydata$AQI_AvgAnnual[11] <- mean(mydata$AQI_AvgAnnual[11:13])
# # mydata$AQI_AvgAnnual[12:13] <- NA
# # mydata$AQI_AvgAnnual[14] <- mean(mydata$AQI_AvgAnnual[14:16])
# # mydata$AQI_AvgAnnual[15:16] <- NA
# 
# mydata$NonWhite_Prcnt[3] <- mean(mydata$NonWhite_Prcnt[3:5])
# mydata$NonWhite_Prcnt[4:5] <- NA
# mydata$NonWhite_Prcnt[6] <- mean(mydata$NonWhite_Prcnt[6:8])
# mydata$NonWhite_Prcnt[7:8] <- NA
# mydata$NonWhite_Prcnt[9] <- mean(mydata$NonWhite_Prcnt[9:10])
# mydata$NonWhite_Prcnt[10] <- NA
# mydata$NonWhite_Prcnt[11] <- mean(mydata$NonWhite_Prcnt[11:13])
# mydata$NonWhite_Prcnt[12:13] <- NA
# mydata$NonWhite_Prcnt[14] <- mean(mydata$NonWhite_Prcnt[14:16])
# mydata$NonWhite_Prcnt[15:16] <- NA
# # 
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[3] <- mean(mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[3:5])
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[4:5] <- NA
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[6] <- mean(mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[6:8])
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[7:8] <- NA
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[9] <- mean(mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[9:10])
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[10] <- NA
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[11] <- mean(mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[11:13])
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[12:13] <- NA
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[14] <- mean(mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[14:16])
# # mydata$Budget_EnvrnmtlHlth_Fed_St_Grnts[15:16] <- NA
# # 
# # mydata$MedIncm_HshldAnnual[3] <- mean(mydata$MedIncm_HshldAnnual[3:5])
# # mydata$MedIncm_HshldAnnual[4:5] <- NA
# # mydata$MedIncm_HshldAnnual[6] <- mean(mydata$MedIncm_HshldAnnual[6:8])
# # mydata$MedIncm_HshldAnnual[7:8] <- NA
# # mydata$MedIncm_HshldAnnual[9] <- mean(mydata$MedIncm_HshldAnnual[9:10])
# # mydata$MedIncm_HshldAnnual[10] <- NA
# # mydata$MedIncm_HshldAnnual[11] <- mean(mydata$MedIncm_HshldAnnual[11:13])
# # mydata$MedIncm_HshldAnnual[12:13] <- NA
# # mydata$MedIncm_HshldAnnual[14] <- mean(mydata$MedIncm_HshldAnnual[14:16])
# # mydata$MedIncm_HshldAnnual[15:16] <- NA
# 
# mydata <- mydata[complete.cases(mydata), ]
# #mydata$Budget_PbPrjctsAndPrgrms <- mydata$Budget_PbPrjctsAndPrgrms + 0.000000000001
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# # lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
# #   DrnkngWtr_PbLvls_Avg_MG_L ~ 
# #     log(Budget_PbPrjctsAndPrgrms) + 
# #     poly(NonWhite_Prcnt, 2) + 
# #     poly(AQI_AvgAnnual, 2), 
# #   data = mydata
# # )
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   DrnkngWtr_PbLvls_Avg_MG_L ~ 
#     poly(Budget_EnvrnmtlHlth_Fed_St_Grnts, 2) + 
#     Budget_Housing_Ecnm_Dev + 
#     poly(NonWhite_Prcnt, 2), 
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

############################################################### RMVD EBLL_Chldrn_5_Prcnt#################

### EBLL_Chldrn_5_Prcnt

## SET UP
# Assign a variable
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "Pre1980_Owned_Prcnt", 
#   "Pre1980_Rented_Prcnt", 
#   "NonWhite_Prcnt",
#   "GoogleTrendsIndex"
# )
# 
# dep_variable <- "EBLL_Chldrn_5_Prcnt"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   EBLL_Chldrn_5_Prcnt ~ 
#     log(Pre1980_Owned_Prcnt) + 
#     Pre1980_Rented_Prcnt + 
#     log(NonWhite_Prcnt) + 
#     log(GoogleTrendsIndex), 
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

#############################################################RMVD FuelUsage_ThousGals#####################

### FuelUsage_ThousGals

# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "VMT"
# )
# 
# dep_variable <- "FuelUsage_ThousGals"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[-21,] # remove 2020
# 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   FuelUsage_ThousGals ~ poly(VMT, 2), 
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
# View(output_data)
# 
# ## SAVE RESULTS 
# # Bind to mined relationships data frame 
# mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22")     

######################################################### RMVD GRAPI_35PctOrMore_Prcnt######################

### GRAPI_35PctOrMore_Prcnt

# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "BlwPvrty_Prcnt"
# )
# 
# dep_variable <- "GRAPI_35PctOrMore_Prcnt"  
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
#   GRAPI_35PctOrMore_Prcnt ~ poly(BlwPvrty_Prcnt, 2), 
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


############################################################## RMVD GrnWtr_PBCntm_Area####################

### GrnWtr_PBCntm_Area

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "AQI_AvgAnnual", "Urban_LULC_Prcnt"
# )
# 
# dep_variable <- "GrnWtr_PBCntm_Area"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[-1,] #Remove 2000, extreme outlier 
# #mydata <- mydata[-15,] #Remove 2000, extreme outlier 
# 
# #mydata$GrnWtr_PBCntm_Area <- sqrt(mydata$GrnWtr_PBCntm_Area)
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   GrnWtr_PBCntm_Area ~ 
#     AQI_AvgAnnual + 
#     log(Urban_LULC_Prcnt), 
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

############################################################ RMVD HsngBlt_1960to79###################

### HsngBlt_1960to79

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#    "Budget_Housing_Ecnm_Dev", "Urban_LULC_Prcnt"
# )
# 
# dep_variable <- "HsngBlt_1960to79"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[-11,]
# mydata <- mydata[-7,]
# 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   HsngBlt_1960to79 ~ 
#     Budget_Housing_Ecnm_Dev + 
#     poly(Urban_LULC_Prcnt, 2), 
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

###########################################RMVD Langu_AtHomeNotEnglish_Prcnt#############################################################
# 
# 
### Langu_AtHomeNotEnglish_Prcnt

## SET UP
# Assign variables
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "NonWhite_Prcnt"
# )
# 
# 
# dep_variable <- "Langu_AtHomeNotEnglish_Prcnt"
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# 
# mydata <- mydata[rownames(mydata) != "2010", , drop = FALSE] #Remove 2015 outlier
# mydata <- mydata[rownames(mydata) != "2017", , drop = FALSE] #Remove 2015 outlier
# mydata <- mydata[rownames(mydata) != "2019", , drop = FALSE] #Remove 2015 outlier
# 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS
#   Langu_AtHomeNotEnglish_Prcnt ~ 
#     NonWhite_Prcnt,
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

######################################################################### RMVD Pop_Ttl#########

### Pop_Ttl

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "MedIncm_HshldAnnual"
# )
# 
# dep_variable <- "Pop_Ttl"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[-13,]
# mydata <- mydata[-13,]
# #mydata <- mydata[-9,]
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Pop_Ttl ~ 
#     poly(MedIncm_HshldAnnual, 2), 
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
# 
# # Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships,
#   fieldname = "Date_Time",
#   fieldvalue = "2024-03-29 11:52:21")

##########################################################RMVD Schlrly_Actv_TtlArtcls######################

### Schlrly_Actv_TtlArtcls

## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "EBLL_Adults_Prcnt", "EBLL_Chldrn_Prcnt", "Budget_EnvrnmtlHlth_Fed_St_Grnts"
# )
# 
# dep_variable <- "Schlrly_Actv_TtlArtcls"  
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
#   Schlrly_Actv_TtlArtcls ~ 
#     poly(EBLL_Adults_Prcnt, 2) + 
#     poly(EBLL_Chldrn_Prcnt, 2) + 
#     poly(Budget_EnvrnmtlHlth_Fed_St_Grnts, 2), 
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

############################################################## RMVD SldWste_Dspsd_Tons#####################

### SldWste_Dspsd_Tons

# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "MedIncm_HshldAnnual", "BrthRt_PrcntofWmn"
# )
# 
# dep_variable <- "SldWste_Dspsd_Tons"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[-1,]
# mydata <- mydata[-11,]
# 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   SldWste_Dspsd_Tons ~ 
#     poly(MedIncm_HshldAnnual, 2), 
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

#################################################RMVD SMOCAPI_wMrtg_35PctOrMore_Prcnt##############################

### SMOCAPI_wMrtg_35PctOrMore_Prcnt

# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "BlwPvrty_Prcnt"
# )
# 
# dep_variable <- "SMOCAPI_wMrtg_35PctOrMore_Prcnt"  
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
#   SMOCAPI_wMrtg_35PctOrMore_Prcnt ~ 
#     BlwPvrty_Prcnt, 
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

###############################################RMVD SMOCAPI_wNoMrtg_35PctOrMore_Prcnt###############################

### SMOCAPI_wNoMrtg_35PctOrMore_Prcnt

# ## SET UP
# # Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "BlwPvrty_Prcnt"
# )
# 
# dep_variable <- "SMOCAPI_wNoMrtg_35PctOrMore_Prcnt"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[-2,] # Remove 2011 outlier 
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   SMOCAPI_wNoMrtg_35PctOrMore_Prcnt ~ 
#     log(BlwPvrty_Prcnt), 
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

################################################################ RMVD Urban_LULC_Prcnt##################

### Urban_LULC_Prcnt


## SET UP
# Assign variables 
# indep_variables <- c(                                    # ASSIGN VARIABLES
#   "MedIncm_HshldAnnual", "HousingAfrd_Prcnt"
# )
# 
# dep_variable <- "Urban_LULC_Prcnt"  
# 
# # Drop rows with nulls
# all_vars <- c(indep_variables, dep_variable)
# 
# mydata <- alldata[, all_vars, drop = FALSE]
# mydata <- mydata[complete.cases(mydata), ]
# mydata <- mydata[-13,] #remove 2022
# #mydata <- mydata[-7,] #remove 2016
# 
# ## RUN FUNCTION FOR PHASE 1
# # Function
# phase_one(indep_variables = indep_variables, dep_variable = dep_variable)
# 
# ## SELECT MODEL FOR PHASE 2
# # Manually copy and paste that model and add transformations 
# 
# lm_model <- lm(                                     # SET MODEL TRANSFORMATIONS      
#   Urban_LULC_Prcnt ~ 
#     log(MedIncm_HshldAnnual) + 
#     HousingAfrd_Prcnt,  
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
# # ALT 
# greymodelfit <- GM(mydata$Urban_LULC_Prcnt)
# a <- greymodelfit[["a"]]
# b <- greymodelfit[["b"]]
# x <- mydata$Urban_LULC_Prcnt[1]
# k <- 2
# 
# ((b-0.5*a)/(b+0.5*a))^(k-2)*((b-a*x)/(b+0.5*a))
# 
# ## SAVE RESULTS 
# # Bind to mined relationships data frame 
# mined_relationships <- rbind(mined_relationships, output_data)

# Trim outdated data by field name and value as needed
# mined_relationships <- trim_df(
#   df = mined_relationships, 
#   fieldname = "Date_Time",                                    
#   fieldvalue = "2024-02-08 16:33:22") 
