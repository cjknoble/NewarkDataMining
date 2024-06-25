##################################################################################
### Mining Google Search Results - Pulling total number of results for given search terms 

library(RSelenium)
library(stringr)

#final.dataframe.name <- "googles_results_CommentPaper"

keywords_df <- read.csv("C:/...GoogleKeywords.csv")

# Create an empty dataframe to store the results
result_df <- data.frame(year = integer(0), month = integer(0), keyword = character(0), total_results = numeric(0))

# Define a list of keywords, sequence of years, and pattern for parsing result values from text
months <- 1:11 # Choose Months (DECEMBER NEEDS TO BE RUN SEPARATELY)
years <- 2004:2022 # Choose Years
pattern <- "\\d{1,3}(,\\d{3})*"

firefox_profile_path <- "C:/Users/user/AppData/Roaming/Mozilla/Firefox/Profiles/0skylha8.default-release"

# Start a Selenium server and create a remote driver
driver <- rsDriver(browser = "firefox",
                   version = "latest",
                   chromever = NULL,
                   geckover = "latest",
                   iedrver = NULL,
                   phantomver = NULL,
                   verbose = TRUE,
                   extraCapabilities = list(firefox_profile_path))

remote_driver <- driver$client

#Begin Scraping - Jan to November 
for (i in 1:nrow(keywords_df)){ 
  keyword <- keywords_df$Keywords[i]
  for (year in years){
    for (month in months){
    
      # Set webpage url
      url <- paste0("https://www.google.com/search?q=", URLencode(keyword), "&sca_esv=570700320&source=lnt&tbs=cdr%3A1%2Ccd_min%3A", month, "%2F1%2F", year, "%2Ccd_max%3A", month+1,"%2F1%2F",year,"&tbm=")
      
      # Navigate to the webpage
      remote_driver$navigate(url)
	  
	  # Pause
      Sys.sleep(4)
      
      # Find the button element by its CSS selector 
      button <- remote_driver$findElement(using = "css selector", value = ".WjMmQ") # If this value fails, alt option is  #hdtb-tls 
      
      # Click the button
      button$clickElement()
      
      # Pause
      Sys.sleep(2)
      
      # Find the element with the #result-statistics CSS ID
      result_stats <- remote_driver$findElement(using = "css", value = "#result-stats")
      
      # Extract the text content of the element
      result_text <- result_stats$getElementText()
      
      # Extract the numeric part from the string
      result_numeric_value <- as.numeric(gsub(",", "", str_extract(result_text[[1]][1], pattern)))
      
      # Add the results to the df
      result_df <- rbind(result_df, data.frame(year = year, month = month, keyword = keyword, total_results = result_numeric_value))
    }
  }
}

# Repeat for December Only
months <- 12

for (i in 1:nrow(keywords_df)){ 
  keyword <- keywords_df$Keywords[i]
  for (year in years){
    for (month in months){
      
      # Set webpage url
      url <- paste0("https://www.google.com/search?q=", URLencode(keyword), "&sca_esv=570700320&source=lnt&tbs=cdr%3A1%2Ccd_min%3A", month, "%2F1%2F", year, "%2Ccd_max%3A", month+1,"%2F31%2F",year,"&tbm=")
      
      # Navigate to the webpage
      remote_driver$navigate(url)
      
	  # Pause
      Sys.sleep(4)
      
      # Find the button element by its CSS selector 
      button <- remote_driver$findElement(using = "css selector", value = "#hdtb-tls") # Alt option is .WjMmQ
      
      # Click the button
      button$clickElement()
      
      # Pause
      Sys.sleep(2)
      
      # Find the element with the #result-statistics CSS ID
      result_stats <- remote_driver$findElement(using = "css", value = "#result-stats")
      
      # Extract the text content of the element
      result_text <- result_stats$getElementText()
      
      # Extract the numeric part from the string
      result_numeric_value <- as.numeric(gsub(",", "", str_extract(result_text[[1]][1], pattern)))
      
      # Add the results to the df
      result_df <- rbind(result_df, data.frame(year = year, month = month, keyword = keyword, total_results = result_numeric_value))
    }
  }
}

# Close the remote driver and server when done
remote_driver$close()
driver$server$stop()

# View results 
View(result_df)

# Export results
write.csv(result_df, file = "C/...result_df.csv")


########################################################################################################################
### Mining Google Trends Data

library(readr) 
library(gtrendsR) 
library(purrr) 
library(devtools)

# Install gtrendsR package
devtools::install_github("PMassicotte/gtrendsR")

setwd("C:/.../Google Search")

# Load your keywords list (.csv file) 
kwlist <- readLines("GoogleKeywords.csv")

# Function to get city code
city_code <- function(geo) {
  codes <- unique(countries$sub_code[substr(countries$sub_code, 1, 5) == geo])
  if (length(codes) > 1) {
    countries[countries$sub_code %in% codes[2:length(codes)], 2:3]
  } else {
    message('No city code for this geo')
  }
}

code <- city_code('US-NJ') # Choose area of interest 

# Function to fetch Google Trends data
googleTrendsData <- function(keywords) { 
  country <- c(paste0(code)) 
  time <- "2004-01-01 2022-12-31" # Set time frame
  channel <- 'web' 
  
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time,
                    low_search_volume = TRUE) 
  
  trends$interest_over_time 
} 

# Run GoogleTrendsData function over the keyword list
output <- map_dfr(.x = kwlist, .f = googleTrendsData)

# Download the dataframe "output" as a .csv file 
write.csv(output, "download.csv")
