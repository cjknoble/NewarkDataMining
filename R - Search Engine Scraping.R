##################################################################################
### RSELENIUM 

### R SELENIUM LOOP
# Install and load the RSelenium package
library(RSelenium)
library(stringr)

# Create an empty dataframe to store the results
result_df <- data.frame(year = integer(0), keyword = character(0), total_results = numeric(0))

# Define a list of keywords, sequence of years, and pattern for parsing result values from text
#keywords <- c("lead poisoning Newark NJ", "lead paint Newark NJ", "lead contamination Newark NJ")
keywords_df <- read.csv("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/Social Media/R Project - Google Data Collection/R Project - Google Data Collection/GoogleKeywordsforBot.csv")
years <- 2000:2022
pattern <- "\\d{1,3}(,\\d{3})*"

# Start a Selenium server and create a remote driver
driver <- rsDriver(browser = "firefox",
                   version = "latest",
                   chromever = NULL,
                   geckover = "latest",
                   iedrver = NULL,
                   phantomver = NULL,
                   verbose = TRUE)

remote_driver <- driver$client

for (i in 1:nrow(keywords_df)){ 
  keyword <- keywords_df$Keywords[i]
  
  for (year in years){
    
    # Set webpage url
    url <- paste0("https://www.google.com/search?q=", URLencode(keyword), "&sca_esv=570700320&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F",year,"&tbm=")
    
    # Navigate to the webpage
    remote_driver$navigate(url)
    
    Sys.sleep(3)
    
    # Find the button element by its CSS selector
    button <- remote_driver$findElement(using = "css selector", value = "#hdtb-tls") #Alt option is .WjMmQ
    
    # Click the button
    button$clickElement()
    
    # Wait for a few seconds to ensure the page content loads after clicking the button
    Sys.sleep(2)
    
    # Find the element with the #result-statistics CSS ID
    result_stats <- remote_driver$findElement(using = "css", value = "#result-stats")
    
    # Extract the text content of the element
    result_text <- result_stats$getElementText()
    
    # Extract the numeric part from the string
    result_numeric_value <- as.numeric(gsub(",", "", str_extract(result_text[[1]][1], pattern)))
    
    # Add the results to the df
    result_df <- rbind(result_df, data.frame(year = year, keyword = keyword, total_results = result_numeric_value))
  }
}

# Close the remote driver and server when done
remote_driver$close()
driver$server$stop()

# Print results 
View(result_df)

# Save to separate df 
result_df <- rbind(google_results, result_df)

google_results <- result_df



# Save results to a CSV file
write.csv(results_df, "google_results.csv", row.names = FALSE)
