### Rent Burden by State
#     Pulls ACS data to produce summary table of rent burden by US staate by year
#
#     By Patrick Rogers, California Research Bureau
#       October 2024
#
#     Uses the following Packages
#       {here}
#       {httr2}
#
#     Uses the following data
#       data.csv

# To Do
#   1. Pull ACS Data
#   2. Clean and format
#   3. Export table

# Tables of  Interest:
#   B25070_001E: Total households
#   B25070_007E to B25070_010E: 30% of HH income or above

# Clear the workspace
remove(list=ls(all=TRUE))

# Load Packages
library(here)
library(httr2)

# Inits
setwd(here::here())

# Load Sourcefiles
source(here::here('private', 'private_acs_api_key.R'))

# User Parameters
numerator_tables <- c('B25070_007E', 'B25070_008E', 'B25070_009E', 'B25070_010E')
denominator_table <- 'B25070_001E'
tables <-  paste0(c('NAME', denominator_table, numerator_tables), collapse=',')

years <- 2005:2023

# Custom Functions ####

# Loop over years ####
for(year in years){
  # Skip 2020
  if(year == 2020){
    acs_data_burden <- cbind(acs_data_burden, `2020`=NA)
    acs_data_severe <- cbind(acs_data_severe, `2020`=NA)
    acs_data_burden_pct <- cbind(acs_data_burden_pct, `2020`=NA)
    acs_data_severe_pct <- cbind(acs_data_severe_pct, `2020`=NA)
    next
  }
  # Pull ACS Data
  
  # Build request
  request <- request(paste0('https://api.census.gov/data/', year, '/acs/acs1'))
  request <- req_url_query(request,
                           get = tables,
                           'for' = 'state:*',
                           key = my.key)
  req_dry_run(request)
  
  # Response
  response <- req_perform(request)
  
  # Parse JSON
  if(resp_status(response) == 200){
    data <- resp_body_json(response, simplifyVector = TRUE)
    names <- data[1,]
    data <- data.frame(data[-1,])
    colnames(data) <- names
  }
  
  # Convert to numeric
  data[,c(denominator_table, numerator_tables)] <- sapply(data[,c(denominator_table, numerator_tables)], as.numeric)
  
  # Initialize acs_data table
  if(year == years[1]){
    acs_data_burden <- data.frame(State = data$NAME)
    acs_data_severe <- data.frame(State = data$NAME)
    acs_data_burden_pct <- data.frame(State = data$NAME)
    acs_data_severe_pct <- data.frame(State = data$NAME)
  }
  
  # Calculate % Rent Burdened
  acs_data_burden <- merge(acs_data_burden,
                           data.frame(State = data$NAME, var = rowSums(data[,numerator_tables])), by = 'State')
  acs_data_burden_pct <- merge(acs_data_burden_pct,
                           data.frame(State = data$NAME, var = rowSums(data[,numerator_tables])/data[,denominator_table]), by  = 'State')
  
  # Calculate % Rent Burdened
  acs_data_severe <- merge(acs_data_severe,
                           data.frame(State = data$NAME, var = data[,'B25070_010E']), by = 'State')
  acs_data_severe_pct <- merge(acs_data_severe_pct,
                           data.frame(State = data$NAME, var = data[,'B25070_010E']/data[,denominator_table]), by  = 'State')
}

colnames(acs_data_burden) <- c('State', years)
colnames(acs_data_severe) <- c('State', years)
colnames(acs_data_burden_pct) <- c('State', years)
colnames(acs_data_severe_pct) <- c('State', years)

# Save Results ####
write.csv(acs_data_burden, file = here::here('output', 'rent_burdened.csv'), row.names = FALSE)
write.csv(acs_data_severe, file = here::here('output', 'rent_severe.csv'), row.names = FALSE)
write.csv(acs_data_burden_pct, file = here::here('output', 'rent_burdened_pct.csv'), row.names = FALSE)
write.csv(acs_data_severe_pct, file = here::here('output', 'rent_severe_pct.csv'), row.names = FALSE)




# Step 3 ####