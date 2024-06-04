# Duty Roster Committee
library(lubridate).     # Package to work with dates
library(tidyverse)      # Data plyers 
library(datamods)


# Enter the required number of fields here
course = "MD"
year = 2024
session = "July"
n_joinees = 12

# List of Posting Areas - modify if there are any changes to the program
adult <- sapply(1:6, function(x) paste("Unit", x))
periph <- c("CAP", "CAM", "CLP", "Neuro", "BT/FT", "PRS/Community")

n_adult_units = length(adult)
n_postings = length(c(adult, periph))

index_adult = c(1,2,3,4,11,12)
index_periph = 5:10

# Get list of JRs
list_jrs = sapply(1:n_joinees, function(x) {paste0(course, "_", 
                                                    session, year, "_",
                                                    x)})

# Specify minimum number of JRs per Unit, the remainder, and maximum (=min+1)
min_per_unit = floor(n_joinees/n_adult_units)
remainder = n_joinees - n_adult_units*min_per_unit
max_per_unit = min_per_unit + 1


# Get a list of posting schedule dates, using date arithmetic
joining_date = as.Date(paste0(year, "-", match(session, month.name), "-", "01"))

posting_start_dates = sapply(0:(n_postings-1), function(x) 
                                              {joining_date %m+% months(x*3)}) %>% 
                                              as.Date() %>% 
                                              format("%b %Y")
posting_end_dates = sapply(0:(n_postings-1), function(x) 
                                            {joining_date %m+% months(x*3 + 2)}) %>% 
                                              as.Date() %>% 
                                              format("%b %Y")

posting_dates <- paste0(substr(posting_start_dates, 1, 3), "-", posting_end_dates)

# Make a blank datasheet of posting orders for each JR
jr_df <- data.frame(matrix(nrow = n_joinees, ncol = n_postings + 1))
colnames(jr_df) <- c("JR", posting_dates)
jr_df$JR <- list_jrs

# Make a blank datasheet of posting areas
posting_df <- matrix(nrow = length(posting_dates), ncol = 1 + n_postings) %>% data.frame()
colnames(posting_df) <- c("Month", adult, periph)
posting_df$Month <- posting_dates
posting_df[,-1] <- 0 # Make a blank datasheet of posting counters

# Edit the datasheet or import an existing one




# Running a loop to populate all the fields

# For Adult Psychiatry

for(curr_month in posting_dates[index_adult]) {   # Looping across months 

 for(jr in list_jrs) { # Looping across JRs
  
   # Get list of Units completed by the JR in previous months
   jr_completed_units <- jr_df %>% filter(JR == jr) %>% select(-1) %>%  as.character()
   jr_completed_units <- jr_completed_units[jr_completed_units != "NA" & !is.na(jr_completed_units)]
   jr_units_remaining <- adult[!(adult %in% jr_completed_units)]
   
  # Get a count of how many JRs have already been allotted in each unit for that month
  curr_tally  = posting_df %>% filter(Month == curr_month) %>%          # Subset the current month
                              select(starts_with("Unit")) %>% t() %>%   # Work with only adult psych units here
                              data.frame() %>% rename("N" = ".") %>%   # Housekeeping
                              rownames_to_column(var = "Unit") %>%     # More housekeeping
                              mutate(min_reached = case_when(N >= min_per_unit ~ 1, .default = 0),
                                     max_reached = case_when(N >= max_per_unit ~ 1, .default = 0))
  
  # Take those Units that have not reached the minimum number of JRs needed
  units_avail = curr_tally %>% filter(min_reached != 1) %>% 
                               select(Unit) %>% 
                               unlist() %>% unname()
  
  # Check the number of times 'maximum' number of JRs was reached for each Unit
  curr_tally$n_max <- sapply(adult, function(x) {sum(posting_df[,x] == max_per_unit)})
  
  
  # Keep backup units that have reached the minimum number but can take one more JR if needed
  # while ensuring that units got more/max JRs < 'remainder' number of times 
  
  units_avail2 = curr_tally %>% filter(min_reached == 1 & 
                                         max_reached == 0 & 
                                         n_max < remainder) %>% 
                                select(Unit) %>% 
                                unlist() %>% unname()
  
  # Chose set of units that JR hasn't completed, but slots are available                               
  jr_units_avail <- jr_units_remaining[jr_units_remaining %in% units_avail]
  
  # Chose among those units that have minimum required JRs but need to fit somewhere else
  jr_units_avail2 <- jr_units_remaining[jr_units_remaining %in% units_avail2]
  
  
  if(length(jr_units_avail) > 0) {
    
   # Select unit randomly from those available
  curr_posting = sample(jr_units_avail, 1)
  
  } else {
    
   # Select unit from all adult units
  curr_posting = sample(jr_units_avail2, 1)
  
    }

  # Assign a JR to that Unit
  jr_df[jr_df$JR == jr, curr_month] <- curr_posting 
  
  # Update the tally of the number of JRs posted in each Unit 
  posting_df[posting_df$Month == curr_month, curr_posting] <- sum(posting_df[posting_df$Month == curr_month, 
                                                                             curr_posting], 1)
 }
  
}
