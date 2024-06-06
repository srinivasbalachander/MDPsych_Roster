# Duty Roster Committee
library(lubridate)     # Package to work with dates
library(tidyverse)      # Data plyers 
library(editData)

# Enter the required number of fields here
course = "MD"
year = 2025
session = "July"
n_joinees = 12

# Get list of JRs
list_jrs = sapply(1:n_joinees, function(x) {paste0(course, "_", 
                                                   session, year, "_",
                                                   x)})

# Make a data frame to input JR characteristics (optional)
jr_details <- data.frame(ID = list_jrs, Name = NA, Gender = NA, Kannada = NA, 
                                        Hindi = NA, Tamil = NA, Telugu = NA, Malayalam = NA)

#jr_details <- editData(jr_details)

# List of Posting Areas
adult <- c("Unit 1", "Unit 2", "Unit 3", "Unit 4", "Unit 5", "Unit 6")
periph <- c("CAP", "CAM", "CLP", "Neuro", "BT/FT/PRS", "Comm/Geriat/Forens")

n_adult_units = length(adult)
n_periph_units = length(periph)
n_postings = n_adult_units + n_periph_units

# Index/Order of Adult and Peripheral postings 
index_adult = c(1,2,3,4,11,12)
index_periph = 5:10

# Get a list of posting schedule dates, using date arithmetic
joining_date = as.Date(paste0(year, "-", match(session, month.name), "-", "01"))

posting_start_dates = sapply(0:(n_postings-1), 
                             function(x) {joining_date %m+% months(x*3)}) %>% 
                                           as.Date() %>%format("%b %Y")
posting_end_dates = sapply(0:(n_postings-1), 
                           function(x) {joining_date %m+% months(x*3 + 2)}) %>% 
                                            as.Date() %>% 
                                            format("%b %Y")

posting_dates <- paste0(substr(posting_start_dates, 1, 3), "-", posting_end_dates)

cat(posting_dates, sep = "\n")

# Specify minimum number of JRs per Unit, the remainder, and maximum (=min+1)

# ----- Adult
min_per_adult_unit = floor(n_joinees/n_adult_units)
remainder_adult = n_joinees - n_adult_units*min_per_adult_unit
max_per_adult_unit = min_per_adult_unit + 1

# ----- Peripheries
min_per_periph_unit = floor(n_joinees/n_periph_units)
remainder_periph = n_joinees - n_periph_units*min_per_periph_unit
max_per_periph_unit = min_per_periph_unit + 1

# Make a blank template of posting orders for each JR
jr_df_template <- data.frame(matrix(nrow = n_joinees, ncol = n_postings + 1))
colnames(jr_df_template) <- c("JR", posting_dates)
jr_df_template$JR <- list_jrs

# Edit this data frame if necessary, or choose it from an existing file
# jr_df_template <- editData(jr_df_template)
# jr_df_template <- file.choose()

# Make a blank template for each posting area's tally for the month
posting_df_template <- matrix(nrow = length(posting_dates), ncol = 1 + n_postings) %>% data.frame()
colnames(posting_df_template) <- c("Month", adult, periph)
posting_df_template$Month <- posting_dates
posting_df_template[,-1] <- 0 # Make a blank datasheet of posting counters

# If any JRs have already been assigned postings, update the posting tally
for(i in c(adult, periph)) {
  for(j in posting_dates) {
    posting_df_template[posting_df_template$Month == j, i] <- sum(jr_df_template[,j] == i, na.rm = TRUE)
  }
}

# Running a loop to populate all the fields

# ----------- For Adult Psychiatry

jr_df <- jr_df_template
posting_df <- posting_df_template

while(sum(is.na(jr_df[, posting_dates[index_adult]])) > 0) { # Keep repeating this step till a solution is found  
  
  # Restart with the blank templates
  jr_df <- jr_df_template
  posting_df <- posting_df_template
  
for(jr in list_jrs) { # Looping across JRs
    
for(curr_month in posting_dates[index_adult]) {   # Looping across months 

   curr_posting = "NA"   # This value will change after the first JR is allocated a posting for that month
  
   # Skip this month enitrely is the JR has already been assigned a Unit
   if(!is.na(jr_df_template[jr_df_template$JR == jr, 
                            curr_month])) {next}
   
   # Get list of Units completed by the JR in previous months
   jr_completed_units <- jr_df %>% filter(JR == jr) %>% select(-1) %>%  as.character()
   jr_completed_units <- jr_completed_units[jr_completed_units != "NA" & !is.na(jr_completed_units)]
   jr_units_remaining <- adult[!(adult %in% jr_completed_units)]
   
  # Get a count of how many JRs have already been allotted in each unit for that month
  curr_tally  = posting_df %>% filter(Month == curr_month) %>%          # Subset the current month
                              select(starts_with("Unit")) %>% t() %>%   # Work with only adult psych units here
                              data.frame() %>% rename("N" = ".") %>%   # Housekeeping
                              rownames_to_column(var = "Unit") %>%     # More housekeeping
                              mutate(min_reached = case_when(N >= min_per_adult_unit ~ 1, .default = 0),
                                     max_reached = case_when(N >= max_per_adult_unit ~ 1, .default = 0),
                                     prev_max = sapply(adult, function(x) {sum(posting_df[,x] == max_per_adult_unit)}))
  
  # Take those Units that have not reached the minimum number of JRs needed
  units_avail = curr_tally %>% filter(min_reached == 0) %>% 
                               select(Unit) %>% 
                               unlist() %>% unname()
  
  # Remove the unit that was taken by the JR in the previous iteration from this list
  units_avail = units_avail[units_avail != curr_posting]
  
  # Keep backup units that have reached the minimum number but can take one more JR if needed
  # while ensuring that units got more/max JRs < 'remainder' number of times 
  
  units_avail2 = curr_tally %>% filter(min_reached == 1 & 
                                         max_reached == 0 & 
                                         prev_max < remainder_adult) %>% 
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
    
    if(length(jr_units_avail2) > 0) {
    
   # Select unit from any other adult units which the JR hasn't done
  curr_posting = sample(jr_units_avail2, 1)
  
    } else {curr_posting = NA } # Do not assign posting now, retry in the next iteration

  } # For  else statement above that

# Assign a JR to that Unit
  jr_df[jr_df$JR == jr, curr_month] <- curr_posting 
  
# Update the tally of the number of JRs posted in each Unit 
  
  if(!is.na(curr_posting)) {
  
  posting_df[posting_df$Month == curr_month, curr_posting] <- sum(posting_df[posting_df$Month == curr_month, curr_posting], 1)
                           } #Curr_posting is NA
  
    } # JR's for loop
  } # Month's for loop
} # While statement

# ------------- For Peripheral Postings

jr_df_template2 <- jr_df
posting_df_template2 <- posting_df

while(sum(is.na(jr_df[, posting_dates[index_periph]])) > 0) { # Keep repeating this step till a solution is found  
  
  # Restart with the blank templates
  jr_df <- jr_df_template2
  posting_df <- posting_df_template2
  
  for(jr in list_jrs) { # Looping across JRs
    
    for(curr_month in posting_dates[index_periph]) {   # Looping across months 
      
      curr_posting = "NA"   # This value will change after the first JR is allocated a posting for that month
      
      # Skip this month enitrely is the JR has already been assigned a Unit
      if(!is.na(jr_df_template[jr_df_template$JR == jr, 
                               curr_month])) {next}
      
      # Get list of Units completed by the JR in previous months
      jr_completed_units <- jr_df %>% filter(JR == jr) %>% select(-1) %>%  as.character()
      jr_completed_units <- jr_completed_units[jr_completed_units != "NA" & !is.na(jr_completed_units)]
      jr_units_remaining <- periph[!(periph %in% jr_completed_units)]
      
      # Get a count of how many JRs have already been allotted in each unit for that month
      curr_tally  = posting_df %>% filter(Month == curr_month) %>%          # Subset the current month
        select(!starts_with("Unit"), - Month) %>% t() %>%   # Work with only adult psych units here
        data.frame() %>% rename("N" = ".") %>%   # Housekeeping
        rownames_to_column(var = "Unit") %>%     # More housekeeping
        mutate(min_reached = case_when(N >= min_per_periph_unit ~ 1, .default = 0),
               max_reached = case_when(N >= max_per_periph_unit ~ 1, .default = 0),
               prev_max = sapply(periph, function(x) {sum(posting_df[,x] == max_per_periph_unit)}))
      
      # Take those Units that have not reached the minimum number of JRs needed
      units_avail = curr_tally %>% filter(min_reached == 0) %>% 
        select(Unit) %>% 
        unlist() %>% unname()
      
      # Remove the unit that was taken by the JR in the previous iteration from this list
      units_avail = units_avail[units_avail != curr_posting]
      
      # Keep backup units that have reached the minimum number but can take one more JR if needed
      # while ensuring that units got more/max JRs < 'remainder' number of times 
      
      units_avail2 = curr_tally %>% filter(min_reached == 1 & 
                                             max_reached == 0 & 
                                             prev_max < remainder_periph) %>% 
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
        
        if(length(jr_units_avail2) > 0) {
          
          # Select unit from any other adult units which the JR hasn't done
          curr_posting = sample(jr_units_avail2, 1)
          
        } else {curr_posting = NA } # Do not assign posting now, retry in the next iteration
        
      } # For  else statement above that
      
      # Assign a JR to that Unit
      jr_df[jr_df$JR == jr, curr_month] <- curr_posting 
      
      # Update the tally of the number of JRs posted in each Unit 
      
      if(!is.na(curr_posting)) {
        
        posting_df[posting_df$Month == curr_month, curr_posting] <- sum(posting_df[posting_df$Month == curr_month, curr_posting], 1)
      } #Curr_posting is NA
      
    } # JR's for loop
  } # Month's for loop
} # While statement

