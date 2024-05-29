# Read in csv file 
library(tidyverse)
library(lubridate)

gauges = unique(daily_data$GaugeID)

data = select(daily_data, c("GaugeID", "DATE", "PRCP_C", "PRCP_M"))

# get directory file path for all csv files 
filepath = "D:/University of Texas at Dallas/CombinedDataset/CommonBasins"
# create a list of all the daymet files with the pattern listed, 1 per gauge in the dataset
allfiles = fs::dir_ls(filepath, regexp = "\\_Daymet_prcp.csv$")

# read in all files in the directory, use purrr::map() to map read_csv and map_dfr returns 
# dataframe by row binding each element together
datatable = allfiles %>% map_dfr(read_csv, .id = "source")

# rename source column 
datatable = datatable %>% rename(GaugeID = source)

# edit the GaugeID, remove the filepath before and the extra filename pattern before and after the gauge number
datatable$GaugeID%<>%gsub("D:/University of Texas at Dallas/CombinedDataset/CommonBasins/basin_USGS-", "",.) %>% 
  gsub("_Daymet_prcp.csv", "", .)

datatable = datatable %>% filter(between(DATE, as.Date("1980-10-01"), as.Date("2000-09-30")))

add_leap_year_rows <- function(df) {
  # Extract the years from the date column
  years <- unique(year(df$DATE))
  
  # Identify leap years
  leap_years <- years[leap_year(years)]
  
  # extract the unique GaugeID
  gauge_id = unique(df$GaugeID)
  
  # Create new rows for each leap year on December 31 with prcp value of NA
  new_rows <- data.frame(
    GaugeID = gauge_id, 
    DATE = as.Date(paste0(leap_years, "-12-31")),
    PRCP = NA
  )
  
  # Ensure the new rows are only added if they do not already exist in the dataframe
  new_rows <- new_rows[!new_rows$DATE %in% df$DATE, ]
  
  # Bind the new rows to the original dataframe
  df <- bind_rows(df, new_rows)
  
  # Sort the dataframe by date
  df <- df %>% arrange(DATE)
  
  return(df)
}

# apply the function by group to add leap year rows with NA values for PRCP
datatable2 <- datatable %>% 
  group_by(GaugeID) %>% 
  do(add_leap_year_rows(.)) %>% 
  ungroup()

# remove the last row of each group with year 2000 (water year ends in 9/30)
datatable3 = datatable2 %>% 
  group_by(GaugeID) %>% 
  slice(1:(n() - 1)) %>% 
  ungroup()

library(imputeTS)
# interpolate missing prcp value for leap years using linear imputation
datatable4 = na_interpolation(datatable3$PRCP, option = "linear")
# as dataframe
datatable4 = as.data.frame(datatable4)
# combine dataframes to add date and gaugeID columns 
datatable4 = cbind(datatable3, datatable4)
# remove PRCP column with NA value 
datatable4 = subset(datatable4, select = -PRCP)
# rename column from cbind 
datatable4 = datatable4 %>% rename(Daymet = datatable4)

#gauge2 = gauge2[-c(7306),] # remove last row

gauges = unique(datatable4$GaugeID)
daily_data2 = daily_data %>% filter(GaugeID %in% gauges)

daily_data2 = select(daily_data2, c("GaugeID", "DATE", "PRCP_C", "PRCP_M"))

df = left_join(daily_data2, datatable4, by = c("GaugeID", "DATE"))

d2 = df %>% group_by(GaugeID) %>% summarise(c_m = cor(PRCP_C, PRCP_M, method = "spearman"), 
                    c_d = cor(PRCP_C, Daymet, method = "spearman"), 
                    m_d = cor(PRCP_M, Daymet, method = "spearman"))



# create year, month, and day columns using date column 
datatable = datatable %>% mutate(YR = year(DATE), MNTH = month(DATE), DY = day(DATE))
# add wateryear
wYear = function(date) {
    ifelse(month(date) < 10, year(date), year(date)+1)}
# create a function that will make Jan and Feb of a given year associate with the 
# previous year since winter is December, January and February
# i.e. December 1980, January 1981, February 1981 will all have season year of 1980
# the remainder of the months, March - November will have the same year as the date
sYR = function(date) {
  ifelse(month(date) == 1 | month(date) == 2, year(date)-1, year(date))
       }
# add column called SEASONYR which uses the function 
seasons = seasons %>%          
         mutate(SEASONYR = sYR(DATE))

# add column called SEASON which will assign the season based on the month
seasons = seasons %>% 
  mutate(SEASON = case_when(
    MNTH %in% 9:11 ~ "Fall",
    MNTH %in% c(12, 1, 2) ~ "Winter",
    MNTH %in% 3:5 ~ "Spring",
      TRUE ~ "Summer"))

replace_random_na <- function(df, column_name, num_to_replace) {
  # Get the indices of the dataframe
  indices <- 1:nrow(df)
  
  # Randomly sample the indices to replace
  indices_to_replace <- sample(indices, num_to_replace)
  
  # Replace the values with NA
  df[[column_name]][indices_to_replace] <- NA
  
  return(df)
}


# Replace random values in the 'value' column with NA
df <- replace_random_na(test, "PRCP", 100)




