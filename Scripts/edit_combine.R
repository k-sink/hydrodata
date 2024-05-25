# Read in csv file 
gauges = unique(daily_data$GaugeID)

data = select(daily_data, c("GaugeID", "DATE", "PRCP_C", "PRCP_M"))
a = data %>% filter(GaugeID == "01423000")
b = data %>% filter(GaugeID == "01543500")

gauge1 = read_csv("D:/University of Texas at Dallas/CombinedDataset/Daily_data/basin_USGS-01423000_Daymet_prcp.csv")
gauge2 = read_csv("D:/University of Texas at Dallas/CombinedDataset/Daily_data/basin_USGS-01543500_Daymet_prcp.csv")
gauge3 = read_csv("D:/University of Texas at Dallas/CombinedDataset/Daily_data/basin_USGS-01548500_Daymet_prcp.csv")
gauge4 = read_csv("D:/University of Texas at Dallas/CombinedDataset/Daily_data/basin_USGS-01606500_Daymet_prcp.csv")
gauge5 = read_csv("D:/University of Texas at Dallas/CombinedDataset/Daily_data/basin_USGS-01664000_Daymet_prcp.csv")

gauge1 = gauge1 %>% filter(between(DATE, as.Date("1980-10-01"), as.Date("2000-09-30")))
gauge2 = gauge2 %>% filter(between(DATE, as.Date("1980-10-01"), as.Date("2000-09-30")))
gauge3 = gauge3 %>% filter(between(DATE, as.Date("1980-10-01"), as.Date("2000-09-30")))
gauge4 = gauge4 %>% filter(between(DATE, as.Date("1980-10-01"), as.Date("2000-09-30")))
gauge5 = gauge5 %>% filter(between(DATE, as.Date("1980-10-01"), as.Date("2000-09-30")))


add_leap_year_rows <- function(df) {
  # Extract the years from the date column
  years <- unique(year(df$DATE))
  
  # Identify leap years
  leap_years <- years[leap_year(years)]
  
  # Create new rows for each leap year on December 31 with prcp value of NA
  new_rows <- data.frame(
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

# Apply the function
gauge1 <- add_leap_year_rows(gauge1)
gauge2 <- add_leap_year_rows(gauge2)

gauge1 = gauge1[-c(7306),] # remove last row
gauge2 = gauge2[-c(7306),] # remove last row

library(imputeTS)

m = na_interpolation(gauge1$PRCP, option = "linear")
m2 = na_interpolation(gauge2$PRCP, option = "linear")
#m = as.data.frame(m)
#m = cbind(m, gauge1$DATE)


a = cbind(a, m)

d = a %>% summarise(c_m = cor(PRCP_C, PRCP_M, method = "spearman"), 
                    c_d = cor(PRCP_C, m, method = "spearman"), 
                    m_d = cor(PRCP_M, m, method = "spearman"))

b = cbind(b, m2)
d2 = b %>% summarise(c_m = cor(PRCP_C, PRCP_M, method = "spearman"), 
                    c_d = cor(PRCP_C, m2, method = "spearman"), 
                    m_d = cor(PRCP_M, m2, method = "spearman"))

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




