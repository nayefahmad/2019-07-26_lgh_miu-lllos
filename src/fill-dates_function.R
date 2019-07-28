
#******************************************************
# Function to take a dataframe with dates, and fill in missing dates 
#******************************************************

library(lubridate)


# function definition: -------------
fill_dates <- function(df, date_col, start_date, end_date){
  # inputs: 
  # df is a dataframe 
  # date_col is a column with date ids - e.g. "20190101" 
  # start_date and end_date create the range of dates to "fill in"
  
  # create date range: 
  df_full_dates <- data.frame(dates_fill = seq(as.Date(start_date), 
                                           as.Date(end_date), 
                                           by = "1 day"))
  
  # "quote"/"capture" the value that the user inputs (the actual arg),
  # but don't evaluate it:
  date_col <- enexpr(date_col)
  
  # now join: 
  df <- 
    df %>% 
    mutate(dates_fill = ymd(!!date_col)) %>% 
    full_join(df_full_dates) %>% 
    arrange(dates_fill)
  
  return(df)
  
}



# test the function: ---------
# create test df
# df1 <- data.frame(this_col_has_dates = c("20190101", "20190104"), 
#                   other_col = rnorm(2))
# 
# # fill in dates: 
# df1 %>% 
#   fill_dates(date_col = this_col_has_dates, 
#              start_date = "2019-01-01", 
#              end_date = "2019-01-07")

