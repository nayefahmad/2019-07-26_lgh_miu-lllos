

#'--- 
#' title: "LLOS patterns in LGH MIU unit"
#' author: "Nayef Ahmad"
#' date: "2019-07-26"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 

#+ lib, include = FALSE
library(tidyverse)
library(denodoExtractor)
library(here)
library(DT)
library(gghighlight)

source(here("src",
            "fill-dates_function.R"))

# pull and wrangle data: -------------
setup_denodo()

#' ## Data and methodology
#'
#' We pulled data on the number of LGH MIU patients with `admit to census days > 30`
#' from Jan 2016 to the present. Our goal is to break down the time series into 
#' trend and seasonal components. 
#' 
#' * **Trend component**: shows long-term changes in the series, which are not affected by day-to-day variation
#' * **Seasonal component**: shows regular repeating behaviour in the series, which occurs every year
#' 
#' We use the standard [STL](https://otexts.com/fpp2/stl.html) method for decomposition. 

#+ analsis, include = TRUE, warning = FALSE
df1.census <- 
  vw_census %>% 
  filter(facility_short_name == "LGH", 
         census_date_id >= "20160101", 
         admit_to_census_acute_elapsed_time_days > 30, 
         nursing_unit_short_desc_at_census == "LGH MIU") %>% 
  select(census_date_id, 
         patient_id, 
         facility_short_name) %>%  # show_query()
  
  collect() %>% 
  
  count(census_date_id) %>% 
  
  # fill in missing dates: 
  fill_dates(date_id_col = census_date_id, 
             start_date = "2016-01-01", 
             end_date = "2019-07-27") %>% 
  replace_na(list(n = 0))
  
  # fix problems with count: 
  # mutate(n = as.integer(n)) %>% 
  # rename(count = n)

# str(df1.census)
# summary(df1.census)
# df1.census %>% 
#   filter(is.na(n))

df1.census %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


# plots ------------

#' ## Raw time series
#'
#' The graph below plots the data as is, without any decomposition. It is clear
#' that there are certain prominent trend patterns - for e.g. for most of 2017,
#' the census of LLOS patients was trending downward. This may have been due to
#' preparations for CST go-live in April 2018.

df1.census %>% 
  ggplot(aes(x = dates_fill, 
             y = `n`)) + 
  geom_point(alpha = 0.3) + 
  geom_line(alpha = 0.3) + 
  geom_smooth() + 
  theme_light() +
  labs(title = "LGH LLOS Census in MIU unit", 
       y = "num patients") + 
  scale_y_continuous(limits = c(0, 12)) + 
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))


#' ## STL decomposition of time series
#'
#' The graph below shows the original data (top panel) and all the components
#' found by the STL algorithm. Note that the remainder is the part left over
#' after accounting for trend and seasonal components.

# stl decomposition 
ts1 <- ts(df1.census %>% 
            filter(!is.na(n)) %>% 
            pull(n), 
          frequency = 365)      

stl(ts1, s.window = "periodic") %>% plot

df2.census_decomp <- stl(ts1, s.window = "periodic")[[1]] %>% as.tibble()


# join original data with decomp -------
# df1.census %>% 
#   filter(!is.na(n)) %>% 
#   bind_cols(df2.census_decomp) %>% 
#   select(dates_fill, 
#          n, 
#          seasonal, 
#          trend, 
#          remainder) %>% 
#   
#   gather(key = variable, 
#          value = value, 
#          -dates_fill) %>% 
#   
#   ggplot(aes(x = dates_fill, 
#              y = value, 
#              group = variable, 
#              colour = variable)) + 
#   geom_line() +  
#   # gghighlight(max(value) >= 9) +  # highlight only actual data
#   
#   labs(title = "LGH LLOS patients in MIU unit") + 
#   theme_light() +
#   theme(panel.grid.minor = element_line(colour = "grey95"), 
#       panel.grid.major = element_line(colour = "grey95"))
      
  
 

# plot all components:  
# df1.census %>% 
#   filter(!is.na(n)) %>% 
#   bind_cols(df2.census_decomp) %>% 
#   select(dates_fill, 
#          n, 
#          seasonal, 
#          trend) %>%
#   gather(key = variable, 
#          value = value, 
#          -dates_fill) %>%
#   
#   ggplot(aes(x = dates_fill, 
#              y = value, 
#              group = variable, 
#              colour = variable)) + 
#   geom_line() + 
#   labs(title = "LGH LLOS patients in MIU unit") + 
#   theme_light() +
#   theme(panel.grid.minor = element_line(colour = "grey95"), 
#         panel.grid.major = element_line(colour = "grey95"))
# 

#' The graph below shows that there are seasonal peaks in the LLOS census in
#' February, and intermittently throughout the winter. In the summer months,
#' seasonality effects are negative (i.e. below the trend component). 

# plot seasonal for 1 year 
df1.census %>% 
  filter(!is.na(n)) %>% 
  bind_cols(df2.census_decomp) %>% 
  select(dates_fill, 
         seasonal) %>%
  filter(dates_fill >= "2018-01-01", 
         dates_fill <= "2018-12-31") %>% 
  
  ggplot(aes(x = dates_fill, 
             y = seasonal)) + 
  geom_line() + 
  geom_hline(yintercept = 0, 
             col = "blue") + 
  
  scale_x_date(date_breaks = "1 month") + 
      
  
  labs(title = "LGH LLOS patients in MIU unit - seasonal pattern", 
       subtitle = "The pattern is shown for 2018, but is similar for all years") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


#' Finally, the graph below highlights the long-term trend from Jan 2016 to the present. 

# plot trend for all years:  
df1.census %>% 
  filter(!is.na(n)) %>% 
  bind_cols(df2.census_decomp) %>% 
  select(dates_fill, 
         trend) %>%
  
  ggplot(aes(x = dates_fill, 
             y = trend)) + 
  geom_line() + 
  
  scale_x_date(date_breaks = "3 month") + 
  
  
  labs(title = "LGH LLOS patients in MIU unit - trend pattern", 
       subtitle = "Jan 2016 to present") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))



