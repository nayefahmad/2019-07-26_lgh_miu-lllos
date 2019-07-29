

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

library(tidyverse)
library(denodoExtractor)
library(here)
library(gghighlight)

source(here("src",
            "fill-dates_function.R"))

# pull and wrangle data: -------------
setup_denodo()

df1.census <- 
  vw_census %>% 
  filter(facility_short_name == "LGH", 
         census_date_id >= "20160101", 
         is_llos_p4p_definition_at_census == 1, 
         nursing_unit_short_desc_at_census == "LGH MIU") %>% 
  select(census_date_id, 
         patient_id, 
         facility_short_name) %>%  # show_query()
  
  collect() %>% 
  
  count(census_date_id) %>% 
  
  # fill in missing dates: 
  fill_dates(date_id_col = census_date_id, 
             start_date = "2016-01-01", 
             end_date = "2019-07-27") # %>% 
  
  # fix problems with count: 
  # mutate(n = as.integer(n)) %>% 
  # rename(count = n)

str(df1.census)
summary(df1.census)

# plots ------------

df1.census %>% 
  ggplot(aes(x = dates_fill, 
             y = `n`)) + 
  geom_point(alpha = 0.3) + 
  geom_line(alpha = 0.3) + 
  geom_smooth() + 
  theme_light() +
  labs(title = "LGH Census in MIU unit", 
       y = "num patients") + 
  scale_y_continuous(limits = c(0, 12)) + 
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))


# stl decomposition 
ts1 <- ts(df1.census %>% 
            filter(!is.na(n)) %>% 
            pull(n), 
          frequency = 365)      

stl(ts1, s.window = "periodic") %>% plot

df2.census_decomp <- stl(ts1, s.window = "periodic")[[1]] %>% as.tibble()


# join original data with decomp -------
df1.census %>% 
  filter(!is.na(n)) %>% 
  bind_cols(df2.census_decomp) %>% 
  select(dates_fill, 
         n, 
         seasonal, 
         trend, 
         remainder) %>% 
  
  gather(key = variable, 
         value = value, 
         -dates_fill) %>% 
  
  ggplot(aes(x = dates_fill, 
             y = value, 
             group = variable, 
             colour = variable)) + 
  geom_line() +  
  # gghighlight(max(value) >= 9) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))
      
  
 





