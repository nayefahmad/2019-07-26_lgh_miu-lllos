

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


setup_denodo()


vw_census %>% 
  filter(facility_short_name == "LGH", 
         census_date_id >= "20160101", 
         is_llos_p4p_definition_at_census == 1, 
         nursing_unit_short_desc_at_census == "LGH MIU") %>% 
  select(census_date_id, 
         patient_id, 
         facility_short_name) %>% 
  count(census_date_id)
