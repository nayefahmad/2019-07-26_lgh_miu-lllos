select census_date_id
    , patient_id
    , nursing_unit_short_desc_at_census
    , nursing_unit_desc_at_census
    , nursing_unit_short_desc_at_census
    , is_llos_p4p_definition_at_census
    , admit_to_census_acute_elapsed_time_days

from publish.census

where facility_short_name = 'LGH' 
    and (nursing_unit_cd_at_census = 'MIU'
        or nursing_unit_desc_at_census = 'LGH MIU Mental Health Inpatient Unit' 
        or nursing_unit_short_desc_at_census = 'LGH MIU'
    ) 
    
    and census_date_id > '20160101' -- no results after 20180426
    and admit_to_census_acute_elapsed_time_days > 30 
order by census_date_id;
