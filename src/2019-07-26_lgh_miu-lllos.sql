
/*----------------------------------------
LGH MIU LLOS cases 
2019-07-27
Nayef 

----------------------------------------*/

select census_date_id
    , patient_id
    , nursing_unit_short_desc_at_census
    , nursing_unit_desc_at_census
    , nursing_unit_short_desc_at_census
from publish.census
where facility_short_name = 'LGH' 
    and (nursing_unit_cd_at_census = 'MIU'
        or nursing_unit_desc_at_census = 'LGH MIU Mental Health Inpatient Unit' 
        or nursing_unit_short_desc_at_census = 'LGH MIU'
    ) 
    
    and census_date_id >= '20180426' -- no results after 20180426
order by census_date_id;