library(ggplot2)
library(reshape2)
library(lubridate)
library(data.table)
library(covidcast)

##########################################  Pulling relevant data ##########################################  
# State data
population_data = fread("excess_death/populations.csv")
population_data_st = population_data[abbreviation != ""]
population_data_st = population_data_st[, abbreviation := tolower(abbreviation)]
# County data
population_data_cnty = population_data[abbreviation == ""]
population_data_cnty = population_data_cnty[, abbreviation := tolower(abbreviation)]
state_to_abbrev = population_data_st[,.(abbreviation,location_name)]

# Pull confirmed cases and deaths from JHU
# State level data
confirmed_cases = fread("confirmed_cases_state.csv")
confirmed_cases = confirmed_cases[,.(geo_value,time_value,confirmed_cumulative_cases)]
confirmed_deaths = fread("confirmed_deaths_state.csv")
confirmed_deaths = confirmed_deaths[,.(geo_value,time_value,confirmed_deaths)]
#County level data
confirmed_cases_county = fread("confirmed_cases_county.csv")
confirmed_cases_county = confirmed_cases_county[,.(geo_value,time_value,confirmed_cumulative_cases)]
confirmed_deaths_county = fread("confirmed_deaths_county.csv")
confirmed_deaths_county = confirmed_deaths_county[,.(geo_value,time_value,confirmed_deaths)]

# Pull confirmed cases and deaths from USA facts
# State Level data
confirmed_cases_uf = fread("confirmed_cases_state_usa_facts.csv")
confirmed_cases_uf = confirmed_cases_uf[,.(geo_value,time_value,confirmed_cases_uf)]
confirmed_deaths_uf = fread("confirmed_deaths_state_usa_facts.csv")
confirmed_deaths_uf = confirmed_deaths_uf[,.(geo_value,time_value,confirmed_deaths_uf)]

# County level
confirmed_cases_uf_county = fread("confirmed_cases_county_usa_facts.csv")
confirmed_cases_uf_county = confirmed_cases_uf_county[,.(geo_value,time_value,confirmed_cumulative_cases_uf)]
confirmed_deaths_uf_county = fread("confirmed_deaths_county_usa_facts.csv")
confirmed_deaths_uf_county = confirmed_deaths_uf_county[,.(geo_value,time_value,confirmed_deaths_uf)]

# Pull daily cases from USA facts
# State level
daily_cases_uf = fread("daily_cases_data_state.csv")
daily_cases_uf = daily_cases_uf[,.(geo_value,time_value,daily_cases)]
# County level
daily_cases_uf_county = fread("daily_cases_data_county.csv")
daily_cases_uf_county = daily_cases_uf_county[,.(geo_value,time_value,daily_cases)]

# Pull safegraph data 
# State level data
completely_home_prop = fread("completely_home_prop_state.csv")
completely_home_prop = completely_home_prop[, .(geo_value,time_value,completely_home_prop)]
# County level data
completely_home_prop_county = fread("completely_home_prop_county.csv")
completely_home_prop_county = completely_home_prop_county[, .(geo_value,time_value,completely_home_prop)]


# Pull probability data at state level
p_all = fread("p_all.csv")
p_all_s = p_all[,.(state_code,intercept,slope,R_sq)]
p_all_s = p_all_s[,p0:= intercept]
p_all_s = p_all_s[,p1:= intercept+slope]


county_smoothed_data = fread("CMU-US-symptom-survey-aggregates/overall-county-smoothed-2.csv")
cdso = county_smoothed_data[gender == "overall" & age_bucket == "overall"]
state_smoothed_data = fread("CMU-US-symptom-survey-aggregates/overall-state-smoothed-2.csv")
sdso = state_smoothed_data[gender == "overall" & age_bucket == "overall"]

##########################################  Merging relevant data ########################################## 
# Merge confirmed cases and deaths from both data sources
ccd = merge(confirmed_cases,confirmed_deaths, 
            by.x = c("geo_value","time_value"), 
            by.y = c("geo_value","time_value"))
ccd = merge(ccd, daily_cases_uf, 
            by.x = c("geo_value","time_value"), 
            by.y = c("geo_value","time_value"))
ccd = merge(ccd, completely_home_prop, 
            by.x = c("geo_value","time_value"), 
            by.y = c("geo_value","time_value"))
ccd = ccd[confirmed_deaths > 0]

ccd_uf = merge(confirmed_cases_uf,confirmed_deaths_uf, by.x = c("geo_value","time_value"), 
               by.y = c("geo_value","time_value"))
ccd_uf = ccd_uf[confirmed_deaths_uf > 0]

ccd_all = merge(ccd,ccd_uf,by.x = c("geo_value","time_value"), 
                by.y = c("geo_value","time_value"))

# Compute Case Fatality Ratio (CFR) for both data sources
ccd_all = ccd_all[,cfr := confirmed_deaths/confirmed_cumulative_cases]
ccd_all = ccd_all[,cfr_uf := confirmed_deaths_uf/confirmed_cases_uf]

# Merge CFR data with total population data
ccd_all = merge(ccd_all,population_data_st, 
                by.x = c("geo_value"),
                by.y = c("abbreviation"))

ccd_all = merge(ccd_all, p_all_s, by.x = c("geo_value"), by.y = c("state_code"))

rt = fread("rt.csv")
rt = rt[, region := tolower(region)]
ccd_all = merge(ccd_all, rt,  by.x = c("time_value","geo_value"),
                by.y = c("date","region"))

st_av = sdso[
  ,.(state_code,date,
     smoothed_pct_contact_covid_positive,
     smoothed_pct_cli,
     smoothed_pct_avoid_contact_all_or_most_time,
     smoothed_pct_could_not_get_tested,
     smoothed_pct_self_none_of_above,
     smoothed_pct_tested_and_positive,
     smoothed_pct_cmnty_cli)]
st_av = st_av[,pos_contact := smoothed_pct_contact_covid_positive/100]
st_av = st_av[,avoid_contact := smoothed_pct_avoid_contact_all_or_most_time/100]
st_av = st_av[,excess_demand := smoothed_pct_could_not_get_tested*(100-smoothed_pct_self_none_of_above)/100]
st_av = st_av[,active_cases := smoothed_pct_cli/100 ]

rt_m = merge(ccd_all,st_av,
             by.x = c("geo_value","time_value"),by.y = c("state_code","date"))

rt_m = rt_m[,p_hat_t := ((avoid_contact*(1-p1) + (1-avoid_contact)*(1-p0)))*1]
rt_m = rt_m[,p_hat_t_2 := 1 - completely_home_prop]
rt_m = rt_m[,TC_CFR_N := confirmed_cumulative_cases*cfr_uf*excess_demand/Population]
rt_m = rt_m[,X1 := p_hat_t*TC_CFR_N]

fwrite(rt_m,"mSIR_st_train.csv")

############ Do the same thing for county level
# Merge confirmed cases and deaths from both data sources
ccdc = merge(confirmed_cases_county,confirmed_deaths_county, 
             by.x = c("geo_value","time_value"), 
             by.y = c("geo_value","time_value"))
ccdc = merge(ccdc, daily_cases_uf_county, 
             by.x = c("geo_value","time_value"), 
             by.y = c("geo_value","time_value"))
ccdc = merge(ccdc, completely_home_prop_county, 
             by.x = c("geo_value","time_value"), 
             by.y = c("geo_value","time_value"))
ccdc = ccdc[confirmed_deaths > 0]

ccdc_uf = merge(confirmed_cases_uf_county,confirmed_deaths_uf_county, by.x = c("geo_value","time_value"), 
                by.y = c("geo_value","time_value"))
ccdc_uf = ccdc_uf[confirmed_deaths_uf > 0]

ccdc_all = merge(ccdc,ccdc_uf,by.x = c("geo_value","time_value"), 
                 by.y = c("geo_value","time_value"))


# Compute Case Fatality Ratio (CFR) for both data sources
ccdc_all = ccdc_all[,cfr := confirmed_deaths/confirmed_cumulative_cases]
ccdc_all = ccdc_all[,cfr_uf := confirmed_deaths_uf/confirmed_cumulative_cases_uf]

# Merge CFR data with population data
ccdc_all = merge(ccdc_all,population_data_cnty, 
                 by.x = c("geo_value"),
                 by.y = c("location"))

ct_av = cdso[
  ,.(fips,
     date,
     state_code,
     smoothed_pct_contact_covid_positive,
     smoothed_pct_cli,
     smoothed_pct_avoid_contact_all_or_most_time,
     smoothed_pct_could_not_get_tested,
     smoothed_pct_self_none_of_above,
     smoothed_pct_tested_and_positive,
     smoothed_pct_cmnty_cli)]
ct_av = ct_av[,pos_contact := smoothed_pct_contact_covid_positive/100]
ct_av = ct_av[,avoid_contact := smoothed_pct_avoid_contact_all_or_most_time/100]
ct_av = ct_av[,excess_demand := smoothed_pct_could_not_get_tested*(100-smoothed_pct_self_none_of_above)/100]
ct_av = ct_av[,active_cases := smoothed_pct_cli/100 ]

m_ct = merge(ct_av, ccdc_all, by.x = c("fips", "date"), by.y = c("geo_value", "time_value"))
m_ct = merge(m_ct, p_all_s, by.x = c("state_code"), by.y = c("state_code"))

m_ct = m_ct[,p_hat_t := ((avoid_contact*(1-p1) + (1-avoid_contact)*(1-p0)))*1]
m_ct = m_ct[,p_hat_t_2 := 1 - completely_home_prop]
m_ct = m_ct[,TC_CFR_N := confirmed_cumulative_cases*cfr_uf*excess_demand/Population]
m_ct = m_ct[,X1 := p_hat_t*TC_CFR_N]
#m_ct_s = na.omit(m_ct)

fwrite(m_ct,"mSIR_county_test.csv")


