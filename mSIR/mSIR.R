library(ggplot2)
library(reshape2)
library(lubridate)
library(data.table)
library(covidcast)

daily_cases_data = suppressMessages(
  covidcast_signal(data_source = "jhu-csse", signal = "confirmed_7dav_incidence_num",
                   start_day = "2020-01-01", end_day = "2020-10-01",
                   geo_type = "county")
)
setnames(daily_cases_data, old ="value", new = "daily_cases")
fwrite(daily_cases_data,"daily_cases_data_county.csv")
daily_cases_uf = fread("daily_cases_data_county_usa_facts.csv")
daily_cases_uf_st = daily_cases_uf[geo_value == "ca"]
ggplot(daily_cases_uf_st, aes(x = time_value, y = daily_cases)) +geom_point()

# Pull population data
  # state data
population_data = fread("excess_death/populations.csv")
population_data_st = population_data[abbreviation != ""]
population_data_st = population_data_st[, abbreviation := tolower(abbreviation)]
  #county data
population_data_cnty = population_data[abbreviation == ""]
population_data_cnty = population_data_cnty[, abbreviation := tolower(abbreviation)]
state_to_abbrev = population_data_st[,.(abbreviation,location_name)]
excess_mort = fread("COVID-Tracking-Data-US/national_and_state_summary_excess_mortality.csv")
excess_mort = excess_mort[, V1 := NULL]
excess_mort = excess_mort[, state := tolower(state)]
excess_mort_st = excess_mort[state == "ca"]

# Pull confirmed cases and deaths from JHU
confirmed_cases = fread("confirmed_cases_state.csv")
confirmed_cases = confirmed_cases[,.(geo_value,time_value,confirmed_cumulative_cases)]
confirmed_deaths = fread("confirmed_deaths_state.csv")
confirmed_deaths = confirmed_deaths[,.(geo_value,time_value,confirmed_deaths)]
  #county level
confirmed_cases_cnty = fread("confirmed_cases_county.csv")
confirmed_cases_cnty = confirmed_cases_cnty[,.(geo_value,time_value,confirmed_cumulative_cases)]
confirmed_deaths_cnty = fread("confirmed_deaths_county.csv")
confirmed_deaths_cnty = confirmed_deaths_cnty[,.(geo_value,time_value,confirmed_deaths)]

# Pull confirmed cases and deaths from USA facts
confirmed_cases_uf = fread("confirmed_cases_state_usa_facts.csv")
confirmed_cases_uf = confirmed_cases_uf[,.(geo_value,time_value,confirmed_cases_uf)]
confirmed_deaths_uf = fread("confirmed_deaths_state_usa_facts.csv")
confirmed_deaths_uf = confirmed_deaths_uf[,.(geo_value,time_value,confirmed_deaths_uf)]

# Pull daily cases from USA facts
daily_cases_uf = fread("daily_cases_data_state.csv")
daily_cases_uf = daily_cases_uf[,.(geo_value,time_value,daily_cases)]

# Pull safegraph data 
completely_home_prop = fread("completely_home_prop_state.csv")
completely_home_prop = completely_home_prop[, .(geo_value,time_value,completely_home_prop)]

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

# Compute CFR for both data sources
ccd_all = ccd_all[,cfr := confirmed_deaths/confirmed_cumulative_cases]
ccd_all = ccd_all[,cfr_uf := confirmed_deaths_uf/confirmed_cases_uf]

# Merge CFR data with population data
ccd_all = merge(ccd_all,population_data_st, 
                by.x = c("geo_value"),
                by.y = c("abbreviation"))


# Melt data for plotting
ccd_all_melt = melt(ccd_all, 
                    id.vars = c("geo_value","time_value"),
                    measure.vars = c("cfr","cfr_uf"))
ccd_st = ccd_all_melt[geo_value =="fl"]
ccd_all_cases_melt = melt(ccd_all, 
                    id.vars = c("geo_value","time_value"),
                    measure.vars = c("confirmed_cumulative_cases","confirmed_cases_uf"))
ccd_cases_st = ccd_all_cases_melt[geo_value =="fl"]

ggplot(ccd_st, aes(x = time_value, y = value, color = variable)) + geom_point()
ggplot(ccd_cases_st, aes(x = time_value, y = value, color = variable)) + geom_point()


########## Constructing SIR model ########## 
rt = fread("rt.csv")
rt_epi = fread("rt_epi.csv")
rt_epi = merge(rt_epi, state_to_abbrev, by.x = c("state"), by.y = c("location_name"))
rt_epi = rt_epi[,region:= abbreviation]
rt = rt[, region := tolower(region)]
rt = rt[, rt_0 := mean - 1]
rt = rt[, rt_0_lag := shift(rt_0, 7), by = .(region)]

rt_m = merge(rt, ccd_all, by.x = c("region","date"),
             by.y = c("geo_value","time_value"))

p_all_s = p_all[,.(state_code,p0,p1,R_sq)]
#p_all_s = p_all[,p1 := slope + intercept]
#setnames(p_all_s,old = c("intercept"), new = c("p0"))

rt_m = merge(rt_m,p_all_s,by.x = c("region"), by.y = c("state_code"))

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

rt_m = merge(rt_m,st_av,
             by.x = c("region","date"),by.y = c("state_code","date"))

rt_m = rt_m[,p_hat_t := ((avoid_contact*(1-p1) + (1-avoid_contact)*(1-p0)))*1]
rt_m = rt_m[,p_hat_t_2 := 1 - completely_home_prop]
rt_m = rt_m[,TC_CFR_N := confirmed_cumulative_cases*cfr_uf*excess_demand/Population]
rt_m = rt_m[,X1 := p_hat_t*TC_CFR_N]
rt_m = rt_m[,X2 := p_hat_t^2]
rt_m = rt_m[,X3 := active_cases*p_hat_t]
rt_m = na.omit(rt_m)


# Where I "run" the model
st = "ca"
st_2 = "ca"
start_date = as.numeric(as.Date("2020-04-12"))
out_sample_dt = data.table()
for(t in 1:140){
  train_start = 0 + t
  train_end = 15 + t
  current_date = train_end + 1
  rt_m_st = rt_m[region == st]
  rt_m_st_out = rt_m[region == st_2]
  rt_m_train = rt_m_st[date <= train_end + start_date & date >= train_start + start_date]
  fit_MSIR = lm(mean ~ p_hat_t + X1  - 1 , rt_m_train)
  #fit_MSIR = lm(mean ~ smoothed_pct_cli + smoothed_pct_avoid_contact_all_or_most_time + confirmed_cases_uf + cfr_uf, rt_m_train)
  rt_m_st_curr = rt_m_st_out[date == current_date + start_date]
  pred_result = predict(fit_MSIR,rt_m_st_curr, interval = 'confidence')
  rt_m_st_curr[,"pred_rt"] = pred_result[,1]
  rt_m_st_curr[,"pred_rt_lwr"] = pred_result[,2]
  rt_m_st_curr[,"pred_rt_upr"] = pred_result[,3]
  out_sample_dt = rbind(out_sample_dt,rt_m_st_curr)
}
train_start = 0 
train_end = 200 
current_date = train_end + 1
rt_m_st = rt_m[region == st]
rt_m_train = rt_m_st[date <= train_end + start_date & date >= train_start + start_date]
#rt_m_train = rt_m_st[date < "2020-09-25" & date >= "2020-05-01"]

fit_MSIR = lm(mean ~ p_hat_t + X1  - 1 , rt_m_train)
#fit_MSIR = lm(mean ~ smoothed_pct_cli + smoothed_pct_avoid_contact_all_or_most_time + confirmed_cases_uf + cfr_uf, rt_m_train)
summary(fit_MSIR)
pred_result = predict(fit_MSIR,rt_m_st, interval = 'confidence')
rt_m_st[,"pred_rt"] = pred_result[,1]
rt_m_st[,"pred_rt_lwr"] = pred_result[,2]
rt_m_st[,"pred_rt_upr"] = pred_result[,3]

rt_m_st = out_sample_dt
rt_m_st = rt_m_st[,smoothed_pct_cmnty_cli := smoothed_pct_cmnty_cli/max(smoothed_pct_cmnty_cli)]
rt_m_st[,daily_cases := daily_cases/max(daily_cases)]
rt_m_st[,active_cases := active_cases/max(active_cases)]
rt_m_st[,confirmed_cases_uf := confirmed_cases_uf/max(confirmed_cases_uf)]
rt_m_st_melt = melt(rt_m_st, id.vars = c("region","date"), 
                    measure.vars = c("mean","pred_rt","daily_cases","active_cases","confirmed_cases_uf","smoothed_pct_cmnty_cli"))

ggplot(rt_m_st_melt, aes(x = date, y = value, color = variable)) + geom_point()

rt_m_st = rt_m_st[, lag_cases := shift(active_cases, 14, type="lead")]
cor(rt_m_st$mean,rt_m_st$lag_cases, use="complete.obs")





