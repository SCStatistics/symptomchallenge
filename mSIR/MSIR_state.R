library(ggplot2)
library(reshape2)
library(lubridate)
library(data.table)
library(covidcast)

# Reading data
rt_m = fread("mSIR_st_train.csv")

# We train the mSIR model by training with data of the last 2 weeks
st = "ca"
st_2 = "ca"
start_date = as.numeric(as.Date("2020-04-12"))
out_sample_dt = data.table()

for(t in 1:140){
  train_start = 0 + t
  train_end = 15 + t
  current_date = train_end + 1
  rt_m_st = rt_m[geo_value == st]
  rt_m_st_out = rt_m[geo_value == st_2]
  rt_m_train = rt_m_st[as.numeric(time_value) <= train_end + start_date & time_value >= train_start + start_date]
  fit_MSIR = lm(mean ~ p_hat_t + X1  - 1 , rt_m_train)
  #fit_MSIR = lm(mean ~ smoothed_pct_cli + smoothed_pct_avoid_contact_all_or_most_time + confirmed_cases_uf + cfr_uf, rt_m_train)
  rt_m_st_curr = rt_m_st_out[time_value == current_date + start_date]
  pred_result = predict(fit_MSIR,rt_m_st_curr, interval = 'confidence')
  rt_m_st_curr[,"pred_rt"] = pred_result[,1]
  rt_m_st_curr[,"pred_rt_lwr"] = pred_result[,2]
  rt_m_st_curr[,"pred_rt_upr"] = pred_result[,3]
  out_sample_dt = rbind(out_sample_dt,rt_m_st_curr)
}

# Plot results

rt_m_st = out_sample_dt
rt_m_st = rt_m_st[,smoothed_pct_cmnty_cli := smoothed_pct_cmnty_cli/max(smoothed_pct_cmnty_cli)]
rt_m_st[,daily_cases := daily_cases/max(daily_cases)]
rt_m_st[,active_cases := active_cases/max(active_cases)]
rt_m_st[,confirmed_cases_uf := confirmed_cases_uf/max(confirmed_cases_uf)]
rt_m_st_melt = melt(rt_m_st, id.vars = c("geo_value","time_value"), 
                    measure.vars = c("mean","pred_rt","daily_cases","active_cases","confirmed_cases_uf","smoothed_pct_cmnty_cli"))

ggplot(rt_m_st_melt, aes(x = time_value, y = value, color = variable)) + geom_point()
