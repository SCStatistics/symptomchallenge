library(ggplot2)
library(reshape2)
library(lubridate)
library(data.table)
library(covidcast)

############################ Run the data for county level ############################ 

m_ct_s = fread("mSIR_county_test.csv")
rt_m = fread("mSIR_st_train.csv")
rt_m_st = rt_m[geo_value == st]
m_ct_s$date = as.Date(m_ct_s$date)

counties = matrix(NA, nrow = 6, ncol = 3)
counties[1,] = c("Los Angeles county", 6037, "ca")
counties[2,]  = c("New York county", 36061, "ny")
counties[3,] = c("Palm Beach county", 12099, "fl")
counties[4,] = c("San Francisco county", 06075, "ca")
counties[5,] = c("San Diego county", 6073, "ca")
counties[6, ] = c("Orange county", 6059, "ca")
colnames(counties) = c("name", "fip","state")
counties = as.data.frame(counties)
counties$fip = as.numeric(as.character(counties$fip))
counties$name = as.character(counties$name)
counties$state = as.character(counties$state)

# Where I "run" the model

m_ct_s_all = m_ct_s_one[1,]


i = 1
  ct = counties[i,2]
  ct_2 = counties[i,2]
  st = counties[i,3]
  start_date = as.numeric(as.Date("2020-04-12"))
  out_sample_dt = data.table()
  rt_m_st = rt_m[geo_value == st]
  for(t in 1:140){
    train_start = 0 + t
    train_end = 30 + t
    current_date = train_end + 1
    m_ct_s_one = m_ct_s[fips == ct]
    m_ct_s_one_out = m_ct_s[fips == ct_2]
    rt_m_train = rt_m_st[as.numeric(time_value) <= train_end + start_date & 
                           as.numeric(time_value) >= train_start + start_date]
    fit_MSIR = lm(mean ~ p_hat_t + X1  - 1 , rt_m_train)
    m_ct_s_one_curr = m_ct_s_one_out[date == current_date + start_date]
    pred_result = predict(fit_MSIR, m_ct_s_one_curr, interval = 'confidence')
    m_ct_s_one_curr[,"pred_rt"] = pred_result[,1]
    m_ct_s_one_curr[,"pred_rt_lwr"] = pred_result[,2]
    m_ct_s_one_curr[,"pred_rt_upr"] = pred_result[,3]
    out_sample_dt = rbind(out_sample_dt,m_ct_s_one_curr)
  }
  
  
  m_ct_s_one = out_sample_dt
  
  p5 = ggplot(m_ct_s_one, aes(x = date)) + 
    geom_line(aes(y = pred_rt, colour = "Rt"))+
    geom_point(aes(y = daily_cases/10000, colour = "Daily New Cases"))+
    geom_point(aes(y = 100*active_cases, colour = "Active Cases"))+
    theme_bw() +
    scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Number of New Cases"))+
    geom_hline(yintercept=1, linetype="dashed", color = "darkred") +
    labs(y = "Predicted R_t",
         x = "Date", 
         colour = "Index",
         title = counties[i,1])
  p5
#  m_ct_s_one$name = counties[i,1]
#  m_ct_s_all  = rbind(m_ct_s_all, m_ct_s_one)
# m_ct_s_all = m_ct_s_all[-1,]
# fwrite(m_ct_s_all, file = "county_level_estimate_for_five_counties.csv")

  
library(ggpubr)
p = ggarrange(p1, p6, p4, p2, p3, p5,
                             ncol = 3, nrow = 2, common.legend = TRUE)
library(tikzDevice)
tikz('plot_rt_counties.tex',width = 6, height = 4)
p
dev.off()



m_ct_s_all %>% 
  filter(name == "San Francisco county") %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = pred_rt, colour = "Rt"))+
  geom_line(aes(y = daily_cases/500, colour = "Daily New Cases"))+
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "Number of New Cases"))+
  geom_hline(yintercept=1, linetype="dashed", color = "darkred") +
  labs(y = "Predicted Rt",
       x = "Date", 
       colour = "Index",
       title = "San Francisco county")







m_ct_s_one = out_sample_dt
m_ct_s_one =  m_ct_s_one[,smoothed_pct_cmnty_cli := smoothed_pct_cmnty_cli/max(smoothed_pct_cmnty_cli)]
m_ct_s_one[,daily_cases := daily_cases/max(daily_cases)]
m_ct_s_one[,active_cases := active_cases/max(active_cases)]
m_ct_s_one[,confirmed_cases_uf := confirmed_cumulative_cases_uf/max(confirmed_cumulative_cases_uf)]







m_ct_s_one_melt = melt( m_ct_s_one, id.vars = c("fips","date"), 
                    measure.vars = c("pred_rt","daily_cases","active_cases","smoothed_pct_cmnty_cli"))

ggplot(m_ct_s_one_melt, aes(x = date, y = value, color = variable)) + 
  geom_line()+
  theme_bw() +
  geom_hline(yintercept=1, linetype="dashed", color = "darkred")
  

rt_m_st = rt_m_st[, lag_cases := shift(active_cases, 14, type="lead")]
cor(rt_m_st$mean,rt_m_st$lag_cases, use="complete.obs")
