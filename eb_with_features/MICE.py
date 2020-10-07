# https://scikit-learn.org/stable/modules/generated/sklearn.impute.IterativeImputer.html


# explicitly require this experimental feature
from sklearn.experimental import enable_iterative_imputer  # noqa
# now you can import normally from sklearn.impute
from sklearn.impute import IterativeImputer

import pandas as pd
import pandas_select
import numpy as np


demo_data = pd.read_csv("Data/join_demographics_csu_survey.csv")
prop_cum_cases = pd.read_csv("Data/prop-cum-cases-county.csv")
prop_cum_deaths = pd.read_csv("Data/prop-cum-deaths-county.csv")

prop_cum_deaths['prop_cum_deaths'] = prop_cum_deaths['value']
prop_cum_deaths['date'] = prop_cum_deaths['time_value']
prop_cum_deaths['fips'] = prop_cum_deaths['geo_value']

prop_cum_deaths = prop_cum_deaths.loc[:,['date', 'fips', 'prop_cum_deaths']]

prop_cum_cases['prop_cum_cases'] = prop_cum_cases['value']
prop_cum_cases['date'] = prop_cum_cases['time_value']
prop_cum_cases['fips'] = prop_cum_cases['geo_value']
prop_cum_cases = prop_cum_cases.loc[:,['date', 'fips', 'prop_cum_cases']]

demo_data = demo_data.merge(prop_cum_deaths, on=['date', 'fips'], how='inner')
demo_data = demo_data.merge(prop_cum_cases, on=['date', 'fips'], how='inner')

demo_data['date'] = pd.to_datetime(demo_data['date'])
demo_data = demo_data.drop(columns = ['pct_avoid_contact_all_or_most_time'])
# demo_data.index = demo_data['date']
# aggregate by week

def impute_data(data, weekly=False):
    dat = data.copy()
    if weekly:
        dat = dat.groupby(['fips', pd.Grouper(key='date', freq='W') ]).aggregate('mean')
    dat = dat.loc[:,~(dat.columns.str.startswith('smoothed_mean') |
                                                  dat.columns.str.startswith('mean_') |
                                                  dat.columns.isin(['Unnamed: 0', 'n', 'pct_avoid_contact_all_or_most_time']))]

    keep_columns = dat[pandas_select.StartsWith('pct_') | pandas_select.StartsWith('Total households!!') |
                                          pandas_select.StartsWith('RELATIONSHIP!!') |
                                          pandas_select.StartsWith('SCHOOL ENROLLMENT')].columns.to_list() + \
    dat.columns[dat.columns.get_loc("Civilian_labor_force_2018"):dat.columns.get_loc("Median_Household_Income_2018")+1].to_list() +\
    dat.columns[dat.columns.get_loc("Total_Male"):dat.columns.get_loc("Total households")+1].to_list() +\
    ["prop_cum_deaths","prop_cum_cases"]

    dat_selected = dat[keep_columns]

    min_values = np.concatenate([np.repeat(0,88),np.repeat(-np.inf,dat_selected.shape[1]-90), np.repeat(0,2)])
    max_values = np.concatenate([np.repeat(100,88),np.repeat(np.inf,dat_selected.shape[1]-90),np.repeat(100,2)])

    imp = IterativeImputer(max_iter=10, random_state=100620, sample_posterior=True, min_value=min_values, max_value=max_values)

    imputed_features = imp.fit_transform(dat_selected)

    imputed_df = dat_selected.copy()
    imputed_df.loc[:,keep_columns] = imputed_features
    return imputed_df


weekly = True
imputed_data = impute_data(demo_data, weekly=weekly)

imputed_data.to_csv(f"Data/imputed_features_{'weekly' if weekly else 'daily'}.csv")
