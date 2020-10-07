from EmpBayes import EmpBayes
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler

WEEKLY = True

id_columns = ['date','fips']
response_column = 'pct_avoid_contact_all_or_most_time'
non_feature_columns = id_columns + ['pct_avoid_contact_all_or_most_time','n', 'x']

# Load data
combined_data = pd.read_csv("Data/count-merged-data.csv", parse_dates=['date'], infer_datetime_format=True)
imputed_data = pd.read_csv("Data/imputed_features_weekly.csv", parse_dates=['date'], infer_datetime_format=True)

# Remove smoothed mean and mean columns
combined_data = combined_data.loc[:, ~(combined_data.columns.str.startswith('smoothed_mean') | combined_data.columns.str.startswith('mean_'))]

# Get our response column and extract x and n columns from the response in order to use Vishal's method
combined_data['x'] = combined_data['n'] * (combined_data['pct_avoid_contact_all_or_most_time']/100)

response_data = combined_data[non_feature_columns]


def spread_data(data):
    # this assumes that we are filling forward for each week, meaning that pandas aggregated starting at the beginning of the each week
    foo1 = (data.loc[data.groupby('fips')['date'].idxmax()]
            .assign(date = lambda x: x['date'] + pd.Timedelta(6, unit='d')))
    data = data.append(foo1, ignore_index=True)
    df = (data.set_index('date').groupby('fips').resample('d').ffill()
         .div(7).drop(columns=['fips'])
         .reset_index()
         )
    return df

if WEEKLY:
    # need to spread the weekly aggregated values out
    imputed_data = spread_data(imputed_data)

# now join the response data with our imputed features
merged_data = response_data.merge(imputed_data, on=['date', 'fips'], how='inner')


# split out data missing responses
missing_responses = merged_data.loc[merged_data[response_column].isnull(),:]
use_to_fit = merged_data.loc[~merged_data[response_column].isnull(),:]
# sanity check that we're splitting correctly
assert missing_responses.shape[0] + use_to_fit.shape[0] == merged_data.shape[0]

# Scale our features before we fit
scaler = StandardScaler()
emp_bayes = EmpBayes(features= scaler.fit_transform(use_to_fit.drop(columns=non_feature_columns).values),
# emp_bayes = EmpBayes(features= use_to_fit.drop(columns=non_feature_columns).values,
                     regression_method="krr",
                     x=use_to_fit['x'].values,
                     n=use_to_fit['n'].values)
# This fits both M1 and M2 and then calculates our response for the data that is not missing values
estimates = emp_bayes.estimate()



# to get estimates for data that has missing values, we need to replace the nan's in missing_responses x column with 0 so that our point estimate is equivalent to getting the mean from our beta distribution
missing_estimates = emp_bayes.estimate(features=scaler.fit_transform(missing_responses.drop(columns=non_feature_columns).values),
                                       x=np.repeat(0, missing_responses.shape[0]), # here we are setting x to zero for the counties with missing data
                                       n = np.repeat(0, missing_responses.shape[0]))

# some diagnostics
import matplotlib.pyplot as plt
plt.hist(estimates)
plt.hist(missing_estimates)
plt.show()

# Main issue is that we predict a negative percentage sometimes...
pd.DataFrame(estimates).describe()
pd.DataFrame(missing_estimates).describe()

# add the estimates as columns to each dataset, then combine together
use_to_fit['est_pct_avoid_all_contact'] = estimates
missing_responses['est_pct_avoid_all_contact'] = missing_estimates

df_with_estimates = pd.concat([use_to_fit, missing_responses], axis=0)

df_with_estimates.to_csv("Data/est_pct_avoid_all_contact.csv", index=False)