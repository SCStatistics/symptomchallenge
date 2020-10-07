import numpy as np
from sklearn.kernel_ridge import KernelRidge
from sklearn.model_selection import GridSearchCV
from sklearn import ensemble


class EmpBayes:
    def __init__(self, features, regression_method, responses=None, x=None, n=None):
        """
        :param responses: An n x 1 numpy array with the pct response for each type
        :param features: An n x p numpy array where each column is a different feature and each row is a different type
        :param regression_method: A string specifying which nonparametric regression method to use to estimate M_1 and M_2
        :param num_positives: An n x 1 numpy array where each row is a different "type" (county or state in our setting) (None by default)
        :param num_tests: An n x 1 numpy array where each row is a different "type" (county or state in our setting) (None by default)
        """
        self.n = n
        self.x = x
        self.responses = responses
        self.features = features

        self.regression_method = regression_method

        self.regression_options = {
            "krr": self.fit_krr,
            "rf": self.fit_random_forest
        }

    def fit_M1_model(self, features=None):
        if self.x is not None and self.n is not None:
            responses = self.x / self.n
        else:
            responses = self.responses
        self.fitted_M1_model = self.regression_options[self.regression_method](features=self.features, responses=responses)
        if features is not None:
            self.M1_hat = self.fitted_M1_model.predict(features)
        else:
            self.M1_hat = self.fitted_M1_model.predict(self.features)

    def fit_M2_model(self, features=None):
        if self.x is not None and self.n is not None:
            responses = (self.x - 1) / (self.n - 1)
        else:
            responses = self.responses
        self.fitted_M2_model = self.regression_options[self.regression_method](features=self.features, responses=responses)
        if features is not None:
            # here we are getting M2 hat for the data with missing response
            self.M2_hat = self.fitted_M2_model.predict(features)
        else:
            # by default, we are fitting the model on data that has a response
            self.M2_hat = self.fitted_M2_model.predict(self.features)

    def fit_krr(self, features, responses):
        """
        Fits a kernel ridge regression and does a grid search for parameter values.
        See here for more details: https://scikit-learn.org/stable/modules/generated/sklearn.kernel_ridge.KernelRidge.html
        :param features: An n x p numpy array where each column is a different feature and each row is a different type
        :param responses: An n x 1 numpy array where each row is a different "type"
        :return: fitted kernel ridge regression estimator class that had the lowest loss
        """
        krr = GridSearchCV(KernelRidge(kernel="rbf", gamma=0.1),
                           param_grid={"alpha": [1e0, 0.1, 1e-2, 1e-3],
                              "gamma": np.logspace(-2, 2, 5)}
                           )
        krr.fit(features, responses)
        return krr.best_estimator_

    def fit_random_forest(self, features, responses):
        """
        Fits a random forest using default parameters.
        See here for more details: https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestRegressor.html
        :param features: An n x p numpy array where each column is a different feature and each row is a different type
        :param responses: An n x 1 numpy array where each row is a different "type"
        :return: A fitted random forest estimator class
        """
        rf = ensemble.RandomForestRegressor()
        rf.fit(features, responses)
        return rf

    def estimate(self, features=None, x=None, n=None):
        """
        Uses the fitted models to get a point estimate of prevalence and stores the vector as self.prevalence
        """
        if features is not None:
            # here we want to get the estimate for observations without responses and we plug those into the fitted
            # model.
            self.M1_hat = self.fitted_M1_model.predict(features)
            self.M2_hat = self.fitted_M2_model.predict(features)
        else:
            print(f"fitting M1 using {self.regression_method}")
            self.fit_M1_model()
            print(f"fitting M2 using {self.regression_method}")
            self.fit_M2_model()

        self.alpha_hat = self.M1_hat * ( ( (self.M1_hat * (1 - self.M1_hat)) / (self.M2_hat - np.square(self.M1_hat)) ) - 1 )
        self.beta_hat = (1 - self.M1_hat) * ( ( (self.M1_hat * (1 - self.M1_hat)) / (self.M2_hat - np.square(self.M1_hat)) ) - 1 )

        if x is not None and n is not None:
            self.missing_data_estimates = (self.alpha_hat + x) / (self.alpha_hat + self.beta_hat + n)
            return ((self.alpha_hat + x) / (self.alpha_hat + self.beta_hat + n))
        else:
            self.estimates = (self.alpha_hat + self.x) / (self.alpha_hat + self.beta_hat + self.n)
            return self.estimates
