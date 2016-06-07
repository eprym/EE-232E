# -*- coding: utf-8 -*-
"""
Created on Sat Jun  4 16:27:27 2016

@author: YuLiqiang
"""

from math import sqrt
import numpy as np
import matplotlib.pyplot as plt
from sklearn import linear_model
from sklearn.ensemble import RandomForestRegressor
from sklearn.cross_validation import KFold

f = open("../project_2_data/data.txt", 'r')
matrix = [map(float, line.split()) for line in f]
matrix = np.array(matrix)
data = matrix[:, 0:106]
target = matrix[:, 106]

kf = KFold(len(target), n_folds=10, shuffle=True, random_state=None)
lr = linear_model.LinearRegression(normalize = True)
rfr = RandomForestRegressor(n_estimators = 30,max_depth = 12, max_features='auto')
RMSE_LINEAR=[]
RMSE_RFR=[]

for train_index, test_index in kf:
    data_train, data_test = data[train_index], data[test_index]
    target_train, target_test = target[train_index], target[test_index]
    lr.fit(data_train, target_train)
    rfr = rfr.fit(data_train, target_train)
    rmse_linear = sqrt(np.mean((lr.predict(data_test) - target_test) **2))
    RMSE_LINEAR.append(rmse_linear)
    rmse_rfr = sqrt(np.mean((rfr.predict(data_test) - target_test) ** 2))
    RMSE_RFR.append(rmse_rfr)


print(np.mean(RMSE_LINEAR))
print(np.mean(RMSE_RFR))

f_i = open("../project_2_data/data_interest.txt", 'r')
matrix_i = [map(float, line.split()) for line in f_i]
matrix_i = np.array(matrix_i)
data_i = matrix_i[:, 0:106]
rate_linear = lr.predict(data_i)
rate_rfr = rfr.predict(data_i)
print(rate_linear)
print(rate_rfr)
"""
1.27417955416
1.26341071465
[ 6.14947715  6.16647388  6.16990139]
[ 6.09615458  6.05967233  6.13684098]
"""




