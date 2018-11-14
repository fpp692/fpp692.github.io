#!/usr/bin/python

# Imports
import pandas as pd
import numpy as np
from lmfit import minimize, Minimizer, Parameter, Parameters,report_fit


# Load data to be fitted
x = np.array([-2,-1.64,-1.33,-0.7,0,0.45,1.2,1.64,2.32,2.9])
y = np.array([0.699369,0.700462,0.695354,1.03905,1.97389,2.41143,1.91091,0.919576,-0.730975,-1.42001])

# define objective function: returns the array to be minimized
def fcn2min(params,x,y):
    p1 = params['p1']
    p2 = params['p2']
    mfit = p1*np.cos(p2*x) + p2*np.sin(p1*x)
    return mfit - y

# Create a set of parameters
parms = Parameters()
parms.add('p1',value= 1)
parms.add('p2',value = 0.2)

# do fit
minner = Minimizer(fcn2min,params,fcn_args=(x,y))
result = minner.minimize()

# calculate final result
final = y + result.residual

# Summary of the fit
report_fit(result)
