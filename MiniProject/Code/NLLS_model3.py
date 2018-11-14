#!/usr/bin/python

# Imports
import pandas as pd
import numpy as np
from lmfit import minimize, Minimizer, Parameter, Parameters, report_fit

# Loading data
d = pd.read_csv('../Data/TidyOriginalTrait.csv')

# Convert IDs to categories
d['FinalID'] = d['FinalID'].astype('category')

# Obtain individual IDs in list
ids = d.FinalID.cat.categories.tolist()

# Create empty Data Frame
output = pd.DataFrame()

# Define objective function: returns the array to be minimized
def fcn2min_three(params,x,y):
    b0 = params['b0']
    E = params['E']
    tl = params['tl']
    el = params['el']
    k = params['k']
    modelfit = b0 * np.exp(-E/k * (1/x - 1/283.15))/1 + np.exp(el/k * (1/tl - 1/x))
    return modelfit - y

# FOR LOOP
for i in ids:
    
    # Set the x and y values
    x = d.TempKelv.loc[d['FinalID']==i].tolist() # This sets the temperature in Kelvin! as the x values
    y = d.log_Trait.loc[d['FinalID']==i].tolist() # This sets the log of the trait values as the y values
    x = np.array(x)
    y = np.array(y)
    
    # Create a set of Parameters
    fit_params = Parameters() # defines a Parameter object
    fit_params.add('b0',value = min(y)) # this is taken from estimated B0 value
    fit_params.add('E',value = d.E.loc[d['FinalID']==i].tolist()[0]) # this is taken from estimated E value
    fit_params.add('tl',value = min(x)) # this is taken from the estimated tl value
    fit_params.add('el',value=d.E.loc[d['FinalID']==i].tolist()[0] - 0.5,max = d.E.loc[d['FinalID']==i].tolist()[0]) # bounded so that is less than E, starts at E
    fit_params.add('k',value=8.617e-5,vary=False) # this is a constant
    
    try:
    # Perform fitting
        minner = Minimizer(fcn2min_three,fit_params,fcn_args=(x,y))
        result = minner.minimize()

    # Transform AIC values
        AIC = result.aic + (2 * sum(y))

    # Extract params and model stats
        output = output.append({'ID':i,'Model':'NLLS_model3','b0':result.params['b0'].value,'E':result.params['E'].value,'tl':result.params['tl'].value,'el':result.params['el'].value,'k':result.params['k'].value,'AIC':AIC,'BIC':result.bic},ignore_index=True)
    
    except ValueError:
        print(i+' seems to have an outlier! Model cannot converge. The estimated parameters will be set to 0')
        output =  output.append({'ID':i,'Model':'NLLS_model3','b0':0,'E':0,'tl':0,'el':0,'k':0,'AIC':1e8,'BIC':0},ignore_index=True)

# Sort Columns
output = output[['ID','Model','b0','E','tl','el','k','AIC','BIC']]

# Save output as a csv file in the Results directory
output.to_csv('../Data/NLLS_model3.csv',index=False)
                                                    
