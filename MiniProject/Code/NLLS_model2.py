#!/usr/bin/python

# Imports
import pandas as pd
import numpy as np
from lmfit import minimize, Minimizer, Parameter, Parameters,report_fit

# Loading data
d = pd.read_csv('../Data/TidyOriginalTrait.csv')

# Convert IDs to categories
d['FinalID'] = d['FinalID'].astype('category')

# Obtain individual IDs in list
ids = d.FinalID.cat.categories.tolist()

# Create empty Data Frame
output = pd.DataFrame()

# Define objective function: returns the array to be minimized
def fcn2min_two(params,x,y):
    b0 = params['b0']
    E = params['E']
    th = params['th']
    eh = params['eh']
    k = params['k']
    modelfit = b0 * np.exp(-E/k * (1/x - 1/283.15))/1 + np.exp(eh/k * (1/th - 1/x))
    return modelfit - y

# FOR LOOP
for i in ids:
    # Set the x and y values
    x = d.TempKelv.loc[d['FinalID']==i].tolist() # This sets the temperature in Kelvin! as the x values
    y = d.log_Trait.loc[d['FinalID']==i].tolist() # This sets the log of the trait values as the y values
    x = np.array(x)
    y = np.array(y)
    
    fit_params = Parameters() # defines a Parameter object
    fit_params.add('b0', value = min(y)) # this is taken from estimated B0 value
    fit_params.add('E', value= d.E.loc[d['FinalID']==i].tolist()[0]) # this is taken from estimated E value
    fit_params.add('th', value = max(x)) # this is bounded between 2*E and 4*E, starts at 3*E
    fit_params.add('eh', value= d.E.loc[d['FinalID']==i].tolist()[0] + 0.5, min=d.E.loc[d['FinalID']==i].tolist()[0]) # bounded so that is greater than E, starts at E
    fit_params.add('k', value=8.617e-5,vary=False) # this is a constant
    
    # Create a set of Parameters
    #fit_params = Parameters() # defines a Parameter object
    #fit_params.add('b0', value=d.B0.loc[d['FinalID']==i].tolist()[0]) # this is taken from estimated B0 value
    #fit_params.add('E', value=d.E.loc[d['FinalID']==i].tolist()[0]) # this is taken from estimated E value
    #fit_params.add('th', value=d.Th.loc[d['FinalID']==i].tolist()[0]) # this is bounded between 2*E and 4*E, starts at 3*E
    #fit_params.add('eh', value=d.Eh.loc[d['FinalID']==i].tolist()[0],min=d.E.loc[d['FinalID']==i].tolist()[0]) # bounded so that is greater than E, starts at E
    #fit_params.add('k', value=8.617e-5,vary=False) # this is a constant
    
    try:
        # Perform fitting
        minner = Minimizer(fcn2min_two,fit_params,fcn_args=(x,y))
        result = minner.minimize()
        
        # Transform AIC values
        AIC = result.aic + (2 * sum(y))
        
        # Extract params and model stats
        output = output.append({'ID':i,'Model':'NLLS_model3','b0':result.params['b0'].value,'E':result.params['E'].value,'th':result.params['th'].value,'eh':result.params['eh'].value,'k':result.params['k'].value,'AIC':AIC,'BIC':result.bic},ignore_index=True)
    
    except ValueError:
        print(i+' Model cannot converge. The estimated parameters will be set to 0')
        output =  output.append({'ID':i,'Model':'NLLS_model2','b0':0,'E':0,'th':0,'eh':0,'k':0,'AIC':1e8,'BIC':0},ignore_index=True)

# Sort Columns
output = output[['ID','Model','b0','E','th','eh','k','AIC','BIC']]

# Save output as a csv file in the Results directory
output.to_csv('../Data/NLLS_model2.csv',index=False)



