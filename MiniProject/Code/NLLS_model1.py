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
def fcn2min_one(params,x,y):
    b0 = params['b0']
    E = params['E']
    tl = params['tl']
    th = params['th']
    el = params['el']
    eh = params['eh']
    k = params['k']
    modelfit = b0 * np.exp(-E/k * (1/x - 1/283.15)) / 1 + np.exp(eh/k * (1/th - 1/x)) + np.exp(el/k * (1/tl - 1/x))
    return modelfit - y

def SI(d,ids):
# FOR LOOP
for i in ids:

    # Set the x and y values
    x = d.TempKelv.loc[d['FinalID']==i].tolist() # This sets the temperature in Kelvin! as the x values
    y = d.log_Trait.loc[d['FinalID']==i].tolist() # This sets the log of the trait values as the y values
    x = np.array(x)
    y = np.array(y)
    
    # Create a set of Parameters
    fit_params = Parameters() # defines a Parameter object
    fit_params.add('b0', value = min(y)) # this is taken from estimated B0 value
    fit_params.add('E', value = d.E.loc[d['FinalID']==i].tolist()[0]) # this is taken from estimated E value
    fit_params.add('th', value = max(x)) # this is bounded between 2*E and 4*E, starts at 3*E
    fit_params.add('tl', value = min(x)) # this is taken from the estimated tl value
    fit_params.add('el', value = d.E.loc[d['FinalID']==i].tolist()[0] - 0.5, max = d.E.loc[d['FinalID']==i].tolist()[0]) # bounded so that is less than E, starts at E
    fit_params.add('eh',value = d.E.loc[d['FinalID']==i].tolist()[0] + 0.5,min = d.E.loc[d['FinalID']==i].tolist()[0]) # bounded so that is greater than E, starts at E
    fit_params.add('k',value=8.617e-5,vary=False) # this is a constant
    
    try:
        # Perform fitting
        minner = Minimizer(fcn2min_one,fit_params,fcn_args=(x,y))
        result = minner.minimize()
        
        # Transform AIC values
        AIC = result.aic + (2 * sum(y))
        
        # Extract params and model stats
        output = output.append({'ID':i,'Model':'SI','b0':result.params['b0'].value,'E':result.params['E'].value,'tl':result.params['tl'].value,'th':result.params['th'].value,'el':result.params['el'].value,'eh':result.params['eh'].value,'k':result.params['k'].value,'AIC':AIC,'BIC':result.bic},ignore_index=True)
    
    except ValueError:
        print(i+': Model cannot converge. The estimated parameters will be set to 0')
        output =  output.append({'ID':i,'Model':'SI','b0':0,'E':0,'tl':0,'th':0,'el':0,'eh':0,'k':0,'AIC':1e8,'BIC':0},ignore_index=True)

    except TypeError:
        print(i+': 6 Parameters are trying to be estimated with 5 data points. Estimated parameters will be set to 0')
        output =  output.append({'ID':i,'Model':'SI','b0':0,'E':0,'tl':0,'th':0,'el':0,'eh':0,'k':0,'AIC':1e8,'BIC':0},ignore_index=True)


# Sort Columns
output = output[['ID','Model','b0','E','tl','th','el','eh','k','AIC','BIC']]

# Save output as a csv file in the Results directory
output.to_csv('../Data/NLLS_model1.csv',index=False)


import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from lmfit import minimize, Minimizer, Parameter, Parameters, report_fit


def fcn2min_one(params,x,y):
    """
        This function will be minimized when the SI funciton is called. It uses a set of parameteres
        and the structure of the Schoolfield model to compute the difference between predicted and
        observed trait values.
        """
    # A set of parameters
    params = params.valuesdict()
    b0 = params['b0']
    E = params['E']
    tl = params['tl']
    th = params['th']
    el = params['el']
    eh = params['eh']
    k = params['k']
    # The model. Given the parameters and x values (temperatures), computes the expected trait values
    model_num = b0 * np.exp((-E/k) * (1/x - 1/283.15))
    model_denom = 1 + np.exp((eh/k) * (1/th - 1/x)) + np.exp((el/k) * (1/tl - 1/x))
    modelfit = model_num/model_denom
    # Returns the difference between expected and observed trait values
    return np.log(modelfit) - y

def SI(d,ids):
    """
        This function performs the fitting of the SI model. Requires a data set and a set of ids. For
        each id, sets an x and y array from the provided data set. The function creates the required
        parameters and sets them according to previous calculations. The function tries to minimize the
        residuals. If successful, the AIC and parameter estimates are saved to an output dataframe. If
        the residuals cannot be minimized, then the parameter estimates are set to 0 and the AIC is set
        as 1e8.
        """
    
    # Create an empty dataframe
    output = pd.DataFrame(columns=['Model','b0','E','tl','th','el','eh','k','AIC'],index = ids)
    output = output.fillna(0)
    
    for i in ids:
        
        # Set the x and y values
        x = d.TempKelv.loc[d['FinalID']==i].tolist() # Temperature values used are in Kelvin
        y = d.log_Trait.loc[d['FinalID']==i].tolist() # Logged trait values are used
        x = np.array(x)
        y = np.array(y)
        
        # Create a set of Parameters
        fit_params = Parameters() # defines a Parameter object
        fit_params.add('b0', value = np.array(d.B0.loc[d['FinalID']==i].tolist()[0])) # this is taken from estimated B0 value
        fit_params.add('E', value = np.array(d.E.loc[d['FinalID']==i].tolist()[0])) # this is taken from estimated E value
        fit_params.add('th', value = np.array(d.Th.loc[d['FinalID']==i].tolist()[0])) # this is taken from the estimated th value
        fit_params.add('tl', value = np.array(d.Tl.loc[d['FinalID']==i].tolist()[0])) # this is taken from the estimated tl value
        fit_params.add('el', value = np.array(d.El.loc[d['FinalID']==i].tolist()[0]), max = np.array(d.E.loc[d['FinalID']==i].tolist()[0])) # this is taken from the estimated el value
        fit_params.add('eh', value = np.array(d.Eh.loc[d['FinalID']==i].tolist()[0]), min = np.array(d.E.loc[d['FinalID']==i].tolist()[0])) # this is taken from the estimated eh value
        fit_params.add('k',value =8.617e-5,vary=False) # this is a constant
        
        # This block of code tries to minimize the residuals of the model
        try:
            # Perform fitting
            result = minimize(fcn2min_one,fit_params,args=(x,y),nan_policy='omit')
            
            # Transform AIC values, because logged trait values are being used
            non_logged_residuals = np.exp(result.residual)
            non_logged_residuals = sum(non_logged_residuals**2)
            AIC = len(x) * np.log((2*np.pi)/len(x)) + len(x) + 2 + len(x) * np.log(non_logged_residuals) + 2 * 6
            #AIC = result.aic + (2 * sum(y))
            # Extract estimated parameter values and AIC values, theses are added to a dataframe
            output.loc[i] = pd.Series({'Model':'SI','b0':result.params['b0'].value,'E':result.params['E'].value,'tl':result.params['tl'].value,'th':result.params['th'].value,'el':result.params['el'].value,'eh':result.params['eh'].value,'k':result.params['k'].value,'AIC':AIC})
        

        
        # If a ValueError is encountered (the model cannot converge) the estimated parameter values
        # are set to 0 and the AIC is set to 1e8.
        except TypeError:
            output.loc[i] = pd.Series({'Model':'SI','b0':0,'E':0,'tl':0,'th':0,'el':0,'eh':0,'k':0,'AIC':1e8})
            print(result.params['b0'].value)
        
        # If a TypeError is encountered (because more parameters are trying to be estimated than data points
        # available), estimated parameter values are set to 0 and AIC is set to 1e8.
        except ValueError:
            output.loc[i] = pd.Series({'Model':'SI','b0':0,'E':0,'tl':0,'th':0,'el':0,'eh':0,'k':0,'AIC':1e8})
            print(result.params['b0'].value)


# The output dataframe columns are sorted and organized
output = output[['FinalID','Model','b0','E','tl','th','el','eh','k','AIC']]
output.index.name = 'FinalID'
# The model parameter estimates and AIC values are saved to a file located in the Results directory
output.to_csv('../Data/NLLS_output.csv',index=True)

