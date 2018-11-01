#!/usr/bin/python

"""
Models.py
Fernando Pedraza Perez
This script contains functions to perform:

a) Fitting of cubic model (cubic function)
b) Fitting the full Schoolfield model (SI function)
c) Fitting of the reduced Schoolfield model (SII function)
d) Fitting of the reduced Schoolfield model (SIII function)
"""

# Imports
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from lmfit import minimize, Minimizer, Parameter, Parameters, report_fit

def cubic(d,ids):
    """
        A function that fits cubic models. Requires a panda data frame and a set of IDs. For each ID,
        an x and y vector is declared where x is the Temperature and y the Original traitvalue.
        A cubic model is fit to y given x. The outputs from the model are saved to a file called
        'CubicModels.csv'. The file is saved in the Data directory.
     """
    
    # Create empty panda dataframe, this will hold the results of the cubic models
    # The dataframe holds the column names and ID names.
    output = pd.DataFrame(columns=['Model','Intercept','x','x2','x3','AIC','Habitat','ConKingdom','StandardisedTraitName','Observations'],index = ids)
    output = output.fillna(0)
    
    # Iterate through IDs
    for i in ids:
        # X is set as the temperature for a given ID
        x = d.ConTemp.loc[d['FinalID']==i].tolist()
        # Y is set as the trait value for a given ID
        y = d.OriginalTraitValue.loc[d['FinalID']==i].tolist()
        # Create a Dataframe given x and y
        df = pd.DataFrame({"x":x,"y":y})
        # Fit the cubic model and save object to 'model'
        model = smf.ols('y~x + I(x**2) + I(x**3)',data=df).fit()
        # Append relevant model outputs and metadata to a dataframe
        output.loc[i] = pd.Series({'Model':'Cubic','Intercept':model.params.tolist()[0],'x':model.params.tolist()[1],'x2':model.params.tolist()[2],'x3':model.params.tolist()[3],'AIC':model.aic,'Habitat':d.Habitat.loc[d['FinalID']==i].tolist()[0],'ConKingdom':d.ConKingdom.loc[d['FinalID']==i].tolist()[0],'StandardisedTraitName':d.StandardisedTraitName.loc[d['FinalID']==i].tolist()[0],'Observations':d.Observations.loc[d['FinalID']==i].tolist()[0]})
    
    
    # Add relevant results to output file
    output = output[['Model','Intercept','x','x2','x3','AIC','Habitat','ConKingdom','StandardisedTraitName','Observations']]
    # Set the index name of the rows to be the Final IDs
    output.index.name = 'FinalID'
    # Save the results to a csv
    output.to_csv('../Data/CubicModels.csv',index=True)


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

def calculate_aic_one(b0,E,tl,th,el,eh,k,x,y):
    """
        This function calculates the AIC scores for the SIII model, requires the minimized parameter values
        and the temperature and trait values
        """
    model_num = b0 * np.exp((-E/k) * (1/x - 1/283.15))
    model_denom = 1 + np.exp((eh/k) * (1/th - 1/x)) + np.exp((el/k) * (1/tl - 1/x))
    modelfit = model_num/model_denom
    residuals = y - modelfit
    residuals = sum(residuals**2)
    AIC = len(x) * np.log((2*np.pi)/(len(x))) + len(x) + 2 + len(x) * np.log(residuals) + 2*6
    return(AIC)


def fcn2min_two(params,x,y):
    """
        This function will be minimized when the SII funciton is called. It uses a set of parameteres
        and the structure of the Schoolfield model to compute the difference between predicted and
        observed trait values.
    """
    # A set of parameters
    params = params.valuesdict()
    b0 = params['b0']
    E = params['E']
    th = params['th']
    eh = params['eh']
    k = params['k']
    # The model. Given the parameters and x values (temperatures), computes the expected trait values
    model_num = b0 * np.exp((-E/k) * (1/x - 1/283.15))
    model_denom = 1 + np.exp((eh/k) * (1/th - 1/x))
    modelfit = model_num/model_denom
    # Returns the difference between expected and observed trait values
    return np.log(modelfit) - y

def calculate_aic_two(b0,E,th,eh,k,x,y):
    """
        This function calculates the AIC scores for the SII model, requires the minimized parameter values
        and the temperature and trait values
        """
    model_num = b0 * np.exp((-E/k) * (1/x - 1/283.15))
    model_denom = 1 + np.exp((eh/k) * (1/th - 1/x))
    modelfit = model_num/model_denom
    residuals = y - modelfit
    residuals = sum(residuals**2)
    AIC = len(x) * np.log((2*np.pi)/(len(x))) + len(x) + 2 + len(x) * np.log(residuals) + 2*4
    return(AIC)


def fcn2min_three(params,x,y):
    """
        This function will be minimized when the SIII funciton is called. It uses a set of parameteres
        and the structure of the Schoolfield model to compute the difference between predicted and
        observed trait values.
    """
    # A set of parameters
    params = params.valuesdict()
    b0 = params['b0']
    E = params['E']
    tl = params['tl']
    el = params['el']
    k = params['k']
    # The model. Given the parameters and x values (temperatures), computes the expected trait values
    model_num = b0 * np.exp((-E/k) * (1/x - 1/283.15))
    model_denom = 1 + np.exp((el/k) * (1/tl - 1/x))
    modelfit = model_num/model_denom
    # Returns the difference between expected and observed trait values
    return np.log(modelfit) - y

def calculate_aic_three(b0,E,tl,el,k,x,y):
    """
        This function calculates the AIC scores for the SIII model, requires the minimized parameter values
        and the temperature and trait values
    """
    model_num = b0 * np.exp((-E/k) * (1/x - 1/283.15))
    model_denom = 1 + np.exp((el/k) * (1/tl - 1/x))
    modelfit = model_num/model_denom
    residuals = y - modelfit
    residuals = sum(residuals**2)
    AIC = len(x) * np.log((2*np.pi)/(len(x))) + len(x) + 2 + len(x) * np.log(residuals) + 2*4
    return(AIC)


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
    # The dataframe holds the column names and ID names.
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
            AIC = calculate_aic_one(result.params['b0'].value,result.params['E'].value,result.params['tl'].value,result.params['th'].value,result.params['el'].value,result.params['eh'].value,result.params['k'].value,x,np.exp(y))
            
            
            # Extract estimated parameter values and AIC values, theses are added to a dataframe
            output.loc[i] = pd.Series({'Model':'SI','b0':result.params['b0'].value,'E':result.params['E'].value,'tl':result.params['tl'].value,'th':result.params['th'].value,'el':result.params['el'].value,'eh':result.params['eh'].value,'k':result.params['k'].value,'AIC':AIC})
        
        # If a ValueError is encountered (the model cannot converge) the estimated parameter values
        # are set to 0 and the AIC is set to 1e8.
        except ValueError:
            output.loc[i] = pd.Series({'Model':'SI','b0':0,'E':0,'tl':0,'th':0,'el':0,'eh':0,'k':0,'AIC':1e8})

        
        # If a TypeError is encountered (because more parameters are trying to be estimated than data points
        # available), estimated parameter values are set to 0 and AIC is set to 1e8.
        except TypeError:
            output.loc[i] = pd.Series({'Model':'SI','b0':0,'E':0,'tl':0,'th':0,'el':0,'eh':0,'k':0,'AIC':1e8})


    # The output dataframe columns are sorted and organized
    output = output[['Model','b0','E','tl','th','el','eh','k','AIC']]
    #
    output.index.name = 'FinalID'

    # The model parameter estimates and AIC values are saved to a file located in the Results directory
    output.to_csv('../Data/NLLS_model1.csv',index=True)

def SII(d,ids):
    """
        This function performs the fitting of the SII model. Requires a data set and a set of ids. For
        each id, sets an x and y array from the provided data set. The function creates the required
        parameters and sets them according to previous calculations. The function tries to minimize the
        residuals. If successful, the AIC and parameter estimates are saved to an output dataframe. If
        the residuals cannot be minimized, then the parameter estimates are set to 0 and the AIC is set
        as 1e8.
    """
    
    # Create an empty dataframe
    output = pd.DataFrame(columns=['Model','b0','E','th','eh','k','AIC'],index = ids)
    # The dataframe holds the column names and ID names.
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
        fit_params.add('eh', value = np.array(d.Eh.loc[d['FinalID']==i].tolist()[0]), min = np.array(d.E.loc[d['FinalID']==i].tolist()[0])) # this is taken from the estimated eh value
        fit_params.add('k',value =8.617e-5,vary=False) # this is a constant
    
        # This block of code tries to minimize the residuals of the model
        try:
            # Perform fitting
            result = minimize(fcn2min_two,fit_params,args=(x,y),nan_policy='omit')
        
            # Transform AIC values, because logged trait values are being used
            AIC = calculate_aic_two(result.params['b0'].value,result.params['E'].value,result.params['th'].value,result.params['eh'].value,result.params['k'].value,x,np.exp(y))
            
            # Extract estimated parameter values and AIC values, theses are added to a dataframe
            output.loc[i] = pd.Series({'Model':'SII','b0':result.params['b0'].value,'E':result.params['E'].value,'th':result.params['th'].value,'eh':result.params['eh'].value,'k':result.params['k'].value,'AIC':AIC})
    
        # If a ValueError is encountered (the model cannot converge) the estimated parameter values
        # are set to 0 and the AIC is set to 1e8.
        except ValueError:
            output.loc[i] = pd.Series({'Model':'SII','b0':0,'E':0,'th':0,'eh':0,'k':0,'AIC':1e8})
        except TypeError:
            output.loc[i] = pd.Series({'Model':'SII','b0':0,'E':0,'th':0,'eh':0,'k':0,'AIC':1e8})

    # The output dataframe columns are sorted and organized
    output = output[['Model','b0','E','th','eh','k','AIC']]
    # Set the index name of the rows to be the Final IDs
    output.index.name = 'FinalID'

    # The model parameter estimates and AIC values are saved to a file located in the Results directory
    output.to_csv('../Data/NLLS_model2.csv',index=True)

def SIII(d,ids):
    """
        This function performs the fitting of the SIII model. Requires a data set and a set of ids. For
        each id, sets an x and y array from the provided data set. The function creates the required
        parameters and sets them according to previous calculations. The function tries to minimize the
        residuals. If successful, the AIC and parameter estimates are saved to an output dataframe. If
        the residuals cannot be minimized, then the parameter estimates are set to 0 and the AIC is set
        as 1e8.
    """
    
    # Create an empty dataframe
    output = pd.DataFrame(columns=['Model','b0','E','tl','el','k','AIC'],index = ids)
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
        fit_params.add('tl', value = np.array(d.Tl.loc[d['FinalID']==i].tolist()[0])) # this is taken from the estimated tl value
        fit_params.add('el', value = np.array(d.El.loc[d['FinalID']==i].tolist()[0]), max = np.array(d.E.loc[d['FinalID']==i].tolist()[0])) # this is taken from the estimated el value
        fit_params.add('k',value =8.617e-5,vary=False) # this is a constant
        
        # This block of code tries to minimize the residuals of the model
        try:
            # Perform fitting
            result = minimize(fcn2min_three,fit_params,args=(x,y),nan_policy='omit')
        
            # Transform AIC values, because logged trait values are being used
            AIC = calculate_aic_three(result.params['b0'].value,result.params['E'].value,result.params['tl'].value,result.params['el'].value,result.params['k'].value,x,np.exp(y))
            
            # Extract estimated parameter values and AIC values, theses are added to a dataframe
            output.loc[i] = pd.Series({'Model':'SIII','b0':result.params['b0'].value,'E':result.params['E'].value,'tl':result.params['tl'].value,'el':result.params['el'].value,'k':result.params['k'].value,'AIC':AIC})
                    
        # If a ValueError is encountered (the model cannot converge) the estimated parameter values
        # are set to 0 and the AIC is set to 1e8.
        except ValueError:
            output.loc[i] = pd.Series({'Model':'SIII','b0':0,'E':0,'tl':0,'el':0,'k':0,'AIC':1e8})
        except TypeError:
            output.loc[i] = pd.Series({'Model':'SIII','b0':0,'E':0,'tl':0,'el':0,'k':0,'AIC':1e8})

    # The output dataframe columns are sorted and organized
    output = output[['Model','b0','E','tl','el','k','AIC']]
    # Set the index name of the rows to be the Final IDs
    output.index.name = 'FinalID'

    # The model parameter estimates and AIC values are saved to a file located in the Results directory
    output.to_csv('../Data/NLLS_model3.csv',index=True)

"""
    The next block of code will:
        a) Read the data
        b) Convert Ids to categories and save them as a list
        c) Call the above functions to perform model fitting
"""

# Loading data
d = pd.read_csv('../Data/TidyOriginalTrait.csv')

# Convert IDs into categories
d['FinalID'] = d['FinalID'].astype('category')

# Obtain individual IDs in list
ids = d.FinalID.cat.categories.tolist()

# Running the functions
print('The cubic models are about to be fitted')
cubic(d,ids)
print('Cubic models have finished, the SI models are about to be fitted')
SI(d,ids)
print('SI models have finished, the SII models are about to be fitted')
SII(d,ids)
print('SII models have finished, the SIII models are about to be fitted')
SIII(d,ids)
print('Model fitting is done!')

