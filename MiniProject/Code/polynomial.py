#!/usr/bin/python

# Imports
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

# Loading data
d = pd.read_csv('../Data/TidyOriginalTrait.csv')

# Convert IDs into categories
d['FinalID'] = d['FinalID'].astype('category')

# Obtain individual IDs in list
ids = d.FinalID.cat.categories.tolist()

# Create empty Data Frame
output = pd.DataFrame()

# Fit a cubic model for each thermal response curve

for i in ids:
    x = d.ConTemp.loc[d['FinalID']==i].tolist()
    y = d.OriginalTraitValue.loc[d['FinalID']==i].tolist()
    df = pd.DataFrame({"x":x,"y":y})
    model = smf.ols('y~x + I(x**2) + I(x**3)',data=df).fit()
    output = output.append({"ID":i,"Model":"Cubic","Intercept":model.params.tolist()[0],"x":model.params.tolist()[1],"x2":model.params.tolist()[2],"x3":model.params.tolist()[3],"AIC":model.aic,"BIC":model.bic,"Habitat":d.Habitat.loc[d['FinalID']==i].tolist()[0],"ConKingdom":d.ConKingdom.loc[d['FinalID']==i].tolist()[0],"StandardisedTraitName":d.StandardisedTraitName.loc[d['FinalID']==i].tolist()[0],"Observations":d.Observations.loc[d['FinalID']==i].tolist()[0]},ignore_index=True)


# Sort Columns
output = output[['ID','Model','Intercept','x','x2','x3','AIC','BIC','Habitat','ConKingdom','StandardisedTraitName','Observations']]

# Save "output" as a csv file in the Results Directory
output.to_csv('../Data/CubicModels.csv',index=False)


