# TPC_data.R
# Fernando Pedraza Perez
# This script does the following:
# i) reads data and performs data wrangling
# ii) estimates initial parameter estimates for each TPC curve
# iii) saves wrangled data with parameter values to file

# Cleanup
rm(list=ls())

Data_Wrangling = function(d){
    
    # A function that performs wrangling. Subsets columns, removes NAs, filters out Trait Values smaller
    # than zero, subsets TPC curve for which at least five data points were caputred and adds columns
    # with parameter values that will be filled out later.
    
    # Subset relevant columns including, ID, temperature, trait value and auxiliary information
    d = subset(d,select=c("FinalID","StandardisedTraitName","OriginalTraitValue","ConTemp","Consumer","Habitat","ConKingdom"))
    
    # Removes NAs in the OriginalTraitValues, Consumer and ConTemp
    d = d[which(d$OriginalTraitValue != 'NA' & d$Consumer != 'NA' & d$ConTemp != 'NA'),]

    # Filters out trait values that are smaller than zero
    d = d[which(d$OriginalTraitValue > 0),]

    # Subsets TPC curves for which at least five datapoints are recorded.
    # This utilizes the FinalID as a unique identification key for each curve.
    d = subset(d,FinalID %in% names(which(table(d$FinalID)>=5)))

    # Add columns to the data
    d$log_Trait = log(d$OriginalTraitValue) # adds a column with the logged trait values
    d$TempKelv = d$ConTemp + 273.15 # adds a column with the temperatures converted to Kelvin
    d$KT = 1/((d$TempKelv) * 8.617e-05) #??adds a column with 1/KT values were K is the Boltzman constant
    
    d$E <- 0 # adds a column with zeros for the activation energy
    d$B0 <- 0 # adds a column with zeros for the B0 values
    d$Observations <- 0 # adds a column with zeros for the number of observations for each curve
    d$Tl <- 0
    d$Th <- 0
    d$El <- 0
    d$Eh <- 0 
    return(d) # returns the wrangled data
}

Estimate_Params = function(d){
    
    # A function that will estimate the initial parameter values in a given TPC:
    # B0: trait value closest to 10 degrees Celsius
    # Observations: number of observations in a TPC curve
    # E (activation energy) is estimated as the slope of the linear model fit to Temp vs 1/KT
    # Eh is set to be twice E
    # El is set to be half E
    # Th is set to be the high temperature at which the trait value is halved from the 'peak' of the curve
    # Tl is set to be the low temperature at which the trait value is havled from the 'peak' of the curve

    # This will cycle throughout the IDs
    for(i in unique(d$FinalID)){
        
        # subset the data
        test = subset(d,d$FinalID == i)
        
        # Estimate B0 and record the number of observations in the dataset
        d$B0[d$FinalID == i] = test$OriginalTraitValue[which(abs(test$TempKelv-283.15)==min(abs(test$TempKelv-283.15)))]
        d$Observations[d$FinalID == i] = nrow(test)
        
        # Define the peak of the curve, i.e. the highest trait value
        peak = test[test[,"OriginalTraitValue"] == max(test[,"OriginalTraitValue"]),]
        
        # If the peak trait value corresponds to either the maximum or minimum temperature recorded, then a model will not be able to fit the
        # data to estimate parameter value. In these cases Th is set to be the highest recorded temperature, Tl is the lowest. E is set to be
        # 0.65, Eh is double E and El half E.
        if(peak[,"TempKelv"]==max(test[,"TempKelv"])|peak[,"TempKelv"]==min(test[,"TempKelv"])){
            d$Th[d$FinalID == i] = test[test[,"TempKelv"]==max(test[,"TempKelv"]), "TempKelv"]
            d$Tl[d$FinalID == i] = test[test[,"TempKelv"]==min(test[,"TempKelv"]), "TempKelv"]
            d$E[d$FinalID == i] = 0.65
            d$Eh[d$FinalID == i] = 0.65 * 2
            d$El[d$FinalID == i] = 0.65 * 0.5
        }
            
        else{
            # If the peak is not the the highest or lowest recorded temperature:
            
            # Subset the curve into two halves one for higher temperatures and another for lower temperatures than the peak
            test_high = test[test[,"TempKelv"]>=peak[,"TempKelv"],]
            test_low = test[test[,"TempKelv"]<=peak[,"TempKelv"],]
            
            # Fit a linear model to the 'high' part of the curve to model the effect of 1/KT on the trait value
            model_high = lm(test_high[,'OriginalTraitValue']~test_high[,"KT"])
            # From the model, estimate the temperature at which the peak trait value is halved, in the higher temperature part of the curve.
            # this value (back transformed from 1/KT) is Th
            th = ((peak[,"OriginalTraitValue"]/2)-as.numeric(model_high$coefficients[1])/as.numeric(model_high$coefficients[2]))
            th = 1/(th*8.617e-05)
            
            # Fit a linear model to the 'low' part of the curve to model the effect of 1/KT on the trait value
            model_low = lm(test_low[,'OriginalTraitValue']~test_low[,"KT"])
            # From the model, estimate the temperature at which the peak trait value is halved, in the lower temperature part of the curve.
            # this value (back transformed from 1/KT) is Tl
            tl = ((peak[,"OriginalTraitValue"]/2)-as.numeric(model_low$coefficients[1])/as.numeric(model_low$coefficients[2]))
            tl = 1/(tl*8.617e-05)
            
            # Fit a linear model on the 'low' part of the curve to model the effect of 1/KT on the log of the trait values
            model_E = lm(log(test_low[,"OriginalTraitValue"])~test_low[,"KT"])
            # Extract the slope of the fitted model, multiply by -1 and set value to equal E
            d$E[d$FinalID == i] = model_E$coefficients[2] * -1
            # Set the values for tl and th to be estimated from models (see above)
            d$Tl[d$FinalID == i] = tl
            d$Th[d$FinalID == i] = th
            # Set Eh to be twice E and El to be half E
            d$Eh[d$FinalID == i] = (model_E$coefficients[2] * -1) * 2
            d$El[d$FinalID == i] = (model_E$coefficients[2] * -1) * 0.5
                
        }
    }
    
    # Save the wrangled dataset with the estimated parameter values to a file located in the Data directory
    write.csv(d,file='../Data/TidyOriginalTrait.csv',row.names = FALSE)
}

######### Running the functions ###########

# Read data
print('Reading data')
d = read.csv("../Data/GrowthRespPhotoData_new.csv")

# Wrangle data
print('Wrangling data')
d = Data_Wrangling(d)

# Estimate parameters and save csv file
print('Estimating parameter values and saving data')
Estimate_Params(d)


