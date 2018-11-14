library(minpack.lm)

# Read data 
data_nls = read.csv("../Data/TidyGrowthRespPhotoData.csv")

for (i in data_nls$FinalID[1:2]){
  
  ########## Define x as temperature in Kelvin ##########
  x = data_nls[data_nls$FinalID == i,]$ConTemp
  x = x + 273.15 # Convert temperature to Kelvin
  
  ########## Define y as trait value ##########
  y = data_nls[data_nls$FinalID == i,]$StandardisedTraitValue
  
  plot(x,y)
  
  ########## Define constants ##########
  k = 8.617e-5 # Boltzmann constant 
  E = 0.65 # Activation energy
  
  ########## Starting values for parameters ##########
  b0 = y[which(abs(x - 283.15) == min(abs(x - 283.15)))] # b0 is the trait value at 283.15 K (10 C)
  El =
  Eh = 
  Tl =
  Th = 
  
  # The Schoolfield model
  model = nlsLM(y~(bo*exp((-E/k)*((1/x)-(1/283.15))))/1+exp((El/k)*((1/Tl)-(1/x)))+exp((Eh/k)*((1/Th)-(1/x))),start = list(b0 = b0,E = E,El = El, Tl = Tl, Eh = Eh,Th = Th))
  plot(x,y)
  lines(x,predict(model,y))
            
  # Simplified Schoolfield model I: no/weak low temperature inactivation detected
  model2 = nlsLM(y~((bo*exp((-E/k)*((1/x)-(1/283.15))))/(1+exp((Eh/k)*((1/Th)-(1/x))))),start = list(b0=b0,E=E,Eh=Eh,Th=Th))
  plot(x,y)
  lines(x,predict(model2,y))
  
  # Simplified Schoolfield model II: no/weak high temperature inactivation detected
  model3 = nlsLM(y~((bo*exp((-E/k)*((1/T)-(1/283.15))))/(1+exp((El/k)*((1/Tl)-(1/T))))),start = list(b0=b0,E=E,El=El,Tl=Tl))
  plot(x,y)
  lines(x,predict(model3,y))
  
}


model1 <- function(){
  
  x = data_nls[data_nls$FinalID == i,]$ConTemp
  x = x + 273.15 # Convert temperature to Kelvin
  ########## Define y as trait value ##########
  y = data_nls[data_nls$FinalID == i,]$StandardisedTraitValue
  model = nlsLM(y~(bo*exp((-E/k)*((1/x)-(1/283.15))))/1+exp((El/k)*((1/Tl)-(1/x)))+exp((Eh/k)*((1/Th)-(1/x))),start = list(b0 = b0,E = E,El = El, Tl = Tl, Eh = Eh,Th = Th))
  plot(x,y)
  lines(x,predict(model,y))
  
}

B0 = which()







# x values are the temperature values in Celsius!
x = data_nls[data_nls$FinalID == "MTD2081",]$ConTemp

# Converted temperature values to Kelvin
x = x + 273.15

# y values are the trait values
y = data_nls[data_nls$FinalID == "MTD2081",]$StandardisedTraitValue

# Constants
k = 8.617e-5



# The Schoolfield model
model = nlsLM(y~(bo*exp((-E/k)*((1/x)-(1/283.15))))/1+exp((El/k)*((1/Tl)-(1/x)))+exp((Eh/k)*((1/Th)-(1/x))),start = list(bo = ,E = ,El =, Tl =, Eh = ,Th =))

# Simplified Schoolfield model I: no/weak low temperature inactivation detected
model2 = nlsLM(y~((bo*exp((-E/k)*((1/x)-(1/283.15))))/(1+exp((Eh/k)*((1/Th)-(1/x))))),start = list(bo=2.314815e-08,E=E,Eh=1,Th=290))

# Simplified Schoolfield model II: no/weak high temperature inactivation detected
model3 = nlsLM(y~((bo*exp((-E/k)*((1/T)-(1/283.15))))/(1+exp((El/k)*((1/Tl)-(1/T))))),start = list(bo=,E=,El=,Tl=))

