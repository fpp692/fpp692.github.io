#### SOMETHING ####

rm(list = ls())  # clear objects
graphics.off()  #close all open figures and graphics objects

# Read data
points = read.csv("../Data/TidyGrowthRespPhotoData.csv")
cubic_coef = read.csv('../Data/CubicModels.csv')
nlls1_coef = read.csv('../Data/NLLS_model1.csv')
nlls2_coef = read.csv('../Data/NLLS_model2.csv')
nlls3_coef = read.csv('../Data/NLLS_model3.csv')

# Require
require('ggplot2')

