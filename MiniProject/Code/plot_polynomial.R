# Plot_models
# Fernando Pedraza Perez
# This script does the following:
# i) reads data and calculates for each curve the predicted trait values from fitted models
# ii) plots each thermal curve with estimated values from models in different colors
# (graphs are saved to results directory). the number of desired plots must be specified. Currently 100 plots will
# be generated. Keep in mind that around 2,000 models were run so might not want to produce all of the PDFs!
# iii) compares AIC values of fitted models to perform model selection.
# iv) generates four plots which look at the proportion of best fitting models for different habitat types,
# phyla and trait type.
# i and ii are performed in SECTION 1 of the code
# iii and iv are performed in SECTION 2 of the code

# Cleanup
rm(list = ls())
graphics.off()


# Required packages
require('ggplot2') # -> gglopt will be used in both SECTION 1 and SECTION 2 of the code
require('tidyr') # -> tidyr will be used in SECTION 2 of the code
require('cowplot') # -> cowplot displays two ggplots side by side
######################################################################

# SECTION 1 #


# This block of code defines functions used to calculate estimated trait values from
# the model parameters

plot_poly = function(x,b0,b1,b2,b3){
    # This function takes a vector of temperature values (in Celsius) and the estimated parameter
    # values from the cubic model for the particular temperature values. It returns a vector of
    # estimated trait values given a cubic model.
    y = b0 + b1*x + b2*(x^2) + b3*(x^3)
    y = log(y)
    return(y)
}

plot_nlls1 = function(x,b0,E,tl,th,el,eh){
    # This function takes a vector of temperature values (in Celsius) and the estimated parameter
    # values from the SI model for the particular temperature values. It returns a vector of
    # estimated trait values given the SI model.
    k = 8.617e-05 # defines the Boltzmann constant
    x = x + 273.15 # converts temperature from Celsius to Kelvin, as model was fit using Kelvin
    y = (b0 * exp(-E/k * (1/x - 1/283.15))) / (1 + exp(el/k * (1/tl - 1/x)) + exp(eh/k * (1/th - 1/x))) # Calculates the predicted trait values
    y = log(y)
    return(y)
}

plot_nlls2 = function(x,b0,E,th,eh){
    # This function takes a vector of temperature values (in Celsius) and the estimated parameter
    # values from the SII model for the particular temperature values. It returns a vector of
    # estimated trait values given the SII model.
    k = 8.617e-05 # defines the Boltzmann constant
    x = x + 273.15 # converts temperature from Celsius to Kelvin, as model was fit using Kelvin
    y = (b0 * exp(-E/k * (1/x - 1/283.15))) / (1 + exp(eh/k * (1/th - 1/x))) # Calculates the predicted trait values
    y = log(y)
    return(y)
}

plot_nlls3 = function(x,b0,E,tl,el){
    # This function takes a vector of temperature values (in Celsius) and the estimated parameter
    # values from the SII model for the particular temperature values. It returns a vector of
    # estimated trait values given the SII model.
    k = 8.617e-05 # defines the Boltzmann constant
    x = x + 273.15 # converts temperature from Celsius to Kelvin, as model was fit using Kelvin
    y = (b0 * exp(-E/k * (1/x - 1/283.15))) / (1 + exp(el/k * (1/tl - 1/x))) # Calculates the predicted trait values
    y = log(y)
    return(y)
}

plot_graphs = function(points,cubic_coef,nlls1_coef,nlls2_coef,nll3_coef,ngraphs){
    # This function requires a dataset of:
    # a) Observed trait values and temperatures
    # b) Estimated parameter values for a cubic model
    # c) Estimated parameter values for a SI model
    # d) Estimated parameter values for a SII model
    # e) Estimated parameter values for a SIII model
    # f) Number of graphs to be generated (i.e. number of TPC curves to be plotted), IF ALL
    # CURVES ARE WANTED, ENTER 0 AS NGRAPHS!
    # This function plots the observed trait values of each TPC curve with the corresponding
    # estimated trait values from the cubic, SI, SII and SIII models. # If a model failed to converge
    # the estimated values are set to zero. Graphs are saved as PDFs to the results directory.
    # Temperatures in Celsius and log of trait values are plotted!
    
    # If NGRAPHS is set to zero, all the TPC curves will be drawn and saved.
    # WARNING: NOT RECOMMENDED AS MANY TPC CURVES WERE USED
    if(ngraphs == 0){
        ngraphs = nrow(cubic_coef)
    }
    
    # TPC to be plotted
    for (i in unique(points$FinalID)[1:ngraphs]){
        # For each specified TPC curve
        
        # Set the x and y values
        x = points[points$FinalID == i,]$ConTemp # x is the temperature values in Celsius
        y = points[points$FinalID == i,]$log_Trait # y is the Original Trait Value
        
        # Calls the plot_poly function. This saves the estimated trait values from the cubic model
        # The arguments passed to the function are: the x values (temperature), Intercept, linear
        # term of x, quadratic term of x and cubic termn of x. All of these values are from the estimated
        # parameter values.
        y_coef = plot_poly(x,cubic_coef[cubic_coef$FinalID == i,]$Intercept,cubic_coef[cubic_coef$FinalID == i,]$x,cubic_coef[cubic_coef$FinalID == i,]$x2,cubic_coef[cubic_coef$FinalID == i,]$x3)
        
        # Calls the plot_nlls1 function. This saves the estimated trait values from the SI model.
        # The arguments passed to the function are: the x values (temperature), B0, E, tl, th, el, and eh
        # All of these values are from the estimated parameter values.
        y_nlls1 = plot_nlls1(x,nlls1_coef[nlls1_coef$FinalID == i,]$b0,nlls1_coef[nlls1_coef$FinalID == i,]$E,nlls1_coef[nlls1_coef$FinalID == i,]$tl,nlls1_coef[nlls1_coef$FinalID == i,]$th,nlls1_coef[nlls1_coef$FinalID == i,]$el,nlls1_coef[nlls1_coef$FinalID == i,]$eh)
        
        # Calls the plot_nlls2 function. This saves the estimated trait values from the SII model.
        # The arguments passed to the function are: the x values (temperature), B0, E, th, and eh
        # All of these values are from the estimated parameter values.
        y_nlls2 = plot_nlls2(x,nlls2_coef[nlls2_coef$FinalID == i,]$b0,nlls2_coef[nlls2_coef$FinalID == i,]$E,nlls2_coef[nlls2_coef$FinalID == i,]$th,nlls2_coef[nlls2_coef$FinalID == i,]$eh)
        
        # Calls the plot_nlls3 function. This saves the estimated trait values from the SIII model.
        # The arguments passed to the function are: the x values (temperature), B0, E, tl, and el
        # All of these values are from the estimated parameter values.
        y_nlls3 = plot_nlls3(x,nlls3_coef[nlls3_coef$FinalID == i,]$b0,nlls3_coef[nlls3_coef$FinalID == i,]$E,nlls3_coef[nlls3_coef$FinalID == i,]$tl,nlls3_coef[nlls3_coef$FinalID == i,]$el)
        
        # This block of code will generate the plots
        
        pdf(file=paste('../Results/',i,'.pdf',sep=''))
        # Plots the observed temperature and trait values
        p = ggplot(points[points$FinalID == i,],aes(x = x, y = y))
        # Defines colors, shape and size of observed point values
        p = p + geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = 0.4,color='orange') + theme_minimal()
        # Adds labels to x and y axis. The title is set as the name of the Consumer species.
        p = p + labs(x = 'Temperature',y = 'log(Trait Value)',title=points[points$FinalID == i,]$Consumer[1])
        # Adds the estimated trait values from the cubic model as a blue line
        p = p + geom_line(aes(x,y_coef,color='royalblue2'),linetype = 'solid',size = 1,show.legend = TRUE)
        # Adds the estimated trait values from the SI model as a green line
        p = p + geom_line(aes(x,y_nlls1,color='darkgreen'),linetype = 'solid',size = 1,show.legend = TRUE)
        # Adds the estimated trait values from the SII model as a red line
        p = p + geom_line(aes(x,y_nlls2,color='firebrick2'),linetype = 'solid',size = 1,show.legend = TRUE)
        # Adds the estimated trait values from the SIII model as a purple line
        p = p + geom_line(aes(x,y_nlls3,color='purple2'),linetype = 'solid',size = 1,show.legend = TRUE)
        # Adds a legend to the graph for the different lines (i.e. model types)
        p = p + scale_color_manual('Model', values=c(darkgreen = 'darkgreen',firebrick2 = 'firebrick2',purple2 = 'purple2',royalblue2 = 'royalblue2'),labels=c('Schoolfield I','Schoolfield II','Schoolfield III','Cubic')) 
        # print the graph
        print(p)
        dev.off()
    }
}

# This part of the script were used to generate Figure 1 in the writeup!
# This section requires the library cowplot to set two plots side by side:
plot_example = function(ids){
  # This function plots two examples of fitted TPCs
  # Uses code from above for more information read comments in plot_graphs
  # Requires two IDs
  i = ids[1]
  
  x = points[points$FinalID == i,]$ConTemp # x is the temperature values in Celsius
  y = points[points$FinalID == i,]$log_Trait # y is the Original Trait Value
  
  y_coef = plot_poly(x,cubic_coef[cubic_coef$FinalID == i,]$Intercept,cubic_coef[cubic_coef$FinalID == i,]$x,cubic_coef[cubic_coef$FinalID == i,]$x2,cubic_coef[cubic_coef$FinalID == i,]$x3)
  y_nlls1 = plot_nlls1(x,nlls1_coef[nlls1_coef$FinalID == i,]$b0,nlls1_coef[nlls1_coef$FinalID == i,]$E,nlls1_coef[nlls1_coef$FinalID == i,]$tl,nlls1_coef[nlls1_coef$FinalID == i,]$th,nlls1_coef[nlls1_coef$FinalID == i,]$el,nlls1_coef[nlls1_coef$FinalID == i,]$eh)
  y_nlls2 = plot_nlls2(x,nlls2_coef[nlls2_coef$FinalID == i,]$b0,nlls2_coef[nlls2_coef$FinalID == i,]$E,nlls2_coef[nlls2_coef$FinalID == i,]$th,nlls2_coef[nlls2_coef$FinalID == i,]$eh)
  y_nlls3 = plot_nlls3(x,nlls3_coef[nlls3_coef$FinalID == i,]$b0,nlls3_coef[nlls3_coef$FinalID == i,]$E,nlls3_coef[nlls3_coef$FinalID == i,]$tl,nlls3_coef[nlls3_coef$FinalID == i,]$el)
  
  plt1 = ggplot(points[points$FinalID == i,],aes(x = x, y = y))
  plt1 = plt1 + geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = 0.4,color='orange') + theme_minimal()
  plt1 = plt1 + labs(x = 'Temperature',y = 'log(Trait Value)',title=points[points$FinalID == i,]$Consumer[1])
  plt1 = plt1 + geom_line(aes(x,y_coef,color='royalblue2'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt1 = plt1 + geom_line(aes(x,y_nlls1,color='darkgreen'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt1 = plt1 + geom_line(aes(x,y_nlls2,color='firebrick2'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt1 = plt1 + geom_line(aes(x,y_nlls3,color='purple2'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt1 = plt1 + scale_color_manual('Model', values=c(darkgreen = 'darkgreen',firebrick2 = 'firebrick2',purple2 = 'purple2',royalblue2 = 'royalblue2'),labels=c('Schoolfield I','Schoolfield II','Schoolfield III','Cubic')) 
  
  # generate plot for second ID
  i = ids[2]
  
  x2 = points[points$FinalID == i,]$ConTemp # x is the temperature values in Celsius
  y2 = points[points$FinalID == i,]$log_Trait # y is the Original Trait Value
  
  y_coef2 = plot_poly(x2,cubic_coef[cubic_coef$FinalID == i,]$Intercept,cubic_coef[cubic_coef$FinalID == i,]$x,cubic_coef[cubic_coef$FinalID == i,]$x2,cubic_coef[cubic_coef$FinalID == i,]$x3)
  y_nlls12 = plot_nlls1(x2,nlls1_coef[nlls1_coef$FinalID == i,]$b0,nlls1_coef[nlls1_coef$FinalID == i,]$E,nlls1_coef[nlls1_coef$FinalID == i,]$tl,nlls1_coef[nlls1_coef$FinalID == i,]$th,nlls1_coef[nlls1_coef$FinalID == i,]$el,nlls1_coef[nlls1_coef$FinalID == i,]$eh)
  y_nlls22 = plot_nlls2(x2,nlls2_coef[nlls2_coef$FinalID == i,]$b0,nlls2_coef[nlls2_coef$FinalID == i,]$E,nlls2_coef[nlls2_coef$FinalID == i,]$th,nlls2_coef[nlls2_coef$FinalID == i,]$eh)
  y_nlls32 = plot_nlls3(x2,nlls3_coef[nlls3_coef$FinalID == i,]$b0,nlls3_coef[nlls3_coef$FinalID == i,]$E,nlls3_coef[nlls3_coef$FinalID == i,]$tl,nlls3_coef[nlls3_coef$FinalID == i,]$el)
  
  plt2 = ggplot(points[points$FinalID == i,],aes(x = x2, y = y2))
  plt2 = plt2 + geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = 0.4,color='orange') + theme_minimal()
  plt2 = plt2 + labs(x = 'Temperature',y = 'log(Trait Value)',title=points[points$FinalID == i,]$Consumer[1])
  plt2 = plt2 + geom_line(aes(x2,y_coef2,color='royalblue2'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt2 = plt2 + geom_line(aes(x2,y_nlls12,color='darkgreen'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt2 = plt2 + geom_line(aes(x2,y_nlls22,color='firebrick2'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt2 = plt2 + geom_line(aes(x2,y_nlls32,color='purple2'),linetype = 'solid',size = 1,show.legend = TRUE)
  plt2 = plt2 + scale_color_manual('Model', values=c(darkgreen = 'darkgreen',firebrick2 = 'firebrick2',purple2 = 'purple2',royalblue2 = 'royalblue2'),labels=c('Schoolfield I','Schoolfield II','Schoolfield III','Cubic')) 
  
  # Display both plots side by side 
  pdf(file=paste('../Results/Compare_TPC.pdf',sep=''))
  c = plot_grid(plt1+theme(legend.position = "none"), plt2+theme(legend.position = "none"), labels = c('A','B'),align = 'vh',hjust=-1,nrow=1)
  legend_c = get_legend(plt1+theme(legend.position = "bottom"))
  c = plot_grid(c,legend_c,ncol=1,rel_heights = c(1,.2))
  print(c)
  dev.off()
  
}

######################################################################

# SECTION 2 #

# This section of the code will compare AIC values between the four fitted models (i.e. cubic, SI, SII and
# SIII). Plots will be generated comparing the relative performance of each model type in different:
# i) habitats, ii) kingdoms and iii) trait types.

compare_AIC = function(AIC_matrix,cubic_coef){
    # This functions compares the AIC values obtained for each model for each TPC.
    
    # Creates an empty matrix which has as many rows as TPCs and four columns which correspond to the
    # 4 models fit
    res_AIC = matrix(0,nrow = nrow(AIC_matrix),ncol=4)
    # Checks for any NAs and replaces them for a very large number
    AIC_matrix[which(is.na(AIC_matrix))] <- 1e8
    # If AIC is infinity it is set to be a large number
    AIC_matrix[which(is.infinite(AIC_matrix))] <- 1e8

    # Compares each TPC
    for (i in 1:nrow(AIC_matrix)){
        comp = AIC_matrix[i,1:4]
        # Determines which model had the lowest AIC value
        low = min(comp)
        # Finds models which differed by less than two AIC units from the lowest AIC value
        comp = abs(low - comp) <= 2
        # Adds a vector of TRUE/FALSE indicating which model(s) was/were the best for each TPC curve
        res_AIC[i,] = comp
    }
    # Set the column names corresponding to the model type
    colnames(res_AIC) = c('Cubic','SI','SII','SIII')
    # Add a set of columns which hold relevant 'metadata' contained in the Cubic Coef data set
    res_AIC = data.frame(res_AIC,cubic_coef[,8:11])
    return(res_AIC)
}

compare_general = function(results_matrix){
    # This function compares the relative performance of each model
    
    # Creates a dataframe with the performance values for each model and the model name
    general = data.frame(Performance=c(sum(results$Cubic)/nrow(results),sum(results$SI)/nrow(results),sum(results$SII)/nrow(results),sum(results$SIII)/nrow(results)),
    Model=c('Cubic','SI', 'SII','SIII'))
    
    # Generate the plot
    pdf(file=paste('../Results/compare_models.pdf',sep=''))
    h = ggplot(data=general, aes(x=Model, y=Performance)) + geom_col(aes(fill = Model)) + ylab('Fraction of TPC curves') + scale_fill_discrete(guide=FALSE)
    print(h)
    dev.off()
    
}

compare_habitat = function(results_matrix){
    # This function compares the relative performance of each model within different habitats
    
    # Creates an empty matrix of 3 rows (number of habitats to be compared) and 4 columns (number of types
    # of models)
    habitat = matrix(0,nrow=3,ncol=4)
    for (i in 1:4) {
        
        # For every model type calculates the proportion of times the particular model was the best performing
        # one for a given habitat type. Habitats were Marine (coded in the data as Marine and marine),
        # Terrestrial (coded in the data as Terrestrial and terrestrial) and Freshwater (coded in the data as
        # freshwater)
        habitat[1,i] = sum(results_matrix[results_matrix$Habitat %in% c('Marine','marine'),i])/length(results_matrix[results_matrix$Habitat %in% c('Marine','marine'),i])
        habitat[2,i] = sum(results_matrix[results_matrix$Habitat %in% c('Terrestrial','terrestrial'),i])/length(results_matrix[results_matrix$Habitat %in% c('Terrestrial','terrestrial'),i])
        habitat[3,i] = sum(results_matrix[results_matrix$Habitat %in% c('freshwater'),i])/length(results_matrix[results_matrix$Habitat %in% c('freshwater'),i])
    }
    # Adds the corresponding names of the fitted models
    colnames(habitat) = c('Cubic','SI','SII','SIII')
    # Adds the habitat names
    Habitat = c('Marine','Terrestrial','Freshwater')
    # Creates a dataframe of Habitat names and relative performs of model types
    habitat = as.data.frame(cbind(Habitat,habitat))
    # Reorders the data so that for each Kingdom all four model performance values are displayed
    habitat = habitat %>% gather(Models,counts,Cubic:SIII)
    
    # Generates plot of performance by habitat
    pdf(file=paste('../Results/Habitat_models.pdf',sep=''))
    h = ggplot(habitat,aes(Habitat,as.numeric(counts))) + geom_col(aes(fill = Models),position = 'dodge') + xlab('Habitat') + ylab('Fraction of TPC curves')
    print(h)
    dev.off()
    
    # Uncomment return statement if you want to manipulate the dataframe
    #return(habitat)
}

compare_kingdom = function(results_matrix){
    # This function compares the relative performance of each model within different kingdoms
    
    # Creates an empty matrix of 5 rows (number of kingdoms to be compared) and 4 columns (number of types
    # of models)
    kingdom = matrix(0,nrow=5,ncol=4)
    for (i in 1:4){
        
        # For every model type calculates the proportion of times the particular model was the best performing
        # one for a given kingdom. Habitats were Bacteria (coded in the data as Archaea and Bacteria, this
        # isn't technically correct but is done for simplicity), Fungi (coded in the data as Fungi and),
        # Metazoa (coded in the data as Metazoa), Plantae (coded in the data as Plantae) and Protista (coded
        # in the data as Chromista, Protista and Protozoa, again not technically correct)

        kingdom[1,i] = sum(results_matrix[results_matrix$ConKingdom %in% c('Archaea','Bacteria'),i])/length(results_matrix[results_matrix$ConKingdom %in% c('Archaea','Bacteria'),i])
        kingdom[2,i] = sum(results_matrix[results_matrix$ConKingdom %in% c('Fungi'),i])/length(results[results$ConKingdom %in% c('Fungi'),i])
        kingdom[3,i] = sum(results_matrix[results_matrix$ConKingdom %in% c('Metazoa'),i])/length(results_matrix[results_matrix$ConKingdom %in% c('Metazoa'),i])
        kingdom[4,i] = sum(results_matrix[results_matrix$ConKingdom %in% c('Plantae'),i])/length(results_matrix[results_matrix$ConKingdom %in% c('Plantae'),i])
        kingdom[5,i] = sum(results_matrix[results_matrix$ConKingdom %in% c('Chromista','Protista','Protozoa'),i])/length(results_matrix[results_matrix$ConKingdom %in% c('Chromista','Protista','Protozoa'),i])
    }
    # Adds the corresponding names of the fitted models
    colnames(kingdom) = c('Cubic','SI','SII','SIII')
    # Adds the Kingdom names
    Kingdom = c('Monera','Fungi','Metazoa','Plantae','Protista')
    # Creates a dataframe of Kingdom names and relative performs of model types
    kingdom = as.data.frame(cbind(Kingdom,kingdom))
    # Reorders the data so that for each Kingdom all four model performance values are displayed
    kingdom = kingdom %>% gather(Models,counts,Cubic:SIII)
    
    # Generates plot of performance by Kingdom
    pdf(file=paste('../Results/Kingdom_models.pdf',sep=''))
    k = ggplot(kingdom,aes(Kingdom,as.numeric(counts))) + geom_col(aes(fill = Models),position = 'dodge') + xlab('Kindgom') + ylab('Fraction of TPC curves')
    print(k)
    dev.off()
    
    # Uncomment return statement if you want to manipulate the dataframe
    #return(kingdom)
}

compare_trait = function(results_matrix){
    # This function compares the relative performance of each model within different trait types
    
    # Creates an empty matrix of 3 rows (number of trait types to be compared) and 4 columns (number of types
    # of models)
    metabolic = matrix(0,nrow = 3,ncol = 4)
    for (i in 1:4){
        
        # For every model type calculates the proportion of times the particular model was the best performing
        # one for a given metabolic trait. Traits were Growth Rate (coded in the data as Individual Length
        # Growth Rate, Individual Mass Growth Rate, Population Growth Rate, Radial Growth Rate, Specific
        # Growth Rate), Photosynthesis (coded in the data as Rate of photosynthesis, gross photosynthesis, net
        # photosynthesis) and Respiration (coded in the data as Mass-Specific Respiration Rate and respiration
        # rate)
        
        metabolic[1,i] = sum(results_matrix[results_matrix$StandardisedTraitName %in% c('Individual Length Growth Rate','Individual Mass Growth Rate','Population Growth Rate','Radial Growth Rate','Specific Growth Rate'),i])/length(results_matrix[results_matrix$StandardisedTraitName %in% c('Individual Length Growth Rate','Individual Mass Growth Rate','Population Growth Rate','Radial Growth Rate','Specific Growth Rate'),i])
        metabolic[2,i] = sum(results_matrix[results_matrix$StandardisedTraitName %in% c('Rate of photosynthesis','gross photosynthesis','net photosynthesis'),i])/length(results_matrix[results_matrix$StandardisedTraitName %in% c('Rate of photosynthesis','gross photosynthesis','net photosynthesis'),i])
        metabolic[3,i] = sum(results_matrix[results_matrix$StandardisedTraitName %in% c('Mass-Specific Respiration Rate','respiration rate'),i])/length(results_matrix[results_matrix$StandardisedTraitName %in% c('Mass-Specific Respiration Rate','respiration rate'),i])
    }
    # Adds the corresponding names of the fitted models
    colnames(metabolic) = c('Cubic','SI','SII','SIII')
    # Adds the trait type names
    Metabolic = c('Growth','Photosynthesis','Respiration')
    # Creates a dataframe of trait type names and relative performs of model types
    metabolic = as.data.frame(cbind(Metabolic,metabolic))
    # Reorders the data so that for each trait type all four model performance values are displayed
    metabolic = metabolic %>% gather(Models,counts,Cubic:SIII)
    
    # Uncomment return statement if you want to manipulate the dataframe
    # Generates plot of performance by Trait
    pdf(file=paste('../Results/Metabolic_models.pdf',sep=''))
    m = ggplot(metabolic,aes(Metabolic,as.numeric(counts))) + geom_col(aes(fill = Models),position = 'dodge') + xlab('Metabolic Trait') + ylab('Fraction of TPC curves')
    print(m)
    dev.off()
    
    # Uncomment return statement if you want to manipulate the dataframe
    #return(metabolic)
}


######################################################################
# Load data and run code

# Read data

print('Data is being read')
points = read.csv('../Data/TidyOriginalTrait.csv')
cubic_coef = read.csv('../Data/CubicModels.csv')
nlls1_coef = read.csv('../Data/NLLS_model1.csv')
nlls2_coef = read.csv('../Data/NLLS_model2.csv')
nlls3_coef = read.csv('../Data/NLLS_model3.csv')
full_AIC = cbind(cubic_coef$AIC,nlls1_coef$AIC,nlls2_coef$AIC,nlls3_coef$AIC)

# Call the plotting function
print('Plots are being generated')
plot_graphs(points,cubic_coef,nlls1_coef,nlls2_coef,nll3_coef,100) # This will generate the first 100 plots!

# Plotting of TPCs is done!
print('Plotting Finished!')

# Comparing AIC values between models
print('Comparing AIC values between models')
results = compare_AIC(full_AIC,cubic_coef)

# Generating plots comparing AIC values between habitats, kingdoms and trait types
print('Plotting AIC comparison between habitats, kingdoms and trait types')
compare_general(results)
compare_habitat(results)
compare_kingdom(results)
compare_trait(results)
print('Plotting Finished')

# Plot two examples of TPC curves
print('Plotting an example of TPC curves')
plot_example(c('MTD3538','MTD2079'))

# Completed all tasks
print('All tasks complete!')

# Basic information from the results and discussion
# This section is commented out as the output is just numbers
# These values were cited in the write-up

# What proportion of models converged for the SI, SII and SIII models?
 #1 - (length(which(nlls1_coef$k == 0)) + (length(which(is.na(nlls1_coef$b0))))) / nrow(nlls1_coef) #-> answer: 0.99
 #1 - (length(which(nlls2_coef$k == 0)) + (length(which(is.na(nlls1_coef$b0))))) / nrow(nlls2_coef) #-> #answer: 0.99
 #1 - (length(which(nlls3_coef$k == 0)) + (length(which(is.na(nlls1_coef$b0))))) / nrow(nlls3_coef) #-> #answer: 0.80

# What is the mode of the parameter values?
#Mode = function(data) {
# This function gives the mode of a dataset.
# Sets zeros as NAs and then removes them
# CODE:
#  data[which(data==0)] <- NA
#  data = na.omit(data)
#  ux = unique(data)
#  ux[which.max(tabulate(match(data, ux)))]
#}

# What is the mode of? :

# Estimated E values for SI: 
# Mode(nlls1_coef$E) answer: 1.32

# Estimated E values for SII: 
# Mode(nlls2_coef$E) answer: 1.63

# Estimated E values for SIII:
# Mode(nlls3_coef$E) answer: 5.62

# Estimated Eh values for SI:
# Mode(nlls1_coef$eh) answer: 4.33
# Mode(nlls2_coef$eh) answer: 0.65

# Estimated El values for SI:
# Mode(nlls1_coef$el) answer: -0.0004
# Mode(nlls3_coef$el) answer: 0.65
 