rm(list = ls())  # clear objects
require('tidyr')

# Read data
cubic_coef = read.csv('../Data/CubicModels.csv')
nlls1_coef = read.csv('../Data/NLLS_model1.csv')
nlls2_coef = read.csv('../Data/NLLS_model2.csv')
nlls3_coef = read.csv('../Data/NLLS_model3.csv')

full_AIC = cbind(cubic_coef$AIC,nlls1_coef$AIC,nlls2_coef$AIC,nlls3_coef$AIC)
names = c('cubic','s1','s2','s3')
res_AIC = matrix(0,nrow=nrow(full_AIC),ncol=4)
for (i in 1:nrow(full_AIC)) {
  comp = full_AIC[i,1:4]
  low = min(comp)
  comp = abs(low - comp) <= 2
  res_AIC[i,] = comp
}

res_AIC = as.data.frame(res_AIC)
colnames(res_AIC) = names
results = data.frame(res_AIC,cubic_coef[,9:12])

marine = c(0,0,0,0)
terrestrial = c(0,0,0,0)
freshwater = c(0,0,0,0)

for (i in 1:4) {
  marine[i] = sum(results[results$Habitat %in% c('Marine','marine'),i])/length(results[results$Habitat %in% c('Marine','marine'),i])
  terrestrial[i] = sum(results[results$Habitat %in% c('Terrestrial','terrestrial'),i])/length(results[results$Habitat %in% c('Terrestrial','terrestrial'),i])
  freshwater[i] = sum(results[results$Habitat %in% c('freshwater'),i])/length(results[results$Habitat %in% c('freshwater'),i])
}
models = c('Cubic','SI','SII','SIII')
habitat = as.data.frame(cbind(models,marine,terrestrial,freshwater))
habitat = habitat %>% gather(hab,counts,marine:freshwater)

monera = c(0,0,0,0)
fungi = c(0,0,0,0)
metazoa = c(0,0,0,0)
plantae = c(0,0,0,0)
protista = c(0,0,0,0)

for (i in 1:4) {
  monera[i] = sum(results[results$ConKingdom %in% c('Archaea','Bacteria'),i])/length(results[results$ConKingdom %in% c('Archaea','Bacteria'),i])
  fungi[i] = sum(results[results$ConKingdom %in% c('Fungi'),i])/length(results[results$ConKingdom %in% c('Fungi'),i])
  metazoa[i] = sum(results[results$ConKingdom %in% c('Metazoa'),i])/length(results[results$ConKingdom %in% c('Metazoa'),i])
  plantae[i] = sum(results[results$ConKingdom %in% c('Plantae'),i])/length(results[results$ConKingdom %in% c('Plantae'),i])
  protista[i] = sum(results[results$ConKingdom %in% c('Chromista','Protista','Protozoa'),i])/length(results[results$ConKingdom %in% c('Chromista','Protista','Protozoa'),i])
}

kingdom = as.data.frame(cbind(models,monera,fungi,metazoa,plantae,protista))
kingdom = kingdom %>% gather(king,counts,monera:protista)

Growth = c(0,0,0,0)
Photosynthesis = c(0,0,0,0)
Respiration = c(0,0,0,0)

for (i in 1:4) {
  Growth[i] = sum(results[results$StandardisedTraitName %in% c('Individual Length Growth Rate','Individual Mass Growth Rate','Population Growth Rate','Radial Growth Rate','Specific Growth Rate'),i])/length(results[results$StandardisedTraitName %in% c('Individual Length Growth Rate','Individual Mass Growth Rate','Population Growth Rate','Radial Growth Rate','Specific Growth Rate'),i])
  Photosynthesis[i] = sum(results[results$StandardisedTraitName %in% c('Rate of photosynthesis','gross photosynthesis','net photosynthesis'),i])/length(results[results$StandardisedTraitName %in% c('Rate of photosynthesis','gross photosynthesis','net photosynthesis'),i])
  Respiration[i] = sum(results[results$StandardisedTraitName %in% c('Mass-Specific Respiration Rate','respiration rate'),i])/length(results[results$StandardisedTraitName %in% c('Mass-Specific Respiration Rate','respiration rate'),i])
}

metabolic = as.data.frame(cbind(models,growth,photo,resp))
metabolic = metabolic %>% gather(metabolic,counts,growth:resp)

scaleFUN <- function(x) sprintf("%.2f", x)

require('ggplot2')
ggplot(habitat,aes(hab,as.numeric(counts),fill)) + geom_col(aes(fill = models),position = 'dodge') + xlab('Habitat') + ylab('Fraction of TPC curves')
ggplot(kingdom,aes(king,as.numeric(counts))) + geom_col(aes(fill = models),position = 'dodge') + xlab('Habitat') + ylab('Fraction of TPC curves')
ggplot(metabolic,aes(metabolic,as.numeric(counts))) + geom_bar(aes(fill = models),position = 'dodge',stat='identity') + xlab('Habitat') + ylab('Fraction of TPC curves')

cubic_logistic = glm(cubic~Habitat+Observations,family=binomial,data=results)
s2_logistic = glm(s2~Habitat+Observations,family=binomial,data=results)
s3_logistic = glm(s3~Habitat+Observations,family=binomial,data=results)
