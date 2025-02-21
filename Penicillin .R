library (lmtest)
library(lme4)
library(readr)
library(dplyr)
library(tidyr)
library("faraway")
data("penicillin")
force(penicillin)

#changing the data type of the blend entries from character to numeric 
#to allow us the plot the graph
penicillin$blend = as.numeric(penicillin$blend)
library(ggplot2)

# line graph of the 4 treatments on the same axis 
ggplot(penicillin,aes(blend,yield,colour = treat)) + 
  geom_point(aes(shape = treat)) + 
  geom_line(aes(linetype = treat), size = 1) + labs(title = "Penicillin")

#here we are checking the significance of interaction between treatments and blends
anova_interactions = aov(yield ~ treat*blend, penicillin)
summary(anova_interactions)


anova= aov(yield ~ treat+blend, penicillin)
summary(anova)

#variance components and  proportional allocation 
model= lmer(yield ~ treat+blend+(1|blend), penicillin)
var_components = as.data.frame(VarCorr(model))
var_components$proportion = var_components$vcov/sum(var_components$vcov)
var_components

#examines fitment, normality, outliers, independece of observations
plot(anova)

#examine whether the distribution of the residuals is Normal on Not
shapiro.test(residuals(anova))

#examine whether the avriance of the residuals is constant or Not
bptest(anova)
