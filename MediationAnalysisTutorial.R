# Mediation analysis tutorial
# Adapted from https://data.library.virginia.edu/introduction-to-mediation-analysis/
# In my case the microbe will be mediated by the HPLC data to produce a change in the behavioral score.
# Baron and Kenny (1986)
# Not great for small sample sizes.
# Three sets of regression: X->Y, X->M, X + M -> Y
# 
# References:
# Tingley, D., Yamamoto, T., Hirose, K., Keele, L., & Imai, K. (2014). Mediation: R package for causal mediation analysis.
# Baron, R. M., & Kenny, D. A. (1986). The moderator–mediator variable distinction in social psychological research: Conceptual, strategic, and statistical considerations. Journal of Personality and Social Psychology, 5, 1173-1182.
# Shrout, P. E., & Bolger, N. (2002). Mediation in experimental and nonexperimental studies: new procedures and recommendations. Psychological Methods, 7, 422-445.
# https://ademos.people.uic.edu/Chapter14.html
# Both mediation and moderation assume that there is little to no measurement error in the mediator/moderator variable and that the DV did not CAUSE the mediator/moderator. 
# We kind of have replicates for this reason
# We know that behaviors do not cause changes in chemical concentrations
# If mediator error is likely to be high, researchers should collect multiple indicators of the construct and use SEM to estimate latent variables. 
# TODO Consider the construction of a latent variable from the HPLC measurements

rm(list =ls())

# library('multilevel')
library('mediation')

dataFileName <- 'http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv'

myData <- read.csv(dataFileName)

model.0 <- lm(Y ~ X, myData)
summary(model.0)
# Can move forward if you don't have significance if we have a theoretical backing
# TODO Literature provides the theoretical backing here
model.M <- lm(M ~ X, myData)
summary(model.M)
# The goal in the next step is due reduce the significance of Y's dependence on X
# TODO Is is still fair to say that sex is a modulator then?
model.Y <- lm(Y ~ X + M, myData)
summary(model.Y)
# Here we have full mediation, but if the effect of X on Y is still significant
# then we call it partial mediation
# The next goal is to see if this is statistically significant
# 1) Sobel Test: This has fallen out of favor
# 2) Bootstrapping


# TODO How do we work out if X mediates M or M mediates X?

results <- mediate(model.M, model.Y, treat='X', mediator='M',
                   boot=TRUE, sims=500)
summary(results)

# Can be used in logistic regression as well
# Consider structural equation modeling if it can't be done in a small set of regressions

# TODO What about mediation analyses in machine learning approaches

# TODO How do you manage with bacteria, etc. nested within cage?

# Will need to do the subsetting by datasets before here?
# IV would be more like stress or diet (it is invidual)
# IV to metabolite
# metabolite to bacteria
# metabolite or bacteria to behavior
# The bacteria are grouped
# Many many hypotheses to correct for.
# TODO Find out how this was done in the pig study
# mma package may be helpful here.
# Is there a way I can bypass the nesting of the bacteria?
# If you consider the bacteria as just one mediator that works on the metabolite
# data to influence the behavior, it may be contained within that mouse.

for(cohort in pooledCohorts){
  # TODO Remember to set the rarefilter
  for(X in taxa){
    for(M in metabolites){
      for(Y in behaviors){
        fitM <- lm(M ~ X,     data=Meddata) #IV on M; Hours since dawn predicting coffee consumption
        fitY <- lm(Y ~ X + M, data=Meddata) #IV and M on DV; Hours since dawn and coffee predicting wakefuln
        fitMed <- mediate(fitM, fitY, treat="X", mediator="M")
        #summary(fitMed)
        #plot(fitMed)
        fitMedBoot <- mediate(fitM, fitY, boot=TRUE, sims=999, treat="X", mediator="M")
        #summary(fitMedBoot)
        #plot(fitMedBoot)
      }
    }
  }
}

