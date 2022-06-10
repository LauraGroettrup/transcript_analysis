# ANOVA VII - ANOVA Model Comparision

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed
# - calculations_miraculous_twa1, _twa4, _twa5 executed

source("./main.R")
#-------------------------------------------
# values: dialogTable_gender_role
#   Sentiment
#   Sentiment_transformed_z
#-------------------------------------------

#ANOVA1, ANOVA4, ANOVA5 should be compared - twa1, twa4, twa5
#https://www.scribbr.com/statistics/anova-in-r/
#-------------------------------------------

#model fit: Akaike information criterion (AIC) 
  AIC_model.set <- list(twa1, twa4, twa5)
  AIC_model.names <- c("twa1: Gender", "twa4: Gender_Role", "twa5: Gender_Role_Category")
  aictab(AIC_model.set, modnames = AIC_model.names)

#best fit: Gender_Role!

#Check for homoscedasticity of winner: twa4
    par(mfrow=c(2,2))
    plot(twa4)
    par(mfrow=c(1,1))

    
#bootsampling for best fitted model (twa4)

    #bootstrapping (FoCSS) - I
    # obtaining M bootstrap samples and calculate mean value of each bootstrap sample
    bootstrap_m <- 10000 
    bootstrap_slopes <- numeric(bootstrap_m)
    bootstrap_intercepts <- numeric(bootstrap_m)
    for(i in 1:bootstrap_m)
    {
      bootstrap_sample <- sample(nrow(dialogTable_gender_mf$Sentiment_transformed_z), replace = T)
      #bootmodel <- ?aov(Sentiment_transformed_z ~ Gender_From[bootstrap_sample] * Gender_To[bootstrap_sample] * Role_From[bootstrap_sample] * Role_To[bootstrap_sample], data=dialogTable_gender_mf)
      bootstrap_model <- lm(dialogTable_gender_mf$Sentiment_transformed_z[bootstrap_sample] ~ dialogTable_gender_mf$Gender_From[bootstrap_sample] * dialogTable_gender_mf$Gender_To[bootstrap_sample] * dialogTable_gender_mf$Role_From[bootstrap_sample] * dialogTable_gender_mf$Role_To[bootstrap_sample])
      #summary.aov(bootmodel)
      bootstrap_slopes[i] <- bootstrap_model$coefficients[2]
      bootstrap_intercepts[i] <- bootstrap_model$coefficients[1]
    }
    median(bootstrap_slopes)
    sd(bootstrap_slopes)
    hist(bootstrap_slopes)
    quantile(bootstrap_slopes, probs = c(0.025, 0.975))

    bootstrap_coefs <- rbind(bootstrap_intercepts, bootstrap_slopes)
  
    confint(twa4)
    confint(bootstrap_model)
    ##wie weiter?
    
#---aufräumen--------------------------------
    rm(i, )
    
#-END ANOVA - Model Comparision END---------
