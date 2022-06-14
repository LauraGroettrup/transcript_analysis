# ANOVA 1 

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed

source("./main.R")
#-------------------------------------------
# values: dialogTable_gender_role
#   Sentiment
#   Sentiment_transformed_z
#-------------------------------------------

#ANOVA1: two-way-anova (twa1)
#Sentiment 
        #AV: Sentiment.ai-Scores (added constant, log transformed)
        #UV: Gender-From, Gender-To

  twa1 <- aov(Sentiment_transformed_z ~ Gender_From * Gender_To, data=dialogTable_gender_mf)
  Anova(twa1, type=3)
  summary(twa1)
  
  #if necessary
  twa1_pht1 <- dialogTable_gender_mf %>% #post-hoc-test
    pairwise_t_test(
      Sentiment_transformed_z ~ Gender_From, paired = F,
      p.adjust.method = "bonferroni")
  twa1_pht1

  #table nicht mit transformierten Daten
  dialogTable_gender_mf %>% #crosstable twa1_pht1
    group_by(Gender_From) %>%
    get_summary_stats(Sentiment, type = "mean_sd")

  #effect size:
    eta_squared(car::Anova(twa1, type = 3))
    #very small effect sizes
    
  #plots
    
    #https://statdoe.com/scatterplot-for-two-factors-in-r/
     hier weiter 
      ggplot(dialogTable_gender_mf, aes(Gender_From, Gender_To, color = Sentiment)) + 
      geom_point()
#-END ANOVA1--------------------------------------
  