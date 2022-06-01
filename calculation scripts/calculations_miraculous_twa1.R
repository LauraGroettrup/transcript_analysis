# CALCULATIONs I

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed

source("./main.R")

#-------------------------------------------

#ANOVA1: two-way-anova (twa1)
#Sentiment 
        #AV: Sentiment.ai-Scores (added constant, log transformed)
        #UV: Gender-From, Gender-To

  twa1s <- aov(Sentiment_transformed_log ~ Gender_From * Gender_To, data=dialogTable_gender_role)
  Anova(twa1s, type=3)
  summary(twa1s)
  
  twa1s_pht1 <- dialogTable_gender_role %>% #post-hoc-test
    pairwise_t_test(
      Sentiment_transformed_log ~ Gender_From, paired = F,
      p.adjust.method = "bonferroni")
  twa1s_pht1
  
  dialogTable_gender_role %>% #crosstable twa1s_pht1
    group_by(Gender_From) %>%
    get_summary_stats(Sentiment_transformed_log, type = "mean_sd")

  #plots
    #to do
  
  #interpretation twa1: diff of Sentiment_transformed_log - scores between m and f in Gender_From; no other effect (Gender_to, Interaction): FEMALES SHOW sign. LOWER SENTIMENT THAN MALES

#Vader  
  #AV: Vader-Scores (untransformed data)
  #UV: Gender-From, Gender-To
  
  twa1v <- aov(Vader ~ Gender_From * Gender_To, data=dialogTable_gender_role)
  Anova(twa1v, type=3)
  summary(twa1v)
  
  twa1v_pht1 <- dialogTable_gender_role %>% #post-hoc-test
    pairwise_t_test(
      Vader ~ Gender_To, paired = F,
      p.adjust.method = "bonferroni")
  twa1v_pht1
  
  dialogTable_gender_role %>% #crosstable twa1s_pht1
    group_by(Gender_To) %>%
    get_summary_stats(Vader, type = "mean_sd")
  
  #plots
    #to do
  
#-END ANOVA1--------------------------------------
  