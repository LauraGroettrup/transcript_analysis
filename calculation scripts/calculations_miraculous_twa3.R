# CALCULATIONs III

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed

source("./main.R")

#-------------------------------------------

#ANOVA3: two-way-anova (twa3)
#Sentiment
        #AV: Sentiment.ai-Scores over time (added constant, log transformed)
        #UV: Role_Category_From, Role_Category_To
  
  twa3s <- aov(Sentiment_transformed_log ~ Role_Category_From * Role_Category_To, data=dialogTable_gender_role)
  Anova(twa3s, type=3)
  summary(twa3s)
  
  twa3s_pht1 <- dialogTable_gender_role %>% #post-hoc-test - Role_Category_To
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_Category_To, paired = F,
      p.adjust.method = "bonferroni")
  twa3s_pht1
  
  dialogTable_gender_role %>% #crosstable twa3_pht1
    group_by(Role_Category_To) %>%
    get_summary_stats(Sentiment_transformed_log, type = "mean_sd")
  #interpretation twa3s_pht1: s. overview - results
  
  twa3s_pht2<-TukeyHSD(twa3s) # interaction
  twa3s_pht2
  
  #plots
    #to do
  
#Vader
  #AV: Vader-Scores (untransformed)
  #UV: Role_Category_From, Role_Category_To
  
  twa3v <- aov(Vader ~ Role_Category_From * Role_Category_To, data=dialogTable_gender_role)
  Anova(twa3v, type=3)
  summary(twa3v)
  
  twa3v_pht1 <- dialogTable_gender_role %>% #post-hoc-test - Role_Category_To
    pairwise_t_test(
      Vader ~ Role_Category_From, paired = F,
      p.adjust.method = "bonferroni")
  twa3v_pht1
  
  dialogTable_gender_role %>% #crosstable twa3_pht1
    group_by(Role_Category_From) %>%
    get_summary_stats(Vader, type = "mean_sd")
  #interpretation twa3s_pht1: s. overview - results
  
  
  twa3v_pht2 <- dialogTable_gender_role %>% #post-hoc-test - Role_Category_To
    pairwise_t_test(
      Vader ~ Role_Category_To, paired = F,
      p.adjust.method = "bonferroni")
  twa3v_pht2
  
  dialogTable_gender_role %>% #crosstable twa3_pht1
    group_by(Role_Category_To) %>%
    get_summary_stats(Vader, type = "mean_sd")
  #interpretation twa3s_pht2: s. overview - results
  
  #plots
    #to do
  
#----END ANOVA3-----------------------------

#ANOVA4: two-way-anova (twa4)
        #AV: Sentiment.ai-Scores over time (added constant, log transformed)
        #UV: Gender-From, Gender-To, Role_Category_From, Role_Category_To
  twa4 <- aov(Sentiment_transformed_log ~ Gender_From * Gender_To+Role_Category_From+Role_Category_To+Role_Category_From*Role_Category_To, data=dialogTable_gender_mf)
  Anova(twa4, type=3)
  summary(twa4)
  
  twa4_pht1 <- dialogTable_gender_mf %>% #post-hoc-test -Gender_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Gender_From, paired = F,
      p.adjust.method = "bonferroni")
  twa4_pht1
  
  twa4_pht2 <- dialogTable_gender_mf %>% #post-hoc-test - Role_Category_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_Category_From, paired = F,
      p.adjust.method = "bonferroni")
  twa4_pht2
  
  twa4_pht3 <- dialogTable_gender_mf %>% #post-hoc-test - Role_Category_To
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_Category_To, paired = F,
      p.adjust.method = "bonferroni")
  twa4_pht3
  
  twa4_pht4<-TukeyHSD(twa4)
#-------------------------------------------

  
        
        