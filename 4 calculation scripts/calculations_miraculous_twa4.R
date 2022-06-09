# ANOVA IV

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

#ANOVA4: two-way-anova (twa4)
#Sentiment 
#AV: Sentiment.ai-Scores (added constant, log transformed)
#UV: Gender-From, Gender-To, Role-From, Role-To

twa4 <- aov(Sentiment_transformed_z ~ Gender_From * Gender_To * Role_From * Role_To, data=dialogTable_gender_role)
Anova(twa4, type=3)
summary(twa4)

#if necessary
twa4_pht1 <- dialogTable_gender_role %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_category_, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa4_pht1

twa4_pht2<-TukeyHSD(twa4)
twa4_pht2

#table nicht mit transformierten Daten
dialogTable_gender_role %>% #crosstable twa1_pht1
  group_by(Gender_From, Role_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")



#plots
#to do

#interpretation twa1: diff of Sentiment_transformed_log - scores between m and f in Gender_From; no other effect (Gender_to, Interaction): FEMALES SHOW sign. LOWER SENTIMENT THAN MALES


#plots
#to do

#-END ANOVA4--------------------------------------
