# ANOVA III

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

#ANOVA2: two-way-anova (twa1)
#Sentiment 
#AV: Sentiment.ai-Scores (added constant, log transformed)
#UV: Role-Category-From, Role-Category-To

twa3 <- aov(Sentiment_transformed_z ~ Role_category_From * Role_category_To, data=dialogTable_gender_role)
Anova(twa3, type=3)
summary(twa3)

#if necessary
twa3_pht1 <- dialogTable_gender_role %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_category_, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa3_pht1

twa3_pht2<-TukeyHSD(twa3)
twa3_pht2

#table nicht mit transformierten Daten
dialogTable_gender_role %>% #crosstable twa1_pht1
  group_by(Role_category_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#plots
#to do

#interpretation twa1: diff of Sentiment_transformed_log - scores between m and f in Gender_From; no other effect (Gender_to, Interaction): FEMALES SHOW sign. LOWER SENTIMENT THAN MALES


#plots
#to do

#-END ANOVA3--------------------------------------
