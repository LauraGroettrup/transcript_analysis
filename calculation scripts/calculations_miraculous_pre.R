# CALCULATIONs PRE
#prerequisites
  # - download script executed
  # - cleaning script executed
  # - analyzing script executed
  # - gender_role script executed

#PREREQUISITES for further calculations

source("./main.R")
#-----------------------------------------
#if necessary
#CALCULATIONS WITH GENDER "MALE" AND "FEMALE" - "GENDERLESS" DELETED for ANOVAS (dialogs in "From" and "To" with kwawis and multiple people)

  #subset with gender: "male" & "female" only, "genderless" deleted: result -> dialogTable_gender_mf
      #dialogTable_gender_role$id<-1:nrow(dialogTable_gender_role) #ID hinzufügen
      #dialogTable_gender_mf <- dialogTable_gender_role[ which(dialogTable_gender_role$Gender_From=='male'| dialogTable_gender_role$Gender_From=='female'), ]
      #dialogTable_gender_mf <- dialogTable_gender_mf[ which(dialogTable_gender_mf$Gender_To=='male'| dialogTable_gender_mf$Gender_To=='female'), ]
        
    #find out if subset worked - just male&female groups
      #dialogTable_gender_role %>%
        #group_by(Gender_From, Gender_To) %>%
        #summarize(Frequency=n())%>% arrange(desc(Frequency))  

#Descriptives
        dialogTable_gender_role$id<-1:nrow(dialogTable_gender_role) #ID hinzufügen
        summary(dialogTable_gender_role) #overview
        #Sentiment over seasons
        dialogTable_gender_role %>%
          group_by(Season) %>%
          get_summary_stats(Sentiment, type = "mean_sd")
        #Vader over seasons
        dialogTable_gender_role %>%
          group_by(Season) %>%
          get_summary_stats(Vader, type = "mean_sd")
        
#visuals        
        #visuals of sentiment
        ggdensity(dialogTable_gender_role, x = "Sentiment", 
                  fill = "#0073C2FF", color = "#0073C2FF",
                  add = "mean", rug = TRUE, title = "plot of (all) Sentiment.Ai Scores")
        hist(dialogTable_gender_role$Sentiment, col='steelblue', main='histogram of (all) Sentiment.Ai Scores')
        #visuals of Vader
        ggdensity(dialogTable_gender_role, x = "Vader", 
                  fill = "#0073C2FF", color = "#0073C2FF",
                  add = "mean", rug = TRUE, title = "plot of (all) Vader Scores")
        hist(dialogTable_gender_role$Vader, col='steelblue', main='histogram of (all) Vader Scores')

#Outliers        
        #outliers Sentiment
        dialogTable_gender_role %>%
          group_by(Season) %>% identify_outliers(Sentiment)
        #outliers Vader
        dialogTable_gender_role %>%
          group_by(Season) %>% identify_outliers(Vader)
#-----------------------------------------
#Assumption of normal distribution
  #Sentiment
      #Shapiro-Wilk
        #interpretation: no normal-distribution - Shapiro-Wilk is very sensitive with higher sample sizes - looking at QQ plot instead
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Sentiment)
        
      #QQ Plot  
        #interpretation: also with QQ Plots a normal distribution of "Sentiment" cannot be assumed
        ggqqplot(dialogTable_gender_role, "Sentiment", facet.by = "Gender_From", main='untransformed Sentiment-data')
    
      #tranformation of Sentiment data to fit normal distribution
        #step1 - adding constant to avoid negative values (add min of Sentiment to all values)
        min(dialogTable_gender_role$Sentiment)
        dialogTable_gender_role$Sentiment_addedconstant <- dialogTable_gender_role$Sentiment+0.835 #adding smalles value
        min(dialogTable_gender_role$Sentiment_addedconstant)
        #step2 - actual transformations after all values are positiv
        #log
        dialogTable_gender_role$Sentiment_transformed_log <- log10(dialogTable_gender_role$Sentiment_addedconstant)
        which(is.na(dialogTable_gender_role$Sentiment_transformed_log)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Sentiment_transformed_log)
        hist(dialogTable_gender_role$Sentiment_transformed_log, col='steelblue', main='log-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_role, "Sentiment_transformed_log", facet.by="Gender_From", main='log-transformed Sentiment-data')
        #sqrt
        dialogTable_gender_role$Sentiment_transformed_sqrt <- sqrt(dialogTable_gender_role$Sentiment_addedconstant)
        which(is.na(dialogTable_gender_role$Sentiment_transformed_sqrt)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Sentiment_transformed_sqrt)
        hist(dialogTable_gender_role$Sentiment_transformed_sqrt, col='steelblue', main='sqrt-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_role, "Sentiment_transformed_sqrt", facet.by="Gender_From", main='sqrt-transformed Sentiment-data')
        #cuberoot
        dialogTable_gender_role$Sentiment_transformed_cuberoot <- dialogTable_gender_role$Sentiment_addedconstant^(1/3)
        which(is.na(dialogTable_gender_role$Sentiment_transformed_cuberoot)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Sentiment_transformed_cuberoot)
        hist(dialogTable_gender_role$Sentiment_transformed_cuberoot, col='steelblue', main='cuberoot-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_role, "Sentiment_transformed_cuberoot", facet.by="Gender_From", main='cuberoot-transformed Sentiment-data')

        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Sentiment_transformed_log)
        
  #Vader
      #Shapiro-Wilk
        #interpretation: no normal-distribution - Shapiro-Wilk is very sensitive with higher sample sizes - looking at QQ plot instead
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader)
        
      #QQ Plot  
        #interpretation: also with QQ Plots a normal distribution of "Vader" can hardly be assumed
        ggqqplot(dialogTable_gender_role, "Vader", facet.by = "Gender_From", main='untransformed Vader-data') 
        
      #tranformation of Vader data to fit normal distribution
        #step1 - adding constant to avoid negative values (add min of Sentiment to all values)
        min(dialogTable_gender_role$Vader)
        dialogTable_gender_role$Vader_addedconstant <- dialogTable_gender_role$Vader+0.962 #adding smalles value
        min(dialogTable_gender_role$Vader_addedconstant)
        #step2 - actual transformations after all values are positiv
        #log
        dialogTable_gender_role$Vader_transformed_log <- log10(dialogTable_gender_role$Vader_addedconstant)
        which(is.na(dialogTable_gender_role$Vader_transformed_log)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Vader_transformed_log)
        hist(dialogTable_gender_role$Vader_transformed_log, col='steelblue', main='log-transformed Vader-data')
        ggqqplot(dialogTable_gender_role, "Vader_transformed_log", facet.by="Gender_From", main='log-transformed Vader-data')
        #sqrt
        dialogTable_gender_role$Vader_transformed_sqrt <- sqrt(dialogTable_gender_role$Vader_addedconstant)
        which(is.na(dialogTable_gender_role$Vader_transformed_sqrt)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Vader_transformed_sqrt)
        hist(dialogTable_gender_role$Vader_transformed_sqrt, col='steelblue', main='sqrt-transformed Vader-data')
        ggqqplot(dialogTable_gender_role, "Vader_transformed_sqrt", facet.by="Gender_From", main='sqrt-transformed Vader-data') 
        #cuberoot
        dialogTable_gender_role$Vader_transformed_cuberoot <- dialogTable_gender_role$Vader_addedconstant^(1/3)
        which(is.na(dialogTable_gender_role$Vader_transformed_cuberoot)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Vader_transformed_cuberoot)
        hist(dialogTable_gender_role$Vader_transformed_cuberoot, col='steelblue', main='cuberoot-transformed Vader-data')
        ggqqplot(dialogTable_gender_role, "Vader_transformed_cuberoot", facet.by="Gender_From",main='cuberoot-transformed Vader-data') 
        
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader_transformed_log)
        
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader_transformed_sqrt)
        
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader_transformed_cuberoot)

        #intepretation - visually non of the transformed values is normal distributed, untransformed data might fits best

#-----------------------------------------       
#Assumption of sphericity: Mauchly’s test - for repeated ANOVAS
    #needed to run ANOVAS with repeated measures; should also be computed when running the repeated measures ANOVA
    #Sentiment & Gender
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Sentiment, wid = id, between = Gender_From)
        get_anova_table(res.aov)
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Sentiment, wid = id, between = Gender_To)
        get_anova_table(res.aov)
        #sphericity violated
    #Vader & Gender
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Vader, wid = id, between = Gender_From)
        get_anova_table(res.aov)
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Vader, wid = id, between = Gender_To)
        get_anova_table(res.aov)
        #sphericity violated

#----END OF PRE (NEXT: ANOVAS)------------------------
