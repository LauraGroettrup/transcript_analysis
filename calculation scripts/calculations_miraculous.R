# CALCULATIONs

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed

source("./main.R")

#prerequisites
    #Descriptives
        summary(dialogTable_gender_role) #overview
    
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To, Season) %>%
          get_summary_stats(Sentiment, type = "mean_sd")
        
        dialogTable_gender_role %>%
          group_by(Season) %>%
          get_summary_stats(Sentiment, type = "mean_sd")
        
        plot_gender_from <- ggboxplot(dialogTable_gender_role, x = "Gender_From", y = "Sentiment", add = "point")
        plot_gender_from
        plot_gender_to <- ggboxplot(dialogTable_gender_role, x = "Gender_To", y = "Sentiment", add = "point")
        plot_gender_to
        
        dialogTable_gender_role %>%
          group_by(Gender_From) %>% identify_outliers(Sentiment)
        
    #Normality assumption
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Sentiment)
        