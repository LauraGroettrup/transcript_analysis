# CALCULATIONs VIII
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed
# - calculations_miraculous_wordcount

# Descriptives Martin

source("./main.R")
#-----------------------------------------
#todo
#drop: gender:genderless
#todo - relative lines/gender



#Characteristics: Episodes / Seasons
    desc_no_episodes<-episodeTable %>%
    group_by(Season) %>%
    summarize(Frequency=n())%>% arrange(desc(Frequency))
    view(desc_no_episodes)

#Characteristics: Gender

    view(characters_series_gender)
    
    #Anzahl an Frequenzen nach Geschlecht pro Season
      desc_gender_by_season<-characters_season %>% 
        group_by(Season, Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_gender_by_season <-desc_gender_by_season[order(desc_gender_by_season$Season, desc_gender_by_season$Gender),]
      view(desc_gender_by_season)
      
      interaction.plot(x.factor = desc_gender_by_season$Season, trace.factor = desc_gender_by_season$Gender, 
                       response = desc_gender_by_season$Frequency, 
                       type = "b", legend = TRUE, 
                       xlab = "Season", ylab="Frequency", main="Frequency of gender & season",
                       pch=c(1,19), col = c("#00AFBB", "#E7B800"))
    
    #In allen Season vorkommenden Charactern #45 Charaktere
      desc_reoccurring_gender_by_series<-character_seasons_sentiment_time_complete %>% 
        group_by(Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_reoccurring_gender_by_series <-desc_reoccurring_gender_by_series[order(desc_reoccurring_gender_by_series$Gender),]
      view(desc_reoccurring_gender_by_series)
      
      view(character_seasons_sentiment_time_complete)
    
#Characteristics: Role
    View(characters_series_role)

#Characteristics: Role_Category    
    View(characters_series_role_category)
    
    desc_gender_by_rolecategory<-characters_series %>% 
      group_by(Gender, Role_Category) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_gender_by_rolecategory<-desc_gender_by_rolecategory[order(desc_gender_by_rolecategory$Role_Category, desc_gender_by_rolecategory$Gender),]
    view(desc_gender_by_rolecategory)
    
    #search for specific group
    subset<-subset(lineTable_gender_role, Gender == "female" & Role == "main" & Role_Category=="no_villain")
    view(subset)
    rm(subset)

#Lines per gender
    desc_lines_by_gender<-lineTable_gender_role %>% 
      group_by(Gender) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_lines_by_gender<-desc_lines_by_gender[order(desc_lines_by_gender$Gender),]
    view(desc_lines_by_gender)
    
    
#Lines per gender & Role
    desc_lines_by_gender_role<-lineTable_gender_role %>% 
      group_by(Gender, Role) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_lines_by_gender_role<-desc_lines_by_gender_role[order(desc_lines_by_gender_role$Gender, desc_lines_by_gender_role$Role),]
    view(desc_lines_by_gender_role)
    
#Lines per gender & season
    desc_lines_by_gender_season<-lineTable_gender_role %>% 
      group_by(Gender, Season) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_lines_by_gender_season<-desc_lines_by_gender_season[order(desc_lines_by_gender_season$Gender, desc_lines_by_gender_season$Season),]
    view(desc_lines_by_gender_season)
    
    interaction.plot(x.factor = desc_lines_by_gender_season$Season, trace.factor = desc_lines_by_gender_season$Gender, 
                     response = desc_lines_by_gender_season$Frequency, 
                     type = "b", legend = TRUE, 
                     xlab = "Season", ylab="Line Frequency", main="lines per gender & season",
                     pch=c(1,19), col = c("#00AFBB", "#E7B800"))
    
    #todo - relative lines/gender
    #äteren pro gender racanzahl corrigiert nach character (relative) - korrigieren, dass female mehr characters haben als male (genderless raus)

#WCount Outliers - not necessary - all relevant
            describeBy(lineTable_gender_role$WCount, lineTable_gender_role$Gender)
            
            lineTable_gender_role %>%
              identify_outliers("WCount")
            
        #outliers by groups
            lineTable_gender_role %>% 
              group_by(Gender) %>%
              identify_outliers("WCount")
            
          #outliers by groups - visual
            hist(lineTable_gender_role$WCount,
                 xlab = "WCount",
                 main = "Histogram of WCount",
                 breaks = sqrt(nrow(lineTable_gender_role)))
            
            ggplot(lineTable_gender_role) +
              aes(x = WCount) +
              geom_histogram(bins = 30L, fill = "#0c4c8a") +
              theme_minimal()
            
            ggbetweenstats(lineTable_gender_role,
                           Gender, WCount, outlier.tagging = TRUE)
            
            boxplot(lineTable_gender_role$WCount,
                    ylab = "WCount")
            
            #boxplot.stats(lineTable_gender_role$WCount)$out
            
#WCount per gender
            
    desc_wcount_by_gender<-lineTable_gender_role %>% 
      group_by(Gender) %>%
      summarize(Mean = mean(WCount, na.rm=TRUE))
    desc_wcount_by_gender<-desc_wcount_by_gender[order(desc_wcount_by_gender$Gender),]
    view(desc_wcount_by_gender)
    
#WCount per gender & season   
    desc_wcount_by_gender_season<-lineTable_gender_role %>% 
      group_by(Gender, Season) %>%
      summarize(Mean = mean(WCount, na.rm=TRUE))
    desc_wcount_by_gender_season<-desc_wcount_by_gender_season[order(desc_wcount_by_gender_season$Gender, desc_wcount_by_gender_season$Season),]
    view(desc_wcount_by_gender_season)
    
    interaction.plot(x.factor = desc_wcount_by_gender_season$Season, trace.factor = desc_wcount_by_gender_season$Gender, 
                     response = desc_wcount_by_gender_season$Mean, 
                     type = "b", legend = TRUE, 
                     xlab = "Season", ylab="Line Frequency", main="lines per gender & season",
                     pch=c(1,19), col = c("#00AFBB", "#E7B800"))
    
    
    
    
#describeBy(episodeTable$Episode_No_Overall, episodeTable$Season)
#-------Descriptives Martin--------------------------------------
