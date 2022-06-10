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
#plots

#gender elimination (genderless) from dialogtable_gender_role => dialogTable_gender_mf
  dialogTable_gender_mf<-dialogTable_gender_role[dialogTable_gender_role$Gender_From != "genderless", ] 
  dialogTable_gender_mf<-dialogTable_gender_mf[dialogTable_gender_mf$Gender_To != "genderless", ] 

#gender elimination (genderless) from lineTable_gender_role => lineTable_gender_mf
  lineTable_gender_mf<-lineTable_gender_role[lineTable_gender_role$Gender != "genderless", ]   
  
#gender elimination (genderless) from characters_season => characters_season_mf
  characters_season_mf<-characters_season[characters_season$Gender != "genderless", ]

#gender elimination (genderless) from characters_series => characters_series_mf 
  characters_series_mf<-characters_series[characters_series$Gender != "genderless", ]
  
  dialogTable_gender_mf %>%
    group_by(Gender_From) %>%
    summarize(Frequency=n())%>% arrange(desc(Frequency))

#Characteristics: Episodes / Seasons
    desc_no_episodes<-episodeTable %>%
    group_by(Season) %>%
    summarize(Frequency=n())%>% arrange(desc(Frequency))
    view(desc_no_episodes)

#Characteristics: Gender

    view(characters_series_gender) #anzahl pro gender
    
    #Anzahl an Frequenzen nach Geschlecht pro Season
      desc_gender_by_season<-characters_season_mf %>% 
        group_by(Season, Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_gender_by_season <-desc_gender_by_season[order(desc_gender_by_season$Season, desc_gender_by_season$Gender),]
      view(desc_gender_by_season)
      
      interaction.plot(x.factor = desc_gender_by_season$Season, trace.factor = desc_gender_by_season$Gender, 
                       response = desc_gender_by_season$Frequency, 
                       type = "b", legend = TRUE, 
                       xlab = "Season", ylab="Frequency", main="Frequency of gender & season",
                       pch=c(1,19), col = c("#00AFBB", "#E7B800"))
    
    #In allen Season vorkommenden Charactern #45 Charaktere (inkl. genderless)
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
    
    desc_gender_by_rolecategory<-characters_series_mf %>% 
      group_by(Gender, Role_Category) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_gender_by_rolecategory<-desc_gender_by_rolecategory[order(desc_gender_by_rolecategory$Role_Category, desc_gender_by_rolecategory$Gender),]
    view(desc_gender_by_rolecategory)
    
    #search for specific group
    subset<-subset(lineTable_gender_mf, Gender == "female" & Role == "main" & Role_Category=="no_villain")
    view(subset)
    rm(subset)

#Lines per gender
    desc_lines_by_gender<-lineTable_gender_mf %>% 
      group_by(Gender) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_lines_by_gender<-desc_lines_by_gender[order(desc_lines_by_gender$Gender),]
    view(desc_lines_by_gender)
    
    
#Lines per gender & Role
    desc_lines_by_gender_role<-lineTable_gender_mf %>% 
      group_by(Gender, Role) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_lines_by_gender_role<-desc_lines_by_gender_role[order(desc_lines_by_gender_role$Gender, desc_lines_by_gender_role$Role),]
    view(desc_lines_by_gender_role)
    
#Lines per gender & season
    desc_lines_by_gender_season<-lineTable_gender_mf %>% 
      group_by(Gender, Season) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_lines_by_gender_season<-desc_lines_by_gender_season[order(desc_lines_by_gender_season$Gender, desc_lines_by_gender_season$Season),]
    view(desc_lines_by_gender_season)
    
    interaction.plot(x.factor = desc_lines_by_gender_season$Season, trace.factor = desc_lines_by_gender_season$Gender, 
                     response = desc_lines_by_gender_season$Frequency, 
                     type = "b", legend = TRUE, 
                     xlab = "Season", ylab="Line Frequency", main="lines per gender & season",
                     pch=c(1,19), col = c("#00AFBB", "#E7B800"))
    
#Lines per gender & season - relative to number of female/male characters
    desc_lines_by_gender_season<-lineTable_gender_mf %>% 
      group_by(Season, Gender) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_lines_by_gender_season<-desc_lines_by_gender_season[order(desc_lines_by_gender_season$Gender, desc_lines_by_gender_season$Season),]
    view(desc_lines_by_gender_season)
    
    desc_gender_by_season<-characters_season_mf %>% 
      group_by(Season, Gender) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_gender_by_season <-desc_gender_by_season[order(desc_gender_by_season$Gender, desc_gender_by_season$Season),]
    view(desc_gender_by_season)
    
    desc_rel_lines_by_gender_season <-left_join(desc_lines_by_gender_season, desc_gender_by_season, by = c("Season" = "Season", "Gender" = "Gender"))
    colnames(desc_rel_lines_by_gender_season)<-c("Season", "Gender","Number_of_lines", "Number_of_characters")
    view(desc_rel_lines_by_gender_season)
    desc_rel_lines_by_gender_season$Relative_number_of_lines <- with(desc_rel_lines_by_gender_season, Number_of_lines/Number_of_characters) #relative Häufigkeit berechnen
        
    interaction.plot(x.factor = desc_rel_lines_by_gender_season$Season, trace.factor = desc_rel_lines_by_gender_season$Gender, 
                     response = desc_rel_lines_by_gender_season$Relative_number_of_lines, 
                     type = "b", legend = TRUE, 
                     xlab = "Season", ylab="Line Frequency", main="relative Number of Lines (=Lines/No of Characters) by gender & season",
                     pch=c(1,19), col = c("#00AFBB", "#E7B800"))

#WCount Outliers - not necessary - all relevant
            describeBy(lineTable_gender_mf$WCount, lineTable_gender_mf$Gender)
            
            lineTable_gender_mf %>%
              identify_outliers("WCount")
            
        #outliers by groups
            lineTable_gender_mf %>% 
              group_by(Gender) %>%
              identify_outliers("WCount")
            
          #outliers by groups - visual
            hist(lineTable_gender_mf$WCount,
                 xlab = "WCount",
                 main = "Histogram of WCount",
                 breaks = sqrt(nrow(lineTable_gender_mf)))
            
            ggplot(lineTable_gender_mf) +
              aes(x = WCount) +
              geom_histogram(bins = 30L, fill = "#0c4c8a") +
              theme_minimal()
            
            ggbetweenstats(lineTable_gender_mf,
                           Gender, WCount, outlier.tagging = TRUE)
            
            #boxplot(lineTable_gender_role$WCount,ylab = "WCount")
            
            #boxplot.stats(lineTable_gender_role$WCount)$out
            
#WCount per gender
            
    desc_wcount_by_gender<-lineTable_gender_mf %>% 
      group_by(Gender) %>%
      summarize(Mean = mean(WCount, na.rm=TRUE))
    desc_wcount_by_gender<-desc_wcount_by_gender[order(desc_wcount_by_gender$Gender),]
    view(desc_wcount_by_gender)
    
#WCount per gender & season   
    desc_wcount_by_gender_season<-lineTable_gender_mf %>% 
      group_by(Gender, Season) %>%
      summarize(Mean = mean(WCount, na.rm=TRUE))
    desc_wcount_by_gender_season<-desc_wcount_by_gender_season[order(desc_wcount_by_gender_season$Gender, desc_wcount_by_gender_season$Season),]
    view(desc_wcount_by_gender_season)
    
    interaction.plot(x.factor = desc_wcount_by_gender_season$Season, trace.factor = desc_wcount_by_gender_season$Gender, 
                     response = desc_wcount_by_gender_season$Mean, 
                     type = "b", legend = TRUE, ylim = range(1:13, na.rm = TRUE),
                     xlab = "Season", ylab="Line Frequency", main="lines per gender & season",
                     pch=c(1,19), col = c("#00AFBB", "#E7B800"))
    
#WCount per gender_From
    #from gender
    desc_questions_by_gender_from<-dialogTable_gender_mf %>% 
      group_by(Gender_From, Question) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_questions_by_gender_from<-desc_questions_by_gender_from[order(desc_questions_by_gender_from$Gender_From),]
    view(desc_questions_by_gender_from)
    
    #to gender
    desc_questions_by_gender_to<-dialogTable_gender_role %>% 
      group_by(Gender_To, Question) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_questions_by_gender_to<-desc_questions_by_gender_to[order(desc_questions_by_gender_to$Gender_To),]
    view(desc_questions_by_gender_to)
    
    
    
    #from/to gender
    desc_questions_by_gender_both<-dialogTable_gender_mf %>% 
      group_by(Gender_From, Gender_To, Question) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_questions_by_gender_both<-desc_questions_by_gender_both[order(desc_questions_by_gender_both$Gender_From, desc_questions_by_gender_both$Gender_To),]
    view(desc_questions_by_gender_both)
    
    #todo: qui-Square - Aussagekräftig?
    chisq.test(dialogTable_gender_mf$Gender_From, dialogTable_gender_mf$Question)
    chisq.test(dialogTable_gender_mf$Gender_To, dialogTable_gender_mf$Question)
    
    
    # Create the barplot - todo
    ggplot(data=df_cumsum, aes(x=dose, y=len, fill=supp)) +
      geom_bar(stat="identity")+
      geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
                color="white", size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()
    
#-------Descriptives Martin-END----------------------------------
