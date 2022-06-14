# CALCULATIONs IX - H2.2 Calculations (number of words spoken)
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre I executed
# - calculations_miraculous_pre II executed
# - desc-calculations_miraculous_descriptives executed

# H2.2 Calculations (number of words spoken)
source("./main.R")
#---------------------------------------------------------------------------------------------
#H2.2 Number of words spoken (lines per gender)
#---------------------------------------------------------------------------------------------
#WCount per gender
      #http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
      hist(lineTable_gender_mf$WCount, col='steelblue', main='histogram of (all) Sentiment.Ai Scores')
      plot_WCoung_g<-ggplot(lineTable_gender_mf, aes(x=WCount, fill=Gender, color=Gender)) + geom_histogram(binwidth=7, alpha=0.5, position="dogle")+theme(legend.position="right")      
      plot_WCoung_g<-plot_WCoung_g+scale_color_brewer(palette="Dark2")
      plot_WCoung_g<-plot_WCoung_g+geom_vline(data=mu, aes(xintercept=grp.mean, color=Gender),
                 linetype="dashed")
      plot_WCoung_g
      
      desc_wcount_by_gender<-lineTable_gender_mf %>% 
        group_by(Gender) %>%
        summarize(Mean = mean(WCount, na.rm=TRUE))
      desc_wcount_by_gender<-desc_wcount_by_gender[order(desc_wcount_by_gender$Gender),]
      view(desc_wcount_by_gender)

     #t-test
      describeBy(lineTable_gender_mf$WCount, lineTable_gender_mf$Gender)
      t.test(lineTable_gender_mf$WCount~lineTable_gender_mf$Gender, var.equal=T)
      cohensD(lineTable_gender_mf$WCount~lineTable_gender_mf$Gender) 
      #t-test not sign.
#---------------------------------------------------------------------------------------------
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

#---------------------------------------------------------------------------------------------    
      # #WCount Outliers - not necessary - all relevant
      #             describeBy(lineTable_gender_mf$WCount, lineTable_gender_mf$Gender)
      #             
      #             lineTable_gender_mf %>%
      #               identify_outliers("WCount")
      #             
      #         #outliers by groups
      #             lineTable_gender_mf %>% 
      #               group_by(Gender) %>%
      #               identify_outliers("WCount")
      #             
      #           #outliers by groups - visual
      #             hist(lineTable_gender_mf$WCount,
      #                  xlab = "WCount",
      #                  main = "Histogram of WCount",
      #                  breaks = sqrt(nrow(lineTable_gender_mf)))
      #             
      #             ggplot(lineTable_gender_mf) +
      #               aes(x = WCount) +
      #               geom_histogram(bins = 30L, fill = "#0c4c8a") +
      #               theme_minimal()
      #             
      #             ggbetweenstats(lineTable_gender_mf,
      #                            Gender, WCount, outlier.tagging = TRUE)
      #             
      #             #boxplot(lineTable_gender_role$WCount,ylab = "WCount")
      #             
      #             #boxplot.stats(lineTable_gender_role$WCount)$out
#-------H2.2 Calculations-END----------------------------------

