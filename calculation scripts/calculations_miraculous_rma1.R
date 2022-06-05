# CALCULATIONs 5

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed

source("./main.R")

#descriptives

#Sentiment
    attach(character_seasons_sentiment_time)
    par(mfrow=c(1,4))
    hist(s1_mean_sentiment,prob=T)
    hist(s2_mean_sentiment, prob=T)
    hist(s3_mean_sentiment,prob=T)
    hist(s4_mean_sentiment,prob=T)

    #transponieren
    character_seasons_sentiment_time_B<-matrix(nrow=2760,ncol=4,c(rep(1:690,4), rep(character_seasons_sentiment_time$Character,4),character_seasons_sentiment_time$s1_mean_sentiment, character_seasons_sentiment_time$s2_mean_sentiment,character_seasons_sentiment_time$s3_mean_sentiment,character_seasons_sentiment_time$s4_mean_sentiment,rep(1,690),rep(2,690),rep(3,690),rep(4,690)))
    character_seasons_sentiment_time_B<-data.frame(character_seasons_sentiment_time_B)
    colnames(character_seasons_sentiment_time_B)<-c("ID", "Character", "Mean_Sentiment", "Season")
    character_seasons_sentiment_time_B$Season<-factor(character_seasons_sentiment_time_B$Season)
    character_seasons_sentiment_time_B$Mean_Sentiment <- as.numeric(character_seasons_sentiment_time_B$Mean_Sentiment)
    attach(character_seasons_sentiment_time_B)
  
    #problem: NAs
    #only work with compete cases 
    character_seasons_sentiment_time_B<-character_seasons_sentiment_time_B[complete.cases(character_seasons_sentiment_time_B), ]
    #problem - still missing data when running rmANOVA - todo
    
    #run rmANOVA (rma1)
    rma1<-ezANOVA(data=character_seasons_sentiment_time_B,dv=.(Mean_Sentiment),wid=.(ID),within=.(Season),type=3)