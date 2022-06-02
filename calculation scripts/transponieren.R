#transforming sentimentTable to character_sentiment_time
source("./main.R")
rm(character_sentiment_time, character_vader_time)

ep_no_list<-unique(sentimentTable$Episode_Overall) #alle vorhandenen ep nummern holen
#colnames(characters_ep)<-c("Character", paste("Sentiment_Ep_",i), paste("Vader_Ep_",i))

character_sentiment_time<-characters[1]
character_sentiment_time<-data.frame(character_sentiment_time)
colnames(character_sentiment_time)<-c("Character")

character_vader_time<-characters[1]
character_vader_time<-data.frame(character_vader_time)
colnames(character_vader_time)<-c("Character")

#loop 
for(i in ep_no_list) {  
  #sentiment
  characters_ep_sentiment<-subset(sentimentTable, Episode_Overall == i, select = c(Character, Sentiment))
  colnames(characters_ep_sentiment)<-c("Character", paste("Sentiment_Ep_",i))
  character_sentiment_time <- merge(character_sentiment_time, characters_ep_sentiment,by = 'Character', all=TRUE)
  #vader
  characters_ep_vader<-subset(sentimentTable, Episode_Overall == i, select = c(Character, Vader))
  colnames(characters_ep_vader)<-c("Character", paste("Vader_Ep_",i))
  character_vader_time <- merge(character_vader_time, characters_ep_vader,by = 'Character', all=TRUE)
  }

#join gender and role 
character_sentiment_time<-character_sentiment_time %>%left_join(character_lookup, by='Character')
character_sentiment_time <-character_sentiment_time[order(character_sentiment_time$Character),]

character_vader_time<-character_vader_time %>%left_join(character_lookup, by='Character')
character_vader_time <-character_vader_time[order(character_vader_time$Character),]

view(character_sentiment_time)
length(character_sentiment_time)
view(character_vader_time)
length(character_vader_time)

#---aufräumen
rm(ep_no_list, i, characters_ep_sentiment, characters_ep_vader)
#---------End Transformieren--------
