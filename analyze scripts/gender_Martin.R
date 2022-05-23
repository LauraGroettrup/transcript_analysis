# GENDER

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed

source("./main.R")

#dataframe of all characters occuring in episodeTable$Betweenness 1 to 5
    character_betweenness<-append(episodeTable$Betweenness_1,episodeTable$Betweenness_2)
    character_betweenness<-append(character_betweenness,episodeTable$Betweenness_3)
    character_betweenness<-append(character_betweenness,episodeTable$Betweenness_4)
    character_betweenness<-append(character_betweenness,episodeTable$Betweenness_5)
    character_betweenness<-data.frame(character_betweenness)
    character_betweenness <-character_betweenness %>%
      group_by(character_betweenness) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    character_betweenness$Gender <- NA
    colnames(character_betweenness) <- data.frame("Characters with 1-5 ranked Betweenness in episodes", "Frequency", "Gender")
    write.table(character_betweenness,"./data/miraculous/tables/gender_betweeness.csv", row.names = F, append = F, col.names = T, sep = "|")

    
#dataframe of all characters occuring in episodeTable$Eigenvector 1 to 5
    character_eigenvector<-append(episodeTable$Eigenvector_1,episodeTable$Eigenvector_2)
    character_eigenvector<-append(character_eigenvector,episodeTable$Eigenvector_3)
    character_eigenvector<-append(character_eigenvector,episodeTable$Eigenvector_4)
    character_eigenvector<-append(character_eigenvector,episodeTable$Eigenvector_5)
    character_eigenvector<-data.frame(character_eigenvector)
    character_eigenvector <-character_eigenvector %>%
      group_by(character_eigenvector) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    character_eigenvector$Gender <- NA
    colnames(character_eigenvector) <- data.frame("Characters with 1-5 ranked Eigenvector in episodes", "Frequency", "Gender")
    write.table(character_eigenvector,"./data/miraculous/tables/gender_eigenvector.csv", row.names = F, append = F, col.names = T, sep = "|")
    
    
print("<End of Script>")
#------------------------------------------------------------------------