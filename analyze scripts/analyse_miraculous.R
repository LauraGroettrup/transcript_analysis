##zielverzeichnis muss beinhalten: processed, plots, dialogtable
source("./main.R")
source("./metadata scripts/metainfo_series_miraculous_Martin.R")
source("./analyze scripts/sentimentAI - initiate_v3.R")

process_transcript<-function(filepath){
  #filepath<-"./data/miraculous/processed/Oni-Chan.txt"
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  characterName <- strsplit(transcript_lines, ":::")

  #### DIAGRAMS ####
  script <- matrix(unlist(characterName), ncol = 2, nrow=length(characterName), byrow = T) # file format suitable for sentiment analysis and graph
  sentimentMiraculous <- sentiment_score(script[, 2])
  sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = mean)
  #sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = length)
  
  characters <- script[, 1]
  characters <- str_trim(characters) #new
  
  nNodes <- table(characters)
  pairs <- matrix(NA, ncol = 2, nrow = length(characters))
  pairs[, 1] <- characters[1:length(characters)]
  pairs[, 2] <- characters[c(2:(length(characters)),1)]
  ep_edges <- as.character(str_split(unlist(pairs), ","))
  
  ep_sociogram_igraph<-graph_from_adjacency_matrix(table(pairs[, 1], pairs[, 2]), weighted=TRUE) 
  
  ### SentimentTable: Name | Sentiment
  write.table(sentimentProCharacter,"./data/miraculous/tables/sentiment.csv", row.names = F, append = T, col.names = F, sep = "|")
  
  ### DialogTable for all eps: From | To | Sentiment | Text
  dialogTable <- data.frame(pairs[, 1], pairs[, 2], sentimentMiraculous[1:(length(sentimentMiraculous))], script[, 2][1:(length(script[, 2]))])
  write.table(dialogTable,"./data/miraculous/tables/dialogs.csv", row.names = F, append = T, col.names = F, sep = "|")
  # For each ep
  # write.table(dialogTable, paste("./data/miraculous/tables/",title_intermediate,".csv"), row.names = F, col.names = F, sep = "|")
  
  ###EpisodeTable: ep_no | season | ep_per_season | air_date | ep_edge_density_value | ep_reciprocity_value | ep_diameter_value
  ep_df_values <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(ep_df_values) <- data.frame("no", "season", "ep_per_season", "air_date", "ep_edge_density_value", "ep_reciprocity_value", "ep_diameter_value")
  ep_title <- gsub(".txt", "", filepath)
  ep_title <- gsub("./data/miraculous/processed/", "", ep_title)
  subset <- season_ep_list[season_ep_list$title %like% ep_title, ] 
  ep_no <- paste(subset[1, 1])
  ep_per_season<-paste(subset[1, 2])
  air_date<-paste(subset[1, 4])
  season<-paste(subset[1, 5])
  ep_edge_density_value<-edge_density(ep_sociogram_igraph) #Anzahl an Verbindungen im Verhältnis zu Anzahl aller möglichen Verbindungen; The density of a graph is the ratio of the number of edges and the number of possible edges.
  ep_reciprocity_value<-reciprocity(ep_sociogram_igraph) #Aussage wird getätigt, Antwort an diese Person auf Episodenebene
  ep_diameter_value<-diameter(ep_sociogram_igraph, directed=T)
  ep_df_values <- cbind(ep_no, season, ep_per_season, air_date, ep_edge_density_value, ep_reciprocity_value, ep_diameter_value)
  write.table(ep_df_values,"./data/miraculous/tables/episodes.csv", row.names = F, append = T, col.names = F, sep = "|")
  
  return(characters)
  
}

plotOverall <- function(dialogTable){
  pairs <- dialogTable[,c("From","To")]
  myEdges <- as.character(str_split(unlist(pairs), ","))
  sociogram_intermediate <- graph(c(myEdges))
  ep_sociogram_plot<-assign("Miraculous_sociagram",sociogram_intermediate)
  pdf(file="./data/miraculous/plots/Miracolous.pdf")
  plot(ep_sociogram_plot, main = "plot: sociogram", sub = "Miracolous_sociogram")
}

# Coming soon
files <- list.files(path='./data/miraculous/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
allCharacters <- vector(mode = "list", length = length(files))
i <- 0
for (file in files){
  characters <- process_transcript(file)
  i <- i+1
  allCharacters[[i]] <- unique(characters)
}
subset(table(unlist(allCharacters)), table(unlist(allCharacters))>20)
dialogTable <- read.csv("./data/miraculous/tables/dialogs.csv", sep="|")
episodeTable <- read.csv("./data/miraculous/tables/episodes.csv", sep="|")
sentimentTable <- read.csv("./data/miraculous/tables/sentiment.csv", sep="|")
colnames(dialogTable) <- c("From", "To", "Sentiment", "Text")
colnames(episodeTable) <- c("no", "season", "ep_per_season", "air_date", "ep_edge_density_value", "ep_reciprocity_value", "ep_diameter_value")
colnames(sentimentTable) <- c("Name", "Sentiment")

#cleanup
#rm(characterName, ep_sociogram_plot, filetext, pairs, script, sentimentProCharacter, sociogram_intermediate, characters, ep_sentiment_plot, file, file_intermediate, filepath, files, myEdges, nNodes, sentimentMiraculous, title_intermediate, transcript_lines)