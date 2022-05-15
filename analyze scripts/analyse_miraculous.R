##zielverzeichnis muss beinhalten: processed, plots, dialogtable
source("./main.R")
source("./analyze scripts/sentimentAI - initiate.R")

process_transcript<-function(filepath){
  #filepath<-file
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  characterName <- strsplit(transcript_lines, ":::")
  file_intermediate<-strsplit(file, "/")[[1]]
  title_intermediate<-file_intermediate[[5]]
  series<-file_intermediate[[3]]

  #### DIAGRAMS ####
  str(characterName)
  script <- matrix(unlist(characterName), ncol = 2, nrow=length(characterName), byrow = T) # file format suitable for sentiment analysis and graph
  sentimentMiraculous <- sentiment_score(script[, 2])
  sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = mean)
  sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = length)
  
  characters <- script[, 1]
  
  nNodes <- table(characters)
  pairs <- matrix(NA, ncol = 2, nrow = length(characters)-1)
  pairs[, 1] <- characters[1:(length(characters)-1)]
  pairs[, 2] <- characters[2:length(characters)]
  myEdges <- as.character(str_split(unlist(pairs), ","))
  
  table(pairs[, 1], pairs[, 2])
  
  # DialogTable for all eps: From | To | Sentiment | Text
  dialogTable <- data.frame(pairs[, 1], pairs[, 2], sentimentMiraculous[1:(length(sentimentMiraculous)-1)], script[, 2][1:(length(script[, 2])-1)])
  write.table(dialogTable,"./data/miraculous/tables/dialogs.csv", row.names = F, append = T, col.names = F, sep = "|")
  # For each ep
  # write.table(dialogTable, paste("./data/miraculous/tables/",title_intermediate,".csv"), row.names = F, col.names = F, sep = "|")
  
  #SociogramPlot: format: series_titel.txt_sociogram
  sociogram_intermediate <- graph(c(myEdges))
  ep_sociogram_plot<-assign(paste(series, title_intermediate,"sociogram", sep="_"),sociogram_intermediate)
  plot(ep_sociogram_plot, main = "plot: sociogram", sub = paste(series, title_intermediate,"sociogram", sep="_"))

  #SentimentPlot (sentiment scores/lines): format: series_titel.txt_sentiment
  ep_sentiment_plot<-assign(paste(series, title_intermediate,"sentiment", sep="_"),sentimentMiraculous)
  ts.plot(ep_sentiment_plot, main = "plot: sentiment score per lines (time)", sub = paste(series, title_intermediate,"sentiment", sep="_"), xlab="line", ylab="sentiment score")
  #ts.plot(sentimentMiraculous, main = file)s
  
  #write to pdf
  pdf(file=paste("./data/miraculous/plots/", series, "_", title_intermediate, ".pdf", sep=""))
  # 2. Create the plot
  plot(ep_sociogram_plot, main = "plot: sociogram", sub = paste(series, title_intermediate,"sociogram", sep="_"))
  ts.plot(ep_sentiment_plot, main = "plot: sentiment score per lines (time)", sub = paste(series, title_intermediate,"sentiment", sep="_"), xlab="line", ylab="sentiment score")
  # 3. Close the file
  dev.off()
  #end write
  return(assign(paste(series, title_intermediate,"sociogram", sep="_"),sociogram_intermediate))
}

plotOverall <- funtion(dialogTable){
  pairs <- dialogTable[,c("From","To")]
  myEdges <- as.character(str_split(unlist(pairs), ","))
  sociogram_intermediate <- graph(c(myEdges))
  ep_sociogram_plot<-assign("Miraculous_sociagram",sociogram_intermediate)
  pdf(file="./data/miraculous/plots/Miracolous.pdf")
  plot(ep_sociogram_plot, main = "plot: sociogram", sub = "Miracolous_sociogram")
  dev.off()
}

# Coming soon
files <- list.files(path='./data/miraculous/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  process_transcript(file)
}
dialogTable <- read.csv("./data/miraculous/tables/dialogs.csv", sep="|")
colnames(dialogTable) <- c("From", "To", "Sentiment", "Text")
plotOverall (dialogTable)

#cleanup
#rm(characterName, ep_sociogram_plot, filetext, pairs, script, sentimentProCharacter, sociogram_intermediate, characters, ep_sentiment_plot, file, file_intermediate, filepath, files, myEdges, nNodes, sentimentMiraculous, title_intermediate, transcript_lines)