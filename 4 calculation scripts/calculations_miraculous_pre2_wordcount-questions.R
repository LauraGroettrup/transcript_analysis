# CALCULATIONs PRE II
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre1 executed

# Word Count & Questions

source("./main.R")
#-----------------------------------------
#linetable_gender_role
  for(i in 1:nrow(lineTable_gender_role)) { 
    #i=27
  lineTable_gender_role$ID<-1:nrow(lineTable_gender_role)
  linetext<-subset(lineTable_gender_role, ID == i, select = c(Text))
  linelenght<-length(strsplit(as.character(linetext), " ")[[1]])-1
  lineTable_gender_role$WCount[i] = linelenght
  } 

#dialogTable_gender_role
for(i in 1:nrow(dialogTable_gender_role)) { 
  #i=27
  #dialogTable_gender_role$ID<-1:nrow(dialogTable_gender_role)
  linetext<-subset(dialogTable_gender_role, ID == i, select = c(Text))
  linelenght<-length(strsplit(as.character(linetext), " ")[[1]])-1
  dialogTable_gender_role$WCount[i] = linelenght
} 
#---Questions

questionLines <- lineTable_gender_role[which(grepl('\\?$',lineTable$Text)),]
table(questionLines$Gender)
view(questionLines)

#---aufräumen
  rm(linelenght, linetext, i)

#----END OF WORD COUNT ------------------------
  
