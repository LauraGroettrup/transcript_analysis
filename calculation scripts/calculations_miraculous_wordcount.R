# CALCULATIONs PRE
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed

# Word Count

source("./main.R")
#-----------------------------------------
#linetable_gender_role
  for(i in 1:nrow(lineTable_gender_role)) { 
    #i=27
  lineTable_gender_role$id<-1:nrow(lineTable_gender_role)
  linetext<-subset(lineTable_gender_role, id == i, select = c(Text))
  linelenght<-length(strsplit(as.character(linetext), " ")[[1]])-1
  lineTable_gender_role$wlength[i] = linelenght
  } 

#dialogTable_gender_role
for(i in 1:nrow(dialogTable_gender_role)) { 
  #i=27
  #dialogTable_gender_role$id<-1:nrow(dialogTable_gender_role)
  linetext<-subset(dialogTable_gender_role, id == i, select = c(Text))
  linelenght<-length(strsplit(as.character(linetext), " ")[[1]])-1
  dialogTable_gender_role$wlength[i] = linelenght
} 

#---aufräumen
  rm(linelenght, linetext, i)

#----END OF WORD COUNT ------------------------
  
