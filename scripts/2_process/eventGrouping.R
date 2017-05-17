# Group events into similar geographic and temporal categories
# new column added = "eventGroup"

library(dplyr)

eventGroups <- function(cached.path, base.name, cached.save){
  
  summaryDF <- readRDS(file.path(cached.path,"8_process_new_categories","rds",paste0("summary",base.name,".rds")))
  groupData <- read.csv(file.path(cached.path,"8_process_new_categories","eventFreqAndDatesWGroups.csv"),stringsAsFactors = FALSE)
  
  eventGroups <- groupData[,"EventGroup"]
  names(eventGroups) <- groupData[,"eventNum"]
  summaryDF$eventGroup <- eventGroups[summaryDF$eventNum]
  
  cr <- which(names(summaryDF)=="sources2")
  summaryDF <- summaryDF[,c(1:cr,ncol(summaryDF),(cr+1):(ncol(summaryDF)-1))]
  
  # dir.create(file.path(cached.path,cached.save), showWarnings = FALSE)
  # dir.create(file.path(cached.path,cached.save,"rds"), showWarnings = FALSE)
  
  saveRDS(summaryDF,file=file.path(cached.path,cached.save,"rds",paste0("summary",base.name,".rds")))
  write.csv(summaryDF,file=file.path(cached.path,cached.save,paste0("summary",base.name,".csv")),row.names=FALSE)
  
}

cached.path <- "cached_data"
base.name <- "_noWW_noQA"
cached.save <- "8_process_new_categories"
eventGroups(cached.path, base.name, cached.save)
base.name <- "_noWW_noQA"
