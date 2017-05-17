library(dplyr)

cached.path <- "cached_data"
base.name <- "_noQA"
cached.save <- "8_process_new_categories"
summaryDF <- readRDS(file.path(cached.path,"8_process_new_categories","rds",paste0("summary",base.name,".rds")))
df.orig <- summaryDF

meanDates <- df.orig %>% group_by(eventNum) %>%
  summarise(eventDate = min(pdate),
            hydroCondition = unique(hydroCondition)[2])

dfEvents <- cbind(meanDates,table(df.orig$eventNum))

eventHydroCond <- character()
for(i in 1:dim(dfEvents)[1]){
  HC <- df.orig[which(df.orig$eventNum==dfEvents[i,"eventNum"]),"hydroCondition"]
  eventHydroCond <- c(eventHydroCond,ifelse(sum(HC == "Event")/length(HC)>0.5,"Event","Base"))
}
dfEvents <- cbind(dfEvents,eventHydroCond)
  
dfEvents$State <- substr(dfEvents$eventNum,1,2)
dfEvents <- arrange(dfEvents,State,eventDate)

write.csv(dfEvents,file="./cached_data/8_process_new_categories/eventFreqAndDates.csv")


plot(dfEvents$Freq~dfEvents$eventDate)
