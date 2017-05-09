library(dplyr)

meanDates2 <- df.orig %>% group_by(eventNum) %>%
  summarise(eventDate = min(pdate),
            hydroCondition = unique(hydroCondition)[2])
cbind(meanDates$hydroCondition,meanDates2$hydroCondition)

dfEvents <- cbind(meanDates,table(df.orig$eventNum))

eventHydroCond <- character()
for(i in 1:dim(dfEvents)[1]){
  HC <- df.orig[which(df.orig$eventNum==dfEvents[i,"eventNum"]),"hydroCondition"]
  eventHydroCond <- c(eventHydroCond,ifelse(sum(HC == "Event")/length(HC)>0.5,"Event","Base"))
}
dfEvents <- cbind(dfEvents,eventHydroCond)
  
dfEvents$State <- substr(dfEvents$eventNum,1,2)
dfEvents <- arrange(dfEvents,State,eventDate)

write.csv(dfEvents,file="eventFreqAndDates.csv")


plot(dfEvents$Freq~dfEvents$eventDate)
