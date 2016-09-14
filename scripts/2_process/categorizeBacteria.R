library(dplyr)

meta.file <- function(df, base.name){
  types <- lapply(df,class)
  types <- unlist(types)
  metaData <- data.frame(variable = names(types),
                         dataType = types, stringsAsFactors = FALSE)
  metaData$description <- ""
  write.csv(metaData, row.names = FALSE, file = file.path(cached.path,"final","optic_summary",paste0("meta",base.name,".csv")))
}

categorizeBacteria <- function(cached.path, base.name){
  
  dfall <- readRDS(file.path(cached.path,"final","rds",paste0("summary",base.name,".rds")))
  bacteria <- dfall[,c("CAGRnumber","bacHum", "lachno")]
  bacteria$bacHum_cat <- as.numeric(cut(x = bacteria$bacHum, breaks = c(225,1000, 10000, 100000, 1000000, Inf), right = FALSE))
  bacteria$lachno_cat <- as.numeric(cut(x = bacteria$lachno, breaks = c(225,1000, 10000, 100000, 1000000, Inf), right = FALSE))
  bacteria$sum_cat <- cut(x = bacteria$lachno + bacteria$bacHum, breaks = c(450,2000, 20000, 200000, 2000000, Inf), right = FALSE)
  bacteria$same <- bacteria$bacHum_cat == bacteria$lachno_cat
  # bacteria$difference <- bacteria$bacHum - bacteria$lachno
  # bacteria$geom.mean <- sqrt(bacteria$bacHum*bacteria$lachno)
  # bacteria$geom.mean_cat <- cut(x = bacteria$geom.mean, breaks = c(0,225,1000, 10000, 100000, Inf), right = FALSE)
  # bacteria$diff.over.mean <- (bacteria$bacHum - bacteria$lachno)/sqrt(bacteria$bacHum*bacteria$lachno)

  bacteria$how.close <- abs(bacteria$bacHum_cat - bacteria$lachno_cat)
  bacteria <- bacteria[!is.na(bacteria$bacHum),]
  
  rightLimits <- c(10^3,10^4,10^5,10^6,10^7)
  leftLimits <- c(225,10^3,10^4,10^5,10^6)
  
  bacteria$nearRightHum <- (rightLimits[as.numeric(bacteria$bacHum_cat)] - bacteria$bacHum)/rightLimits[as.numeric(bacteria$bacHum_cat)]
  bacteria$nearRightLac <- (rightLimits[as.numeric(bacteria$lachno_cat)] - bacteria$lachno)/rightLimits[as.numeric(bacteria$lachno_cat)]
  bacteria$nearLeftHum <- (bacteria$bacHum - leftLimits[as.numeric(bacteria$bacHum_cat)])/leftLimits[as.numeric(bacteria$bacHum_cat)]
  bacteria$nearLeftLac <- (bacteria$lachno - leftLimits[as.numeric(bacteria$lachno_cat)])/leftLimits[as.numeric(bacteria$lachno_cat)]

  bacteria$comboCat <- NA
  
  bacteria$comboCat[bacteria$how.close == 0] <- as.character(bacteria$bacHum_cat[bacteria$how.close == 0])
  #switch to: if one is within 15% of the boarder... 
  bacteria$comboCat[bacteria$how.close == 1] <- ifelse(bacteria$bacHum[bacteria$how.close == 1] < bacteria$lachno[bacteria$how.close == 1],
                                                       ifelse(bacteria$nearRightHum[bacteria$how.close == 1] < 0.2 & bacteria$nearLeftLac[bacteria$how.close == 1] < 0.2,
                                                              as.character(bacteria$lachno_cat[bacteria$how.close == 1]), 
                                                              ifelse(bacteria$nearRightHum[bacteria$how.close == 1] < 0.2,
                                                                     as.character(bacteria$lachno_cat[bacteria$how.close == 1]),
                                                                     "MediumLachno")),
                                                       ifelse(bacteria$nearRightLac[bacteria$how.close == 1] < 0.2 & bacteria$nearLeftHum[bacteria$how.close == 1] < 0.2,
                                                              as.character(bacteria$bacHum_cat[bacteria$how.close == 1]),
                                                              ifelse(bacteria$nearRightLac[bacteria$how.close == 1] < 0.2,
                                                                     as.character(bacteria$bacHum_cat[bacteria$how.close == 1]),
                                                                     "MediumBact")))
  bacteria$comboCat[bacteria$how.close == 1 & 
                    bacteria$bacHum_cat  %in% c(4,5) & 
                    bacteria$lachno_cat  %in% c(4,5)] <- "5"
  
  bacteria$comboCat[bacteria$comboCat == "MediumLachno" & bacteria$lachno_cat == "4"] <- "MediumLachno,high"
  bacteria$comboCat[bacteria$comboCat == "MediumLachno" & bacteria$lachno_cat == "3"] <- "MediumLachno,medium"
  bacteria$comboCat[bacteria$comboCat == "MediumLachno" & bacteria$lachno_cat == "2"] <- "MediumLachno,low"
  
  bacteria$comboCat[bacteria$comboCat == "MediumBact" & bacteria$bacHum_cat == "4"] <- "MediumBact,high"
  bacteria$comboCat[bacteria$comboCat == "MediumBact" & bacteria$bacHum_cat == "3"] <- "MediumBact,medium"
  bacteria$comboCat[bacteria$comboCat == "MediumBact" & bacteria$bacHum_cat == "2"] <- "MediumBact,low"
  
  
  bacteria$comboCat[bacteria$how.close == 2] <- ifelse(bacteria$bacHum[bacteria$how.close == 2] > bacteria$lachno[bacteria$how.close == 2], 
                                                       "HigherBact",
                                                       "HigherLachno")
  
  bacteria$comboCat[bacteria$comboCat == "HigherLachno" & bacteria$lachno_cat %in% c("4","5")] <- "HigherLachno,high"
  bacteria$comboCat[bacteria$comboCat == "HigherLachno" & bacteria$lachno_cat == "3"] <- "HigherLachno,medium"
  bacteria$comboCat[bacteria$comboCat == "HigherBact" & bacteria$bacHum_cat %in% c("4","5")] <- "HigherBact,high"
  bacteria$comboCat[bacteria$comboCat == "HigherBact" & bacteria$bacHum_cat == "3"] <- "HigherBact,medium"
  
  bacteria$comboCat[bacteria$how.close == 3] <- ifelse(bacteria$bacHum[bacteria$how.close == 3] > bacteria$lachno[bacteria$how.close == 3], 
                                                       "MuchHigherBact",
                                                       "MuchHigherLachno")
  
# Original:
  # bacteria <- bacteria %>%
  #   mutate(bact.cat = ifelse(same,
  #                             as.character(bacHum_cat),
  #                             ifelse(abs(diff.over.mean) <= 10,
  #                                    ifelse(bacHum > lachno,
  #                                           as.character(bacHum_cat),
  #                                           as.character(lachno_cat)),
  #                                    ifelse(bacHum > lachno,
  #                                           "HighBac,LowLachno",
  #                                           "HighLachno,LowBact"))))
  # bacteria$bact.cat <-factor(bacteria$bact.cat, levels = c("<225","225-1000","10^3-10^4","10^4-10^5",
  #                                                          ">10^5","HighBac,LowLachno",
  #                                                          "HighLachno,LowBact"))
  
  summaryDF <- bacteria %>%
    select(CAGRnumber, bacHum, lachno, bacHum_cat, lachno_cat, comboCat, sum_cat)
  
  summaryDF$comboCat[summaryDF$comboCat == "1"] <- "VeryLow"
  summaryDF$comboCat[summaryDF$comboCat == "2"] <- "Low"
  summaryDF$comboCat[summaryDF$comboCat == "3"] <- "Medium"
  summaryDF$comboCat[summaryDF$comboCat == "4"] <- "High"
  summaryDF$comboCat[summaryDF$comboCat == "5"] <- "Highest"
  
  summaryDF$comboCat <- factor(summaryDF$comboCat, levels = c("VeryLow","Low","MediumBact,low","MediumLachno,low",
                                                              "Medium","MediumBact,medium","MediumLachno,medium", "HigherLachno,medium",
                                                              "High", "HigherLachno", "HigherBact", "HigherLachno,high","MuchHigherBact","MuchHigherLachno","Highest"))

  summaryDF$sum_cat <- as.character(summaryDF$sum_cat)
  summaryDF$sum_cat[summaryDF$sum_cat == "[450,2e+03)"] <- "VeryLow"
  summaryDF$sum_cat[summaryDF$sum_cat == "[2e+03,2e+04)"] <- "Low"
  summaryDF$sum_cat[summaryDF$sum_cat == "[2e+04,2e+05)"] <- "Medium"
  summaryDF$sum_cat[summaryDF$sum_cat == "[2e+05,2e+06)"] <- "High"
  summaryDF$sum_cat[summaryDF$sum_cat == "[2e+06,Inf)"] <- "Highest"
  
  summaryDF$sum_cat <- factor(summaryDF$sum_cat, levels = c("VeryLow","Low","Medium","High","Highest"))
  
  for(i in c("lachno_cat","bacHum_cat")){
    summaryDF[,i] <- as.character(summaryDF[[i]])
    summaryDF[summaryDF[[i]] == "1",i] <- "VeryLow"
    summaryDF[summaryDF[[i]] == "2",i] <- "Low"
    summaryDF[summaryDF[[i]] == "3",i] <- "Medium"
    summaryDF[summaryDF[[i]] == "4",i] <- "High"
    summaryDF[summaryDF[[i]] == "5",i] <- "Highest"
    
    summaryDF[,i] <- factor(summaryDF[,i], levels = c("VeryLow","Low","Medium","High","Highest"))
    
  }
  
  dfall <- left_join(dfall, summaryDF,
                     by=c("CAGRnumber","bacHum","lachno")) %>%
    rename(bacteria_cat = comboCat)

  meta.file(dfall, base.name)
  saveRDS(dfall,file=file.path(cached.path,"final","rds",paste0("summary",base.name,'.rds')))
  write.csv(dfall,file=file.path(cached.path,"final",paste0("summary",base.name,".csv")),row.names=FALSE)
}


cached.path <- "cached_data"
base.name <- "_noQA"
categorizeBacteria(cached.path, base.name)
base.name <- "_noWW_noQA"
categorizeBacteria(cached.path, base.name)
base.name <- "_QA"
categorizeBacteria(cached.path, base.name)
