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
  bacteria$bacHum_cat <- as.numeric(cut(x = bacteria$bacHum, breaks = c(0, 225,1000, 10000, 100000, 1000000, Inf), right = TRUE))
  bacteria$lachno_cat <- as.numeric(cut(x = bacteria$lachno, breaks = c(0, 225,1000, 10000, 100000, 1000000, Inf), right = TRUE))
  bacteria$sum_cat <- cut(x = bacteria$lachno + bacteria$bacHum, breaks = c(0,450,2000, 20000, 200000, 2000000, Inf), right = TRUE)
  bacteria$same <- bacteria$bacHum_cat == bacteria$lachno_cat
  # bacteria$difference <- bacteria$bacHum - bacteria$lachno
  # bacteria$geom.mean <- sqrt(bacteria$bacHum*bacteria$lachno)
  # bacteria$geom.mean_cat <- cut(x = bacteria$geom.mean, breaks = c(0,225,1000, 10000, 100000, Inf), right = FALSE)
  # bacteria$diff.over.mean <- (bacteria$bacHum - bacteria$lachno)/sqrt(bacteria$bacHum*bacteria$lachno)

  bacteria$how.close <- abs(bacteria$bacHum_cat - bacteria$lachno_cat)
  bacteria <- bacteria[!is.na(bacteria$bacHum),]
  
  rightLimits <- c(225,10^3,10^4,10^5,10^6,10^7)
  leftLimits <- c(0,225,10^3,10^4,10^5,10^6)
  
  bacteria$nearRightHum <- (rightLimits[as.numeric(bacteria$bacHum_cat)] - bacteria$bacHum)/rightLimits[as.numeric(bacteria$bacHum_cat)]
  bacteria$nearRightLac <- (rightLimits[as.numeric(bacteria$lachno_cat)] - bacteria$lachno)/rightLimits[as.numeric(bacteria$lachno_cat)]
  bacteria$nearLeftHum <- (bacteria$bacHum - leftLimits[as.numeric(bacteria$bacHum_cat)])/leftLimits[as.numeric(bacteria$bacHum_cat)]
  bacteria$nearLeftLac <- (bacteria$lachno - leftLimits[as.numeric(bacteria$lachno_cat)])/leftLimits[as.numeric(bacteria$lachno_cat)]

  bacteria$comboCat <- NA
  
  bacteria$comboCat[bacteria$how.close == 0] <- as.character(bacteria$bacHum_cat[bacteria$how.close == 0])
  #switch to: if one is within 15% of the boarder... 
  buffer.percent <- 0.2
  bacteria$comboCat[bacteria$how.close == 1] <- ifelse(bacteria$bacHum[bacteria$how.close == 1] < bacteria$lachno[bacteria$how.close == 1],
                                                       ifelse(bacteria$nearRightHum[bacteria$how.close == 1] < buffer.percent & bacteria$nearLeftLac[bacteria$how.close == 1] < buffer.percent,
                                                              as.character(bacteria$lachno_cat[bacteria$how.close == 1]), 
                                                              ifelse(bacteria$nearRightHum[bacteria$how.close == 1] < buffer.percent,
                                                                     as.character(bacteria$lachno_cat[bacteria$how.close == 1]),
                                                                     "MediumLachno")),
                                                       ifelse(bacteria$nearRightLac[bacteria$how.close == 1] < buffer.percent & bacteria$nearLeftHum[bacteria$how.close == 1] < buffer.percent,
                                                              as.character(bacteria$bacHum_cat[bacteria$how.close == 1]),
                                                              ifelse(bacteria$nearRightLac[bacteria$how.close == 1] < buffer.percent,
                                                                     as.character(bacteria$bacHum_cat[bacteria$how.close == 1]),
                                                                     "MediumBact")))
  
  bacteria$comboCat[bacteria$how.close == 2] <- ifelse(bacteria$bacHum[bacteria$how.close == 2] > bacteria$lachno[bacteria$how.close == 2], 
                                                       "HigherBact",
                                                       "HigherLachno")
  bacteria$comboCat[bacteria$how.close == 3] <- ifelse(bacteria$bacHum[bacteria$how.close == 3] > bacteria$lachno[bacteria$how.close == 3], 
                                                       "MuchHigherBact",
                                                       "MuchHigherLachno")
  
  # bacteria$comboCat[bacteria$how.close == 1 & 
  #                   bacteria$bacHum_cat  %in% c(5,6) & 
  #                   bacteria$lachno_cat  %in% c(5,6)] <- "6"

  bacteria$comboCat[bacteria$comboCat == "MediumLachno" & bacteria$lachno_cat == "6"] <- "MediumLachno,veryhigh"
  bacteria$comboCat[bacteria$comboCat == "MediumLachno" & bacteria$lachno_cat == "5"] <- "MediumLachno,high"
  bacteria$comboCat[bacteria$comboCat == "MediumLachno" & bacteria$lachno_cat == "4"] <- "MediumLachno,medium"
  bacteria$comboCat[bacteria$comboCat == "MediumLachno" & bacteria$lachno_cat == "3"] <- "MediumLachno,low"
  
  bacteria$comboCat[bacteria$comboCat == "MediumBact" & bacteria$bacHum_cat == "6"] <- "MediumBact,veryhigh"
  bacteria$comboCat[bacteria$comboCat == "MediumBact" & bacteria$bacHum_cat == "5"] <- "MediumBact,high"
  bacteria$comboCat[bacteria$comboCat == "MediumBact" & bacteria$bacHum_cat == "4"] <- "MediumBact,medium"
  bacteria$comboCat[bacteria$comboCat == "MediumBact" & bacteria$bacHum_cat == "3"] <- "MediumBact,low"

  bacteria$comboCat[bacteria$comboCat == "HigherLachno" & bacteria$lachno_cat %in% "6"] <- "HigherLachno,veryhigh"  
  bacteria$comboCat[bacteria$comboCat == "HigherLachno" & bacteria$lachno_cat %in% "5"] <- "HigherLachno,high"
  bacteria$comboCat[bacteria$comboCat == "HigherLachno" & bacteria$lachno_cat == "4"] <- "HigherLachno,medium"
  bacteria$comboCat[bacteria$comboCat == "HigherLachno" & bacteria$lachno_cat == "3"] <- "HigherLachno,low"

  bacteria$comboCat[bacteria$comboCat == "HigherBact" & bacteria$bacHum_cat %in% c("6")] <- "HigherBact,veryhigh"  
  bacteria$comboCat[bacteria$comboCat == "HigherBact" & bacteria$bacHum_cat %in% c("5")] <- "HigherBact,high"
  bacteria$comboCat[bacteria$comboCat == "HigherBact" & bacteria$bacHum_cat == "4"] <- "HigherBact,medium"
  bacteria$comboCat[bacteria$comboCat == "HigherBact" & bacteria$bacHum_cat == "3"] <- "HigherBact,low"
  
  bacteria$comboCat[bacteria$comboCat == "MuchHigherBact" & bacteria$bacHum_cat %in% "6"] <- "MuchHigherBact,veryhigh"
  bacteria$comboCat[bacteria$comboCat == "MuchHigherBact" & bacteria$bacHum_cat %in% "5"] <- "MuchHigherBact,high"
  bacteria$comboCat[bacteria$comboCat == "MuchHigherLachno" & bacteria$lachno_cat == "6"] <- "MuchHigherLachno,veryhigh"
  bacteria$comboCat[bacteria$comboCat == "MuchHigherLachno" & bacteria$lachno_cat == "5"] <- "MuchHigherLachno,medium"
  bacteria$comboCat[bacteria$comboCat == "MuchHigherLachno" & bacteria$lachno_cat == "4"] <- "MuchHigherLachno,low"
  
  summaryDF <- bacteria %>%
    select(CAGRnumber, bacHum, lachno, bacHum_cat, lachno_cat, comboCat, sum_cat)
  
  summaryDF$comboCat[summaryDF$comboCat == "1"] <- "ND"
  summaryDF$comboCat[summaryDF$comboCat == "2"] <- "VeryLow"
  summaryDF$comboCat[summaryDF$comboCat == "3"] <- "Low"
  summaryDF$comboCat[summaryDF$comboCat == "4"] <- "Medium"
  summaryDF$comboCat[summaryDF$comboCat == "5"] <- "High"
  summaryDF$comboCat[summaryDF$comboCat == "6"] <- "VeryHigh"
  
  
  orderData <- c("ND",
                  "VeryLow",
                  "HigherLachno,low",
                  "HigherBact,low",
                  "MediumBact,low",
                  "MediumLachno,low",
                  "MuchHigherLachno,low",
                  "Low",
                  "HigherLachno,medium",
                  "MediumBact,medium",
                  "MediumLachno,medium",
                  "MuchHigherLachno,medium",
                  "Medium",
                  "MuchHigherLachno,high",
                  "MuchHigherBact,high",
                  "HigherLachno,high",
                  "MediumLachno,high",
                  "MediumBact,high", 
                  "High",
                  "MuchHigherLachno,veryhigh",
                  "MuchHigherBact,veryhigh",
                  "HigherLachno,veryhigh",
                  "MediumLachno,veryhigh",
                  "MediumBact,veryhigh",
                  "VeryHigh")
  
  summaryDF$comboCat <- factor(summaryDF$comboCat, levels = orderData)

  summaryDF$sum_cat <- as.character(summaryDF$sum_cat)
  summaryDF$sum_cat[summaryDF$sum_cat == "(0,450]"] <- "ND"
  summaryDF$sum_cat[summaryDF$sum_cat == "(450,2e+03]"] <- "VeryLow"
  summaryDF$sum_cat[summaryDF$sum_cat == "(2e+03,2e+04]"] <- "Low"
  summaryDF$sum_cat[summaryDF$sum_cat == "(2e+04,2e+05]"] <- "Medium"
  summaryDF$sum_cat[summaryDF$sum_cat == "(2e+05,2e+06]"] <- "High"
  summaryDF$sum_cat[summaryDF$sum_cat == "(2e+06,Inf]"] <- "VeryHigh"
  
  summaryDF$sum_cat <- factor(summaryDF$sum_cat, levels = c("ND","VeryLow","Low","Medium","High","VeryHigh"))
  
  for(i in c("lachno_cat","bacHum_cat")){
    summaryDF[,i] <- as.character(summaryDF[[i]])
    summaryDF[summaryDF[[i]] == "1",i] <- "ND"
    summaryDF[summaryDF[[i]] == "2",i] <- "VeryLow"
    summaryDF[summaryDF[[i]] == "3",i] <- "Low"
    summaryDF[summaryDF[[i]] == "4",i] <- "Medium"
    summaryDF[summaryDF[[i]] == "5",i] <- "High"
    summaryDF[summaryDF[[i]] == "6",i] <- "VeryHigh"
    
    summaryDF[,i] <- factor(summaryDF[,i], levels = c("ND","VeryLow","Low","Medium","High","VeryHigh"))
    
  }
  
  summaryDF$cat_num <- 1
  summaryDF$cat_num[as.character(summaryDF$comboCat) == "VeryLow"] <- 2
  summaryDF$cat_num[as.character(summaryDF$comboCat) %in% c("HigherLachno,low",
                                                            "HigherBact,low",
                                                            "MediumBact,low",
                                                            "MediumLachno,low",
                                                            "MuchHigherLachno,low",
                                                            "Low")] <- 3

  summaryDF$cat_num[as.character(summaryDF$comboCat) %in% c("HigherLachno,medium",
                                                            "MediumBact,medium",
                                                            "MediumLachno,medium",
                                                            "Medium")] <- 4

  summaryDF$cat_num[as.character(summaryDF$comboCat) %in% c("MuchHigherLachno,high",
                                                            "MuchHigherBact,high",
                                                            "HigherLachno,high",
                                                            "MediumLachno,high",
                                                            "MediumBact,high",
                                                            "High")] <- 5

  summaryDF$cat_num[as.character(summaryDF$comboCat) %in% c("MuchHigherLachno,veryhigh",
                                                            "MuchHigherBact,veryhigh",
                                                            "HigherLachno,veryhigh",
                                                            "MediumLachno,veryhigh",
                                                            "MediumBact,veryhigh",
                                                            "VeryHigh")] <- 6
  
  # not getting VeryHigh in combo
  dfall <- left_join(dfall, summaryDF,
                     by=c("CAGRnumber","bacHum","lachno")) %>%
    rename(bacteria_cat = comboCat,
           contamination_rank = cat_num)

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
