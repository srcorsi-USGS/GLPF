library(dplyr)

newCategories <- function(cached.path, base.name, cached.save){
  
  summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))

  human <- which(summaryDF$lachno > 50000 & summaryDF$bacHum > 50000)
  cond1y <- human
  human2 <- summaryDF$lachno > 50000 & summaryDF$bacHum > 50000
  
  rl2h <- (summaryDF$lachno+1)/(summaryDF$bacHum+1)
  rl2l3 <- (summaryDF$lachno+1)/(summaryDF$lachno3+1)
  rdl3 <- (summaryDF$dog_marker+1)/(summaryDF$lachno3+1)
  
  clean <- which(summaryDF$lachno < 1500 & 
                   summaryDF$bacHum < 1000 & 
                   summaryDF$dog_marker < 1000)
  cond2y <- clean
  clean2 <- summaryDF$lachno < 1500 & 
    summaryDF$bacHum < 1000 & 
    summaryDF$dog_marker < 1000
  
  #define L2/HB > 4 condition
  cond2n <- which(!human2 & !clean2)
  cond3y <- cond2n[which(rl2h[cond2n]>4)]
  cond3n <- cond2n[which(rl2h[cond2n]<=4)]
  
  ##   L2/HB > 4 branch
  cond4y <- cond3n[which(rl2h[cond3n] <= 0.25)]
  cond4n <- cond3n[which(rl2h[cond3n] > 0.25)]
  
  aTypical <- cond4y[which(rl2l3[cond4y] > 0.5 & rl2l3[cond4y] < 10)]
  uncertain <- cond4y[which(!(rl2l3[cond4y] > 0.5 & rl2l3[cond4y] < 10))]
  dog <- cond4n[which(rdl3[cond4n] > 10)]
  human <- c(human,cond4n[which(rdl3[cond4n] <= 10)])
  
  
  ##   L2/HB <= 4 branch
  cond5y <- cond3y[which(rdl3[cond3y] > 10)]
  cond5n <- cond3y[which(rdl3[cond3y] <= 10)]
  
  ###     D/L3 > 10 branch
  dog <- c(dog,cond5y)
  human2nd <- cond5y[which(summaryDF$lachno3[cond5y] > 10000)]
  
  ###     D/L3 > 1.5 branch
  cond6y <- cond5n[which(rdl3[cond5n] > 1.5)]
  cond6n <- cond5n[which(rdl3[cond5n] <= 1.5)]
  
  ####      L3 > 1000 branch
  dog <- c(dog,cond6y)
  human2nd <- c(human2nd,cond6y[which(summaryDF$lachno3[cond6y] > 1000)])
  
  ###     L2/L3 > 10 branch
  cond7y <- cond6n[which(rl2l3[cond6n] > 10)]
  cond7n <- cond6n[which(rl2l3[cond6n] <= 10)]
  
  aTypical <- c(aTypical,cond7y[which(summaryDF$lachno3[cond7y] > 10000)])

  animal <- cond7y[which(summaryDF$lachno3[cond7y] <= 10000)]
  
  aTypical <- c(aTypical,cond7n[which(summaryDF$lachno3[cond7n] > 1000)])
  
  animal <- c(animal,cond7n[which(summaryDF$lachno3[cond7n] <= 1000)])
  
  sources <- rep(NA,nrow(summaryDF))
  sources[human] <- 'Human'
  sources[clean] <- 'Uncontaminated'
  sources[dog] <- 'Dog'
  sources[animal] <- 'Animal'
  sources[aTypical] <- 'Atypical human'
  sources[uncertain] <- 'Uncertain'
  summaryDF$sourcesExtra <- sources
  
  # summaryDF$sourcesExtra[rowSums(!(is.na(summaryDF[,c("dog_marker","racoon_marker", "lachno3")]))) != 3] <- NA
  # 
  summaryDF$sourcesExtra[(is.na(summaryDF$lachno3))] <- NA
  
  human2 <- (summaryDF$lachno > 50000 & summaryDF$bacHum > 50000)
  clean <- summaryDF$lachno < 1500 & summaryDF$bacHum < 1000
  rl2hb <- summaryDF$lachno/summaryDF$bacHum
  
  human3 <- ifelse(!human2 & !clean & 
                     rl2hb <= 4 & rl2hb > 0.25 &
                     summaryDF$lachno > 1500 & 
                     summaryDF$bacHum > 1000,TRUE,FALSE)
  
  human4 <- ifelse(!human2 & !clean & rl2hb > 4 & 
                     summaryDF$bacHum > 7000,TRUE,FALSE)
  
  human <- human2 + human3 +human4
  
  animal <- ifelse(!human & !clean,TRUE,FALSE)

  summaryDF$sources <- ifelse(human,'Human','')
  summaryDF$sources <- ifelse(clean,'Uncontaminated',summaryDF$sources)
  summaryDF$sources <- ifelse(animal,'Animal',summaryDF$sources)
  
  
  cr <- which(names(summaryDF)=="contamination_rank")
  summaryDF <- summaryDF[,c(1:cr,ncol(summaryDF)-1,ncol(summaryDF),(cr+1):(ncol(summaryDF)-2))]
  
  saveRDS(summaryDF,file=file.path(cached.path,cached.save,"rds",paste0("summary",base.name,".rds")))
  write.csv(summaryDF,file=file.path(cached.path,cached.save,paste0("summary",base.name,".csv")),row.names=FALSE)
  
}

cached.path <- "cached_data"
base.name <- "_noQA"
cached.save <- "8_process_new_categories"
newCategories(cached.path, base.name, cached.save)
base.name <- "_noWW_noQA"
newCategories(cached.path, base.name, cached.save)
# base.name <- "_QA"
# newCategories(cached.path, base.name, cached.save)