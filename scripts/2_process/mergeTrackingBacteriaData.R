library(dplyr)
library(data.table)

#Raw data folder:
raw.path <- "raw_data"
cached.track.path <- "cached_data/tracking"
cached.optics.path <- "cached_data/optics"
cached.path <- "cached_data"

mergeTrackingBact <- function(raw.path, cached.track.path, cached.optics.path, cached.path){
  df <- readRDS(file.path(cached.track.path,"tracking.rds" ))

  dfglpfSummary <- setDF(fread(file.path(raw.path,"optics","GLPF_Summary.csv")))
  load(file=file.path(raw.path,"PreCleaned",'GLRI01-04-16_mergedBact.RData'))

  dfall$State <- gsub(' ','',dfall$State)
  dfall$pdate <- as.POSIXct(dfall$Start.date.time..mm.dd.yy.hh.mm.,format='%m/%d/%Y %H:%M')
  dfall$pdate <- as.POSIXct(format(as.POSIXct(dfall$pdate),tz="GMT",usetz=TRUE),tz="GMT")
  GMTOffset <- ifelse(dfall$State=='WI',6,5)
  dfall$pdate <- dfall$pdate + GMTOffset*60*60
  names(dfall) <- gsub("\\.","",names(dfall))
  df$MIBARLID <- as.character(df$MIBARLID)
  
  df <- full_join(df, dfall)
  
  
  df$Site<-substr(df$FilterB02µMUWMSFS,1,nchar(df$FilterB02µMUWMSFS)-3)
  
  # Deal with zeros and BLDs, convert to numeric
  
  BacHumCensLevel <- 225
  LachnoCensLevel <- 225
  entCensLevel <- 225
  eColiCensLevel <- 225
  noDetectIndicators <- c('BLD','BLQ','0')
  parm <- 'bachumcn100g'
  
  checkDetects <- function(df,parm,noDetectIndicators){
    nonDetect <- numeric()
    for (i in 1:dim(df)[1]){
      nonDetect <- c(nonDetect,df[i,parm] %in% noDetectIndicators)
    }
    nonDetect
  }
  
  bactDetect <- checkDetects(df,'bachumcn100g',noDetectIndicators)
  df$bacHum <- as.numeric(as.character(ifelse(bactDetect,BacHumCensLevel,df$bachumcn100g)))
  
  bactDetect <- checkDetects(df,'lachnocn100g',noDetectIndicators)
  df$lachno <- as.numeric(as.character(ifelse(bactDetect,LachnoCensLevel,df$lachnocn100g)))
  
  bactDetect <- checkDetects(df,'enterocn100g',noDetectIndicators)
  df$ent <- as.numeric(as.character(ifelse(bactDetect,entCensLevel,df$enterocn100g)))
  
  bactDetect <- checkDetects(df,'ecolicn100g',noDetectIndicators)
  df$eColi <- as.numeric(as.character(ifelse(bactDetect,eColiCensLevel,df$ecolicn100g)))
  
  # plotCol <- c('red','green','blue')
  # plotCol2 <- c('white','white','yellow')
  # names(plotCol) <- c('WI','MI','NY')
  # names(plotCol2) <- c('WI','MI','NY')
  # df$plotCol <- plotCol[df$State]
  # df$plotCol2 <- plotCol2[df$State]

  dfglpfDOC <- dfglpfSummary[,c('Grnumber','DOCResult','TDNResult')]
  dfglpfDOC$project <- 'glpf'
  dfglpfDOC <- dfglpfDOC[which(dfglpfDOC$Grnumber %in% df$CAGRnumber),]
  
  df <- merge(df,dfglpfDOC,by.x='CAGRnumber',by.y='Grnumber',all=TRUE)
  
  ########################
  # Save Rdata file
  
  saveRDS(df,file=file.path(cached.track.path,'trackingBacteria.rds'))
  
}
  
mergeTrackingBact(raw.path, cached.track.path, cached.optics.path, cached.path)