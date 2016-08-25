library(dplyr)
library(data.table)

#Raw data folder:
raw.path <- "raw_data"
cached.path <- "cached_data"

mergeTrackingBact <- function(raw.path, cached.path){
  df <- readRDS(file.path(cached.path,"tracking","tracking.rds" ))

  dfglpfSummary <- setDF(fread(file.path(raw.path,"optics","GLPF_Summary.csv")))
  load(file=file.path(raw.path,"PreCleaned",'GLRI01-04-16_mergedBact.RData'))

  dfall$State <- gsub(' ','',dfall$State)
  dfall$pdate <- as.POSIXct(dfall$Start.date.time..mm.dd.yy.hh.mm.,format='%m/%d/%Y %H:%M')
  dfall$pdate <- as.POSIXct(format(as.POSIXct(dfall$pdate),tz="GMT",usetz=TRUE),tz="GMT")
  GMTOffset <- ifelse(dfall$State=='WI',6,5)
  dfall$pdate <- dfall$pdate + GMTOffset*60*60
  names(dfall) <- gsub("\\.","",names(dfall))
  names(dfall) <- gsub('\\?','',names(dfall))
  
  dfall <- rename(dfall,
                  FilterA04µMUSGSMIBARL = FilterA04ÂµMUSGSMIBARL,
                  FilterB02µMUWMSFS = FilterB02ÂµMUWMSFS, 
                  enterocn100g = Entero,
                  Esp2cn100ml = espCN100mL,
                  lachnocn100g = Lachno,
                  bachumcn100g = BacHuman)
  
  df$MIBARLID <- as.character(df$MIBARLID)
  dfall$bachumcn100g <- as.character(dfall$bachumcn100g)
  dfall$lachnocn100g <- as.character(dfall$lachnocn100g)
  dfall$enterocn100g <- as.character(dfall$enterocn100g)
  
  dfall$project <- 'GLRI'
  df$project <- 'GLPF'
  
  df <- full_join(df, dfall)

  df <- select(df, -racoon, -dogcn100ml, -lachno3cn100ml,
               -qpcrBacHumanResults,-qpcrEnterococcusResults,-qpcrLachno2Results,
               -esp, -ipaH, -ipaHcn100ml,-Esp2cn100ml)
  # Deal with zeros and BLDs, convert to numeric
  
  BacHumCensLevel <- 225
  LachnoCensLevel <- 225
  entCensLevel <- 225
  eColiCensLevel <- 225
  noDetectIndicators <- c('BLD','BLQ','0','<LRL',"ND")

  splitCens <- function(df, base.name, column.name, noDetectIndicators, censor.level){
    df[[paste0(base.name,"_rk")]] <- ifelse(df[[column.name]] %in% noDetectIndicators, "<", "")
    df[[base.name]] <- suppressWarnings(ifelse(df[[paste0(base.name,"_rk")]] == "<", censor.level, as.numeric(df[[column.name]])))
    df <- select_(df,.dots=paste("-",column.name))
    return(df)
  }

  df <- splitCens(df, "bacHum", "bachumcn100g", noDetectIndicators, BacHumCensLevel)
  df <- splitCens(df, "lachno", "lachnocn100g", noDetectIndicators, LachnoCensLevel)
  df <- splitCens(df, "ent", "enterocn100g", noDetectIndicators, entCensLevel)
  df <- splitCens(df, "eColi", "ecolicn100g", noDetectIndicators, eColiCensLevel)
  
  # MI bacteria:

  # df$Esp2cn100ml[!is.na(df$Esp2cn100ml) & df$Esp2cn100ml == "v"] <- NA
  # df$Esp2cn100ml[!is.na(df$Esp2cn100ml) & df$Esp2cn100ml == "na"] <- NA
  # df <- splitCens(df, "esp", "Esp2cn100ml", noDetectIndicators, 0)
  # df <- splitCens(df, "ipaH", "ipaHcn100ml", noDetectIndicators, 0)
  
  dfglpfDOC <- dfglpfSummary[,c('Grnumber','DOCResult','TDNResult')]

  dfglpfDOC <- dfglpfDOC[which(dfglpfDOC$Grnumber %in% df$CAGRnumber),]
  
  df <- full_join(df,dfglpfDOC,by = c('CAGRnumber'='Grnumber'))
  
  df$Site<-substr(df$FilterB02µMUWMSFS,1,nchar(df$FilterB02µMUWMSFS)-3)
  
  df$FieldID <- df$FilterB02µMUWMSFS
  #How it was:
  # df$FieldID[is.na(df$FieldID)] <- df$FilterA04µMUSGSMIBARL[is.na(df$FieldID)]
  # df$FieldID[is.na(df$FieldID)] <- df$FilterB02µMUWMSFS[is.na(df$FieldID)]
  # 
  ########################
  # Save Rdata file
  df <- filter(df, SampleType9regular2blank7replicate == 9)
  
  saveRDS(df,file=file.path(cached.path,"merged",'trackingBacteria.rds'))
  
}
  
mergeTrackingBact(raw.path, cached.path)