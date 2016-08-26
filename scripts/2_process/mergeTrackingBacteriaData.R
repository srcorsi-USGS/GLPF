library(dplyr)
library(data.table)

#Raw data folder:
raw.path <- "raw_data"
cached.path <- "cached_data"

mergeTrackingBact <- function(raw.path, cached.path){
  df <- readRDS(file.path(cached.path,"tracking","tracking.rds" ))

  dfglpfSummary <- setDF(fread(file.path(raw.path,"optics","GLPF_Summary.csv")))
  load(file=file.path(raw.path,"PreCleaned",'GLRI01-04-16_mergedBact.RData'))
  load(file=file.path(raw.path,"PreCleaned",'GLRIWWMar162016summary.RData'))

  dfall <- left_join(dfall, select(dfOpt, CAGRnumber, DOCResult, TDNResult), by="CAGRnumber")
  
  "All GLRI samples were event samples, so just use that....Steve 8/26/16"
  dfall$Comments[dfall$Comments == ""] <- "Event Grab"
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
                  bachumcn100g = BacHuman) %>%
    filter(SampleType9regular2blank7replicate == 9) %>%
    mutate(bachumcn100g = as.character(bachumcn100g),
           lachnocn100g = as.character(lachnocn100g),
           enterocn100g = as.character(enterocn100g),
           project = 'GLRI')
  
  df$MIBARLID <- as.character(df$MIBARLID)
  df$project <- 'GLPF'
  
  dfglpfDOC <- dfglpfSummary[,c('Grnumber','DOCResult','TDNResult')]
  dfglpfDOC <- dfglpfDOC[which(dfglpfDOC$Grnumber %in% df$CAGRnumber),]
  df <- left_join(df,dfglpfDOC,by = c('CAGRnumber'='Grnumber'))
  
  df <- full_join(df, dfall)

  df <- select(df, -racoon, -dogcn100ml, -lachno3cn100ml,
               -qpcrBacHumanResults,-qpcrEnterococcusResults,-qpcrLachno2Results,
               -esp, -ipaH, -ipaHcn100ml,-Esp2cn100ml) %>%
    filter(CAGRnumber != "")
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

  df$Site<-substr(df$FilterB02µMUWMSFS,1,nchar(df$FilterB02µMUWMSFS)-3)
  
  df$FieldID <- df$FilterB02µMUWMSFS
  
  df$hydroCondition <- "Base"
  df$hydroCondition[grep("event",df$Comments, ignore.case = TRUE)] <- "Event"

  df <- filter(df, SampleType9regular2blank7replicate == 9)
  df$eventNum <- 1
  GMTOffset <- ifelse(df$State=='WI',6,5)[is.na(df$pdate)]

  df$pdate[is.na(df$pdate)] <- as.POSIXct(df$Startdatetimemmddyyhhmm[is.na(df$pdate)], 
                                          format="%m/%d/%Y %H:%M",tz="GMT",usetz=TRUE)+ GMTOffset*60*60
  df$eventNumInd <- 1
  
  eventNum <- ""
  for(state in c("MI","WI","NY")){
    subDF <- df[df$State == state,]
    subDF$Julian <- as.numeric(julian(subDF$pdate,origin=as.Date("1850-01-01")))
    for(i in 2:nrow(subDF)){
      behind <- subDF$Julian[i] - subDF$Julian[i-1]
      if(abs(behind) < 1){
        subDF$eventNumInd[i] <- subDF$eventNumInd[i-1] 
      } else {
        subDF$eventNumInd[i] <- subDF$eventNumInd[i-1] + 1
      }
    }
    df$eventNum[df$State == state] <- paste0(state,subDF$eventNumInd)
  }

    
  saveRDS(df,file=file.path(cached.path,"merged",'trackingBacteria.rds'))
  
}
  
mergeTrackingBact(raw.path, cached.path)