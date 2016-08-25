#Raw data folder:
cached.path <- "cached_data"
raw.path <- "raw_data"
raw.cleaned <- "raw_data/PreCleaned"

library(stringr)
library(dplyr)
library(data.table)

mergeGLPF_GLRI <- function(raw.path, raw.cleaned, cached.path){

  load(file=file.path(raw.cleaned,'GLRIWWMar162016summary.RData'))

  df <- readRDS(file.path(cached.path,"tracking","trackingBacteria.rds"))
  glriNames <- setDF(fread(file.path(raw.cleaned,'glriNames.csv')))

  ################################################################
  dfglri <- dfOpt
  dfglri$State <- gsub(' ','',dfglri$State)
  dfglri$pdate <- as.POSIXct(dfglri$Start.date.time..mm.dd.yy.hh.mm.,format='%m/%d/%Y %H:%M')
  dfglri$pdate <- as.POSIXct(format(as.POSIXct(dfglri$pdate),tz="GMT",usetz=TRUE),tz="GMT")
  GMTOffset <- ifelse(dfglri$State=='WI',6,5)
  dfglri$pdate <- dfglri$pdate + GMTOffset*60*60
  
  dfglpf <- df
  
  names(dfglri) <- gsub('\\.','',names(dfglri))
  names(dfglri) <- gsub('\\?','',names(dfglri))
  
  ## convert MI bacteria to numeric ##
  dfglpf$esp <- gsub(pattern = '<LRL','0',dfglpf$Esp2cn100ml)
  dfglpf$ipaH <- gsub(pattern = '<LRL','0',dfglpf$ipaHcn100ml)
  dfglpf$esp <- gsub(pattern = 'na',NA,dfglpf$esp)
  dfglpf$ipaH <- gsub(pattern = 'na',NA,dfglpf$ipaH)
  # ***** Need to change when "v" is defined ********** #
  dfglpf$esp <- as.numeric(gsub(pattern = 'v',NA,dfglpf$esp))
  dfglpf$ipaH <- as.numeric(gsub(pattern = 'v',NA,dfglpf$ipaH))

  dfglri$project <- 'GLRI'
  dfglpf$project <- 'GLPF'

  
  #names(dfglri) %in% glriNames$glriNamesOrig
  #glriNames$glriNamesOrig %in% names(dfglri)
  names(dfglri) <- glriNames$glriNamesNew
  
  
  df <- merge(dfglri,dfglpf,all=TRUE)
  df$fieldID <- df$USGSFieldID
  df$fieldID[is.na(df$USGSFieldID)] <- df$FilterA04µMUSGSMIBARL[is.na(df$USGSFieldID)]
  df$eventNum <- gsub('-','',str_sub(df$fieldID,-2,-1))

  df$Site <- gsub('-','',str_sub(df$fieldID,1,-3))
  
  saveRDS(df,file.path(cached.path,"tracking",'trackingBacteriaAndGLRI.rds'))

}

mergeGLPF_GLRI(raw.path, raw.cleaned, cached.path)
