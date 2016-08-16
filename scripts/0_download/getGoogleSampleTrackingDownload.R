library(googlesheets)
library(dplyr)

raw.path <- "raw_data/tracking"
# setwd("M:/QW Monitoring Team/GLPF/Data/R")
# Make sure Bison Connect is your default, and log out for good measure any personal gmail
token <- gs_auth(cache = FALSE)

my_sheets <- gs_ls()

# latestGLPFOptSummary <- 'GLPF_123115Summary.csv'

glpfTitle <- gs_title("GLPF sample tracking form.xlsx")

tzone <- c(NA,'EST5EDT','EST5EDT','CST6CDT')

for (i in 2:4){
  dfState <- gs_read(glpfTitle,ws=i,range=cell_cols("A:Y"))
  names(dfState) <- make.names(names(dfState))
  dfState <- filter(dfState,!is.na(Start.date.time..mm.dd.yy.hh.mm.))
  dfState$pdate <- as.POSIXct(dfState$Start.date.time..mm.dd.yy.hh.mm.,format='%m/%d/%Y %H:%M:%S',tz=tzone[i])
  dfState$pdate <- as.POSIXct(format(as.POSIXct(dfState$pdate),tz="GMT",usetz=TRUE),tz="GMT")
  dfState$date <- as.Date(dfState$pdate)
  QA <- which(dfState$Sample.Type........9...regular..2...blank..7...replicate. != 9)
  Auto <- grep('Auto',dfState$Virus.Autosample.or.Sewer.Grab.)
  WW <- grep('WW',dfState$Virus.Autosample.or.Sewer.Grab.)
  WW <- c(WW,grep('Sanitary',dfState$Comments))
  dfState$UWMFT <- suppressWarnings(as.integer(dfState$UWMFT))
  dfState$MIBARLID <- suppressWarnings(as.integer(dfState$MIBARLID))
  # dfState$entero.cn.100g <- suppressWarnings(as.integer(gsub("\\*","",dfState$entero.cn.100g)))
  # dfState$bachum.cn.100g <- suppressWarnings(as.integer(gsub("\\*","",dfState$bachum.cn.100g)))
  # dfState$lachno.cn.100g <- suppressWarnings(as.integer(gsub("\\*","",dfState$lachno.cn.100g)))
  # dfState$ecoli.cn.100g <- suppressWarnings(as.integer(gsub("\\*","",dfState$ecoli.cn.100g)))
  dfState$entero.cn.100g <- suppressWarnings(as.character(dfState$entero.cn.100g))
  dfState$bachum.cn.100g <- suppressWarnings(as.character(dfState$bachum.cn.100g))
  dfState$lachno.cn.100g <- suppressWarnings(as.character(dfState$lachno.cn.100g))
  dfState$ecoli.cn.100g <- suppressWarnings(as.character(dfState$ecoli.cn.100g))
  
  if(i == 2) {
    dfQA <- dfState[QA,]
  } else {
    dfQA <- bind_rows(dfQA,dfState[QA,])
  }
  
  if(i == 2) {
    dfAuto <- dfState[Auto,]
  } else {
    dfAuto <- bind_rows(dfAuto,dfState[Auto,])
  }
  
  if(i == 2) {
    dfWW <- dfState[WW,]
  } else {
    dfWW <- bind_rows(dfWW,dfState[WW,])
  }
  
  dfState <- dfState[-c(QA,Auto,WW),]
  #dfState <- filter(dfState,Sample.Type........9...regular..2...blank..7...replicate. == 9)
  eventDates <- unique(dfState$date)
  eventNums <- 1:length(eventDates)
  names(eventNums) <- as.character(eventDates)
  dfState$State <- gsub(' ','',dfState$State)
  dfState$eventNum <- paste0(dfState$State,eventNums[as.character(dfState$date)])
  
  if(i == 2) {
    df <- dfState
  } else {
    df <- bind_rows(df,dfState)
  }
}

names(df) <- gsub('\\.','',names(df))
names(df) <- gsub('\\?','',names(df))

saveRDS(df, file.path(raw.path,"tracking.rds"))

