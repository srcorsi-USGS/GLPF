library(googlesheets)
library(dplyr)

cached.path <- "cached_data"
# Make sure Bison Connect is your default, and log out for good measure any personal gmail

getGoogleData <- function(cached.path){
  
  token <- gs_auth(cache = FALSE)
  my_sheets <- gs_ls()
  
  glpfTitle <- gs_title("GLPF sample tracking form.xlsx")
  
  tzone <- c(NA,'EST5EDT','EST5EDT','CST6CDT')
  
  for (i in 2:4){
    dfState <- gs_read(glpfTitle,ws=i,range=cell_cols("A:Y"))
    names(dfState) <- make.names(names(dfState))
    dfState <- filter(dfState,!is.na(Start.date.time..mm.dd.yy.hh.mm.))
    dfState$pdate <- as.POSIXct(dfState$Start.date.time..mm.dd.yy.hh.mm.,format='%m/%d/%Y %H:%M:%S',tz=tzone[i])
    dfState$pdate <- as.POSIXct(format(as.POSIXct(dfState$pdate),tz="GMT",usetz=TRUE),tz="GMT")
    dfState$date <- as.Date(dfState$pdate)

    dfState$UWMFT <- suppressWarnings(as.integer(dfState$UWMFT))
    dfState$MIBARLID <- suppressWarnings(as.integer(dfState$MIBARLID))

    dfState$entero.cn.100g <- suppressWarnings(as.character(dfState$entero.cn.100g))
    dfState$bachum.cn.100g <- suppressWarnings(as.character(dfState$bachum.cn.100g))
    dfState$lachno.cn.100g <- suppressWarnings(as.character(dfState$lachno.cn.100g))
    dfState$ecoli.cn.100g <- suppressWarnings(as.character(dfState$ecoli.cn.100g))
    
    dfState <- select(dfState, -racoon.cn.100ml..relative.., -dog.cn.100ml)
    
    dfState$State <- gsub(' ','',dfState$State)
    
    if(i == 2) {
      df <- dfState
    } else {
      df <- bind_rows(df,dfState)
    }
  }
  
  names(df) <- gsub('\\.','',names(df))
  names(df) <- gsub('\\?','',names(df))
  
  saveRDS(df, file.path(cached.path,"tracking","tracking.rds"))

}

getGoogleData(cached.path)
