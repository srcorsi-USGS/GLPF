library(googlesheets)
library(dplyr)

setwd("M:/QW Monitoring Team/GLPF/Data/R")

my_sheets <- gs_ls()

latestGLPFOptSummary <- 'GLPF_123115Summary.csv'

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
  
  
  if(i == 2) {
    dfQA <- dfState[QA,]
  } else {
    dfQA <- rbind(dfQA,dfState[QA,])
  }
  
  if(i == 2) {
    dfAuto <- dfState[Auto,]
  } else {
    dfAuto <- rbind(dfAuto,dfState[Auto,])
  }
  
  if(i == 2) {
    dfWW <- dfState[WW,]
  } else {
    dfWW <- rbind(dfWW,dfState[WW,])
  }
  
  dfState <- dfState[-c(QA,Auto,WW),]
  #dfState <- filter(dfState,Sample.Type........9...regular..2...blank..7...replicate. == 9)
  eventDates <- unique(dfState$date)
  eventNums <- 1:length(eventDates)
  names(eventNums) <- as.character(eventDates)
  dfState$State <- gsub(' ','',dfState$State)
  dfState$eventNum <- paste(dfState$State,eventNums[as.character(dfState$date)],sep="")
  
  if(i == 2) {
    df <- dfState
  } else {
    df <- rbind(df,dfState)
  }
}

names(df) <- gsub('\\.','',names(df))
names(df) <- gsub('\\?','',names(df))

# Generate site names from field IDs
###NEEDS SOME WORK...
df$Site<-substr(df$FilterB02?MUWMSFS,1,nchar(df$FilterB02?MUWMSFS)-3)

# Deal with zeros and BLDs, convert to numeric

BacHumCensLevel <- 225
LachnoCensLevel <- 225
entCensLevel <- 225
eColiCensLevel <- 225
#ipaHCensLevel <- 
#espCensLevel <- 
noDetectIndicators <- c('BLD','BLQ','0')
parm <- 'bachumcn100g'

checkDetects <- function(df,parm,noDetectIndicators){
  nonDetect <- numeric()
  for (i in 1:dim(df)[1]){
    nonDetect <- c(nonDetect,df[i,parm]%in%noDetectIndicators)
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

#df$ipaH <- as.numeric(as.character(ifelse(df$ipaHcn100ml=='<LRL',ipaHCensLevel,df$ipaHcn100ml)))
#df$esp <- as.numeric(as.character(ifelse(df$Esp2cn100ml=='<LRL',espCensLevel,df$Esp2cn100ml)))

# Add some color columns for plotting by state
plotCol <- c('red','green','blue')
plotCol2 <- c('white','white','yellow')
names(plotCol) <- c('WI','MI','NY')
names(plotCol2) <- c('WI','MI','NY')
df$plotCol <- plotCol[df$State]
df$plotCol2 <- plotCol2[df$State]

## Add DOC and TDN data from CA summary file ##
####################################
## Add DOC and nutrient data     ###
####################################
#Read original summary data from CA
dfglpfSummary <- read.csv(paste('../CA/',latestGLPFOptSummary,sep=''),stringsAsFactors = FALSE)
dfglpfDOC <- dfglpfSummary[,c('Grnumber','DOCResult','TDNResult')]
dfglpfDOC$project <- 'glpf'
dfglpfDOC <- dfglpfDOC[which(dfglpfDOC$Grnumber %in% df$CAGRnumber),]

df <- merge(df,dfglpfDOC,by.x='CAGRnumber',by.y='Grnumber',all=TRUE)

########################
# Save Rdata file
fDate <- as.character(Sys.Date())

saveToDir <- '//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/UWM/'
save(df,file=paste(saveToDir,'glpfBact',fDate,'.Rdata',sep=''))
save(dfQA,dfWW,dfAuto,file=paste(saveToDir,'glpfBactQA_WW_Auto',fDate,'.Rdata',sep=''))
#load('glpfBact.Rdata')

