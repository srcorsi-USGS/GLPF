library(dplyr)

#Raw data folder:
raw.track.path <- "raw_data/tracking"
raw.optics.path <- "raw_data/optics"
cached.path <- "cached_data"

df <- readRDS(file.path(raw.track.path,"tracking.rds" ))
dfglpfSummary <- read.csv(file.path(raw.optics.path,"GLPF_Summary.csv"),stringsAsFactors = FALSE)

df$Site<-substr(df$FilterB02µMUWMSFS,1,nchar(df$FilterB02µMUWMSFS)-3)

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

dfglpfDOC <- dfglpfSummary[,c('Grnumber','DOCResult','TDNResult')]
dfglpfDOC$project <- 'glpf'
dfglpfDOC <- dfglpfDOC[which(dfglpfDOC$Grnumber %in% df$CAGRnumber),]

df <- merge(df,dfglpfDOC,by.x='CAGRnumber',by.y='Grnumber',all=TRUE)

########################
# Save Rdata file

saveRDS(df,file=file.path(cached.path,'glpfBact.rds'))



