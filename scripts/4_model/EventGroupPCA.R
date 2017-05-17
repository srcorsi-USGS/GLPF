## FOR MULTINOMIAL, IT CHOOSES DIFFERENT VARIABLES FOR EVERY LEVEL. 


library(glmnet)
library(dplyr)
library(RColorBrewer)

#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")
dfGroups <- read.csv("./cached_data/8_process_new_categories/eventFreqAndDatesWGroups.csv",stringsAsFactors = FALSE)
df <- df.orig

beginIV <- "OB1"
endIV <- "logSn.9"
begin <- which(names(df)==beginIV)
end <- which(names(df)==endIV)
IVs <- names(df)[begin:end]

na.info.list <- na.info(df[,-dim(df)[2]],first.col = beginIV)
rmRows <- unique(c(which(df$CAGRnumber %in% na.info.list$na.rows),
                   na.info.list$nan.rows,
                   na.info.list$inf.rows))
rmCols <- unique(which(names(df) %in% c(na.info.list$na.cols.partial,
                                        na.info.list$nan.cols,
                                        na.info.list$inf.cols)))
dfrmCols <- df[,-rmCols]
dfRemoved <- df[rmRows,]
df <- df[-rmRows,]

x <- as.matrix(df[,IVs])

pca <-prcomp(x,center = TRUE, scale=TRUE) 

cexpoints <- 1
xlabel <- "Component 1"
ylabel <- "Component 2"

groupObs <- group_by(dfGroups,EventGroup) %>%
  summarise(groupObs = sum(Freq))
which(groupObs$groupObs > 40)

#Compute distance between mean of groups: use MMSD PAH Euclidian distance analysis as example

groups <- unique(dfGroups$EventGroup)

colorOptions <- brewer.pal(9, "Set1")


filenm <- "PCAEventGroup.pdf"
pdf(filenm)
for (i in 1:length(groups)){
  
  subdf <- dfGroups[which(dfGroups$EventGroup==groups[i]),]
  events <- unique(subdf[,"eventNum"])
  hydroCond <- unique(subdf[,"eventHydroCond"])
#  eventSub <- dfGroups[which(dfGroups$EventGroup==groups[i]),"eventNum"]
  plotColors <- rep(NA,dim(df)[1])
  # eventColors <- colorOptions[1:length(events)]
  # names(eventColors) <- events
  for(j in 1:length(events))plotColors <- ifelse(df$eventNum==events[j],colorOptions[j],plotColors)

  

  #plotColors[which(df$eventNum %in% eventSub)] <- "blue"
  
  plot(pca$x[,1],pca$x[,2], xlab=xlabel, ylab=ylabel, 
       cex=cexpoints,col="grey",pch=20,main=paste(groups[i],",",hydroCond))   # this puts a small point at the center
  points(pca$x[,1],pca$x[,2],  
       cex=cexpoints,col=plotColors,pch=20)   # this puts a small point at the center
}

dev.off()
shell.exec(filenm)
