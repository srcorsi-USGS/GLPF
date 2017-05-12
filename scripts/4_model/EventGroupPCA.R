## FOR MULTINOMIAL, IT CHOOSES DIFFERENT VARIABLES FOR EVERY LEVEL. 


library(glmnet)
library(dplyr)

#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")
dfGroups <- read.csv("./scripts/4_model/eventFreqAndDates.csv",stringsAsFactors = FALSE)
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
for (i in 1:length(groups)){
  eventSub <- dfGroups[which(dfGroups$EventGroup==groups[i]),"eventNum"]
  plotColors <- rep("grey",dim(df)[1])
  plotColors[which(df$eventNum %in% eventSub)] <- "blue"
  
plot(pca$x[,1],pca$x[,2], xlab=xlabel, ylab=ylabel, 
     cex=cexpoints,col=plotColors,pch=20)   # this puts a small point at the center
}