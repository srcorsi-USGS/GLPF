## FOR MULTINOMIAL, IT CHOOSES DIFFERENT VARIABLES FOR EVERY LEVEL. 


library(glmnet)
library(dplyr)
library(RColorBrewer)

#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")
dfGroups <- read.csv("./cached_data/8_process_new_categories/eventFreqAndDatesWGroups.csv",stringsAsFactors = FALSE)
df <- df.orig

groupVar <- "EventGroup2"

beginIV <- "Sag240_255"
endIV <- "rBS44_S45_BF"
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
df <- df[,-rmCols]

begin <- which(names(df)==beginIV)
end <- which(names(df)==endIV)
IVs <- names(df)[begin:end]
x <- as.matrix(df[,IVs])

countUnique <- function(x)length(unique(x))

test <- apply(df[,IVs],1,countUnique)
which(test < 10)

pca <-prcomp(x,center = TRUE, scale=TRUE) 

cexpoints <- 1
xlabel <- "Component 1"
ylabel <- "Component 2"

# groupObs <- group_by(dfGroups,EventGroup) %>%
#   summarise(groupObs = sum(Freq))
# which(groupObs$groupObs > 40)

#Compute distance between mean of groups: use MMSD PAH Euclidian distance analysis as example

groups <- unique(dfGroups[,groupVar])

colorOptions <- brewer.pal(9, "Set1")


filenm <- "PCAEventGroupRatios4.pdf"
pdf(filenm)
par(mar=c(0,0,0,0),oma=c(6,6,3,2),mfrow=c(4,4))
State <- ""
for (i in 1:length(groups)){
  # if(State != substr(groups[i],1,1)){
  # par(mar=c(0,0,0,0),oma=c(6,6,3,2),mfrow=c(3,4))
  # State <- substr(groups[i],1,1)
  # }
    
  subdf <- dfGroups[which(dfGroups[,groupVar]==groups[i]),]
  events <- unique(subdf[,"eventNum"])
  hydroCond <- unique(subdf[,"eventHydroCond"])
  plotColors <- rep(NA,dim(df)[1])
  for(j in 1:length(events))plotColors <- ifelse(df$eventNum==events[j],colorOptions[j],plotColors)

  plot(pca$x[,1],pca$x[,2], xlab="", ylab="", xaxt="n",yaxt="n",
       cex=cexpoints,col="grey",pch=20)   # 
  points(pca$x[,1],pca$x[,2],  
       cex=cexpoints,col=plotColors,pch=20)   # 
  text(min(pca$x[,1]),min(pca$x[,2]),paste(groups[i],",",hydroCond),cex=0.8,adj=0)
}

legend("bottom",legend=1:7,col=colorOptions[1:7],pch=20,bty="n",horiz=TRUE,inset=-0.2,xpd=NA)
dev.off()
shell.exec(filenm)

unique(plotColors)

