library(glmnetcr)
library(dplyr)

#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")

#df.orig <- summaryDF
df <- df.orig
df$response <- df$sources
df$response <- factor(df$response,levels=
                        c("UncontaminatedLow","Uncontaminated", "Animal","Human","HumanHigh"))
response <- "response"
df <- df[-which(is.na(df$response)),]

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
dfRmRows <- df[rmRows,]
df <- df[,-rmCols]

beginIV <- "Sag240_255"
endIV <- "rBS44_S45_BF"
begin <- which(names(df)==beginIV)
end <- which(names(df)==endIV)
IVs <- names(df)[begin:end]

groupFreq <- table(df$eventGroup2)

groups <- names(groupFreq)[which(groupFreq>30)]

#for(i in 1:length(groups)){
i <- 1

i <- i+1
  subdf <- df[which(df$eventGroup2==groups[i]),]
  
  y <- subdf[,response]
  unique(y)
#  if(length(unique(y))>2){
x <- as.matrix(subdf[,IVs[1:10]])

m <- glmnet.cr(x, y)

plot.glmnet.cr(m)
print(m)

BIC.step <- select.glmnet.cr(m)
mFit<-fitted(m, s = BIC.step)
names(mFit)
table(mFit$class, y)


#Plot overal fit of model
predicted <- factor(mFit$class,
                    levels = 
                      c("UncontaminatedLow","Uncontaminated", "Animal","Human","HumanHigh")) %>%
  as.numeric()
observed <- as.numeric(y)

par(mar=c(6,10,2,1))
plot(jitter(observed),jitter(predicted),xaxt='n',yaxt='n',ylab='',xlab='')
axis(side=1,at = 1:5,labels=levels(y))
axis(side=2,at = 1:5,labels=levels(y),las=2)
mtext("Observed",side=1,line=3,font=2)
mtext("Predicted",side=2,line=8,font=2)
abline(h=3.5,v=3.5,lty=2,col='blue')

