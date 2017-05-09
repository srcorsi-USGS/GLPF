# Heuristic analysis by event

# -Choose a handful of signals that should be important for detecting wastewater
#  contamination, and run regressions for each individual event.
# -Plot the results on an EEMs graph to get a feel for common areas that are 
#  showing up as important for individual event regressions
# -Reduce the number of potential variables by choosing only one of the signals 
# that provide as compared to other signals

# Modeling approaches:
# -Ordinal LASSO using glmnet.cr with "sources2" as response
# -stepwise regression using Lachno and/or bacHum as response
# -Multi-response LASSO using both human markers with glmnet

library(glmnet)
library(glmnetcr)


source("na.info.R")

# Read data 
df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")

df <- df.orig

# Define response variable and transform to factor for ordinal LASSO
df$response <- df$sources2
df$response <- factor(df$response,levels=
                        c("UncontaminatedLow","Uncontaminated", "Animal","Human","HumanHigh"))
response <- "response"

# filter out missing response data
df <- df[-which(is.na(df$response)),]

dfSignals <- read.csv("./scripts/4_model/HeuristicIVs.csv",stringsAsFactors = FALSE)

IVs <- dfSignals[which(dfSignals[,'m1']>0),1]

df <- df[,c(response,"CAGRnumber","eventNum","USGSFieldID",IVs)]

#Remove rows with NA or Inf
na.info.list <- na.info(df,first.col = IVs[1])
rmRows <- unique(c(which(df$CAGRnumber %in% na.info.list$na.rows),
                   na.info.list$nan.rows,
                   na.info.list$inf.rows))
dfRemoved <- df[rmRows,]
if(length(rmRows)>0) df <- df[-rmRows,]


events <- table(df$event)
plotEvents <- names(which(events>13))




#################### Determine models for individual events and plot ############
for(i in 1:length(plotEvents)){
  event <- plotEvents[i]
  
  #Define response and independent variables for individual events
  eventRows <- which(df[,"eventNum"] == event)
  y <- df[eventRows,response]
  x <- as.matrix(df[eventRows,IVs])
  y <- df[,response]
  x <- as.matrix(df[,IVs])
  
  #Fit the model
#  if(length(unique(y))>1){
m <- glmnet.cr(x, y,alpha=1)

#Variable selection of Lasso model using BIC
BIC.step <- select.glmnet.cr(m)
nonzero.glmnet.cr(m, s = BIC.step)
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
}else{ 
  plot(1:10,1:10,main="No variability in response variable",
       pch="",xaxt="n",yaxt="n",ylab="",xlab="")
}
}