# Next steps: 
# 1. Save individual models with some promise and test them on the larger data set
# by highlighting individual events.
# 2. Use variables from the individual group modeling to test with full data set
# -include event as independent variables




library(glmnet)
library(dplyr)
library(RColorBrewer)


#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")

#df.orig <- summaryDF
df <- df.orig
response <- c("lachno","bacHum")

df <- df[-which(is.na(df$lachno)),]

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

groups <- names(groupFreq)[which(groupFreq>21)]

lambdaType <- "lambda.min"
filenm <- "MVLassoByGroupLmin.pdf"
pdf(filenm)
modelCoefList <- list()
for(i in 1:length(groups)){
  
  subdf <- df[which(df$eventGroup2==groups[i]),]
  IVs <- names(df)[begin:end]
  
  #subdf <- df
  foldID <- as.numeric(as.factor(subdf$eventNum))
  events <- unique(subdf$eventNum)
  
# #  Add events as separate dichotomous IVs
#   if(length(events)>1){
#     # eventDF <- data.frame(E1 = ifelse(subdf$eventNum == events[2],1,0))
#     for(j in 2:length(events)){
#       if(j==2)eventDF <- as.data.frame(ifelse(subdf$eventNum == events[j],1,0))
#       else eventDF <- cbind(eventDF,ifelse(subdf$eventNum == events[j],1,0))
#     }
#     names(eventDF) <- events[-1]
#     subdf <- cbind(subdf,eventDF)
#     IVs <- c(IVs,names(eventDF))
#   }
  
  y <- log10(as.matrix(subdf[,response]))
  x <- as.matrix(subdf[IVs])
  
  #If more than 2 events included in group, use event as fold ID, otherwise, use 5-fold XV
  if(length(unique(foldID))>2){
    mg.cv <- cv.glmnet(x=x, y=y,family="mgaussian",alpha=1,foldid = foldID)
    mg <- glmnet(x=x, y=y,family="mgaussian", alpha=1)
  }else{
    mg.cv <- cv.glmnet(x=x, y=y,family="mgaussian",alpha=1,nfolds=5)
    mg <- glmnet(x=x, y=y,family="mgaussian", alpha=1)
  }
  
  
  #Extract Coefficients from cv-determined model using lambda.1se
  if(lambdaType == "lambda.1se"){
    Coefficients <- coef(mg, s = mg.cv$lambda.1se)
    Active.Index <- which(Coefficients[[1]] != 0)
    Active.Coefficients <- Coefficients[[1]][Active.Index];Active.Coefficients
    Active.Coef.names <- row.names(Coefficients[[1]])[Active.Index];Active.Coef.names
  }
  
  
  #Extract Coefficients from cv-determined model using lambda.min
  if(lambdaType == "lambda.min"){
    Coefficients <- coef(mg, s = mg.cv$lambda.min)
    Active.Index <- which(Coefficients[[1]] != 0)
    Active.Coefficients <- Coefficients[[1]][Active.Index];Active.Coefficients
    Active.Coef.names <- row.names(Coefficients[[1]])[Active.Index];Active.Coef.names
  }
  
  modelCoefList[[i]] <- Active.Coef.names[-1]
  
  #model.coefficients <- c(model.coefficients,paste(Active.Coef.names,collapse="+"))
  
  predictions <- predict(mg.cv,newx=as.matrix(subdf[,IVs]),s=lambdaType,type = "response")
  
  #Plot cross validated errors and other model results
  plot(mg.cv)
  
  plotpch <- 20
  colorOptions <- brewer.pal(9, "Set1")
  
  plotCol <- colorOptions[1:length(events)]
  names(plotCol) <- events
  plotcolors <- plotCol[subdf$eventNum]
  
  
  # plotcolors <- ifelse(df$sampleCat1=='sewage','brown4',plotcolors)
  # plotpch <- ifelse(df$sampleCat1=='sewage',20,1)
  # plotcolors <- ifelse(df$eventNum=='WI12', "orange",plotcolors)
  # plotpch <- ifelse(df$eventNum=='WI12',18,plotpch)
  
  #plotcolors <- round(as.numeric(predictions)) + 2
  #plotpch <- 20
  #thresh <- 0.55
  
  par(mfcol=c(2,1),mar=c(3,4,3,1),oma=c(0,2,0,4))
  #Plot Lachno
  plot(subdf[,response[1]],predictions[,1,1],col=plotcolors,pch=plotpch,log='x',xlab="",ylab="")
  mtext(response[1],line=1)
  mtext(paste(Active.Coef.names[-1],collapse=' + '),cex=0.7)
  mtext(groups[i],line=2,font=2)
  
  
  #Plot bacHum
  plot(subdf[,response[2]],predictions[,2,1],col=plotcolors,pch=plotpch,log='x',xlab="",ylab="")
  mtext("Predicted",side=2,line=-2,font=2,xpd=NA,outer=TRUE)
  mtext("Observed",side=1,line=2,font=2)
  mtext(response[2],line=1)
  
  legendNames <- names(plotCol)
  legend('bottomright',legend = legendNames,col=plotCol,pch=plotpch,inset = c(-0.15,0),bty = "n",xpd=NA)
  
  
}
dev.off()
shell.exec(filenm)
