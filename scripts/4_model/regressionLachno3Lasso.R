

library(glmnet)
library(dplyr)
library(RColorBrewer)
library(parallel)
library(doParallel)

#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")

#df.orig <- summaryDF
df <- df.orig
response <- c("lachno3")

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

mg.List <- list()
mg.cv.List <- list()

lambdaType <- c("lambda.min","lambda.1se")
filenm <- "Lachno3LassoMin.pdf"
pdf(filenm)
modelCoefList <- list()
for(i in 1:length(lambdaType)){
  
  subdf <- df[which(!is.na(df[,"lachno3"])),]
  IVs <- names(df)[begin:end]
  
  #subdf <- df
  foldID <- as.numeric(as.factor(subdf$eventNum))
  events <- unique(subdf$eventNum)
  
  y <- log10(as.matrix(subdf[,response]))
  x <- as.matrix(subdf[IVs])
  
    mg.cv <- cv.glmnet(x=x, y=y,family="gaussian",alpha=1,nfolds =10)
    mg <- glmnet(x=x, y=y,family="gaussian", alpha=1)
  
  #Extract Coefficients from cv-determined model using lambda.1se
  if(lambdaType[i] == "lambda.1se"){
    Coefficients <- coef(mg, s = mg.cv$lambda.1se)
    Active.Index <- which(Coefficients != 0)
    Active.Coefficients <- Coefficients[Active.Index];Active.Coefficients
    Active.Coef.names <- row.names(Coefficients)[Active.Index];Active.Coef.names
  }
  
  
  #Extract Coefficients from cv-determined model using lambda.min
  if(lambdaType[i] == "lambda.min"){
    Coefficients <- coef(mg, s = mg.cv$lambda.min)
    Active.Index <- which(Coefficients != 0)
    Active.Coefficients <- Coefficients[Active.Index];Active.Coefficients
    Active.Coef.names <- row.names(Coefficients)[Active.Index];Active.Coef.names
  }
  
  modelCoefList[[i]] <- Active.Coef.names[-1]
  
  
  #Plot cross validated errors and other model results
  plot(mg.cv)
  
  predictions <- predict(mg.cv,newx=as.matrix(subdf[,IVs]),s=lambdaType[i],type = "response")
  
  plotpch <- 20
  colorOptions <- c(brewer.pal(9, "YlGn"),brewer.pal(9, "Oranges"))
  
  plotCol <- colorOptions[1:length(events)]
  names(plotCol) <- events
  plotcolors <- plotCol[subdf$eventNum]
  plotcolors <- ifelse(subdf$State=="WI","red",plotcolors)
  plotcolors <- ifelse(subdf$State=="NY","blue",plotcolors)
  plotcolors <- ifelse(subdf$State=="MI","green",plotcolors)
  
  
  
  par(mfcol=c(1,1),mar=c(3,4,3,1),oma=c(0,2,0,4))
  #Plot Lachno
  plot(subdf[,response[1]],predictions,col=plotcolors,pch=plotpch,log='x',xlab="",ylab="")
  mtext(response[1],line=1)
  mtext(paste(Active.Coef.names[-1],collapse=' + '),cex=0.7)
  mtext(lambdaType[i],line=2,font=2)
  
  eventFreq <- table(subdf$eventNum)
  plotEvents <- names(eventFreq)[which(eventFreq > 1)]
  

    
  
  # calibrate Tobit regression and plot
  library(survival)
  
  IVs <- Active.Coef.names[-1]
  response <- response
  LOQ <- 225
  
  ## Compute survival coefficients for Lachno regression ##
  if(length(IVs) > 0){
    y <- Surv(log10(subdf[,response[1]]), subdf[,response[1]]>LOQ, type="left")
    #dfPredStd <- as.data.frame(scale(dfPred[,IVs]))
    form <- formula(paste('y ~',paste(IVs,collapse=' + ')))
    msurvStd <- survreg(form,data=subdf,dist='weibull')
    summary(msurvStd)
    mStep <- stepAIC(msurvStd)
    summary(mStep)
    coefStep <- coef(mStep)
    predictions <- predict(mStep,newdata = subdf)
    
    par(mfcol=c(1,1),mar=c(3,4,3,1),oma=c(0,2,0,4))
    #Plot Lachno
    plot(subdf[,response[1]],predictions,col=plotcolors,pch=plotpch,log='x',xlab="",ylab="")
    mtext(paste(response[1],"Survival"),line=1)
    mtext(paste(names(coefStep)[-1],collapse=' + '),cex=0.7)
    mtext(lambdaType[i],line=2,font=2)
    abline(h=4,v=10000,col="blue",lty=2)
    
    legendNames <- names(plotCol)
    legend('bottomright',legend = legendNames,col=plotCol,pch=plotpch,inset = c(-0.15,0),bty = "n",xpd=NA)
    
  }

  for (j in 1:length(plotEvents)){
    subdf2 <- subdf[subdf$eventNum !=plotEvents[j],]
    y <- Surv(log10(subdf2[,response[1]]), subdf2[,response[1]]>LOQ, type="left")
    form <- formula(paste('y ~',paste(names(coefStep)[-1],collapse=' + ')))
    msurvStd <- survreg(form,data=subdf2,dist='weibull')
    predictions <- predict(msurvStd,newdata = subdf)

    par(mfrow=c(1,1))
    eventcolors <- ifelse(subdf$eventNum==plotEvents[j],"black",NA)
    plot(subdf[,response[1]],predictions,col=plotcolors,pch=plotpch,log='x',xlab="",ylab="")
    points(subdf[,response[1]],predictions,col=eventcolors,pch=1,cex=1.4)
    mtext(response[1],line=1)
    mtext(paste(names(coefStep)[-1],collapse=' + '),cex=0.7)
    mtext(lambdaType[i],line=2,font=2)
    abline(h=4,v=10000,col="blue",lty=2)
  }
  
  mg.List[[i]] <- mg
  mg.cv.List[[i]] <- mg.cv
}  
dev.off()
shell.exec(filenm)
