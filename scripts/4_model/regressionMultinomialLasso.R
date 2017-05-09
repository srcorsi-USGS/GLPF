## FOR MULTINOMIAL, IT CHOOSES DIFFERENT VARIABLES FOR EVERY LEVEL. 


library(glmnet)

#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")

#df.orig <- summaryDF
df <- df.orig
df$response <- df$sources2
df$response <- factor(df$response,levels=
                        c("UncontaminatedLow","Uncontaminated", "Animal","Human","HumanHigh"))
response <- "response"
df <- df[-which(is.na(df$response)),]

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



y <- df[,response]
x <- as.matrix(df[,IVs])

glm.family <- "multinomial"
mg.cv <- cv.glmnet(x=x, y=y,nfolds = 10,family=glm.family)
mg <- glmnet(x=x, y=y,family=glm.family)

#Extract Coefficients from cv-determined model
Coefficients <- coef(mg, s = mg.cv$lambda.min)
for(i in 1:length(levels(y)))
  Betas <- Coefficients[[i]]
Betas[which(Betas != 0)]

Coefficients <- coef(mg, s = mg.cv$lambda.1se)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index];Active.Coefficients
Active.Coef.names <- row.names(coef(mg.cv))[Active.Index];Active.Coef.names
#model.coefficients <- c(model.coefficients,paste(Active.Coef.names,collapse="+"))
eventNumbers <- c(eventNumbers,events[j])

predictions <- predict(mg.cv,newx=as.matrix(df[,IVs]),s=c("lambda.1se"),type = "response")

#Plot cross validated errors and other model results
plot(mg.cv)

plotCol <- c('red','green','blue')
names(plotCol) <- c('WI','MI','NY')
plotcolors <- plotCol[df$State]
plotcolors <- ifelse(df$sampleCat1=='sewage','brown4',plotcolors)
plotpch <- ifelse(df$sampleCat1=='sewage',20,1)
plotcolors <- ifelse(df$eventNum=='WI12', "orange",plotcolors)
plotpch <- ifelse(df$eventNum=='WI12',18,plotpch)

#plotcolors <- round(as.numeric(predictions)) + 2
#plotpch <- 20
thresh <- 0.4
plot(jitter(df[,response]),predictions,col=plotcolors,pch=plotpch)
plot(jitter(df[,response]),jitter(as.numeric(predictions>thresh)),col=plotcolors,pch=plotpch)

legendNames <- c(names(plotCol),'Sewage','Fall WI')
legend('topleft',legend = legendNames,col=c(plotCol,'brown4','orange'),pch=c(1,1,1,20,18))
mtext(response,line=1)
mtext(paste(Active.Coef.names[-1],collapse=' + '),cex=0.7)
mtext(events[i],line=2)



# Predict on all data using logistic regression for just Human vs Uncontaminated
df.orig$response <- ifelse(df.orig$sources == "Human",1,0)
predictions.orig  <- predict(mg.cv,newx=as.matrix(df.orig[,IVs]),s=c("lambda.1se"),type = "response")

plotCol <- c('red','green','blue')
plotColSources <- c("orange","darkorchid1","springgreen4","blue","firebrick","yellow4")
names(plotColSources) <- c("HumanHigh","Human", "AnimalHigh", "Animal","Uncontaminated","UncontaminatedLow")

names(plotCol) <- c('WI','MI','NY')
plotcolors <- plotCol[df.orig$State]
plotcolors <- ifelse(df.orig$sampleCat1=='sewage','brown4',plotcolors)
plotpch <- ifelse(df.orig$sampleCat1=='sewage',20,1)
plotcolors <- ifelse(df.orig$eventNum=='WI12', "orange",plotcolors)
plotpch <- ifelse(df.orig$eventNum=='WI12',18,plotpch)

plot(jitter(df.orig[,response]),jitter(as.numeric(predictions.orig)),col=plotcolors,pch=plotpch)
plot(jitter(df.orig[,response]),jitter(as.numeric(predictions.orig>thresh)),col=plotcolors,pch=plotpch)

plotcolors <- plotColSources[summaryDF$sources2]
plot(jitter(df.orig[,response]),jitter(as.numeric(predictions.orig>thresh)),col=plotcolors,pch=plotpch)
legend("top",legend = names(plotColSources),col=plotColSources,pch=1)


######### Ordinal Continuation Ratio models  ################################
#  https://cran.r-project.org/web/packages/glmnetcr/vignettes/glmnetcr.pdf
#
#############################################################################

library(glmnetcr)
library(dplyr)


# m <- glmnet.cr(x = x,y = y,alpha=1)
# print(m)
# summary(m)
# BIC.model <- select.glmnet.cr(fit=m,which = "BIC")
# mFit <- fitted(m, s = BIC.model)
# 
# fit <- glmnet.cr(x, y, method = "forward")
# hat<-fitted(fit$class, s = BIC.model)
# 
# table(hat,y)
# 
# 
# 
# nonzero.glmnet.cr(fit=m,s = BIC.model)

y <- df[,response]
x <- as.matrix(df[,IVs])

m <- glmnet.cr(x, y)

BIC.step <- select.glmnet.cr(m)
mFit<-fitted(m, s = BIC.step)
names(mFit)
#[1] "BIC" "AIC" "class" "probs"
table(mFit$class, y)

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



#######################################################
## Plot by event ###################################
events <- table(df$event)
plotEvents <- names(which(events>10))
filenm <- 'LassoOrdinalByEvent.pdf'
pdf(filenm)
for(i in 1:length(plotEvents)){
  eventRows <- which(df$eventNum==plotEvents[i])
  par(mar=c(6,10,2,1))
  plot(jitter(observed),jitter(predicted),xaxt='n',yaxt='n',ylab='',xlab='',col='grey')
  points(jitter(observed[eventRows]),jitter(predicted[eventRows]),col='blue',pch=20)
  axis(side=1,at = 1:5,labels=levels(y))
  axis(side=2,at = 1:5,labels=levels(y),las=2)
  mtext("Observed",side=1,line=3,font=2)
  mtext("Predicted",side=2,line=8,font=2)
  mtext(plotEvents[i],side=3,line=1)
  abline(h=3.5,v=3.5,lty=2,col='blue')
}

dev.off()
shell.exec(filenm)

##########################################################################################
# Remove all variables except those in the model, re-filter dataframe and refit model
##########################################################################################

BetasNames <- grep('cp',names(nonzero.glmnet.cr(m, s = BIC.step)$beta),value=TRUE,invert = TRUE)

df <- df.orig
df$response <- df$sources2
df <- df[,c(response,"CAGRnumber","eventNum",BetasNames)]
df$response <- factor(df$response,levels=
                        c("UncontaminatedLow","Uncontaminated", "Animal","Human","HumanHigh"))
response <- "response"
df <- df[-which(is.na(df$response)),]

IVs <- BetasNames

na.info.list <- na.info(df,first.col = BetasNames[1])
rmRows <- unique(c(which(df$CAGRnumber %in% na.info.list$na.rows),
                   na.info.list$nan.rows,
                   na.info.list$inf.rows))
dfRemoved <- df[rmRows,]
df <- df[-rmRows,]



y <- df[,response]
x <- as.matrix(df[,IVs])

m <- glmnet.cr(x, y)

BIC.step <- select.glmnet.cr(m)
mFit<-fitted(m, s = BIC.step)
names(mFit)
#[1] "BIC" "AIC" "class" "probs"
table(mFit$class, y)

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



#######################################################
## Plot by event ###################################
events <- table(df$event)
plotEvents <- names(which(events>10))
filenm <- 'LassoOrdinalByEvent2.pdf'
pdf(filenm)
for(i in 1:length(plotEvents)){
  eventRows <- which(df$eventNum==plotEvents[i])
  par(mar=c(6,10,2,1))
  plot(jitter(observed),jitter(predicted),xaxt='n',yaxt='n',ylab='',xlab='',col='grey')
  points(jitter(observed[eventRows]),jitter(predicted[eventRows]),col='blue',pch=20)
  axis(side=1,at = 1:5,labels=levels(y))
  axis(side=2,at = 1:5,labels=levels(y),las=2)
  mtext("Observed",side=1,line=3,font=2)
  mtext("Predicted",side=2,line=8,font=2)
  mtext(plotEvents[i],side=3,line=1)
  abline(h=3.5,v=3.5,lty=2,col='blue')
}

dev.off()
shell.exec(filenm)



