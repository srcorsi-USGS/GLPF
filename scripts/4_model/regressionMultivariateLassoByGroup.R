# Next steps: reduce number of groups. Choose events in quadrants and base in quadrants.
# probably just group baseflow by state
# Then run the models and highlight the individual events with grey vs color plots



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

groups <- names(groupFreq)[which(groupFreq>30)]

#for(i in 1:length(groups)){
i <- 0

i <- i+1
subdf <- df[which(df$eventGroup2==groups[i]),]
#subdf <- df

y <- log10(as.matrix(subdf[,response]))
x <- as.matrix(subdf[,IVs])

mg.cv <- cv.glmnet(x=x, y=y,nfolds = 10,family="mgaussian",alpha=1)
plot(mg.cv)

mg <- glmnet(x=x, y=y,family="mgaussian", alpha=1)

#Extract Coefficients from cv-determined model
#Coefficients <- coef(mg, s = mg.cv$lambda.min)
Coefficients <- coef(mg, s = mg.cv$lambda.1se)
Active.Index <- which(Coefficients[[1]] != 0)
Active.Coefficients <- Coefficients[[1]][Active.Index];Active.Coefficients
Active.Coef.names <- row.names(Coefficients[[1]])[Active.Index];Active.Coef.names
#model.coefficients <- c(model.coefficients,paste(Active.Coef.names,collapse="+"))

predictions <- predict(mg.cv,newx=as.matrix(subdf[,IVs]),s=c("lambda.1se"),type = "response")

#Plot cross validated errors and other model results
plot(mg.cv)

events <- unique(subdf$eventNum)
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
thresh <- 0.55
plot(subdf[,response[1]],predictions[,1,1],col=plotcolors,pch=plotpch,log='x',xlab="",ylab="")
mtext("Predicted",side=2,line=2,font=2)
mtext("Observed",side=1,line=2,font=2)
plot(subdf[,response[2]],predictions[,2,1],col=plotcolors,pch=plotpch,log='x',xlab="",ylab="")
mtext("Predicted",side=2,line=2,font=2)
mtext("Observed",side=1,line=2,font=2)


# WI1 rBA275_A295_BA254

legendNames <- names(plotCol)
legend('bottomright',legend = legendNames,col=plotCol,pch=plotpch)

mtext(response,line=1)
mtext(paste(Active.Coef.names[-1],collapse=' + '),cex=0.7)
mtext(events[i],line=2)
