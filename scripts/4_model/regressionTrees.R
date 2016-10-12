##Regression Tree Analysis
library(rpart)
library(party)
library(partykit)
library(rpartScore)
# library(rpart.plot)
library(RColorBrewer)
library(dplyr)

accuracyStuff <- function(summaryDF, m.p, response, threshold){
  summaryDF$predictions <- predict(m.p,newdata = summaryDF)
  summaryDF$observed <- summaryDF[[response]]
  
  subdf <- subset(summaryDF,summaryDF[[response]] >= threshold)
  sensitivity <- sum(subdf$predictions >= threshold & 
                       subdf$observed >= threshold)/sum(subdf$observed >= threshold, na.rm = TRUE)
  
  subdf <- subset(summaryDF,summaryDF$observed < threshold)
  specificity <- sum(subdf$predictions < threshold & subdf$observed < threshold, na.rm = TRUE)/sum(subdf$contamination_rank < threshold, na.rm = TRUE)
  
  correct <- sum(summaryDF$predictions >= threshold & 
                   summaryDF$observed >= threshold, na.rm = TRUE) +
    sum(summaryDF$predictions < threshold & summaryDF$observed < threshold, na.rm = TRUE)
  accuracy <- correct/nrow(summaryDF)
  
  return(list("sensitivity" = sensitivity,
              "specificity" = specificity,
              "accuracy" = accuracy,
              "truePos" = sum(summaryDF$predictions >= threshold & summaryDF$observed >= threshold, na.rm = TRUE),
              "trueNeg" = sum(summaryDF$predictions < threshold & summaryDF$observed < threshold, na.rm = TRUE),
              "falsePos" = sum(summaryDF$predictions >= threshold & summaryDF$observed < threshold, na.rm = TRUE),
              "falseNeg" = sum(summaryDF$predictions < threshold & summaryDF$observed >= threshold, na.rm = TRUE)))
}

plotJitter <- function(summaryDF, m.p, threshold, response, title){
  
  howGood <- accuracyStuff(summaryDF, m.p, response, threshold)
  
  states <-  c('WI','MI','NY')
  state_cols <- c('red','green','blue')
  seasons <- c('Fall','Summer','Spring','Winter')
  season_cols <- c(0,1,2,5)
  types <- c('stream','storm_sewer','sewage')
  type_cols <- c(0.5,1,2)
  
  plotCol <- state_cols
  names(plotCol) <- states
  summaryDF$colors <- plotCol[summaryDF$State]
  plotCol <- season_cols
  names(plotCol) <- seasons
  summaryDF$shapes <- plotCol[summaryDF$Season]
  plotCol <- type_cols
  names(plotCol) <- types
  summaryDF$sizes <- plotCol[summaryDF$sampleCat2]
  
  par(oma=c(0, 0, 0, 7), tcl=0, mgp = c(1.5,0.3,0.01))
  plot(jitter(summaryDF[[response]]),
       as.numeric(jitter(predict(m.p,newdata = summaryDF))),
       col=summaryDF$colors,
       cex=summaryDF$sizes,
       pch = summaryDF$shapes,
       main = title,xlab="obs",ylab="pred", 
       xlim = c(0,7),ylim=c(0,7),axes=FALSE)
  abline(h = threshold, v = threshold, col="grey")
  legend(par('usr')[2], par('usr')[4],  xpd=NA, pch=20,
         legend = states,col = state_cols)
  legend(par('usr')[2], (4*((par('usr')[4] - par('usr')[3])/5)  + par('usr')[3]), xpd=NA, pch=season_cols,
         legend = seasons)
  legend(par('usr')[2], (2*(par('usr')[4] - par('usr')[3])/5), xpd=NA, pch=20,pt.cex = type_cols,
         legend = types)
  box()
  axis(1, at=1:6,labels=1:6)
  axis(2, at=1:6,labels=1:6,las=1)
  
  text(par('usr')[2], (.75*((par('usr')[4] - par('usr')[3])/5) + par('usr')[3]), xpd=NA, pos=4,
       labels = paste(c(paste("sensitivity:",round(howGood[["sensitivity"]], digits = 3)),
                        paste("specificity:",round(howGood[["specificity"]], digits = 3)),
                        paste("accuracy:",round(howGood[["accuracy"]], digits = 3))),"\n",collapse = ""))
  
  text(0,0,paste0("n=",howGood[["truePos"]]),pos=4)
  text(7,7,paste0("n=",howGood[["trueNeg"]]),pos=2)
  text(7,0,paste0("n=",howGood[["falseNeg"]]),pos=2)
  text(0,7,paste0("n=",howGood[["falsePos"]]),pos=4)
}

regressionTree <- function(cached.path, base.name){
  summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
  
  responses <- c("bacHum","lachno","contamination_rank")
  IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):length(names(summaryDF))]

  j <- responses[3]

  form <- formula(paste(j, "~", paste(IVs,collapse="+")))
  
  summaryDF <- summaryDF[!is.na(summaryDF$contamination_rank),]
  summaryDF$contamination_rank <- factor(summaryDF$contamination_rank)
  
  opt.sum.rt <- rpart(formula=form, data=summaryDF,
                   control=rpart.control(minsplit=40, minbucket=40, cp=0.001))#,xval=dim(summaryDF)[1]))
  
  opt.sum.rt.Prune <- prune(opt.sum.rt,cp=opt.sum.rt$cptable[which.min(opt.sum.rt$cptable[,"xerror"]),"CP"])
  
  pdf(file.path("cached_figures",paste0('rpart',base.name,'.pdf')),width=11,height=8)
  plot(as.party(opt.sum.rt), tp_args=list(id=FALSE))
  dev.off()

  summaryDF$contamination_rank <- as.integer(summaryDF$contamination_rank)
  m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
  saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore",base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore",base.name,".rds")))
  
  threshold <- 2.5
  pdf(file.path("cached_figures",paste0('rpartOrdinal',base.name,'.pdf')),width=11,height=8)
  
  plotcp(m)
  
  for(i in 2:6){
    
    cpPrune <- print(m$cptable)[i,'CP']
    m.p <- prune(m, cp=cpPrune)

    plotJitter(summaryDF, m.p, threshold, j, i)
  
    ordRPResult <- as.data.frame(table(paste(summaryDF$contamination_rank,predict(m.p,newdata = summaryDF))))
    # ordRPResult <- ordRPResult[-grep('NA',ordRPResult$Var1),]
    ordRPResult$Bin <- as.numeric(substr(ordRPResult$Var1,1,1))
    ordRPResult$predicted <- as.numeric(substr(ordRPResult$Var1,3,3))

    plot(c(1:6),c(1:6),pch="",xlab='Observed',ylab='Predicted', main=i,las=1,tcl=.3)
    text(ordRPResult$Bin,ordRPResult$predicted,labels = ordRPResult$Freq)
    abline(h=threshold, v=threshold, col="grey")
    plot(as.party(m.p), tp_args=list(id=FALSE), main=i)
  }
  
  dev.off()

  
}

cached.path <- "cached_data"
# base.name <- "_noWW_noQA"
# regressionTree(cached.path, base.name)
base.name <- "_noQA"
regressionTree(cached.path, base.name)

