##Regression Tree Analysis
library(rpart)
library(party)
library(partykit)
library(rpartScore)
# library(rpart.plot)
library(RColorBrewer)
library(dplyr)

regressionTree <- function(cached.path, base.name){
  summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
  
  responses <- c("bacHum","lachno","contamination_rank")
  IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):(length(names(summaryDF))-1)]

  j <- responses[3]

  form <- formula(paste(j, "~", paste(IVs,collapse="+")))
  
  plotCol <- c('red','green','blue')
  names(plotCol) <- c('WI','MI','NY')
  summaryDF$state_col <- plotCol[summaryDF$State]
  plotCol <- c('red','green','blue','purple')
  names(plotCol) <- c('Fall','Summer','Spring','Winter')
  summaryDF$season_col <- plotCol[summaryDF$Season]
  plotCol <- c('red','green')
  names(plotCol) <- c('stream','storm_sewer')
  summaryDF$type_col <- plotCol[summaryDF$sampleCat2]
  
  summaryDF <- summaryDF[!is.na(summaryDF$contamination_rank),]
  summaryDF$contamination_rank <- factor(summaryDF$contamination_rank)
  
  opt.sum.rt <- rpart(formula=form, data=summaryDF,
                   control=rpart.control(minsplit=40, minbucket=40, cp=0.001))#,xval=dim(summaryDF)[1]))
  # plot(as.party(opt.sum.rt), tp_args=list(id=FALSE))
  
  opt.sum.rt.Prune <- prune(opt.sum.rt,cp=opt.sum.rt$cptable[which.min(opt.sum.rt$cptable[,"xerror"]),"CP"])
  
  pdf(file.path("cached_figures",paste0('rpart',base.name,'.pdf')),width=11,height=8)
  plot(as.party(opt.sum.rt), tp_args=list(id=FALSE))
  dev.off()

  summaryDF$contamination_rank <- as.integer(summaryDF$contamination_rank)
  # m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
  # saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore",base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore",base.name,".rds")))
  
  pdf(file.path("cached_figures",paste0('rpartOrdinal',base.name,'.pdf')),width=11,height=8)
  
  plotcp(m)
  
  for(i in 2:6){
    
    cpPrune <- print(m$cptable)[i,'CP']
    m.p <- prune(m, cp=cpPrune)
    par(oma=c(0, 0, 0, 7))
    plot(jitter(summaryDF$contamination_rank),
         jitter(predict(m.p,newdata = summaryDF)),
         col=summaryDF$season_col,
         main = i,las=1,xlab="obs",ylab="pred")
    legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, pch=20,
           legend = c('Fall','Summer','Spring','Winter'),col = c('red','green','blue','purple'))
    par(oma=c(0, 0, 0, 7))
    plot(jitter(summaryDF$contamination_rank),
         jitter(predict(m.p,newdata = summaryDF)),
         col=summaryDF$state_col,
         main = i,las=1,xlab="obs",ylab="pred")
    legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,pch=20,
           legend = c('WI','MI','NY'),col =  c('red','green','blue'))
    par(oma=c(0, 0, 0, 7))
    plot(jitter(summaryDF$contamination_rank),
         jitter(predict(m.p,newdata = summaryDF)),
         col=summaryDF$type_col,
         main = i,las=1,xlab="obs",ylab="pred")
    legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,pch=20,
           legend = c('stream','storm_sewer'),col = c('red','green'))
    
    # browser()
    ordRPResult <- as.data.frame(table(paste(summaryDF$contamination_rank,predict(m.p,newdata = summaryDF))))
    # ordRPResult <- ordRPResult[-grep('NA',ordRPResult$Var1),]
    ordRPResult$Bin <- as.numeric(substr(ordRPResult$Var1,1,1))
    ordRPResult$predicted <- as.numeric(substr(ordRPResult$Var1,3,3))

    plot(c(1:6),c(1:6),pch="",xlab='Observed',ylab='Predicted', main=i,las=1)
    text(ordRPResult$Bin,ordRPResult$predicted,labels = ordRPResult$Freq)

    plot(as.party(m.p), tp_args=list(id=FALSE), main=i)
  }
  
  dev.off()

  
}

cached.path <- "cached_data"
base.name <- "_noWW_noQA"
regressionTree(cached.path, base.name)
base.name <- "_noQA"
regressionTree(cached.path, base.name)

