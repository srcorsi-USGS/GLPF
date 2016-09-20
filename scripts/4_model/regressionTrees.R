##Regression Tree Analysis
library(rpart)
library(party)
library(partykit)
library(rpartScore)
library(rpart.plot)

cached.path <- "cached_data"
base.name <- "_noWW_noQA"


regressionTree <- function(cached.path, base.name){
  summaryDF <- readRDS(file.path("cached_data","final","optic_summary",paste0("summary",base.name,".rds")))
  
  responses <- c("bacHum","lachno","contamination_rank")
  IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):(length(names(summaryDF))-1)]

  i <- responses[3]
  for(i in responses){
    form <- formula(paste(i, "~", paste(IVs,collapse="+")))
    
    opt.sum.rt <- rpart(formula=form, data=summaryDF, 
                     control=rpart.control(minsplit=10, minbucket=8, cp=0.001))#,xval=dim(summaryDF)[1]))
    plot(as.party(opt.sum.rt), tp_args=list(id=FALSE))
    
    opt.sum.rt.Prune <- prune(opt.sum.rt,cp=opt.sum.rt$cptable[which.min(opt.sum.rt$cptable[,"xerror"]),"CP"])
    plot(as.party(opt.sum.rt), tp_args=list(id=FALSE))
    
    prp(opt.sum.rt, faclen = 0, cex = 0.8, extra = 1)
    
    m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=10, xval=10))
    for(i in 2:6){
      
      cpPrune <- print(m$cptable)[i,'CP']
      m.p <- prune(m, cp=cpPrune)
      plot(as.party(m.p), tp_args=list(id=FALSE))
      plot(jitter(dfOpt$lachnoBin),jitter(predict(m.p,newdata = dfOpt)),col=dfOpt$plotCol)
      
      
      ordRPResult <- as.data.frame(table(paste(dfOpt$lachnoBin,predict(m.p,newdata = dfOpt))))
      ordRPResult <- ordRPResult[-grep('NA',ordRPResult$Var1),]
      ordRPResult$lachnoBin <- as.numeric(substr(ordRPResult$Var1,1,1))
      ordRPResult$predicted <- as.numeric(substr(ordRPResult$Var1,3,3))
      
      plot(c(1:5),c(1:5),pch="",xlab='Observed',ylab='Predicted')
      text(ordRPResult$lachnoBin,ordRPResult$predicted,labels = ordRPResult$Freq)
      
    }
    
    
    plot(opt.sum.rt.Prune, uniform=TRUE)
    text(opt.sum.rt.Prune, use.n=TRUE, all=TRUE, cex=.8)
    
    
    plot(as.party(opt.sum.rt.Prune),  tp_args=list(id=FALSE))
  }
  
}





  

  # summary(opt.sum.rt)
  # opt.sum.rt$cptable

  plot(as.party(opt.sum.rt), tp_args=list(id=FALSE))
  plot(opt.sum.rt.Prune)
  plot(summaryDF[[i]],as.numeric(predict(opt.sum.rt.Prune)),pch=20,#col=df$plotColor,
       xlab="Obs",ylab="Pred")
  abline(0,1)
  abline(h=2.37,v=2.37,col="blue",lty=2)
  mtext(paste("Recursive Partitioning Results from",beach),side=3,line=2)
  mtext("For the 2014 Beach Season",side=3,line=1)

  abline(0,1)

}










############################################################################

#BRoe Regression Tree
# Sensitivity = percent of positives correctly identified.
# Specificity = percent of negatives correctly identified.

df$predictions <- predict(mAllrq)
subdf <- subset(df,df[,response]>=log10(235))
sensitivity <- sum(subdf$predictions>=log10(235) & subdf[,response]>=log10(235))/sum(subdf[,response]>=log10(235))
subdf <- subset(df,df[,response]<log10(235))
specificity <- sum(subdf$predictions<log10(235) & subdf[,response]<log10(235))/sum(subdf[,response]<log10(235))

correct <- sum(df$predictions>=log10(235) & df[,response]>=log10(235))+ sum(df$predictions<log10(235) & df[,response]<log10(235))
accuracy <- correct/dim(df)[1]

sensitivity
specificity
accuracy