library(rpart)
library(party)
library(partykit)
library(rpartScore)
library(dplyr)
library(smwrBase)
library(lubridate)

cached.path <- "cached_data"
base.name <- "_noQA"
# base.name <- "_noWW_noQA"

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
       as.numeric(jitter(as.numeric(predict(m.p,newdata = summaryDF)))),
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


accuracyStuff <- function(summaryDF, m.p, response, threshold){
  summaryDF[[response]] <- as.numeric(as.character(summaryDF[[response]]))
  summaryDF$predictions <- as.numeric(as.character(predict(m.p,newdata = summaryDF)))
  summaryDF$observed <- as.numeric(as.character(summaryDF[[response]]))
  
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

na.info <- function(df, key = "CAGRnumber", first.col = "OB1"){
  key.index <- which(names(df) == key)
  opt.df <- df[,c(key.index,which(names(df) == first.col):ncol(df))]
  df.noNA <- na.omit(opt.df)
  df.NA <- opt.df[!(opt.df[[key]] %in% df.noNA[[key]]),]
  na.cols.full <- names(opt.df)[!(names(opt.df) %in% names(df.noNA))]
  na.cols.partial <- colnames(df.NA)[ apply(df.NA, 2, anyNA) ]
  na.rows <- df.NA[[key]]
  
  inf.cols <- names(opt.df)[unlist(do.call(data.frame,lapply(opt.df,
                                                             function(x) any(is.infinite(x)))))]
  inf.rows <- which(is.infinite(rowSums(opt.df[-1])))
  
  nan.cols <- names(opt.df)[unlist(do.call(data.frame,lapply(opt.df,
                                                             function(x) any(is.nan(x)))))]
  nan.rows <- which(is.nan(rowSums(opt.df[-1])))
  
  return(list(na.cols.full = na.cols.full,
              na.cols.partial = na.cols.partial,
              na.rows = na.rows,
              inf.cols = inf.cols,
              inf.rows = inf.rows,
              nan.cols = nan.cols,
              nan.rows = nan.rows))
}


plotStuff_party <- function(m, summaryDF, threshold, model.type, eventDF=NA, subFolder="allCols"){
 
  rules <- partykit:::.list.rules.party(m)
  variablesParty <- c()
  for(i in rules){
    tempText <- unlist(strsplit(unlist(strsplit(i, "&")), ">|<|>=|<="))
    tempText <- suppressWarnings(as.character(tempText[which(is.na(as.numeric(tempText)))]))
    tempText <- gsub(" ", "", tempText)
    variablesParty <- c(variablesParty, tempText)
  }
  
  variablesParty <- unique(variablesParty)
  
  if(all(is.na(eventDF))){
    howGood <- accuracyStuff(summaryDF, m, "contamination_rank", threshold)
  } else {
    howGood <- accuracyStuff(eventDF, m, "contamination_rank", threshold)
  }
  
  importVars <- data.frame(variable = variablesParty,
                           stringsAsFactors = FALSE)
  importVars$accuracy <- howGood$accuracy
  importVars$sensitivity <- howGood$sensitivity
  importVars$specificity <- howGood$specificity
  importVars$model <- model.type
  
  df.sum <- data.frame(t(unlist(howGood)), stringsAsFactors = FALSE)
  df.sum <- cbind(data.frame("model" = model.type, stringsAsFactors = FALSE), df.sum)
  df.sum$variables <- paste(variablesParty,collapse = ",")
  
  if(all(is.na(eventDF))){
    summaryDF$contamination_rank <- as.numeric(as.character(summaryDF$contamination_rank))
    plotJitter(summaryDF, m, threshold, "contamination_rank", model.type)
  } else {
    subDF$contamination_rank <- as.numeric(as.character(subDF$contamination_rank))
    eventDF$contamination_rank <- as.numeric(as.character(eventDF$contamination_rank))
    plotJitter_withModel(subDF, m, threshold, "contamination_rank", 
                         model.type,eventDF)
  }
  plot(m, main=model.type)
  # write.csv(importVars, file.path("cached_figures","trees",subFolder,paste0("treeSummary_",model.type,".csv")), row.names = FALSE,quote=FALSE)
  return(df.sum)
}


plotJitter_withModel <- function(summaryDF, m.p, threshold, response, title,
                                 eventDF){
  
  howGood <- accuracyStuff(eventDF, m.p, response, threshold)
  
  states <-  c('WI','MI','NY')
  state_cols <- c('red','green','blue')
  seasons <- c('Fall','Summer','Spring','Winter')
  season_cols <- c(0,1,2,5)
  types <- c('stream','storm_sewer','sewage')
  type_cols <- c(0.5,1,2)
  
  plotCol <- state_cols
  names(plotCol) <- states
  eventDF$colors <- plotCol[eventDF$State]
  plotCol <- season_cols
  names(plotCol) <- seasons
  eventDF$shapes <- plotCol[eventDF$Season]
  summaryDF$shapes <- plotCol[summaryDF$Season]
  plotCol <- type_cols
  names(plotCol) <- types
  eventDF$sizes <- plotCol[eventDF$sampleCat2]
  summaryDF$sizes <- plotCol[summaryDF$sampleCat2]
  
  par(oma=c(0, 0, 0, 7), tcl=0, mgp = c(1.5,0.3,0.01))
  plot(jitter(summaryDF[[response]]),
       as.numeric(jitter(as.numeric(predict(m.p,newdata = summaryDF)))),
       col = "lightgrey",
       cex = summaryDF$sizes,
       pch = summaryDF$shapes,
       main = title,xlab="obs",ylab="pred", 
       xlim = c(0,7),ylim=c(0,7),axes=FALSE)
  points(jitter(eventDF[[response]]),
         as.numeric(jitter(as.numeric(predict(m.p,newdata = eventDF)))),
         col=eventDF$colors,
         cex=eventDF$sizes,
         pch = eventDF$shapes)
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


###########################################
# All the things!
threshold <- 2.5
responses <- "contamination_rank"

###########################################
# All:

subFolders <- c("allCols","allRows","allCols_noWW","allRows_noWW")
base.names <- c("_noQA","_noQA","_noWW_noQA","_noWW_noQA")

for(job in 1:4){
  
  subFolder <- subFolders[job]
  base.name <- base.names[job]
  
  summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))

  summaryDF <- summaryDF %>%
    mutate(DecYear = decimal_date(pdate)) %>%
    select(DecYear, everything()) %>%
    filter(!is.na(contamination_rank)) %>%
    mutate(contamination_rank = factor(contamination_rank))
  
  na.info.list <- na.info(summaryDF)  
  
  if(job %in% c(1,3)){
    rmRows <- unique(c(which(summaryDF$CAGRnumber %in% na.info.list$na.rows),
                       na.info.list$nan.rows,
                       na.info.list$inf.rows))
    summaryDF <- summaryDF[-rmRows,]
  } else {
    rmCols <- which(names(summaryDF) %in% unique(c(na.info.list$na.cols.partial, 
                                                   na.info.list$inf.cols,
                                                   na.info.list$nan.cols)))
    summaryDF <- summaryDF[,-rmCols]
  }
  
  IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):length(names(summaryDF))]

  form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs,collapse="+"))))
  
  m <- ctree(form, data = summaryDF)
  saveRDS(m, file=file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party",base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party",base.name,".rds")))
  
  pdf(file.path("cached_figures","trees","partyOn",subFolder,paste0('party',base.name,"_","All",'.pdf')),width=11,height=8)
    df.sum <- plotStuff_party(m, summaryDF, threshold, "All", subFolder=paste0("partyOn/",subFolder))
  dev.off()
  
  df.sum.total <- df.sum
  
  ###########################################
  # Just abs:
  IVs_abs <- c(IVs[grep("A\\d{3}",IVs)],IVs[grep("Sag",IVs)],IVs[grep("Aresid",IVs)])
  IVs_fl <- IVs[!(IVs %in% IVs_abs)]
  
  form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs_abs,collapse="+"))))
  
  m <- ctree(form, data = summaryDF)
  saveRDS(m, file=file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_JUST_ABS",base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_JUST_ABS",base.name,".rds")))
  
  pdf(file.path("cached_figures","trees","partyOn",subFolder,paste0('party',base.name,"_","Abs",'.pdf')),width=11,height=8)
  df.sum <- plotStuff_party(m, summaryDF, threshold, "Abs", subFolder=subFolder)
  dev.off()
  df.sum.total <- bind_rows(df.sum.total, df.sum)
  
  ###########################################
  # Just fluorescence:
  form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs_fl,collapse="+"))))
  
  m <- ctree(form, data = summaryDF)
  saveRDS(m, file=file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_JUST_FL",base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_JUST_FL",base.name,".rds")))
  
  pdf(file.path("cached_figures","trees","partyOn",subFolder,paste0('party',base.name,"_","Fl",'.pdf')),width=11,height=8)
    df.sum <- plotStuff_party(m, summaryDF, threshold, "Fl", subFolder=subFolder)
  dev.off()
  df.sum.total <- bind_rows(df.sum.total, df.sum)
  
  
  ###################################################
  
  for(model.text in c("All","Abs","Fl")){
    if(model.text == "All"){
      form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs,collapse="+"))))
    } else if (model.text == "Abs"){
      form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs_abs,collapse="+"))))
    } else {
      form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs_fl,collapse="+"))))
    }
    
    for(j in c('Fall','Summer','Spring','Winter')){
      subDF <- filter(summaryDF, Season != j)
      
      m <- ctree(form, data = summaryDF)
      saveRDS(m, file=file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_NO",j,"_",model.text,base.name,".rds")))
      m <- readRDS(file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_NO",j,"_",model.text,base.name,".rds")))
      eventDF <- filter(summaryDF, Season == j)
      
      pdf(file.path("cached_figures","trees","partyOn",subFolder,paste0('party',base.name,"No_",j,"_",model.text,'.pdf')),width=11,height=8)
      df.sum <- plotStuff_party(m, summaryDF, threshold, paste0("No",j,"_",model.text), eventDF = eventDF, subFolder = subFolder)
      dev.off()
      
      df.sum.total <- bind_rows(df.sum.total, df.sum)
    }
    
    
    #######
    # Pull out 1 state
    
    for(j in c('WI','MI','NY')){
      subDF <- filter(summaryDF, State != j)
      eventDF <- filter(summaryDF, State == j)
      
      m <- ctree(form, data = summaryDF)
      saveRDS(m, file=file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_NO",j,"_",model.text,base.name,".rds")))
      m <- readRDS(file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_NO",j,"_",model.text,base.name,".rds")))
      
      pdf(file.path("cached_figures","trees","partyOn",subFolder,paste0('party',base.name,"No_",j,"_",model.text,'.pdf')),width=11,height=8)
      df.sum <- plotStuff_party(m, summaryDF, threshold, paste0("No",j,"_",model.text), eventDF = eventDF, subFolder = subFolder)
      dev.off()
      df.sum.total <- bind_rows(df.sum.total, df.sum)
    }
  }
  
  for(model.text in c("All","Abs","Fl")){
    if(model.text == "All"){
      form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs,collapse="+"))))
    } else if (model.text == "Abs"){
      form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs_abs,collapse="+"))))
    } else {
      form <- formula(paste(responses, "~", paste("fourier(DecYear) +",paste(IVs_fl,collapse="+"))))
    }
    
    #######
    # Pull out 1 state and 1 event
    
    for(j in c('WI','MI','NY')){
      pdf(file.path("cached_figures","trees","partyOn",subFolder,paste0('party',base.name,"_Only",j,"_",model.text,'.pdf')),width=11,height=8)
      
      subDF <- filter(summaryDF, State == j)
      
      bigE <- group_by(subDF, eventNum) %>%
        summarise(nObs = n()) %>%
        arrange(desc(nObs)) %>%
        filter(nObs >= 15)
      
      for(k in bigE$eventNum){
        eventDF <- filter(subDF, eventNum == k)
        subDF_sub <- filter(subDF, eventNum != k)
        
        m <- ctree(form, data = summaryDF)
        saveRDS(m, file=file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_Only",j,"_No_",k,"_",model.text,base.name,".rds")))
        m <- readRDS(file.path(cached.path,"modeling_objects","partyOn",subFolder,paste0("party_Only",j,"_No_",k,"_",model.text,base.name,".rds")))
        
        df.sum <- plotStuff_party(m, subDF_sub, threshold, paste0("Only",j,"_No_",k,"_",model.text), eventDF = eventDF, subFolder = subFolder)
        df.sum.total <- bind_rows(df.sum.total, df.sum)
      }
      dev.off()
    }
  }
  write.csv(df.sum.total, file.path("cached_figures","trees","partyOn",subFolder,"summaryPartyTreeResults.csv"),row.names = FALSE,quote=FALSE)
  
}

