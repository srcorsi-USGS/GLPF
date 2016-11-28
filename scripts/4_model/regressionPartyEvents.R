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

plotJitter <- function(summaryDF, m.p, threshold, responses, title, log.responses){
  
  howGood <- accuracyStuff(summaryDF, m.p, responses, threshold)
  
  states <-  c('WI','MI','NY')
  state_cols <- c('red','black','blue')
  seasons <- c('Fall','Summer','Spring','Winter')
  season_cols <- c(15,16,17,18)
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
  plot(jitter(summaryDF[[responses]]),
       as.numeric(jitter(as.numeric(predict(m.p,newdata = summaryDF)))),
       col=summaryDF$colors,
       cex=summaryDF$sizes,
       pch = summaryDF$shapes,
       main = title,xlab="obs",ylab="pred") 
       # xlim = c(0,7),ylim=c(0,7),
       # axes=FALSE)
  abline(h = threshold, v = threshold, col="grey")
  legend(par('usr')[2], par('usr')[4],  xpd=NA, pch=20,
         legend = states,col = state_cols)
  legend(par('usr')[2], (4*((par('usr')[4] - par('usr')[3])/5)  + par('usr')[3]), xpd=NA, pch=season_cols,
         legend = seasons)
  legend(par('usr')[2], (2*(par('usr')[4] - par('usr')[3])/5), xpd=NA, pch=20,pt.cex = type_cols,
         legend = types)
  box()
  # axis(1, at=1:6,labels=1:6)
  # axis(2, at=1:6,labels=1:6,las=1)
  
  text(par('usr')[2], (.75*((par('usr')[4] - par('usr')[3])/5) + par('usr')[3]), xpd=NA, pos=4,
       labels = paste(c(paste("sensitivity:",round(howGood[["sensitivity"]], digits = 3)),
                        paste("specificity:",round(howGood[["specificity"]], digits = 3)),
                        paste("accuracy:",round(howGood[["accuracy"]], digits = 3))),"\n",collapse = ""))
  
  legend("bottomleft", legend = paste0("n=",howGood[["trueNeg"]]), bty = "n")
  legend("topright", legend = paste0("n=",howGood[["truePos"]]), bty = "n")
  legend("bottomright", legend = paste0("n=",howGood[["falseNeg"]]), bty = "n")
  legend("topleft", legend = paste0("n=",howGood[["falsePos"]]), bty = "n")
  
  # text(0,0,paste0("n=",howGood[["trueNeg"]]),pos=4)
  # text(7,7,paste0("n=",howGood[["truePos"]]),pos=2)
  # text(7,0,paste0("n=",howGood[["falseNeg"]]),pos=2)
  # text(0,7,paste0("n=",howGood[["falsePos"]]),pos=4)
}

accuracyStuff <- function(summaryDF, m.p, responses, threshold){
  summaryDF[[responses]] <- as.numeric(as.character(summaryDF[[responses]]))
  summaryDF$predictions <- as.numeric(as.character(predict(m.p,newdata = summaryDF)))
  summaryDF$observed <- as.numeric(as.character(summaryDF[[responses]]))
  
  subdf <- subset(summaryDF,summaryDF[[responses]] >= threshold)
  sensitivity <- sum(subdf$predictions >= threshold & 
                       subdf$observed >= threshold)/sum(subdf$observed >= threshold, na.rm = TRUE)
  
  subdf <- subset(summaryDF,summaryDF$observed < threshold)
  specificity <- sum(subdf$predictions < threshold & subdf$observed < threshold, na.rm = TRUE)/sum(subdf[[responses]] < threshold, na.rm = TRUE)
  
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


plotStuff_party <- function(m, summaryDF, threshold, model.type, 
                            eventDF=NA, subFolder="allCols", 
                            responses = "contamination_rank",
                            log.responses = FALSE){
 
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
    howGood <- accuracyStuff(summaryDF, m, responses, threshold)
  } else {
    if(log.responses){
      logEvent <- eventDF
      logEvent[,responses] <- log10(logEvent[[responses]])
      howGood <- accuracyStuff(logEvent, m, responses, threshold)      
    } else {
      howGood <- accuracyStuff(eventDF, m, responses, threshold)
    }
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
    summaryDF[,responses] <- as.numeric(as.character(summaryDF[[responses]]))
    plotJitter(summaryDF, m, threshold, responses, model.type, log.responses)
  } else {
    subDF[,responses] <- as.numeric(as.character(subDF[[responses]]))
    eventDF[,responses] <- as.numeric(as.character(eventDF[[responses]]))
    plotJitter_withModel(subDF, m, threshold, responses, 
                         model.type,eventDF, log.responses)
  }
  plot(m, main=model.type)
  # write.csv(importVars, file.path("cached_figures","trees",subFolder,paste0("treeSummary_",model.type,".csv")), row.names = FALSE,quote=FALSE)
  return(df.sum)
}


plotJitter_withModel <- function(summaryDF, m.p, threshold, responses, title,
                                 eventDF, log.responses){
  
  states <-  c('WI','MI','NY')
  state_cols <- c('red','black','blue')
  seasons <- c('Fall','Summer','Spring','Winter')
  season_cols <- c(15,16,17,18)
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
  
  if(log.responses){
    summaryDF <- summaryDF[summaryDF[[responses]] >= 0, ]
    obs <- log10(summaryDF[[responses]])
    obs_event <- log10(eventDF[[responses]])
    logEvent <- eventDF
    logEvent[,responses] <- log10(logEvent[[responses]])
    howGood <- accuracyStuff(logEvent, m.p, responses, threshold)
    # threshold <- log10(threshold)
  } else {
    obs <- summaryDF[[responses]]
    obs_event <- eventDF[[responses]]
    howGood <- accuracyStuff(eventDF, m.p, responses, threshold)
  }
  
  par(oma=c(0, 0, 0, 7), tcl=0, mgp = c(1.5,0.3,0.01))
  plot(jitter(obs),
       as.numeric(jitter(as.numeric(predict(m.p,newdata = summaryDF)))),
       col = "lightgrey",
       cex = summaryDF$sizes,
       pch = summaryDF$shapes,
       main = title,xlab="obs",ylab="pred") 
       # xlim = c(0,7),ylim=c(0,7),
       # axes=FALSE)
  points(jitter(obs_event),
         as.numeric(jitter(as.numeric(predict(m.p,newdata = eventDF)))),
         col=eventDF$colors,
         cex=eventDF$sizes,
         pch = eventDF$shapes)
  abline(h = threshold, v = threshold, col="grey")
  legend(par('usr')[2], par('usr')[4],  xpd=NA, pch=20,
         legend = states,col = state_cols)
  legend(par('usr')[2], (4*((par('usr')[4] - par('usr')[3])/5)  + par('usr')[3]), xpd=NA, 
         pch=season_cols,
         legend = seasons)
  legend(par('usr')[2], (2*(par('usr')[4] - par('usr')[3])/5), xpd=NA, pch=20,
         pt.cex = type_cols,
         legend = types)
  # box()
  # axis(1, at=1:6,labels=1:6)
  # axis(2, at=1:6,labels=1:6,las=1)
  
  text(par('usr')[2],
       (.75*((par('usr')[4] - par('usr')[3])/5) + par('usr')[3]), 
       xpd=NA, pos=4,
       labels = paste(c(paste("sensitivity:",round(howGood[["sensitivity"]], digits = 3)),
                        paste("specificity:",round(howGood[["specificity"]], digits = 3)),
                        paste("accuracy:",round(howGood[["accuracy"]], digits = 3))),"\n",collapse = ""))
  
  legend("bottomleft", legend = paste0("n=",howGood[["trueNeg"]]), bty = "n")
  legend("topright", legend = paste0("n=",howGood[["truePos"]]), bty = "n")
  legend("bottomright", legend = paste0("n=",howGood[["falseNeg"]]), bty = "n")
  legend("topleft", legend = paste0("n=",howGood[["falsePos"]]), bty = "n")

}


###########################################
# All the things!
# threshold <- 2.5
# responses <- "contamination_rank"
# root.save <- "eventStuff"

# threshold <- 5
# responses <- "ent"
# root.save <- "eventsEnt"
# log.responses <- TRUE

threshold <- 5
responses <- "ent"
root.save <- "eventsEnt"
log.responses <- TRUE


###########################################
# All:
subFolders <- c("allCols","allRows","allCols_noWW","allRows_noWW")
base.names <- c("_noQA","_noQA","_noWW_noQA","_noWW_noQA")

for(job in 1:4){
  
  subFolder <- subFolders[job]
  base.name <- base.names[job]
  
  dir.create(file.path("cached_figures","trees",root.save, subFolder), showWarnings = FALSE)
  
  summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))

  # summaryDF <- summaryDF %>%
  #   mutate(DecYear = decimal_date(pdate)) %>%
  #   select(DecYear, everything()) %>%
  #   filter(!is.na(contamination_rank)) %>%
  #   mutate(contamination_rank = factor(contamination_rank))

  summaryDF <- summaryDF %>%
    mutate(DecYear = decimal_date(pdate)) %>%
    select(DecYear, everything()) %>%
    filter(!is.na(ent))
  
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
  
  unique.events <- unique(summaryDF$eventNum)
  
  nEvents <- data.frame(table(summaryDF$eventNum)) %>%
    filter(Freq > 10) %>%
    mutate(Var1 = as.character(Var1))
  
  IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):length(names(summaryDF))]

  # Just abs:
  IVs_abs <- c(IVs[grep("A\\d{3}",IVs)],IVs[grep("Sag",IVs)],IVs[grep("Aresid",IVs)])
  IVs_fl <- IVs[!(IVs %in% IVs_abs)]
  
  ###################################################
  df.sum.total <- data.frame()
  
  for(model.text in c("All","Abs","Fl")){
    
    form.responses <- ifelse(log.responses, paste("log10(",responses,")"),responses)
    
    if(model.text == "All"){
      form <- formula(paste(form.responses, "~", paste("fourier(DecYear) +",paste(IVs,collapse="+"))))
    } else if (model.text == "Abs"){
      form <- formula(paste(form.responses, "~", paste("fourier(DecYear) +",paste(IVs_abs,collapse="+"))))
    } else {
      form <- formula(paste(form.responses, "~", paste("fourier(DecYear) +",paste(IVs_fl,collapse="+"))))
    }
    
    pdf(file.path("cached_figures","trees",root.save,subFolder,paste0('Event_',model.text,'.pdf')),width=11,height=8)
    
    for(j in nEvents$Var1){
      
      subDF <- filter(summaryDF, eventNum  != j)

      eventDF <- filter(summaryDF, eventNum == j)
      
      m <- tryCatch({ctree(form, data = subDF)
        }, error = function(e) NULL)
 
      if(!is.null(m)){
        df.sum <- plotStuff_party(m, subDF_sub, threshold, 
                                  paste0("Ent Predict_",j,"_",model.text), eventDF = eventDF, 
                                  subFolder = file.path(root.save, subFolder), 
                                  responses = responses, log.responses=log.responses)
        
        df.sum.total <- bind_rows(df.sum.total, df.sum)        
      }


    }
    dev.off()
  }
  write.csv(df.sum.total, file.path("cached_figures","trees",root.save,subFolder,"summaryPartyTreeResults.csv"),row.names = FALSE,quote=FALSE)
  
}

