library(rpart)
library(party)
library(partykit)
library(rpartScore)
library(dplyr)

cached.path <- "cached_data"
base.name <- "_noQA"
# base.name <- "_noWW_noQA"

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
       as.numeric(jitter(predict(m.p,newdata = summaryDF))),
       col = "lightgrey",
       cex = summaryDF$sizes,
       pch = summaryDF$shapes,
       main = title,xlab="obs",ylab="pred", 
       xlim = c(0,7),ylim=c(0,7),axes=FALSE)
  points(jitter(eventDF[[response]]),
       as.numeric(jitter(predict(m.p,newdata = eventDF))),
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

plotStuff <- function(m, summaryDF, threshold, model.type, eventDF=NA, subFolder="allCols"){
  m.min.pos <- which.min(m$cptable[, 4])
  th.1std.rule.mc <- m$cptable[m.min.pos, 4] + m$cptable[m.min.pos, 5]
  best.1std.rule.mc <- which.max(m$cptable[, 4] < th.1std.rule.mc)
  best.1std.rule.mc.cp <- m$cptable[best.1std.rule.mc, 1]
  m.p <- prune(m, cp = best.1std.rule.mc.cp)
  
  signal.list <- as.character(m.p$frame$var)
  signal.list <- signal.list[signal.list != "<leaf>"]
  if(length(signal.list) == 0){
    m.p <- m
    signal.list <- as.character(m.p$frame$var)
    signal.list <- signal.list[signal.list != "<leaf>"]
  }
  signal <- data.frame(signals = signal.list, optical.signals = 1)
  signal$optical.signals[substr(signal.list, 1, 1) == "r"] <- 2
  signal$optical.signals[substr(signal.list, 1, 1) == "rB"] <- 3
  signal$optical.signals[nchar(gsub("[^0-9]", "", signal.list)) == 12]
  #Get the signals that have 12 digits within them, and strip out non-digits
  k <- gsub(pattern = "[^0-9]","",signal.list[nchar(gsub("[^0-9]", "", signal.list)) == 12])
  #Figure out how many unique sets of 3:
  if(length(k) > 0){
    k.length <- apply((sapply(k, function(x) substring(x, seq(1, 11, 3), seq(3, 12, 3)))), 2, function(x) length(unique(x)))
    signal$optical.signals[nchar(gsub("[^0-9]", "", signal.list)) == 12] <- k.length 
  }
  if(all(is.na(eventDF))){
    howGood <- accuracyStuff(summaryDF, m.p, "contamination_rank", threshold)
  } else {
    howGood <- accuracyStuff(eventDF, m.p, "contamination_rank", threshold)
  }
  
  importVars <- data.frame(importance = m.p$variable.importance,
                           variable = names(m.p$variable.importance),
                           stringsAsFactors = FALSE)
  importVars$accuracy <- howGood$accuracy
  importVars$sensitivity <- howGood$sensitivity
  importVars$specificity <- howGood$specificity
  importVars$model <- model.type
  
  df.sum <- data.frame(t(unlist(howGood)), stringsAsFactors = FALSE)
  df.sum <- cbind(data.frame("model" = model.type, stringsAsFactors = FALSE), df.sum)
  df.sum$nSignals <- sum(signal$optical.signals)
  df.sum$variables <- paste(signal$signals,collapse = ",")
  
  if(all(is.na(eventDF))){
    plotJitter(summaryDF, m.p, threshold, "contamination_rank", model.type)
  } else {
    plotJitter_withModel(subDF, m.p, threshold, "contamination_rank", 
                         model.type,eventDF)
  }
  
  plot(as.party(m.p), tp_args=list(id=FALSE), main=model.type)
  write.csv(importVars, file.path("cached_figures","trees",subFolder,paste0("treeSummary_",model.type,".csv")), row.names = FALSE,quote=FALSE)
  return(df.sum)
}

na.info <- function(df, key = "CAGRnumber", first.col = "OB1"){
  key.index <- which(names(df) == key)
  opt.df <- df[,c(key.index,which(names(df) == first.col):ncol(df))]
  df.noNA <- na.omit(opt.df)
  df.NA <- opt.df[!(opt.df[[key]] %in% df.noNA[[key]]),]
  na.cols.full <- names(opt.df)[!(names(opt.df) %in% names(df.noNA))]
  na.cols.partial <- colnames(df.NA)[ apply(df.NA, 2, anyNA) ]
  na.rows <- df.NA[[key]]
  
  inf.cols <- names(opt.df)[unlist(do.call(data.frame,lapply(opt.df, function(x) any(is.infinite(x)))))]
  inf.rows <- which(is.infinite(rowSums(opt.df[-1])))
  
  return(list(na.cols.full = na.cols.full,
              na.cols.partial = na.cols.partial,
              na.rows = na.rows,
              inf.cols = inf.cols,
              inf.rows = inf.rows))
}

###########################################
# All the things!
threshold <- 2.5
responses <- "contamination_rank"

summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
summaryDF <- summaryDF[!is.na(summaryDF$contamination_rank),]

na.info.list <- na.info(summaryDF)

rmRows <- unique(c(which(summaryDF$CAGRnumber %in% na.info.list$na.rows),na.info.list$inf.rows))

summaryDF <- summaryDF[-rmRows,]

IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):length(names(summaryDF))]

###########################################
# All:

subFolder <- "allCols"

form <- formula(paste(responses, "~", paste(IVs,collapse="+")))
# m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
# saveRDS(m, file=file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore",base.name,".rds")))
m <- readRDS(file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore",base.name,".rds")))

pdf(file.path("cached_figures","trees",subFolder,paste0('rpartOrdinal',base.name,"_","All",'.pdf')),width=11,height=8)
  df.sum <- plotStuff(m, summaryDF, threshold, "All", subFolder=subFolder)
dev.off()

df.sum.total <- df.sum


###########################################
# Just abs:
IVs_abs <- c(IVs[grep("A\\d{3}",IVs)],IVs[grep("Sag",IVs)],IVs[grep("Aresid",IVs)])
IVs_fl <- IVs[!(IVs %in% IVs_abs)]

form <- formula(paste(responses, "~", paste(IVs_abs,collapse="+")))
# m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
# saveRDS(m, file=file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_JUST_ABS",base.name,".rds")))
m <- readRDS(file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_JUST_ABS",base.name,".rds")))

pdf(file.path("cached_figures","trees",subFolder,paste0('rpartOrdinal',base.name,"_","Abs",'.pdf')),width=11,height=8)
  df.sum <- plotStuff(m, summaryDF, threshold, "Abs", subFolder=subFolder)
dev.off()
df.sum.total <- bind_rows(df.sum.total, df.sum)

###########################################
# Just fluorescence:
form <- formula(paste(responses, "~", paste(IVs_fl,collapse="+")))
# m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
# saveRDS(m, file=file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_JUST_FL",base.name,".rds")))
m <- readRDS(file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_JUST_FL",base.name,".rds")))
pdf(file.path("cached_figures","trees",subFolder,paste0('rpartOrdinal',base.name,"_","Fl",'.pdf')),width=11,height=8)
  df.sum <- plotStuff(m, summaryDF, threshold, "Fl", base.name, subFolder=subFolder)
dev.off()
df.sum.total <- bind_rows(df.sum.total, df.sum)


###################################################

for(model.text in c("All","Abs","Fl")){
  if(model.text == "All"){
    form <- formula(paste(responses, "~", paste(IVs,collapse="+")))
  } else if (model.text == "Abs"){
    form <- formula(paste(responses, "~", paste(IVs_abs,collapse="+")))
  } else {
    form <- formula(paste(responses, "~", paste(IVs_fl,collapse="+")))
  }
  
  for(j in c('Fall','Summer','Spring','Winter')){
    subDF <- filter(summaryDF, Season != j)
  
    # m <- rpartScore(form,data=subDF,control=rpart.control(minsplit=40))
    # saveRDS(m, file=file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_NO",j,"_",model.text,base.name,".rds")))
    m <- readRDS(file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_NO",j,"_",model.text,base.name,".rds")))
    eventDF <- filter(summaryDF, Season == j)
    df.sum <- plotStuff(m, summaryDF, threshold, paste0("No",j,"_",model.text), base.name, eventDF, subFolder)
    df.sum.total <- bind_rows(df.sum.total, df.sum)
  }
  
  
  #######
  # Pull out 1 state
  
  for(j in c('WI','MI','NY')){
    subDF <- filter(summaryDF, State != j)
    eventDF <- filter(summaryDF, State == j)
    # m <- rpartScore(form,data=subDF,control=rpart.control(minsplit=40))
    # saveRDS(m, file=file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_NO",j,"_",model.text,base.name,".rds")))
    m <- readRDS(file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_NO",j,"_",model.text,base.name,".rds")))
    df.sum <- plotStuff(m, summaryDF, threshold, paste0("No",j,"_",model.text), base.name, eventDF, subFolder)
    df.sum.total <- bind_rows(df.sum.total, df.sum)
  }
}
  

for(model.text in c("All","Abs","Fl")){
  if(model.text == "All"){
    form <- formula(paste(responses, "~", paste(IVs,collapse="+")))
  } else if (model.text == "Abs"){
    form <- formula(paste(responses, "~", paste(IVs_abs,collapse="+")))
  } else {
    form <- formula(paste(responses, "~", paste(IVs_fl,collapse="+")))
  }
  
  #######
  # Pull out 1 state and 1 event
  
  for(j in c('WI','MI','NY')){
    pdf(file.path("cached_figures","trees",subFolder,paste0('rpartOrdinal',base.name,"_Only",j,"_",model.text,'.pdf')),width=11,height=8)
    
    subDF <- filter(summaryDF, State == j)
    
    bigE <- group_by(subDF, eventNum) %>%
      summarise(nObs = n()) %>%
      arrange(desc(nObs)) %>%
      filter(nObs >= 15)
    
    for(k in bigE$eventNum){
      eventDF <- filter(subDF, eventNum == k)
      subDF_sub <- filter(subDF, eventNum != k)
      
      m <- rpartScore(form,data=subDF_sub,control=rpart.control(minsplit=40))
      saveRDS(m, file=file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_Only",j,"_No_",k,"_",model.text,base.name,".rds")))
      m <- readRDS(file.path(cached.path,"modeling_objects",subFolder,paste0("rpartScore_Only",j,"_No_",k,"_",model.text,base.name,".rds")))
      
      df.sum <- plotStuff_noSave(m, subDF_sub, threshold, paste0("Only",j,"_No_",k,"_",model.text), base.name, eventDF, subFolder)
      df.sum.total <- bind_rows(df.sum.total, df.sum)
    }
  dev.off()
  }
}

write.csv(df.sum.total, file.path("cached_figures","trees",subFolder,"summaryTreeResults.csv"),row.names = FALSE,quote=FALSE)



treeSum <- data.frame(accuracy = numeric(),
                      sensitivity = numeric(),
                      specificity = numeric(),
                      variable = character(),
                      importance = numeric(),
                      model = character(),
                      stringsAsFactors = FALSE)

for(i in c("All","NOFall","NOSummer","NOSpring","NOWinter","NOWI","NOMI","NONY")){
  importVars <- read.csv(file.path("cached_figures","trees",paste0("treeSummary_",i,".csv")),stringsAsFactors = FALSE)
  treeSum <- bind_rows(treeSum, importVars)
  
}

treeSum <- treeSum %>%
  mutate(wt_imp = importance/step) %>%
  arrange(-wt_imp) 

treeSum_summary <- treeSum %>%
  group_by(variable) %>%
  summarise(count = n(),
            freq = length(unique(model))/length(unique(treeSum$model)),
            wt_imp_mean = freq*mean(wt_imp),
            n_models = length(unique(model))) %>%
  arrange(-wt_imp_mean)
  
# ##########################################################################
# library('corrplot') 
# M <- cor(summaryDF[,c(which(names(summaryDF) == "OB1"):ncol(summaryDF))]) # get correlations
# M <- cor(summaryDF[,45:100]) 
# pdf("corrplot.pdf")
# corrplot(M, method = "circle") #plot matrix
# dev.off()
# 
# library(qtlcharts)
# iplotCorr(summaryDF[,45:200], reorder=TRUE)
# iplotCorr(summaryDF[,200:300], reorder=TRUE)
# iplotCorr(summaryDF[,300:469], reorder=TRUE)
