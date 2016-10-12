library(rpart)
library(party)
library(partykit)
library(rpartScore)
library(dplyr)

cached.path <- "cached_data"
base.name <- "_noQA"

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



###########################################
# All the things!
cached.path <- "cached_data"
base.name <- "_noQA"
threshold <- 2.5
responses <- "contamination_rank"


# TODO:
# 1. All optic things
# 2. Only absorbance
# 3. Only fluorescence
# 4. Leave out 1 season, predict season
# 5. Leave out 1 state, predict state
# Summarize variables, use subset:
# 5. Leave out 1 event, predict event
# 7. Leave out 1 event in 1 state, predict that event
# Summarize results with predicted accuracy
# 8. Add in non-optic stuff:
#   a. DOC, TDN
#   b. Season/date
#   c. Other?

# 1. All optic things
summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
summaryDF <- summaryDF[!is.na(summaryDF$contamination_rank),]

IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):length(names(summaryDF))]

form <- formula(paste(responses, "~", paste(IVs,collapse="+")))

# m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
# saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore",base.name,".rds")))
m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore",base.name,".rds")))

pdf(file.path("cached_figures","trees",paste0('rpartOrdinal',base.name,'.pdf')),width=11,height=8)
plotcp(m)
treeSum <- data.frame(accuracy = numeric(),
                      sensitivity = numeric(),
                      specificity = numeric(),
                      step = numeric(),
                      variable = character(),
                      importance = numeric(),
                      model = character(),
                      stringsAsFactors = FALSE)

for(i in 2:nrow(m$cptable)){
  cpPrune <- m$cptable[i,'CP']
  m.p <- prune(m, cp=cpPrune)
  howGood <- accuracyStuff(summaryDF, m.p, "contamination_rank", threshold)
  plotJitter(summaryDF, m.p, threshold, "contamination_rank", paste("All",i))
  plot(as.party(m.p), tp_args=list(id=FALSE), main=paste("All",i))
  
  importVars <- data.frame(importance = m.p$variable.importance,
                           variable = names(m.p$variable.importance),
                           stringsAsFactors = FALSE)
  importVars$step <- i
  importVars$accuracy <- howGood$accuracy
  importVars$sensitivity <- howGood$sensitivity
  importVars$specificity <- howGood$specificity
  importVars$model <- "All"
  treeSum <- bind_rows(treeSum, importVars)
  
}
dev.off()
write.csv(treeSum, file.path("cached_figures","trees","treeSummary_All.csv"), row.names = FALSE,quote=FALSE)


###########################################
# Just abs:
cached.path <- "cached_data"
base.name <- "_noQA"
summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
responses <- "contamination_rank"
IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):length(names(summaryDF))]
IVs_abs <- c(IVs[grep("A\\d{3}",IVs)],IVs[grep("Sag",IVs)])
IVs_fl <- IVs[!(IVs %in% IVs_abs)]

form <- formula(paste(responses, "~", paste(IVs_abs,collapse="+")))

summaryDF <- summaryDF[!is.na(summaryDF$contamination_rank),]

# m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
# saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore_JUST_ABS",base.name,".rds")))
m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore_JUST_ABS",base.name,".rds")))

pdf(file.path("cached_figures","trees",paste0('rpartOrdinal_JUST_ABS',base.name,'.pdf')),width=11,height=8)
plotcp(m)
treeSum <- data.frame(accuracy = numeric(),
                      sensitivity = numeric(),
                      specificity = numeric(),
                      step = numeric(),
                      variable = character(),
                      importance = numeric(),
                      model = character(),
                      stringsAsFactors = FALSE)
for(i in 2:nrow(m$cptable)){
  cpPrune <- m$cptable[i,'CP']
  m.p <- prune(m, cp=cpPrune)
  howGood <- accuracyStuff(summaryDF, m.p, "contamination_rank", threshold)
  plotJitter(summaryDF, m.p, threshold, "contamination_rank", paste("Using just Absorbance",i))
  plot(as.party(m.p), tp_args=list(id=FALSE), main=paste("Using just Absorbance",i))
  importVars <- data.frame(importance = m.p$variable.importance,
                           variable = names(m.p$variable.importance),
                           stringsAsFactors = FALSE)
  importVars$step <- i
  importVars$accuracy <- howGood$accuracy
  importVars$sensitivity <- howGood$sensitivity
  importVars$specificity <- howGood$specificity
  importVars$model <- "Absorption"
  treeSum <- bind_rows(treeSum, importVars)
}
dev.off()
write.csv(treeSum, file.path("cached_figures","trees","treeSummary_justAbs.csv"), row.names = FALSE,quote=FALSE)

###########################################
# Just fluorescence:
form <- formula(paste(responses, "~", paste(IVs_fl,collapse="+")))

# m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
# saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore_JUST_FL",base.name,".rds")))
m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore_JUST_FL",base.name,".rds")))

pdf(file.path("cached_figures","trees",paste0('rpartOrdinal_JUST_FL',base.name,'.pdf')),width=11,height=8)
plotcp(m)
treeSum <- data.frame(accuracy = numeric(),
                      sensitivity = numeric(),
                      specificity = numeric(),
                      step = numeric(),
                      variable = character(),
                      importance = numeric(),
                      model = character(),
                      stringsAsFactors = FALSE)
for(i in 2:nrow(m$cptable)){
  cpPrune <- m$cptable[i,'CP']
  m.p <- prune(m, cp=cpPrune)
  howGood <- accuracyStuff(summaryDF, m.p, "contamination_rank", threshold)
  plotJitter(summaryDF, m.p, threshold, "contamination_rank", paste("Using just Fluorescence",i))
  
  plot(as.party(m.p), tp_args=list(id=FALSE), main=paste("Using just Fluorescence",i))
  importVars <- data.frame(importance = m.p$variable.importance,
                           variable = names(m.p$variable.importance),
                           stringsAsFactors = FALSE)
  importVars$step <- i
  importVars$accuracy <- howGood$accuracy
  importVars$sensitivity <- howGood$sensitivity
  importVars$specificity <- howGood$specificity
  importVars$model <- "Fluorescence"
  treeSum <- bind_rows(treeSum, importVars)
}
dev.off()
write.csv(treeSum, file.path("cached_figures","trees","treeSummary_justFL.csv"), row.names = FALSE,quote=FALSE)
###################################################

#######
# Pull out 1 season
form <- formula(paste(responses, "~", paste(IVs,collapse="+")))

for(j in c('Fall','Summer','Spring','Winter')){
  subDF <- filter(summaryDF, Season != j)

  m <- rpartScore(form,data=subDF,control=rpart.control(minsplit=40))
  saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore_NO",j,base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore_NO",j,base.name,".rds")))

  pdf(file.path("cached_figures","trees",paste0('rpartOrdinal_NO',j,base.name,'.pdf')),width=11,height=8)
  plotcp(m)
  treeSum <- data.frame(accuracy = numeric(),
                        sensitivity = numeric(),
                        specificity = numeric(),
                        step = numeric(),
                        variable = character(),
                        importance = numeric(),
                        model = character(),
                        stringsAsFactors = FALSE)
  eventDF <- filter(summaryDF, Season == j)
  
  for(i in 2:nrow(m$cptable)){
    cpPrune <- m$cptable[i,'CP']
    m.p <- prune(m, cp=cpPrune)
    howGood <- accuracyStuff(subDF, m.p, "contamination_rank", threshold)

    plotJitter_withModel(subDF, m.p, threshold, "contamination_rank", 
                         paste("Model has NO",j," Predicts",j,": Step",i),eventDF)
    
    plot(as.party(m.p), tp_args=list(id=FALSE), main=paste("NO",j,i))
    
    importVars <- data.frame(importance = m.p$variable.importance,
                             variable = names(m.p$variable.importance),
                             stringsAsFactors = FALSE)
    importVars$step <- i
    importVars$accuracy <- howGood$accuracy
    importVars$sensitivity <- howGood$sensitivity
    importVars$specificity <- howGood$specificity
    importVars$model <- paste("No:",j)
    treeSum <- bind_rows(treeSum, importVars)
  }
  
  dev.off()
  write.csv(treeSum, file.path("cached_figures","trees",paste0("treeSummary_NO",j,".csv")), row.names = FALSE,quote=FALSE)
}


#######
# Pull out 1 state

for(j in c('WI','MI','NY')){
  subDF <- filter(summaryDF, State != j)
  
  m <- rpartScore(form,data=subDF,control=rpart.control(minsplit=40))
  saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore_NO",j,base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore_NO",j,base.name,".rds")))

  pdf(file.path("cached_figures","trees",paste0('rpartOrdinal_NO',j,base.name,'.pdf')),width=11,height=8)
  plotcp(m)
  treeSum <- data.frame(accuracy = numeric(),
                        sensitivity = numeric(),
                        specificity = numeric(),
                        step = numeric(),
                        variable = character(),
                        importance = numeric(),
                        model = character(),
                        stringsAsFactors = FALSE)
  eventDF <- filter(summaryDF, State == j)
  for(i in 2:nrow(m$cptable)){
    cpPrune <- m$cptable[i,'CP']
    m.p <- prune(m, cp=cpPrune)
    howGood <- accuracyStuff(subDF, m.p, "contamination_rank", threshold)
    plotJitter_withModel(subDF, m.p, threshold, "contamination_rank", 
                         paste("Model has NO",j," Predicts",j,": Step",i),eventDF)

    plot(as.party(m.p), tp_args=list(id=FALSE), main=paste("NO",j,i))
    importVars <- data.frame(importance = m.p$variable.importance,
                             variable = names(m.p$variable.importance),
                             stringsAsFactors = FALSE)
    importVars$step <- i
    importVars$accuracy <- howGood$accuracy
    importVars$sensitivity <- howGood$sensitivity
    importVars$specificity <- howGood$specificity
    importVars$model <- paste("No:",j)
    treeSum <- bind_rows(treeSum, importVars)
  }
  dev.off()
  write.csv(treeSum, file.path("cached_figures","trees",paste0("treeSummary_NO",j,".csv")), row.names = FALSE,quote=FALSE)
}

for(j in c('WI','MI','NY')){
  subDF <- filter(summaryDF, State == j)
  filterEvent <- table(subDF$eventNum)[order(table(subDF$eventNum),decreasing = TRUE)][2]
  subDF <- filter(subDF, eventNum != names(filterEvent))
  
  m <- rpartScore(form,data=subDF,control=rpart.control(minsplit=40))
  saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore_NO",j,base.name,".rds")))
  m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore_NO",j,base.name,".rds")))

  pdf(file.path("cached_figures","trees",paste0('rpartOrdinal_NO',names(filterEvent),base.name,'.pdf')),width=11,height=8)
  plotcp(m)
  treeSum <- data.frame(accuracy = numeric(),
                        sensitivity = numeric(),
                        specificity = numeric(),
                        step = numeric(),
                        variable = character(),
                        importance = numeric(),
                        model = character(),
                        stringsAsFactors = FALSE)
  eventDF <- filter(summaryDF, eventNum == names(filterEvent))
  
  for(i in 2:nrow(m$cptable)){
    cpPrune <- m$cptable[i,'CP']
    m.p <- prune(m, cp=cpPrune)
    howGood <- accuracyStuff(subDF, m.p, "contamination_rank", threshold)

    plotJitter_withModel(subDF, m.p, threshold, "contamination_rank", 
                         paste("Model has NO",names(filterEvent),"only",j," Predicts",names(filterEvent),": Step",i),eventDF)
    plot(as.party(m.p), tp_args=list(id=FALSE), main=paste("NO",names(filterEvent),"only",j,i))
    importVars <- data.frame(importance = m.p$variable.importance,
                             variable = names(m.p$variable.importance),
                             stringsAsFactors = FALSE)
    importVars$step <- i
    importVars$accuracy <- howGood$accuracy
    importVars$sensitivity <- howGood$sensitivity
    importVars$specificity <- howGood$specificity
    importVars$model <- paste(j, "No:",names(filterEvent))
    treeSum <- bind_rows(treeSum, importVars)
  }
  
  dev.off()
  write.csv(treeSum, file.path("cached_figures","trees",paste0("treeSummary_NO",names(filterEvent),".csv")), row.names = FALSE,quote=FALSE)
}

treeSum <- data.frame(accuracy = numeric(),
                      sensitivity = numeric(),
                      specificity = numeric(),
                      step = numeric(),
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
  
##########################################################################

IVs <- treeSum_summary$variable[1:25]
form.new <- formula(paste(responses, "~", paste(IVs,collapse="+")))

m <- rpartScore(form.new,data=summaryDF,control=rpart.control(minsplit=40))
saveRDS(m, file=file.path(cached.path,"modeling_objects",paste0("rpartScore_using25vars",base.name,".rds")))
m <- readRDS(file.path(cached.path,"modeling_objects",paste0("rpartScore_using25vars",base.name,".rds")))

pdf(file.path("cached_figures","trees",paste0('rpartOrdinal_using25vars',base.name,'.pdf')),width=11,height=8)
plotcp(m)
for(i in 2:nrow(m$cptable)){
  cpPrune <- m$cptable[i,'CP']
  m.p <- prune(m, cp=cpPrune)
  howGood <- accuracyStuff(summaryDF, m.p, "contamination_rank", threshold)
  
  plotJitter(summaryDF, m.p, threshold, "contamination_rank", paste("25 vars",i))
  plot(as.party(m.p), tp_args=list(id=FALSE), main=paste("25 vars",i))

}
dev.off()


