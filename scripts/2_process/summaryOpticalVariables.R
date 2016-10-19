library(USGSHydroOpt)
library(dplyr)

meta.file <- function(df, base.name, cached.path){
  types <- lapply(df,class)
  types <- unlist(types)
  metaData <- data.frame(variable = names(types),
                         dataType = types, stringsAsFactors = FALSE)
  metaData$description <- ""
  write.csv(metaData, row.names = FALSE, file = file.path(cached.path,paste0("meta",base.name,".csv")))
}

summaryOpticalVariables <- function(cached.path, base.name, SummaryDir, cached.save){

  dfall <- readRDS(file.path(cached.path, "6_process_cagegorize_Bacteria", "rds",paste0("summary",base.name,".rds")))
  # #Shouldn't need to do this:
  # dfall <- dfall[!duplicated(dfall$CAGRnumber),]
  
  dfabs <- readRDS(file.path(cached.path, "5_process_filterData", "rds",paste0("dfabs",base.name,".rds")))
  dffl <- readRDS(file.path(cached.path, "5_process_filterData", "rds",paste0("dffl",base.name,".rds")))

  EEMs3D <- VectorizedTo3DArray(dffl,"exem",grnum = "CAGRnumber")

  saveRDS(EEMs3D,file=file.path(cached.path,cached.save,"rds",paste0("EEMs3D",base.name,".rds")))

  ##############################################################################################
  # Read summary signals to extract from Fl and abs info
  dfFlSignals <- read.csv(file.path(SummaryDir,"ex_ems_meansCA.csv"),as.is=TRUE)
  dfAbsSignals <- read.csv(file.path(SummaryDir,"abs_wavsCA.csv"),as.is=TRUE)
  AbsSignals <- as.numeric(dfAbsSignals[,1])
  dfSagSignals <- read.csv(file.path(SummaryDir,"SagWavesCA.csv"),as.is=TRUE)
  ratioSignalsAbs <- read.csv(file.path(SummaryDir,"ratioSignalsAbsCA.csv"),as.is=TRUE)
  ratioSignalsAbs <- ratioSignalsAbs[which(ratioSignalsAbs[2]>0),1]
  ratioSignalsSr <- read.csv(file.path(SummaryDir,"ratioSignalsSrCA.csv"),as.is=TRUE)
  ratioSignalsSr <- ratioSignalsSr[which(ratioSignalsSr[2]>0),1]
  ratioSignalsSniff <- read.csv(file.path(SummaryDir,"ratioSignalsSniff.csv"),as.is=TRUE)
  ratioSignalsSniff <- ratioSignalsSniff[which(ratioSignalsSniff[2]>0),1]
  logSignals <- read.csv(file.path(SummaryDir,"logSignalsCA.csv"),as.is=TRUE)[,1]
  ratioSignalsSniffWetStar <- read.csv(file.path(SummaryDir,"ratioSignalsSniffAll.csv"),as.is=TRUE)[,1]
  fractionSignalsAbs <- read.csv(file.path(SummaryDir,"fractionSignals.csv"),as.is=TRUE)
  
  ##############################################################################################
  ######### Add summary variables to summary data frame #######################################
  
  #Fluorescence pairs and means
  dfOpt <- getMeanFl(a=EEMs3D,signals=dfFlSignals,Peak="Peak",
                     Ex1="Ex1",Ex2="Ex2",Em1="Em1",Em2="Em2",
                     dataSummary=dfall,grnum="CAGRnumber")
  dfOpt[,dfFlSignals$Peak] <- as.data.frame(lapply(dfOpt[,dfFlSignals$Peak], function(x){replace(x, x <0,0)}))
  
  #HIX, FI, Freshness 
  dfOpt <- getIndexes(a=EEMs3D,dataSummary=dfOpt,grnum="CAGRnumber")
                      
  #Single absorbance signals
  dfOpt <- getAbs(dataAbs=dfabs,waveCol=,"nm",
                  wavs=dfAbsSignals[,1],colSubsetString="gr",
                  dataSummary=dfOpt,grnum="CAGRnumber")
  
  #Spectral slopes
  dfOpt <- getSag(dataAbs=dfabs,waveCol="nm",
                  sag=dfSagSignals,colSubsetString="gr",
                  dataSummary=dfOpt,grnum="CAGRnumber")
  
  #deviance of abs from exponential regression in the wastewater area
  dfOpt <- getExpResid(wavelength=269,
                       rangeReg=c(240,341),rangeGap=c(254,302),
                       dataAbs=dfabs,
                       waveCol="nm",colSubsetString="gr",
                       dataSummary=dfOpt,grnum="CAGRnumber")
  
  orderRatios <- function(sigs){
    #Ratios of a few things
    ratioVars <- expand.grid(sigs,sigs, stringsAsFactors = FALSE) %>%
      filter(Var1 != Var2) 
    
    ratioVars$lookup <- apply(ratioVars[,c("Var1","Var2")],1, function(x){paste(sort(x),collapse="~")})
    
    ratioVars <- filter(ratioVars, !duplicated(lookup)) %>%
      select(-lookup)    
    return(ratioVars)
  }

  
  dfOpt <- getRatios(dataSummary=dfOpt,
                     sigs=ratioSignalsAbs,
                     grnum="CAGRnumber",
                     specifyOrder = TRUE,
                     ratioVars = orderRatios(ratioSignalsAbs))
  dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSr,
                     grnum="CAGRnumber",specifyOrder = TRUE,
                     ratioVars = orderRatios(ratioSignalsSr))
  dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSniff,
                     grnum="CAGRnumber",specifyOrder = TRUE,
                     ratioVars = orderRatios(ratioSignalsSniff))
  
  dfOpt <- getRatiosBase(dataSummary=dfOpt,sigs=ratioSignalsAbs[ratioSignalsAbs != "A254"], 
                         baseSig = "A254",grnum="CAGRnumber",
                         ratioVars = orderRatios(ratioSignalsAbs[ratioSignalsAbs != "A254"]))
  # dfOpt <- getRatiosBase(dataSummary=dfOpt,sigs=ratioSignalsSr[ratioSignalsSr != "A254"], 
  #                        baseSig = "A254",grnum="CAGRnumber",
  #                        ratioVars = orderRatios(ratioSignalsSr[ratioSignalsSr != "A254"]))
  dfOpt <- getRatiosBase(dataSummary=dfOpt,sigs=ratioSignalsSniff[ratioSignalsSniff != "F"], 
                         baseSig = "F",grnum="CAGRnumber",
                         ratioVars = orderRatios(ratioSignalsSniff[ratioSignalsSniff != "F"]))
  
  #log transform where it makes sense
  dfOpt <- getLog10(dataSummary=dfOpt,signals=logSignals,grnum="CAGRnumber")
  
  dfOpt <- rename(dfOpt, USGSFieldID= FieldID)
  
  dfOpt <- arrange(dfOpt, USGSFieldID) %>%
    distinct()

  meta.file(dfOpt, base.name, file.path(cached.path, cached.save))
  saveRDS(dfOpt,file=file.path(cached.path,cached.save,"rds",paste0("summary",base.name,".rds")))
  write.csv(dfOpt,file=file.path(cached.path,cached.save,paste0("summary",base.name,".csv")),row.names=FALSE)

}

cached.path <- "cached_data"
SummaryDir <- file.path("raw_data","opticalSummary")
base.name <- "_noQA"
cached.save <- "7_process_summarize_optics"
summaryOpticalVariables(cached.path, base.name, SummaryDir, cached.save)
base.name <- "_noWW_noQA"
summaryOpticalVariables(cached.path, base.name, SummaryDir, cached.save)


