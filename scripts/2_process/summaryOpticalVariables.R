library(USGSHydroOpt)
library(dplyr)

meta.file <- function(df, base.name){
  types <- lapply(df,class)
  types <- unlist(types)
  metaData <- data.frame(variable = names(types),
                         dataType = types, stringsAsFactors = FALSE)
  metaData$description <- ""
  write.csv(metaData, row.names = FALSE, file = file.path(cached.path,"final","optic_summary",paste0("meta",base.name,".csv")))
}

summaryOpticalVariables <- function(cached.path, base.name, SummaryDir){

  dfall <- readRDS(file.path(cached.path, "final", "rds",paste0("summary",base.name,".rds")))
  dfabs <- readRDS(file.path(cached.path, "final", "rds",paste0("dfabs",base.name,".rds")))
  dffl <- readRDS(file.path(cached.path, "final", "rds",paste0("dffl",base.name,".rds")))

  EEMs3D <- VectorizedTo3DArray(dffl,"exem",grnum = "CAGRnumber")

  saveRDS(EEMs3D,file=file.path(cached.path,"final","optic_summary",paste0("EEMs3D",base.name,".rds")))

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
  
  #Ratios of a few things
  dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsAbs,grnum="CAGRnumber")
  dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSr,grnum="CAGRnumber")
  dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSniff,grnum="CAGRnumber")
  # dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSniffWetStar,grnum="CAGRnumber")
  
  #Compute fraction of summation indices
  dfOpt <- getFrcSum(dataSummary = dfOpt,
                     sigs = fractionSignals,
                     grnum = "CAGRnumber")
  
  #write.csv(names(dfOpt),file="dfOptnames.csv")
  #log transform where it makes sense
  dfOpt <- getLog10(dataSummary=dfOpt,signals=logSignals,grnum="CAGRnumber")
  
  dfOpt$USGSFieldID <- dfOpt$FieldID
  dfall$USGSFieldID <- dfOpt$FieldID
  
  
  
  dfOpt <- arrange(dfOpt, USGSFieldID) %>%
    distinct()
  
  meta.file(dfOpt, base.name)
  saveRDS(dfOpt,file=file.path(cached.path,"final","optic_summary",paste0("summary",base.name,".rds")))
  write.csv(dfOpt,file=file.path(cached.path,"final","optic_summary",paste0("summary",base.name,".csv")),row.names=FALSE)

}

cached.path <- "cached_data"
SummaryDir <- file.path("raw_data","opticalSummary")
base.name <- "_noQA"
summaryOpticalVariables(cached.path, base.name, SummaryDir)
base.name <- "_noWW_noQA"
summaryOpticalVariables(cached.path, base.name, SummaryDir)

