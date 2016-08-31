# Read data from Google Tracking:
source("scripts/0_download/getGoogleSampleTrackingDownload.R")
#' @examples 
#' tracking <- readRDS(file.path("cached_data","tracking","tracking.rds"))
#' QA <- readRDS(file.path("cached_data","tracking","glpfQA.rds"))
#' WW <- readRDS(file.path("cached_data","tracking","glpfWW.rds"))

# Open state data:
source("scripts/1_munge/openStateData.R")
#' @examples 
#' data.mi <- readRDS(file.path("cached_data","state","dataMI.rds"))
#' data.wi <- readRDS(file.path("cached_data","state","dataWI.rds"))
#' data.ny <- readRDS(file.path("cached_data","state","dataNY.rds"))
# total rows 471, which includes blanks and QAs


# Merge GLRI/GLPF optic fluorescence/absorbance data:
# GLRIWWOptFlAbsVectorized.RData
source("scripts/2_process/mergeGLPF_GLRI_opt.R", encoding = 'UTF-8')
#' @examples 
#' dfabs <- readRDS(file.path("cached_data","optics","dfabs.rds"))
#' dffl <- readRDS(file.path("cached_data","optics","dffl.rds"))
# This is all optics data, QA and blanks

# Merge tracking and bacteria data
# Files that make me nervous because I have no idea where they came from:
# GLRI01-04-16_mergedBact.RData
# GLRIWWMar162016summary.RData
source("scripts/2_process/mergeTrackingBacteriaData.R", encoding = 'UTF-8')
#' @examples 
#' trackingBacteria <- readRDS(file.path("cached_data","merged","trackingBacteria.rds"))

# Merge field and opt data
source("scripts/2_process/mergeFieldOpt.R", encoding = 'UTF-8')
mergedData <- readRDS(file.path("cached_data","merged","mergedData.rds"))

cached.path <- 'cached_data'

dfabs <- readRDS(file.path(cached.path,"optics","dfabs.rds"))
dffl <- readRDS(file.path(cached.path,"optics","dffl.rds"))

mergedData <- readRDS(file.path(cached.path,"merged","mergedData.rds"))

dfabs_filtered <- dfabs[,c("nm",unique(mergedData$CAGRnumber))]
dffl_filtered <- dffl[,c("exem",unique(mergedData$CAGRnumber))]

write.csv(dfabs_filtered, file = file.path(cached.path,"final","dfabs.csv"), row.names = FALSE)
write.csv(dffl_filtered, file = file.path(cached.path,"final","dffl.csv"), row.names = FALSE)
write.csv(mergedData, file = file.path(cached.path,"final","mergedData.csv"), row.names = FALSE)
x <- mergedData[c(which(duplicated(mergedData$CAGRnumber)),which(duplicated(mergedData$CAGRnumber,fromLast = TRUE))),]

