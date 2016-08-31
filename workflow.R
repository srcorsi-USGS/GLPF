# Read data from Google Tracking:
source("scripts/0_download/getGoogleSampleTrackingDownload.R")
#' @examples 
#' tracking <- readRDS(file.path("cached_data","tracking","tracking.rds"))

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
#' @examples
#'mergedData <- readRDS(file.path("cached_data","merged","mergedData.rds"))

# Separate sanitary, ww, qa, blanks, etc
source("scripts/2_process/filterData.R", encoding = 'UTF-8')
#' @examples
#' mergedData <- setDF(fread(file.path("cached_data","final","mergedData.csv")))
#' only.data.with.field.data <- mergedData[rowSums(is.na(mergedData[,c("WT","DO","Turb","SC","pH")])) != 5,]

