# Read data from Google Tracking:
source("scripts/0_download/getGoogleSampleTrackingDownload.R")
#' @examples 
#' tracking <- readRDS(file.path("cached_data","0_download","tracking.rds"))

# Open state data:
source("scripts/1_munge/openStateData.R")
#' @examples 
#' data.mi <- readRDS(file.path("cached_data","1_munge","dataMI.rds"))
#' data.wi <- readRDS(file.path("cached_data","1_munge","dataWI.rds"))
#' data.ny <- readRDS(file.path("cached_data","1_munge","dataNY.rds"))
# total rows 471, which includes blanks and QAs

# Merge GLRI/GLPF optic fluorescence/absorbance data:
# GLRIWWOptFlAbsVectorized.RData
source("scripts/2_process/mergeGLPF_GLRI_opt.R", encoding = 'UTF-8')
#' @examples 
#' dfabs <- readRDS(file.path("cached_data","2_process_merge_GLRI_GLPF","dfabs.rds"))
#' dffl <- readRDS(file.path("cached_data","2_process_merge_GLRI_GLPF","dffl.rds"))
# This is all optics data, QA and blanks

# Merge tracking and bacteria data
# Files that make me nervous because I have no idea where they came from:
# GLRI01-04-16_mergedBact.RData
# GLRIWWMar162016summary.RData
source("scripts/2_process/mergeTrackingBacteriaData.R", encoding = 'UTF-8')
#' @examples 
#' trackingBacteria <- readRDS(file.path("cached_data","3_process_merge_bacteria","trackingBacteria.rds"))

# Merge field and opt data
source("scripts/2_process/mergeFieldOpt.R", encoding = 'UTF-8')
#' @examples
#'mergedData <- readRDS(file.path("cached_data","4_process_merge_field_opt","mergedData.rds"))

# Separate sanitary, ww, qa, blanks, etc
source("scripts/2_process/filterData.R", encoding = 'UTF-8')
#' @examples
#' mergedData <- readRDS(file.path("cached_data","5_process_filterData","rds","summary_noWW_noQA.rds"))
#' only.data.with.field.data <- mergedData[rowSums(is.na(mergedData[,c("WT","DO","Turb","SC","pH")])) != 5,]

source("scripts/2_process/categorizeBacteria.R", encoding = 'UTF-8')
#' @export
#' mergedData <- readRDS(file.path("cached_data","6_process_cagegorize_Bacteria","rds","summary_noWW_noQA.rds"))

source("scripts/2_process/summaryOpticalVariables.R", encoding = 'UTF-8')
#' @examples
#' summaryDF <- readRDS(file.path("cached_data","7_process_summarize_optics","rds",paste0("summary","_noQA",".rds")))

source("scripts/3_visualize/summarizeOptics.R", encoding = 'UTF-8')
#' @examples
#' shell.exec(file.path("cached_figures",paste0("EEMs","_noWW_noQA",".pdf")))
#' shell.exec(file.path("cached_figures",paste0("Abs","_noWW_noQA",".pdf")))