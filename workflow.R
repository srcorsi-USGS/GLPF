# Read data from Google Tracking:
source("scripts/0_download/getGoogleSampleTrackingDownload.R")

# Open state data:
source("scripts/1_munge/openStateData.R")

# Merge GLRI/GLPF optic fluorescence/absorbance data:
# GLRIWWOptFlAbsVectorized.RData
source("scripts/2_process/mergeGLPF_GLRI_opt.R", encoding = 'UTF-8')

# Merge tracking and bacteria data
# GLRI01-04-16_mergedBact.RData
source("scripts/2_process/mergeTrackingBacteriaData.R", encoding = 'UTF-8')

# Merge field and opt data
source("scripts/2_process/mergeFieldOpt.R", encoding = 'UTF-8')


cached.path <- 'cached_data'

dfabs <- readRDS(file.path(cached.path,"optics","dfabs.rds"))
dffl <- readRDS(file.path(cached.path,"optics","dffl.rds"))

mergedData <- readRDS(file.path(cached.path,"merged","mergedData.rds"))



