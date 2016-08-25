# Read data from Google Tracking:
source("scripts/0_download/getGoogleSampleTrackingDownload.R")

# Open state data:
source("scripts/1_munge/openStateData.R")

# Merge GLRI/GLPF optic fluorescence/absorbance data
source("scripts/2_process/mergeGLPF_GLRI_opt.R", encoding = 'UTF-8')

# Merge tracking and bacteria data (only GLPF...no GLRI)
source("scripts/2_process/mergeTrackingBacteriaData.R", encoding = 'UTF-8')

# Merge GLRI/GLPF data
source("scripts/2_process/mergeGLPF_GLRI.R", encoding = 'UTF-8')

# Merge field and opt data
source("scripts/2_process/mergeFieldOpt.R", encoding = 'UTF-8')


cached.path <- 'cached_data'

dfabs <- readRDS(file.path(cached.path,"optics","dfabs.rds"))
dffl <- readRDS(file.path(cached.path,"optics","dffl.rds"))
glpfBact2 <- readRDS(file.path(cached.path,"tracking","trackingBacteriaAndGLRI.rds"))

glriglpf <- readRDS(file.path(cached.path,"glriglpf.rds"))
mergedData <- readRDS(file.path(cached.path,"mergedData.rds"))

# cached.track <- 'cached_data/from_Google'
# dfQA <- readRDS(file=file.path(cached.track,'glpfQA.rds'))
# dfWW <- readRDS(file=file.path(cached.track,'glpfWW.rds'))
# dfAuto <- readRDS(file=file.path(cached.track,'glpfAuto.rds'))
