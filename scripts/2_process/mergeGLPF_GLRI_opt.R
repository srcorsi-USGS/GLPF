#Raw data folder:
cached.path <- "cached_data/optics"
raw.path <- "raw_data"
raw.cleaned <- "raw_data/PreCleaned"

library(stringr)
library(dplyr)
library(data.table)

mergeGLPF_GLRI_opt <- function(raw.path, raw.cleaned, cached.path){

  # GLRI optics:
  load(file.path(raw.cleaned,'GLRIWWOptFlAbsVectorized.RData'))

  # GLPF optics:
  dfabsGLPF <- setDF(fread(file.path(raw.path,"optics","Abs.csv")))
  names(dfabsGLPF)[1] <- "nm"
  dfflGLPF <- setDF(fread(file.path(raw.path,"optics","Fl.csv")))
  names(dfflGLPF)[1] <- "exem"
  
  dfabs <- merge(dfabsGLPF,dfabs,by='nm')
  dfflGLPF$exem <- gsub(" ","", dfflGLPF$exem)
  dffl  <- bind_cols(dfflGLPF,dffl)
  dffl  <- dffl[,-which(names(dffl) == "exem")[-1]]
  
  saveRDS(dfabs, file = file.path(cached.path, "dfabs.rds"))
  saveRDS(dffl, file = file.path(cached.path, "dffl.rds"))
  
}

mergeGLPF_GLRI_opt(raw.path, raw.cleaned, cached.path)