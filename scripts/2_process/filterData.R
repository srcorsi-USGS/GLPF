library(dplyr)

filterOptics <- function(df, dfabs, dffl, base.name, cached.path){
  
  wwGR <- unique(df$CAGRnumber)
  dfabs_filtered <- dfabs[,names(dfabs) %in% c("nm",wwGR)]
  dffl_filtered <- dffl[,names(dffl) %in% c("exem",wwGR)]
  
  write.csv(dfabs_filtered, file = file.path(cached.path,paste0("dfabs",base.name,".csv")), row.names = FALSE)
  write.csv(dffl_filtered, file = file.path(cached.path,paste0("dffl",base.name,".csv")), row.names = FALSE)
  saveRDS(dfabs_filtered, file = file.path(cached.path,"rds",paste0("dfabs",base.name,".rds")))
  saveRDS(dffl_filtered, file = file.path(cached.path,"rds",paste0("dffl",base.name,".rds")))
  
}

meta.file <- function(df, base.name, cached.save){
  types <- lapply(df,class)
  types <- unlist(types)
  metaData <- data.frame(variable = names(types),
                         dataType = types, stringsAsFactors = FALSE)
  metaData$description <- ""
  dir.create(file.path(cached.path,cached.save), showWarnings = FALSE)
  write.csv(metaData, row.names = FALSE, file = file.path(cached.path,cached.save,paste0("meta",base.name,".csv")))
}

filterData <- function(cached.path, cached.save){
  df <- readRDS(file.path(cached.path,"4_process_merge_field_opt","mergedData.rds"))
  
  df <- df[!(df$CAGRnumber %in% c("x","N/A")),]
  
  currentSampleCats <- c("Autosample","Sewer Grab","MB Sample Site","WW Influent","WWTP",
                         "River Grab","Tributary","Sanitary","Sanitary Grab")
  sampleCats <- c('environmental','environmental','environmental','sewage','sewage','environmental',
                  'environmental','sewage','sewage')
  
  names(sampleCats) <- currentSampleCats
  
  df$sampleCat1 <- sampleCats[df$VirusAutosampleorSewerGrab]
  
  sampleCats2 <- c('stream','storm_sewer','stream','sewage','sewage','stream',
                   'stream','sewage','sewage')
  names(sampleCats2) <- currentSampleCats
  df$sampleCat2 <- sampleCats2[df$VirusAutosampleorSewerGrab]
  
  
  dfabs <- readRDS(file.path(cached.path,"2_process_merge_GLRI_GLPF","dfabs.rds"))
  dffl <- readRDS(file.path(cached.path,"2_process_merge_GLRI_GLPF","dffl.rds"))
  
  df_QA <-  filter(df, SampleType9regular2blank7replicate != 9)
  df_regular <- filter(df, SampleType9regular2blank7replicate == 9)
  
  dir.create(file.path(cached.path,cached.save), showWarnings = FALSE)
  
  write.csv(df_regular, file = file.path(cached.path,cached.save,"summary_noQA.csv"), row.names = FALSE)
  saveRDS(df_regular, file = file.path(cached.path,cached.save,"rds","summary_noQA.rds"))
  filterOptics(df_regular, dfabs, dffl, "_noQA", file.path(cached.path,cached.save))
  meta.file(df_regular, "_noQA",cached.save)

  wastewater.columns <- c("Sanitary","WWTP","WW Influent","Sanitary Grab")
  
  df_WW <- filter(df_regular, VirusAutosampleorSewerGrab %in% wastewater.columns)
  df_regular <- df_regular[!(df_regular$VirusAutosampleorSewerGrab %in% wastewater.columns),]
  
  write.csv(df_regular, file = file.path(cached.path,cached.save,"summary_noWW_noQA.csv"), row.names = FALSE)
  write.csv(df_WW, file = file.path(cached.path,cached.save,"summary_WW_noQA.csv"), row.names = FALSE)
  write.csv(df_QA, file = file.path(cached.path,cached.save,"summary_QA.csv"), row.names = FALSE)
  
  saveRDS(df_regular, file = file.path(cached.path,cached.save,"rds","summary_noWW_noQA.rds"))
  saveRDS(df_WW, file = file.path(cached.path,cached.save,"rds","summary_WW_noQA.rds"))
  saveRDS(df_QA, file = file.path(cached.path,cached.save,"rds","summary_QA.rds"))

  filterOptics(df_regular, dfabs, dffl, "_noWW_noQA", file.path(cached.path,cached.save))
  filterOptics(df_WW, dfabs, dffl, "_WW_noQA", file.path(cached.path,cached.save))
  filterOptics(df_QA, dfabs, dffl, "_QA", file.path(cached.path,cached.save))
  meta.file(df_regular, "_noWW_noQA",cached.save)
  meta.file(df_WW, "_WW_noQA",cached.save)
  meta.file(df_QA, "_QA",cached.save)
}

cached.path <- "cached_data"
cached.save <- "5_process_filterData"

filterData(cached.path, cached.save)