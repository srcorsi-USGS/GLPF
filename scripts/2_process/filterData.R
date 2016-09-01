library(dplyr)

cached.path <- "cached_data"

filterOptics <- function(df, dfabs, dffl, base.name, cached.path){
  
  wwGR <- unique(df$CAGRnumber)
  wwGR <- wwGR[!(wwGR %in% c("x","N/A"))]
  dfabs_filtered <- dfabs[,names(dfabs) %in% c("nm",wwGR)]
  dffl_filtered <- dffl[,names(dffl) %in% c("exem",wwGR)]
  
  write.csv(dfabs_filtered, file = file.path(cached.path,"final",paste0("dfabs",base.name,".csv")), row.names = FALSE)
  write.csv(dffl_filtered, file = file.path(cached.path,"final",paste0("dffl",base.name,".csv")), row.names = FALSE)
  saveRDS(dfabs_filtered, file = file.path(cached.path,"final","rds",paste0("dfabs",base.name,".rds")))
  saveRDS(dffl_filtered, file = file.path(cached.path,"final","rds",paste0("dffl",base.name,".rds")))
  
}

filterData <- function(cached.path){
  df <- readRDS(file.path(cached.path,"merged","mergedData.rds"))
  
  dfabs <- readRDS(file.path(cached.path,"optics","dfabs.rds"))
  dffl <- readRDS(file.path(cached.path,"optics","dffl.rds"))
  
  df_QA <-  filter(df, SampleType9regular2blank7replicate != 9)
  df_regular <- filter(df, SampleType9regular2blank7replicate == 9)
  
  write.csv(df_regular, file = file.path(cached.path,"final","summaryAll_noQA.csv"), row.names = FALSE)
  saveRDS(df_regular, file = file.path(cached.path,"final","rds","summaryAll_noQA.rds"))
  
  filterOptics(df_regular, dfabs, dffl, "All_noQA", cached.path)
  
  # some day, we may want to separate auto samples:
  # df_auto <- filter(df_regular, VirusAutosampleorSewerGrab == "Autosample") 
  # df_regular <- filter(df_regular, VirusAutosampleorSewerGrab != "Autosample")
  
  wastewater.columns <- c("Sanitary","WWTP","WW Influent","Sanitary Grab")
  
  df_WW <- filter(df_regular, VirusAutosampleorSewerGrab %in% wastewater.columns)
  df_regular <- df_regular[!(df_regular$VirusAutosampleorSewerGrab %in% wastewater.columns),]
  
  write.csv(df_regular, file = file.path(cached.path,"final","summaryNoWW_noQA.csv"), row.names = FALSE)
  write.csv(df_WW, file = file.path(cached.path,"final","summaryWW.csv"), row.names = FALSE)
  write.csv(df_QA, file = file.path(cached.path,"final","summaryQA.csv"), row.names = FALSE)
  
  saveRDS(df_regular, file = file.path(cached.path,"final","rds","summaryNoWW_noQA.rds"))
  saveRDS(df_WW, file = file.path(cached.path,"final","rds","summaryWW_noQA.rds"))
  saveRDS(df_QA, file = file.path(cached.path,"final","rds","summaryQA.rds"))

  filterOptics(df_regular, dfabs, dffl, "All_noWW_noQA", cached.path)
  
  filterOptics(df_WW, dfabs, dffl, "_WW_noQA", cached.path)
  
  filterOptics(df_QA, dfabs, dffl, "_QA", cached.path)

}

filterData(cached.path)