library(dplyr)

cached.path <- "cached_data"

filterData <- function(cached.path){
  df <- readRDS(file.path(cached.path,"merged","mergedData.rds"))
  
  df_QA <-  filter(df, SampleType9regular2blank7replicate != 9)
  df_regular <- filter(df, SampleType9regular2blank7replicate == 9)
  
  # df_auto <- filter(df_regular, VirusAutosampleorSewerGrab == "Autosample")
  # 
  # df_regular <- filter(df_regular, VirusAutosampleorSewerGrab != "Autosample")
  wastewater.columns <- c("Sanitary","WWTP","WW Influent","Sanitary Grab")
  
  df_WW <- filter(df_regular, VirusAutosampleorSewerGrab %in% wastewater.columns)
  
  df_regular <- df_regular[!(df_regular$VirusAutosampleorSewerGrab %in% wastewater.columns),]
  
  write.csv(df_regular, file = file.path(cached.path,"final","mergedData.csv"), row.names = FALSE)
  write.csv(df_WW, file = file.path(cached.path,"final","wwData.csv"), row.names = FALSE)
  write.csv(df_QA, file = file.path(cached.path,"final","qaData.csv"), row.names = FALSE)
  
  dfabs <- readRDS(file.path(cached.path,"optics","dfabs.rds"))
  dffl <- readRDS(file.path(cached.path,"optics","dffl.rds"))
  dfabs_filtered <- dfabs[,c("nm",unique(df_regular$CAGRnumber))]
  dffl_filtered <- dffl[,c("exem",unique(df_regular$CAGRnumber))]
  
  write.csv(dfabs_filtered, file = file.path(cached.path,"final","dfabs.csv"), row.names = FALSE)
  write.csv(dffl_filtered, file = file.path(cached.path,"final","dffl.csv"), row.names = FALSE)
  
  wwGR <- unique(df_WW$CAGRnumber)
  wwGR <- wwGR[wwGR != "x"]
  dfabs_filtered <- dfabs[,c("nm",wwGR)]
  dffl_filtered <- dffl[,c("exem",wwGR)]
  
  write.csv(dfabs_filtered, file = file.path(cached.path,"final","dfabs_WW.csv"), row.names = FALSE)
  write.csv(dffl_filtered, file = file.path(cached.path,"final","dffl_WW.csv"), row.names = FALSE)
  
  qaGR <- unique(df_QA$CAGRnumber)
  qaGR <- qaGR[qaGR != "N/A"]
  dfabs_filtered <- dfabs[,c("nm",qaGR)]
  dffl_filtered <- dffl[,names(dffl) %in% c("exem",qaGR)]
  
  write.csv(dfabs_filtered, file = file.path(cached.path,"final","dfabs_QA.csv"), row.names = FALSE)
  write.csv(dffl_filtered, file = file.path(cached.path,"final","dffl_QA.csv"), row.names = FALSE)
  
  
}

filterData(cached.path)