newCategories <- function(cached.path, base.name, cached.save){
  summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
  
  
  
  saveRDS(dfOpt,file=file.path(cached.path,cached.save,"rds",paste0("summary",base.name,".rds")))
  write.csv(dfOpt,file=file.path(cached.path,cached.save,paste0("summary",base.name,".csv")),row.names=FALSE)
  
}

cached.path <- "cached_data"
base.name <- "_noQA"
cached.save <- "8_process_new_categories"
