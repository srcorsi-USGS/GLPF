library(USGSHydroOpt)
library(dplyr)
library(ggplot2)

cached.path <- "cached_data"
base.name <- "_noQA"
summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
EEMs <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("EEMs3D",base.name,".rds")))

plotOpticalSummaries_byCat <- function(summaryDF, EEMs, cat_rank, 
                                       base.name, 
                                       plotsPerPage = 6, 
                                       intensity.limits = c(0,5)){
  
  dfall <- filter(summaryDF, contamination_rank == cat_rank)

  labIDs <- dfall$CAGRnumber

  colPal <- c(colors()[30], colors()[143],colors()[554])
  
  filenm <- file.path("cached_figures",paste0("EEMs_Cat",cat_rank,base.name,".pdf"))

  page.chunking <- split(1:nrow(dfall), ceiling(seq_along(1:nrow(dfall))/plotsPerPage))
  
  Peaks <- USGSHydroOpt::ex_ems
  
  pdf(filenm, width = 11, height = 7.6)
  
  for(i in seq_len(length(page.chunking))){
    
    mat <- as.data.frame(as.table(EEMs[,,labIDs[page.chunking[[i]]]])) %>%
      mutate(Ex = as.numeric(as.character(Var1)),
             Em = as.numeric(as.character(Var2)),
             ID = if("Var3" %in% names(.)){
               Var3
              } else {
                 labIDs[page.chunking[[i]]]
              }) %>%
      rename(Intensity = Freq) %>%
      filter(!is.na(Intensity)) %>%
      filter(Intensity > 0) %>%
      select(-Var1, -Var2)
    
    heatPage <- ggplot(data = mat, aes(x=Ex, y=Em)) +
      geom_tile(aes(fill = Intensity)) +
      facet_wrap(~ ID) +
      theme_bw() + 
      scale_fill_gradientn(colours=colPal,
                           na.value = "transparent",
                           limits= intensity.limits) +
      xlim(c(240,500)) +
      ylim(c(210, 600)) +
      labs(title = paste("Contamination Rank:",cat_rank)) +
      geom_text(data = Peaks, aes(x = ExCA, y = EmCA, label = Peak))
    
    print(heatPage)
  }
  
  dev.off()
  
}


base.name <- "_noQA"
plotOpticalSummaries_byCat(summaryDF, EEMs, 6, 
                           base.name, intensity.limits = c(0,5))
plotOpticalSummaries_byCat(summaryDF, EEMs, 5, 
                           base.name, intensity.limits = c(0,5))
plotOpticalSummaries_byCat(summaryDF, EEMs, 4, 
                           base.name, intensity.limits = c(0,3))
plotOpticalSummaries_byCat(summaryDF, EEMs, 3, 
                           base.name, intensity.limits = c(0,3))
plotOpticalSummaries_byCat(summaryDF, EEMs, 2, 
                           base.name, intensity.limits = c(0,3))
plotOpticalSummaries_byCat(summaryDF, EEMs, 1, 
                           base.name, intensity.limits = c(0,3))

