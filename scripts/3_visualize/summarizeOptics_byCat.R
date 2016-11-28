library(USGSHydroOpt)
library(dplyr)
library(ggplot2)
library(tidyr)

cached.path <- "cached_data"
base.name <- "_noQA"
summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))
EEMs <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("EEMs3D",base.name,".rds")))

plotOpticalSummaries_byCat <- function(summaryDF, EEMs, cat_rank, 
                                       base.name, facet_label = "USGSFieldID",
                                       plotsPerPage = 6, 
                                       intensity.limits = c(0,5)){
  
  dfall <- filter(summaryDF, contamination_rank %in% cat_rank)

  labIDs <- dfall$CAGRnumber
  label_df <- data.frame(labels = dfall[[facet_label]],
                         labID = labIDs, stringsAsFactors = FALSE)
  colPal <- c(colors()[30], colors()[143],colors()[554])
  
  filenm <- file.path("cached_figures",paste0("EEMs_Cat",paste(cat_rank,collapse = ","),base.name,".pdf"))

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
      left_join(label_df, by=c("ID"="labID")) %>%
      rename(Intensity = Freq) %>%
      filter(!is.na(Intensity)) %>%
      filter(Intensity > 0) %>%
      select(-Var1, -Var2)
    
    heatPage <- ggplot(data = mat, aes(x=Ex, y=Em)) +
      geom_tile(aes(fill = Intensity)) +
      facet_wrap(~ labels) +
      theme_bw() + 
      scale_fill_gradientn(colours=colPal,
                           na.value = "transparent",
                           limits= intensity.limits) +
      xlim(c(240,500)) +
      ylim(c(210, 600)) +
      labs(title = paste("Contamination Rank:",paste(cat_rank,collapse = ","))) +
      geom_text(data = Peaks, aes(x = ExCA, y = EmCA, label = Peak))
    
    print(heatPage)
  }
  
  dev.off()
  
}

plotOpticalSummaries_byCat(summaryDF, EEMs, 6, 
                           base.name, intensity.limits = c(0,5))
plotOpticalSummaries_byCat(summaryDF, EEMs, 5, 
                           base.name, intensity.limits = c(0,5))
plotOpticalSummaries_byCat(summaryDF, EEMs, c(3,4), 
                           base.name, 
                           intensity.limits = c(0,3),
                           plotsPerPage = 12)
plotOpticalSummaries_byCat(summaryDF, EEMs, 2, 
                           base.name, intensity.limits = c(0,3))
plotOpticalSummaries_byCat(summaryDF, EEMs, 1, 
                           base.name, intensity.limits = c(0,3))

# onlyWW <- summaryDF %>%
  # filter(sampleCat1 == "sewage")

onlyWW <- summaryDF %>%
  filter(sampleCat1 == "sewage") %>%
  filter(VirusAutosampleorSewerGrab %in% c("WW Influent","WWTP"))

plotOpticalSummaries_byCat(onlyWW, EEMs, 6, 
                           base.name="Sanitary", 
                           intensity.limits = c(0,5))


onlyWW <- summaryDF %>%
  filter(sampleCat1 == "sewage") %>%
  filter(VirusAutosampleorSewerGrab %in% c("Sanitary","Sanitary Grab"))


plotOpticalSummaries_byMed <- function(dfall, EEMs, plotTitle, 
                                       base.name, 
                                       plotsPerPage = 6, 
                                       intensity.limits = c(0,5)){
  
  labIDs <- dfall$CAGRnumber
  label_df <- data.frame(labels = dfall$USGSFieldID,
                       labID = labIDs, stringsAsFactors = FALSE)
  
  colPal <- c(colors()[30], colors()[143],colors()[554])
  filenm <- file.path("cached_figures",paste0(plotTitle,base.name,".pdf"))
  Peaks <- USGSHydroOpt::ex_ems
  
  pdf(filenm, width = 11, height = 7.6)
    
  mat <- as.data.frame(as.table(EEMs[,,labIDs])) %>%
    mutate(Ex = as.numeric(as.character(Var1)),
           Em = as.numeric(as.character(Var2)),
           ID = if("Var3" %in% names(.)){
             Var3
           } else {
             labIDs[page.chunking[[i]]]
           }) %>%
    # left_join(label_df, by=c("ID" = "labID")) %>%
    rename(Intensity = Freq) %>%
    filter(!is.na(Intensity)) %>%
    filter(Intensity > 0) %>%
    select(-Var1, -Var2) %>%
    group_by(Ex, Em) %>%
    summarise(median = median(Intensity, na.rm = TRUE),
              mean = mean(Intensity, na.rm = TRUE)) %>%
    gather(stat, Int_va, -Ex, -Em)
  
  heatPage <- ggplot(data = mat, aes(x=Ex, y=Em)) +
    geom_tile(aes(fill = Int_va)) +
    facet_wrap(~ stat) +
    theme_bw() + 
    scale_fill_gradientn(colours=colPal,
                         na.value = "transparent",
                         limits= intensity.limits) +
    xlim(c(240,500)) +
    ylim(c(210, 600)) +
    labs(title = plotTitle) +
    geom_text(data = Peaks, aes(x = ExCA, y = EmCA, label = Peak))
  
  print(heatPage)
  dev.off()
  
}

onlyWW <- summaryDF %>%
  filter(sampleCat1 == "sewage") %>%
  filter(VirusAutosampleorSewerGrab %in% c("WW Influent","WWTP"))

plotOpticalSummaries_byMed(onlyWW, EEMs, "WW Influent", 
                           base.name, intensity.limits = c(0,5))

cat1 <- summaryDF %>%
  filter(sampleCat1 != "sewage") %>%
  filter(contamination_rank == 1)

plotOpticalSummaries_byMed(cat6, EEMs, "Contamination Rank 6, No WW", 
                           base.name, intensity.limits = c(0,5))

plotOpticalSummaries_byMed(cat5, EEMs, "Contamination Rank 5, No WW", 
                           base.name, intensity.limits = c(0,5))
plotOpticalSummaries_byMed(cat1, EEMs, "Contamination Rank 1, No WW", 
                           base.name, intensity.limits = c(0,5))
