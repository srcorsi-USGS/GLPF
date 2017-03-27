library(dplyr)

cached.path <- "cached_data"
base.name <- "_noQA"

summaryDF <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("summary",base.name,".rds")))

sumTable <- summaryDF %>%
  select(pdate, eventNum, hydroCondition, sampleCat2) %>%
  group_by(eventNum, sampleCat2) %>%
  summarise(date = as.Date(mean(pdate, na.rm = TRUE)),
            count = n()) %>%
  arrange(eventNum, desc(count))

freqTable <- summaryDF %>%
  select(eventNum) %>%
  group_by(eventNum) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count >= 10)

#Summary table of events:
sumEvent <- filter(summaryDF, eventNum %in% freqTable$eventNum) %>%
  select(eventNum, CAGRnumber)


plotOpticalSummaries_byCat <- function(summaryDF, EEMs, cat_rank, 
                                       base.name, facet_label = "USGSFieldID",
                                       plotsPerPage = 6, 
                                       intensity.limits = c(0,5), filenm="test.pdf"){
  
  dfall <- filter(summaryDF, contamination_rank %in% cat_rank)
  
  labIDs <- dfall$CAGRnumber
  label_df <- data.frame(labels = dfall[[facet_label]],
                         labID = labIDs, stringsAsFactors = FALSE)
  colPal <- c(colors()[30], colors()[143],colors()[554])
  
  # filenm <- file.path("cached_figures",paste0("EEMs_Cat",paste(cat_rank,collapse = ","),base.name,".pdf"))
  
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

for(i in unique(freqTable$eventNum)){

  subSum <- filter(summaryDF, eventNum %in% i) %>%
    mutate(label = paste(eventNum, CAGRnumber, sampleCat2, as.character(contamination_rank))) %>%
    select(label, everything())
  
  EEMs <- readRDS(file.path(cached.path,"7_process_summarize_optics","rds",paste0("EEMs3D",base.name,".rds")))
  
  plotOpticalSummaries_byCat(subSum, EEMs, 1:6, facet_label = "label",
                             base.name, intensity.limits = c(0,5),
                             filenm = paste0(i,".pdf"))
}

