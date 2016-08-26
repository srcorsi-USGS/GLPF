library(dataRetrieval)
library(lubridate)
library(dplyr)

raw.path <- "raw_data"
cached.path <- "cached_data"

mergeFieldOpt <- function(raw.path, cached.path){

  data.opt <- readRDS(file.path(cached.path,"merged","trackingBacteria.rds"))
  
  data.opt <- mutate(data.opt,
                      USGSSTAID = zeroPad(ifelse(is.na(USGSSTAID), "", as.character(USGSSTAID)), padTo = 8),
                      USGSNWISStationIDifapplicable = zeroPad(ifelse(is.na(USGSNWISStationIDifapplicable), "", as.character(USGSNWISStationIDifapplicable)), padTo = 8),
                      startDateTime = parse_date_time(Startdatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")),
                      endDateTime = parse_date_time(Enddatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")))

  ###############################################################
  # NY:
  ###############################################################
  data.ny <- readRDS(file.path(cached.path,"state","dataNY.rds"))
  data.opt.ny <- filter(data.opt, State == "NY")
  
  data.ny$FieldID[data.ny$FieldID == "WWRCTQ-1" & data.ny$WT == 18] <- "WWRCTQ-002"
  data.ny$FieldID[data.ny$FieldID == "WWRCTE-1or2"] <- "WWRCTE-1"
  data.ny$FieldID[data.ny$FieldID == "WWRCTJ-1or2" & data.ny$SC == 1294] <- "WWRCTJ-1"
  data.ny$FieldID[data.ny$FieldID == "WWRCTJ-1or2"] <-"WWRCTJ-003"
  data.ny$FieldID[data.ny$FieldID == "WWRCTG-1" & data.ny$WT == 16.7] <- "WWRCTG-002"
  data.ny$FieldID[data.ny$FieldID == "WWRCTH-1" & data.ny$SC == 1368] <- "WWRCTH-002"
  data.ny$FieldID[data.ny$FieldID == "WWRCTF-1" & data.ny$SC == 1849] <- "WWRCTF-002"

  data.opt.ny$FieldID <- as.character(sapply(data.opt.ny$FieldID, function(x) {
    paste(strsplit(x,split = "-")[[1]][1],
          strsplit(x,split = "-")[[1]][2], sep="-")
    }))
  
  data.ny.merge <- left_join(data.ny, data.opt.ny, by=c("FieldID", "SAMPLE_START_DT"="startDateTime"))
  data.ny.merge <- data.ny.merge[!is.na(data.ny.merge$CAGRnumber),]

  ###############################################################
  # MI:
  ###############################################################
  data.mi <- readRDS(file.path(cached.path,"state","dataMI.rds"))
  data.opt.mi <- filter(data.opt, State == "MI")
  
  data.opt.mi$roundDate <- as.POSIXct(round(data.opt.mi$startDateTime,"min"))
  data.opt.mi$FieldID_sm <- gsub("WW","", data.opt.mi$FieldID)
  data.opt.mi$FieldID_sm <- sapply(strsplit(data.opt.mi$FieldID_sm, "-"), function(x) x[[1]][1])
  
  data.mi.merge <- left_join(data.mi, data.opt.mi, 
                             by=c("SAMPLE_START_DT"="roundDate",
                                  "FieldID"="FieldID_sm")) %>%
    filter(!is.na(CAGRnumber)) %>%
    mutate(startDateTime=SAMPLE_START_DT,
           UVch1 = NA,
           UVch2 = NA,
           UVch3 = NA) 
  
  data.mi.merge <- data.mi.merge[, names(data.ny.merge)]
  
  ###############################################################
  # WI:
  ###############################################################
  data.wi <- readRDS(file.path(cached.path,"state","dataWI.rds"))
  data.opt.wi <- filter(data.opt, State == "WI")
  
  # data.wi.merge <- left_join(data.wi, data.opt.wi, by=c("FieldID"="FieldID",
  #                                                       "SAMPLE_START_DT"="startDateTime")) %>%
  #   filter(!is.na(CAGRnumber))
  data.wi.merge <- left_join(data.wi, data.opt.wi, by=c("FieldID"="FieldID")) %>%
    filter(!is.na(CAGRnumber))
  data.wi.merge <- data.wi.merge[!duplicated(data.wi.merge$CAGRnumber),]
  
  data.wi.merge <- data.wi.merge[, names(data.ny.merge)]
  
  ###############################################################
  # Full merge:
  ###############################################################
  
  data.merge <- bind_rows(data.mi.merge,data.wi.merge,data.ny.merge)
  
  cuts <- c(0,3,6,9)
  labs <- c("Winter", "Spring", "Summer", "Fall")
  months <- as.POSIXlt(data.merge$pdate)$mon
  months <- months + 1
  months[months == 12] <- 0
  
  data.merge$Season <- labs[findInterval(months,cuts)]
  names(data.merge) <- enc2utf8(names(data.merge))

  data.merge.check <- select(data.merge, CAGRnumber, State, FieldID, 
                             USGSNWISStationID, VolumeFilteredL, Date,
                             FilterA04µMUSGSMIBARL,FilterB02µMUWMSFS,
                             Site,USGSNWISStationIDifapplicable,USGSSTAID,
                             SAMPLE_START_DT, endDateTime,eventNumInd,
                             Startdatetimemmddyyhhmm, Enddatetimemmddyyhhmm,
                             pdate, date, pedate,Time,Season,
                             MIBARLID,UWMFT,hydroCondition,
                             Comments, Comments2,VirusAutosampleorSewerGrab,IfTributarySewerOutfallManholeorDitch,
                             project,Matrix,SampleType,eventNum,
                             SampleType9regular2blank7replicate,
                             FilterAVolumemL,FilterBVolumemL,FilterAUSGSMIBARLVolumemL,FilterBUWMVolumemL,TotalAutoSamplerVolumeL)
  
  saveRDS(data.merge.check, file.path(cached.path,"merged","mergeCheck.rds"))
  
  merged.data <- data.merge[,names(data.merge)[!(names(data.merge) %in% names(data.merge.check))]]
  
  merged.data <- cbind(data.merge[,c("CAGRnumber","SAMPLE_START_DT","endDateTime","FieldID",
                                     "MIBARLID","UWMFT","eventNum",
                                     "Comments","hydroCondition","State","Season",
                                     "USGSNWISStationIDifapplicable",
                                     "IfTributarySewerOutfallManholeorDitch",
                                     "VirusAutosampleorSewerGrab","project")], 
                       merged.data) %>%
    rename(SiteID=USGSNWISStationIDifapplicable,
           pdate = SAMPLE_START_DT,
           pedate = endDateTime)
  
  saveRDS(merged.data, file.path(cached.path,"merged","mergedData.rds"))
  
  write.csv(merged.data, file.path(cached.path,"merged","mergedData.csv"), row.names = FALSE)
  write.csv(data.merge.check, file.path(cached.path,"merged","mergedDataSupplement.csv"), row.names = FALSE)
  
  #############################
  #No new data in NY:
  data.ny.merge.full <- filter(merged.data, State == "WI")
  
  # write.csv(data.ny.merge.full, file.path(cached.path,"merged","dataNYmergeFull.csv"), row.names = FALSE)
  saveRDS(data.ny.merge.full, file.path(cached.path,"merged","dataNYmergeFull.rds"))
  
  #############################
  data.wi.full <- readRDS(file.path(cached.path,"state","dataWI_allData.rds"))
  
  merged.data.wi <- filter(merged.data, State == "WI") %>%
    select(-WT,-DO,-Turb,-SC,-pH,-UVch1,-UVch2,-UVch3)
  
  data.wi.merge.full <- left_join(data.wi.full, merged.data.wi, 
                                   by=c("FieldID"))
  
  # write.csv(data.wi.merge.full, file.path(cached.path,"merged","dataWImergeFull.csv"), row.names = FALSE)
  saveRDS(data.wi.merge.full, file.path(cached.path,"merged","dataWImergeFull.rds"))
  
  
  #############################
  data.mi.full <- readRDS(file.path(cached.path,"state","dataMI_allData.rds"))
  
  merged.data.mi <- filter(merged.data, State == "MI") %>%
    select(-WT,-DO,-Turb,-SC,-pH,-UVch1,-UVch2,-UVch3)
  
  data.mi.merge.full <- left_join(data.mi.full, merged.data.mi, 
                                  by=c("SAMPLE_START_DT"="pdate", "SiteID"))
  
  # write.csv(data.mi.merge.full, file.path(cached.path,"dataMImergeFull.csv"), row.names = FALSE)
  saveRDS(data.mi.merge.full, file.path(cached.path,"dataMImergeFull.rds"))
}

mergeFieldOpt(raw.optics, cached.path)
