library(smwrBase)
library(dataRetrieval)
library(dplyr)

raw.optics <- "raw_data/optics"
cached.path <- "cached_data"

# Optics:
load(file.path(raw.optics,"GLPFWWMay032016summary.RData"))

data.opt <- mutate(dfOpt, 
                   USGSSTAID = zeroPad(ifelse(is.na(USGSSTAID), "", as.character(USGSSTAID)), padTo = 8),
                   USGSNWISStationIDifapplicable = zeroPad(ifelse(is.na(USGSNWISStationIDifapplicable), "", as.character(USGSNWISStationIDifapplicable)), padTo = 8),
                   startDateTime = parse_date_time(Startdatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")),
                   endDateTime = parse_date_time(Enddatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")))
rm(dfOpt)
###############################################################
# NY:
###############################################################
data.ny <- readRDS(file.path(cached.path,"dataNY.rds"))
data.opt.ny <- filter(data.opt, State == "NY")

data.ny.merge <- mergeNearest(left = data.ny, right = data.opt.ny, 
                              dates.left = "SAMPLE_START_DT",
                              all.left = FALSE,
                              dates.right = "startDateTime",
                              max.diff = "1 days")
# All the optics data doesn't have a match...
# data.checks.ny <- data.ny.merge[,c("SAMPLE_START_DT","startDateTime", "FieldID.left", "FieldID.right")]

###############################################################
# MI:
###############################################################
data.mi <- readRDS(file.path(cached.path,"dataMI.rds"))
data.opt.mi <- filter(data.opt, State == "MI")
data.mi.opt.field <- readRDS(file.path(cached.path,"dataMIfieldOpts.rds"))

data.mi.merge <- mergeNearest(left = data.mi, right = data.opt.mi, 
                              dates.left = "SAMPLE_START_DT",
                              all.left = FALSE,
                              dates.right = "startDateTime",
                              max.diff = "1 days")

data.mi.merge <- left_join(data.mi.merge, 
               select(data.mi.opt.field,
                      SAMPLE_START_DT,FieldID,
                      UVch1,UVch2,UVch3,
                      CADeepUVch1, CADeepUVch2, CADeepUVch2,
                      C7PeakT,C7PeakC), 
               by=c("USGSFieldID"="FieldID")) %>%
  rename(SAMPLE_START_DT=SAMPLE_START_DT.x) %>%
  select(-SAMPLE_START_DT.y)

data.mi.merge <- data.mi.merge[, names(data.ny.merge)]

###############################################################
# WI:
###############################################################
data.wi <- readRDS(file.path(cached.path,"dataWI.rds"))
data.opt.wi <- filter(data.opt, State == "WI")

data.wi.merge <- right_join(data.wi, data.opt.wi,
                           by=c("FieldID"="USGSFieldID"))
                                # "SAMPLE_START_DT"="startDateTime"))

###############################################################
# Full merge:
###############################################################
rm(data.mi,data.mi.opt.field,data.ny,data.opt.mi,data.opt.ny,data.opt.wi, data.wi)

data.merge <- bind_rows(data.mi.merge,data.wi.merge,data.ny.merge)

# data.merge <- full_join(data.mi.merge, data.ny.merge)
# data.merge <- full_join(data.merge, data.wi.merge)

data.merge.check <- select(data.merge, state, State, FieldID, USGSFieldID, fieldID, FieldID.left, 
                             FieldID.right,FieldID.y,FilterA04µMUSGSMIBARL,FilterB02µMUWMSFS,
                             Site,USGSNWISStationIDifapplicable,USGSSTAID,
                             SAMPLE_START_DT, startDateTime, endDateTime,
                             Startdatetimemmddyyhhmm, Enddatetimemmddyyhhmm,
                             pdate, date, pedate,Time,CAGRnumber, MIBARLID,UWMFT,
                           Comments, Comments2,hydroCondition,VirusAutosampleorSewerGrab,IfTributarySewerOutfallManholeorDitch,
                           project,Matrix,SampleType,eventNum,plotCol,plotCol2,
                           YSIPrototypeSensorUsed0NO1YES,SampleType9regular2blank7replicate,
                           FilterAVolumemL,FilterBVolumemL,FilterAUSGSMIBARLVolumemL,FilterBUWMVolumemL,TotalAutoSamplerVolumeL)

saveRDS(data.merge.check, file.path(cached.path,"mergeCheck.rds"))

merged.data <- data.merge[,names(data.merge)[!(names(data.merge) %in% names(data.merge.check))]]
merged.data <- cbind(data.merge[,c("SAMPLE_START_DT","USGSFieldID","USGSNWISStationIDifapplicable")], merged.data) %>%
  rename(SiteID=USGSNWISStationIDifapplicable,
         pdate = SAMPLE_START_DT)


saveRDS(merged.data, file.path(cached.path,"mergedData.rds"))
write.csv(merged.data, file.path(cached.path,"mergedData.csv"), row.names = FALSE)

# data.table solution...look into more in future:
# From: http://stackoverflow.com/questions/23342647/how-to-match-by-nearest-date-from-two-data-frames
# dt.mi <- setDT(data.mi)
# dt.opt.mi <- setDT(data.opt.mi)
# 
# dt.mi[, date.sample := SAMPLE_START_DT]  ## create a duplicate of 'date1'
# dt.opt.mi[, date.opt := startDateTime]  ## create a duplicate of 'date1'
# setkey(dt.mi, date.sample)    ## set the column to perform the join on
# setkey(dt.opt.mi, date.opt)    ## same as above
# 
# ans <- dt.mi[dt.opt.mi, roll=Inf] ## perform rolling join
