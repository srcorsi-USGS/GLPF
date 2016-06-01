library(readxl)
library(data.table)
library(dplyr)
library(dataRetrieval)
library(lubridate)
library(smwrBase)

#Raw data folder:
raw.lab.path <- "raw_data/state_lab"
raw.optics <- "raw_data/optics"
cached.path <- "cached_data"

#MI:
file.mi <- "GLPF.NWIS.MI.xlsx"
data.mi <- read_excel(file.path(raw.lab.path,file.mi))

#NY:
file.ny <- "NY_CHEMICAL_PARAMETER_DATA.xlsx"
data.ny <- read_excel(file.path(raw.lab.path,file.ny), na = "X")
data.ny[,4:8] <- sapply(data.ny[,4:8], function(x) as.numeric(gsub(",","", x)))

#WI:
file.wi <- "KK_Compiled_Sensor Data20160513.csv"
data.wi <- setDF(fread(file.path(raw.lab.path,file.wi)))

# Optics:
load(file.path(raw.optics,"GLPFWWMay032016summary.RData"))

data.opt <- mutate(dfOpt, 
                   USGSSTAID = zeroPad(ifelse(is.na(USGSSTAID), "", as.character(USGSSTAID)), padTo = 8),
                   USGSNWISStationIDifapplicable = zeroPad(ifelse(is.na(USGSNWISStationIDifapplicable), "", as.character(USGSNWISStationIDifapplicable)), padTo = 8),
                   startDateTime = parse_date_time(Startdatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")),
                   endDateTime = parse_date_time(Enddatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")))


###############################################################
# MI:
###############################################################
data.opt.mi <- filter(data.opt, State == "MI")

data.mi.merge <- mergeNearest(left = data.mi, right = data.opt.mi, 
                              dates.left = "SAMPLE_START_DT",
                              all.left = FALSE,
                              dates.right = "startDateTime",
                              max.diff = "1 days")

data.checks.mi <- data.mi.merge[,c("SAMPLE_START_DT","startDateTime", "Field ID", "USGSFieldID")]

saveRDS(data.mi, file.path(cached.path,"dataMI.rds"))

###############################################################
# NY:
###############################################################
data.opt.ny <- filter(data.opt, State == "NY")
data.ny$SAMPLE_START_DT <- fast_strptime(paste(data.ny$Date,data.ny$Time), "%m/%d/%Y %H%M", tz = "UTC", lt=FALSE)

data.ny.merge <- mergeNearest(left = data.ny, right = data.opt.ny, 
                              dates.left = "SAMPLE_START_DT",
                              all.left = FALSE,
                              dates.right = "startDateTime",
                              max.diff = "1 days")

data.checks.ny <- data.ny.merge[,c("SAMPLE_START_DT","startDateTime", "FieldID.left", "FieldID.right")]

saveRDS(data.ny, file.path(cached.path,"dataNY.rds"))

###############################################################
# WI:
###############################################################
data.opt.wi <- filter(data.opt, State == "WI")

data.wi$SAMPLE_START_DT <- data.wi$SampleCollectionDateTime
data.wi$SAMPLE_START_DT[is.na(data.wi$SAMPLE_START_DT) | data.wi$SAMPLE_START_DT == ""] <- data.wi$TIMESTAMP[is.na(data.wi$SAMPLE_START_DT) | data.wi$SAMPLE_START_DT == ""]
data.wi$SAMPLE_START_DT <- parse_date_time(data.wi$SAMPLE_START_DT, orders = c("mdY HS", "Ymd H:M:S"))
data.wi$SAMPLE_START_DT[is.na(data.wi$SAMPLE_START_DT)] <- parse_date_time(paste(data.wi$Sample_Collection_Date[is.na(data.wi$SAMPLE_START_DT)], data.wi$Sensor_Start_Time[is.na(data.wi$SAMPLE_START_DT)]), orders = c("mdY H:M:S", "mdY"))

data.wi.merge <- mergeNearest(left = data.wi, right = data.opt.wi, 
                              dates.left = "SAMPLE_START_DT",
                              all.left = FALSE,
                              dates.right = "startDateTime",
                              max.diff = "1 days")

data.checks.wi <- data.wi.merge[,c("SAMPLE_START_DT","startDateTime")]

saveRDS(data.wi, file.path(cached.path,"dataWI.rds"))


data.merge <- full_join(data.mi.merge, data.ny.merge)
data.merge <- full_join(data.merge, data.wi.merge)

data.merge.reorder <- select(data.merge, FieldID, USGSFieldID, FieldID, `Field ID`, fieldID, FieldID.left, 
                             FieldID.right,FilterA04µMUSGSMIBARL,FilterB02µMUWMSFS,
                             Site,Station_Name, Station_Name.1,`Station Name`,STATION_NM,
                             `Station ID`, USGSNWISStationIDifapplicable,USGSSTAID,
                             SAMPLE_START_DT, startDateTime, endDateTime,
                             Startdatetimemmddyyhhmm, Enddatetimemmddyyhhmm,
                             pdate, Sample_Collection_Date, date,Time,Time.left,Time.right, 
                             Sample_Analysis_Date)

sum(is.na(data.merge.reorder$SAMPLE_START_DT))
sum(is.na(data.merge.reorder$fieldID))
sum(is.na(data.merge.reorder$`Station ID`))
sum(is.na(data.merge.reorder$Station_Name))

merged.data <- data.merge[,names(data.merge)[!(names(data.merge) %in% names(data.merge.reorder))]]
merged.data <- cbind(data.merge[,c("SAMPLE_START_DT","fieldID","Station ID","STATION_NM")], merged.data)

merged.data <- rename(merged.data,
                      pdate = SAMPLE_START_DT,
                      SiteID = `Station ID`)

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

