library(readxl)
library(data.table)
library(dplyr)
library(dataRetrieval)
library(lubridate)
library(smwrBase)

#MI:
file.mi <- "GLPF.NWIS.MI.xlsx"
data.mi <- read_excel(file.path("data",file.mi))

#NY:
file.ny <- "NY_CHEMICAL_PARAMETER_DATA.xlsx"
data.ny <- read_excel(file.path("data",file.ny), na = "X")
data.ny[,4:8] <- sapply(data.ny[,4:8], function(x) as.numeric(gsub(",","", x)))

#WI?
file.wi <- "KK_Compiled_Sensor Data20160513.csv"
data.wi <- setDF(fread(file.path("data",file.wi)))

# Optics:
load(file.path("data","GLPFWWMay032016summary.RData"))

data.opt <- mutate(dfOpt, USGSSTAID = zeroPad(USGSSTAID,padTo = 8),
                   USGSNWISStationIDifapplicable = zeroPad(USGSNWISStationIDifapplicable,padTo = 8),
                   startDateTime = parse_date_time(Startdatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")),
                   endDateTime = parse_date_time(Enddatetimemmddyyhhmm, c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")))
                   # date = as.Date(date, format="%m/%d/%Y")) 

data.opt$startDateTime[is.na(data.opt$startDateTime)]

rm(file.wi, file.ny, file.mi)
rm(dfOpt)

######################
#data exploration:
# data.mi$FieldID <- paste0("WW",data.mi$`Field ID`) 
#too many missing opt FieldID's in opt data
#USGSSTAID's have tons of NA'a

# I think you almost have to go nearest date?
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

data.merge <- full_join(data.mi.merge, data.ny.merge)
data.merge <- full_join(data.merge, data.wi.merge)


saveRDS(data.merge, "M:/QW Monitoring Team/GLPF/Data/R/cacheMerge/data.merge.rds")

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

