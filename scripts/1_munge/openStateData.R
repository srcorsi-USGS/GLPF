library(readxl)
library(data.table)
library(dplyr)
library(dataRetrieval)
library(lubridate)
library(smwrBase)


#Raw data folder:
raw.lab.path <- "raw_data/field_data"
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

###############################################################
# MI:
###############################################################
data.mi <- rename(data.mi,
                  SiteID = `Station ID`,
                  fieldID = `Field ID`,
                  stationNM = `Station Name`)

# Seem to be some duplicated rows:
# z <- data.mi[data.mi$stationNM == "Middle Branch Clinton River at Macomb, MI",]

data.mi <- data.mi[data.mi$fieldID != "CM",]

data.mi.wide <- setDT(data.mi) %>%
  dcast(SiteID + fieldID + stationNM + STATION_NM + RECORD_NO + PROJECT_CD + SAMPLE_START_DT ~
          PARM_NM, value.var = "RESULT_VA") %>%
  setDF()

# dupRows <- c(which(duplicated(data.mi.wide$SAMPLE_START_DT)),
#              which(duplicated(data.mi.wide$SAMPLE_START_DT,fromLast = TRUE)))
# dupRows <- dupRows[order(dupRows)]
# x <- data.mi.wide[dupRows,] %>%
#   arrange(SAMPLE_START_DT)

data.mi.wide <- rename(data.mi.wide, 
                       Station_Name=STATION_NM,
                       FieldID=fieldID) %>%
  mutate(SC = `Specific cond at 25C`,
         WT = `Temperature, water`,
         TB = `Turbidity, Nephelom`,
         DO = `Dissolved oxygen`,
         state = "MI") %>%
  select(SAMPLE_START_DT, FieldID, state, WT, DO, TB, SC, pH) 

saveRDS(data.mi.wide, file.path(cached.path,"dataMI.rds"))

###############################################################
# NY:
###############################################################
data.ny$SAMPLE_START_DT <- fast_strptime(paste(data.ny$Date,data.ny$Time), "%m/%d/%Y %H%M", tz = "UTC", lt=FALSE)
data.ny$state <- "NY" 

data.ny <- rename(data.ny,
                  DO = D.O.)

###############################################################
# WI:
###############################################################
data.wi$SAMPLE_START_DT <- data.wi$SampleCollectionDateTime
# data.wi$SAMPLE_START_DT[is.na(data.wi$SAMPLE_START_DT) | data.wi$SAMPLE_START_DT == ""] <- data.wi$TIMESTAMP[is.na(data.wi$SAMPLE_START_DT) | data.wi$SAMPLE_START_DT == ""]
data.wi$SAMPLE_START_DT <- parse_date_time(data.wi$SAMPLE_START_DT, orders = c("mdY HS", "Ymd H:M:S"))
# data.wi$SAMPLE_START_DT[is.na(data.wi$SAMPLE_START_DT)] <- parse_date_time(paste(data.wi$Sample_Collection_Date[is.na(data.wi$SAMPLE_START_DT)], data.wi$Sensor_Start_Time[is.na(data.wi$SAMPLE_START_DT)]), orders = c("mdY H:M:S", "mdY"))

data.wi.munged <- rename(data.wi, 
                         WT = Temp,
                         TB = Turb,
                         FieldID = Station_Name) %>%
                  mutate(DO = as.numeric(NA)) %>%
                  select(SAMPLE_START_DT, FieldID, state, WT, DO, TB, SC, pH,UVch1,UVch2,UVch3) %>%
                  filter(!(FieldID %in% c("BLK-DI","FLD-BLK","EMPTY"))) 

data.ny.opt.field <- data.wi.munged %>%
  filter(state == "NY") %>%
  select(SAMPLE_START_DT, FieldID, UVch1, UVch2, UVch3) 

data.ny.join <- select(data.ny, SAMPLE_START_DT, FieldID, state, WT, DO, TB, SC, pH) 
data.ny.join$FieldID[data.ny.join$FieldID =="WWRCTN"] <- "WWRCTN-2"
data.ny.join$FieldID[data.ny.join$FieldID =="WWRCTE"] <- "WWRCTE-1or2"
data.ny.join$FieldID[data.ny.join$FieldID =="WWRCTJ"] <- "WWRCTJ-1or2"

data.ny.join$FieldID[nchar(data.ny.join$FieldID) == 6] <- paste0(data.ny.join$FieldID[nchar(data.ny.join$FieldID) == 6],"-1")

data.ny.munged <- left_join(data.ny.join, data.ny.opt.field, by="FieldID") %>%
  select(-SAMPLE_START_DT.y) %>%
  rename(SAMPLE_START_DT=SAMPLE_START_DT.x)
saveRDS(data.ny.munged, file.path(cached.path,"dataNY.rds"))

data.mi.opt.field <- data.wi.munged %>%
  filter(state == "MI")

saveRDS(data.mi.opt.field, file.path(cached.path,"dataMIfieldOpts.rds"))

data.wi.munged <- data.wi.munged %>%
  filter(state == "WI")

saveRDS(data.wi.munged, file.path(cached.path,"dataWI.rds"))


# field.merge <- bind_rows(data.ny.munged, data.wi.munged) 
# indexNA <- field.merge[,c("pH","DO","WT","TB","SC")]
# indexNA <- which(rowSums(is.na(indexNA)) != ncol(indexNA))
# 
# field.merge <- field.merge[indexNA,]
# 
# saveRDS(field.merge, file.path(cached.path,"fieldMerge.rds"))


