library(readxl)
library(data.table)
library(dplyr)
library(dataRetrieval)
library(lubridate)

#Raw data folder:
raw.lab.path <- "raw_data/field_data"
cached.path <- "cached_data"

###############################################################
# WI:
###############################################################
file.wi <- "KK_Compiled_Sensor Data20160623.csv"
data.wi <- setDF(fread(file.path(raw.lab.path,file.wi)))

data.wi$SAMPLE_START_DT <- data.wi$SampleCollectionDateTime
data.wi$SAMPLE_START_DT[is.na(data.wi$SAMPLE_START_DT)] <- data.wi$TIMESTAMP[is.na(data.wi$SAMPLE_START_DT)]
data.wi$SAMPLE_START_DT[!is.na(data.wi$SAMPLE_START_DT) & data.wi$SAMPLE_START_DT == ""] <- data.wi$TIMESTAMP[!is.na(data.wi$SAMPLE_START_DT) & data.wi$SAMPLE_START_DT == ""]
data.wi$SAMPLE_START_DT <- parse_date_time(data.wi$SAMPLE_START_DT, orders = c("mdY HM", "Ymd H:M:S"))
data.wi$SAMPLE_START_DT <- as.POSIXct(round(data.wi$SAMPLE_START_DT, "min"))
  
data.wi.field.opts.full <- rename(data.wi, 
                         WT = Temp,
                         FieldID = Station_Name) %>%
  filter(!(FieldID %in% c("BLK-DI","BLANK","FLD-BLK","FLDBLK","WWKKSFLBBLK","EMPTY","WWKKSFblk-02","WWFBLK-11",
                          "TEA","TEA1","TEA2","TEA3","Q","Q1","Q2","Q3",
                          "GREEN","BLUE","YELLOW","BLUE2",
                          "DI","DI1","DI2","DI3","DI2A",
                          "WWRCTL-S02-BLANK","RCTPS01BLANK","WWRCTL-03-BLANK","WWRCTP-D02-BLANK",
                          "WWRCTPS70-BLANK","VAN LARE WWTP","RCTPS02-BLANK")))

data.wi.field.all <- data.wi.field.opts.full %>% 
  filter(state == "WI") %>%
  filter(Filtered == "0") %>%
  mutate(DO = as.numeric(NA)) 

data.wi.field.opts <- select(data.wi.field.opts.full, FieldID, state, UVch1,UVch2,UVch3)
data.wi.field <- select(data.wi.field.all, SAMPLE_START_DT, FieldID, WT, DO, Turb, SC, pH, UVch1, UVch2, UVch3)

saveRDS(data.wi.field, file.path(cached.path,"dataWI.rds"))
saveRDS(filter(data.wi.field.opts.full, state == "WI"), file.path(cached.path,"dataWI_allData.rds"))

###############################################################
# MI:
###############################################################
file.mi <- "GLPF.NWIS.MI.xlsx"
data.mi <- read_excel(file.path(raw.lab.path,file.mi))

data.mi$SAMPLE_START_DT <- as.POSIXct(round(data.mi$SAMPLE_START_DT,"min"))

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

data.mi.wide.all <- rename(data.mi.wide, 
                       Station_Name=STATION_NM,
                       FieldID=fieldID) %>%
  rename(SC = `Specific cond at 25C`,
         WT = `Temperature, water`,
         DO = `Dissolved oxygen`) %>%
  mutate(Turb = ifelse(!is.na(`Turbidity, Nephelom`),`Turbidity, Nephelom`,`Turbidity, NephRatio`))

data.mi.wide <- select(data.mi.wide.all, SAMPLE_START_DT, FieldID, WT, DO, Turb, SC, pH) 

file.mi.new <- "June2016PhysicalParameters_GLPF.xlsx"
data.mi.new <- read_excel(file.path(raw.lab.path,file.mi.new))

data.mi.new.cleaned <- data.mi.new %>%
  rename(SC = `SpCond (00095)`,
         WT = `Temp, Water (00010)`,
         DO = `DO (00300)`,
         Turb = `Turbidity (63676)`,
         pH = `pH (00400)`,
         SAMPLE_START_DT = `Date/Time`,
         `Temperature, air`= `Temp, Air (00020)`,
         `E coli, DSTM, water` = `E Coli (50468)`,
         `Air pressure` = `Barometric Pressure (00025)`,
         `Total coliforms, DSTM, water` = `Total Coliforms (50569)`,
         Station_Name=`Station Name`) %>%
  filter(`Sample Code` == 9) %>%
  mutate(`Total coliforms, DSTM, water` = as.numeric(`Total coliforms, DSTM, water`),
         `E coli, DSTM, water` = as.numeric(`E coli, DSTM, water`),
         `Total coliforms, DSTM, water REMARK` = ifelse(is.na(`Total coliforms, DSTM, water`), "<",""))
  
data.mi.wide <- bind_rows(data.mi.wide, 
                          select(data.mi.new.cleaned, SAMPLE_START_DT, FieldID, WT, DO, Turb, SC, pH))
  
data.mi.wide.all <- bind_rows(data.mi.wide.all, data.mi.new.cleaned)
saveRDS(data.mi.wide, file.path(cached.path,"dataMI.rds"))
saveRDS(data.mi.wide.all, file.path(cached.path,"dataMI_allData.rds"))


data.mi.opt.field <- data.wi.field.opts %>%
  filter(state == "MI") %>%
  select(-state)

saveRDS(data.mi.opt.field, file.path(cached.path,"dataMIfieldOpts.rds"))

###############################################################
# NY:
###############################################################
file.ny <- "CHEMICAL_PARAMETER_DATA.xlsx"
data.ny <- read_excel(file.path(raw.lab.path,file.ny), na = "X")

data.ny[,4:8] <- sapply(data.ny[,4:8], function(x) as.numeric(gsub(",","", x)))

data.ny$SAMPLE_START_DT <- as.POSIXct(round(fast_strptime(paste(data.ny$Date,data.ny$Time), "%m/%d/%Y %H%M", tz = "UTC", lt=FALSE),"min"))

data.ny <- rename(data.ny,
                  DO = D.O.,
                  Turb = TB)

data.ny.opt.field <- data.wi.field.opts %>%
  filter(state == "NY") %>%
  select(-state)

data.ny.join <- select(data.ny, SAMPLE_START_DT, FieldID, WT, DO, Turb, SC, pH) 
data.ny.join$FieldID[data.ny.join$FieldID =="WWRCTN"] <- "WWRCTN-2"
data.ny.join$FieldID[data.ny.join$FieldID =="WWRCTE"] <- "WWRCTE-1or2"
data.ny.join$FieldID[data.ny.join$FieldID =="WWRCTJ"] <- "WWRCTJ-1or2"
data.ny.join$FieldID[nchar(data.ny.join$FieldID) == 6] <- paste0(data.ny.join$FieldID[nchar(data.ny.join$FieldID) == 6],"-1")

data.ny.munged <- left_join(data.ny.join, data.ny.opt.field, 
                            by=c("FieldID")) 

saveRDS(data.ny.munged, file.path(cached.path,"dataNY.rds"))


