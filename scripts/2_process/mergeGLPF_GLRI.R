#Raw data folder:
cached.path <- "cached_data"
raw.path <- "raw_data"

library(stringr)
library(dplyr)
library(data.table)

load(file=file.path(cached.path,'GLRIWWMar162016summary.RData'))
load(file=file.path(cached.path,'GLRI01-04-16_mergedBact.RData'))
load(file=file.path(cached.path,'glpfBact2016-06-02.Rdata'))
glriNames <- setDF(fread(file.path(raw.path,'glriNames.csv')))

# GLRI optics:
load(file.path(cached.path,'GLRIWWOptFlAbsVectorized.RData'))
dfabsGLRI <- dfabs
dfflGLRI <- dffl

rm(dffl)
rm(dfabs)

#Additional data is available from spring/summer 2016 for this that needs to be appended
dfabsGLPF <- setDF(fread(file.path(raw.path,"optics","Abs.csv")))
names(dfabsGLPF)[1] <- "nm"
dfflGLPF <- setDF(fread(file.path(raw.path,"optics","Fl.csv")))
names(dfflGLPF)[1] <- "exem"


#Additional data is available from spring/summer 2016 for this that needs to be appended
# load(file.path(cached.path,'GLPFWWOptFlAbsVectorized12312015.RData'))
# dfabsGLPF <- dfabs
# dfflGLPF <- dffl

################################################################
dfglri <- dfOpt
dfglri$State <- gsub(' ','',dfglri$State)
dfglri$pdate <- as.POSIXct(dfglri$Start.date.time..mm.dd.yy.hh.mm.,format='%m/%d/%Y %H:%M')
dfglri$pdate <- as.POSIXct(format(as.POSIXct(dfglri$pdate),tz="GMT",usetz=TRUE),tz="GMT")
GMTOffset <- ifelse(dfglri$State=='WI',6,5)
dfglri$pdate <- dfglri$pdate + GMTOffset*60*60

dfglpf <- df

names(dfglri) <- gsub('\\.','',names(dfglri))
names(dfglri) <- gsub('\\?','',names(dfglri))

## convert MI bacteria to numeric ##
dfglpf$esp <- sub(pattern = '<LRL','0',dfglpf$Esp2cn100ml)
dfglpf$ipaH <- sub(pattern = '<LRL','0',dfglpf$ipaHcn100ml)
dfglpf$esp <- sub(pattern = 'na',NA,dfglpf$esp)
dfglpf$ipaH <- sub(pattern = 'na',NA,dfglpf$ipaH)
# ***** Need to change when "v" is defined ********** #
dfglpf$esp <- as.numeric(sub(pattern = 'v',NA,dfglpf$esp))
dfglpf$ipaH <- as.numeric(sub(pattern = 'v',NA,dfglpf$ipaH))


##### Reconcile differences in column names ##########
# common <- names(dfglpf) %in% names(dfglri)
# 
# write.csv(names(dfglri),file='glriNamesCheck3.csv')
# 
# write.csv(data.frame(names(dfglpf),common),file='glpfNamesCheck3.csv')

dfglri$project <- 'GLRI'
dfglpf$project <- 'GLPF'

# use FilterA04?MUSGSMIBARL for USGSFieldID GLRI and GLPF data

#names(dfglri) %in% glriNames$glriNamesOrig
#glriNames$glriNamesOrig %in% names(dfglri)
names(dfglri) <- glriNames$glriNamesNew


df <- merge(dfglri,dfglpf,all=TRUE)
df$fieldID <- ifelse(is.na(df$USGSFieldID),df$FilterB02µMUWMSFS,df$USGSFieldID)
df$eventNum <- gsub('-','',str_sub(df$fieldID,-2,-1))
#df$FilterA04?MUSGSMIBARL

df$Site <- gsub('-','',str_sub(df$fieldID,1,-3))

saveRDS(df,file.path(cached.path,'glriglpf.rds'))

########################################################################
# Combine the fluorescence and absorbance results for GLRI and GLPF
dfabsGLPF$wv == dfabsGLRI$nm

dfabs <- merge(dfabsGLPF,dfabsGLRI,by='nm')
dfflGLPF$exem <- gsub(" ","", dfflGLPF$exem)
dffl  <- bind_cols(dfflGLPF,dfflGLRI)
dffl  <- dffl[,-which(names(dffl) == "exem")[-1]]


# save(dfabs,dffl,file=file.path(cached.path,'GLPFGLRIVectorized.Rdata'))
saveRDS(dfabs, file = file.path(cached.path, "dfabs.rds"))
saveRDS(dffl, file = file.path(cached.path, "dffl.rds"))

# sum(dfflGLPF$exem !=dfflGLRI$exem)

