#modelCoefList

# Next steps: 
# consider refitting with lm() and examining slopes by event
# consider automated way to keep track of slopes for individual events within group regressions
# -maybe use median regression for this.

#run regression with the ~90  L3 samples

# Heuristic overlap analysis: decision tree


library(GSHydroTools)
source("D:/SRCData/R/USGSHydrotools/R/multiCor.R")
library(dplyr)
library(RColorBrewer)
#setwd("D:/SRCData/Git/GLPF")
source("na.info.R")

df.orig <- readRDS("./cached_data/8_process_new_categories/rds/summary_noWW_noQA.rds")

#df.orig <- summaryDF
df <- df.orig
response <- c("lachno","bacHum")

df <- df[-which(is.na(df$lachno)),]

beginIV <- "Sag240_255"
endIV <- "rBS44_S45_BF"

begin <- which(names(df)==beginIV)
end <- which(names(df)==endIV)

IVs <- names(df)[begin:end]

na.info.list <- na.info(df[,-dim(df)[2]],first.col = beginIV)
rmRows <- unique(c(which(df$CAGRnumber %in% na.info.list$na.rows),
                   na.info.list$nan.rows,
                   na.info.list$inf.rows))
rmCols <- unique(which(names(df) %in% c(na.info.list$na.cols.partial,
                                        na.info.list$nan.cols,
                                        na.info.list$inf.cols)))
dfrmCols <- df[,-rmCols]
dfRmRows <- df[rmRows,]
df <- df[,-rmCols]

beginIV <- "Sag240_255"
endIV <- "rBS44_S45_BF"
begin <- which(names(df)==beginIV)
end <- which(names(df)==endIV)
IVs <- names(df)[begin:end]


# Define model variables
vars <- character()
for(i in 1:9){
  vars <- c(vars,modelCoefList[[i]])
}

unique(vars)

variable.Cors.List <- list()

# Define correlations between model variables
CorelationCuttoff <- 0.9
rmVars <- numeric()
for(i in 2:length(vars)){
  variableCors <- multiCor(df = df,response = vars[i-1],IVs = vars[-c(1:(i-1))],method = "spearman")
  variable.Cors.List[[i-1]] <- row.names(variableCors)[(which(variableCors>CorelationCuttoff))]
  if(is.null(row.names(variableCors)[(which(variableCors>CorelationCuttoff))])) variable.Cors.List[[i-1]] <- character()
  names(variable.Cors.List)[i-1] <- vars[i-1]
  if(max(variableCors)>CorelationCuttoff) rmVars <- c(rmVars,i-1)
}


varsUncor <- vars[-rmVars]
