library(GSHydroTools)
source("D:/SRCData/R/USGSHydrotools/R/multiCor.R")
#modelCoefList
vars <- character()
for(i in 1:9){
  vars <- c(vars,modelCoefList[[i]])
}

unique(vars)

variable.Cors.List <- list()

rmVars <- numeric()
for(i in 2:length(vars)){
variableCors <- multiCor(df = df,response = vars[i-1],IVs = vars[-c(1:(i-1))],method = "spearman")
variable.Cors.List[[i-1]] <- row.names(variableCors)[(which(variableCors>0.9))]
if(is.null(row.names(variableCors)[(which(variableCors>0.9))])) variable.Cors.List[[i-1]] <- character()
names(variable.Cors.List)[i-1] <- vars[i-1]
if(max(variableCors)>0.90) rmVars <- c(rmVars,i-1)
}


varsUncor <- vars[-rmVars]
