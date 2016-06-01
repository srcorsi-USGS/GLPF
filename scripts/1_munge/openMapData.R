library(data.table)
library(readxl)
library(dplyr)

cached.path <- "cached_data"
raw.map.path <- "raw_data/map"

##Creating site file, with barplot-plotting coordinates
#Import sampling site locations 
ssloc <- setDF(fread(file.path(raw.map.path,"GLPF_WISnappedPoints_Simple__export.txt")))

##Adding in coordinate info for where to plot bargraphs on maps
#Bringing in custom coordinates (for where standard offset won't work)
custom<-read_excel(file.path(raw.map.path,"RPlotting_CustomCoordinates.xlsx"))
ssloc<-left_join(ssloc,custom,by=c("Name_Updt"="Name"))
#Filling in standard locations for other sites, for sewershed maps
ssloc$SewershedGraphLAT <- ifelse(!is.na(ssloc$SewershedGraphLAT),ssloc$SewershedGraphLAT,ssloc$LAT_DD+3.5e-4)
ssloc$SewershedGraphLONG <- ifelse(!is.na(ssloc$SewershedGraphLONG),ssloc$SewershedGraphLONG,ssloc$LONG_DD-9e-4)
#Filling in standard locations for other sites, for watershed maps
ssloc$WatershedGraphLAT <- ifelse(!is.na(ssloc$WatershedGraphLAT),ssloc$WatershedGraphLAT,ssloc$LAT_DD+3.5e-3)
ssloc$WatershedGraphLONG <- ifelse(!is.na(ssloc$WatershedGraphLONG),ssloc$WatershedGraphLONG,ssloc$LONG_DD-5.5e-3)

saveRDS(ssloc, file.path(cached.path,"siteWI.rds"))

##Bring in the custom map extent information (quasi-sewer/watershed info)
basinspecs <- read_excel(file.path(raw.map.path,"RPlotting_BasinSpecs.xlsx"))
saveRDS(basinspecs, file.path(cached.path,"basinSpecs.rds"))


#######################################
#  Prepping Static Base Data for Map  #
#######################################

##Prepping collection linework
#Importing sanitary and storm sewer-related shapefiles
san_pipelines<-readOGR(dsn="//igsarmewfsapa/projects/QW Monitoring Team/GLPF_GIS/layers/20160208_MMSDCollection_FromEmily/output_AsShapefilesMAL/Milwaukee/KKSubset",layer="San_Pipelines_KK")
stm_pipelines<-readOGR(dsn="//igsarmewfsapa/projects/QW Monitoring Team/GLPF_GIS/layers/20160208_MMSDCollection_FromEmily/output_AsShapefilesMAL/Milwaukee/KKSubset",layer="Stm_Pipelines_KK")
#Reprojecting shapefiles to lay on top of Google maps
san_pipelines <- spTransform(san_pipelines, CRS("+proj=longlat +datum=WGS84"))
stm_pipelines <- spTransform(stm_pipelines, CRS("+proj=longlat +datum=WGS84"))
#Convert shapefiles into something that ggplot can understand
san_pipelines <- fortify(san_pipelines)
stm_pipelines <- fortify(stm_pipelines)

saveRDS(san_pipelines, file.path(cached.path, "san_pipelines.rds"))
saveRDS(stm_pipelines, file.path(cached.path, "stm_pipelines.rds"))

##Prepping hydro shapefiles
#Importing flowline and waterbody shapefiles (subsets of WDNR 1:24k dataset)
hydro_line<-readOGR(dsn="//igsarmewfsapa/projects/QW Monitoring Team/GLPF_GIS/layers/20160211_WDNR24kHydroSubsets",layer="HYDRO_FLOWLINE_LN_24K_SEWisc")
hydro_poly<-readOGR(dsn="//igsarmewfsapa/projects/QW Monitoring Team/GLPF_GIS/layers/20160211_WDNR24kHydroSubsets",layer="HYDRO_WATERBODY_AR_24K_SEWisc")
#Reprojecting shapefiles to lay on top of Google maps
hydro_line <- spTransform(hydro_line, CRS("+proj=longlat +datum=WGS84"))
hydro_poly <- spTransform(hydro_poly, CRS("+proj=longlat +datum=WGS84"))
#Convert shapefiles into something that ggplot can understand
hydro_line <- fortify(hydro_line)
hydro_poly <- fortify(hydro_poly)

saveRDS(hydro_poly, file.path(cached.path, "hydro_poly.rds"))
saveRDS(hydro_line, file.path(cached.path, "hydro_line.rds"))


# ##Bring in the sewershed boundaries
ssbas<-readOGR(dsn="//igsarmewfsapa/projects/QW Monitoring Team/GLPF_GIS/layers/WISiteInfoMASTER",layer="GLPF_WISewershedBoundaries")
#Reprojecting shapefiles to lay on top of Google maps
ssbas <- spTransform(ssbas, CRS("+proj=longlat +datum=WGS84"))
# BBox <- bbox(obj = ssbas)
#Convert shapefiles into something that ggplot can understand
# ssbas<-ssbas[ssbas$SiteCode=="WWKKS5",] #This is how you subset a SpatialPolygonsDataFrame
ssbas <- fortify(ssbas)
saveRDS(ssbas, file.path(cached.path, "ssbas.rds"))
