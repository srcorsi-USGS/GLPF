library(Hmisc) #for capitalize
library(ggmap)
library(dplyr)
library(reshape2)
library(ggplot2)
library(rgdal) #for readOGR
library(foreign)
library(grid) #for plotting independent legend on graph
library(gridExtra) #for placing legend on final graphic
library(tidyr)

#Function for extracting legends from ggplot figures
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 

#Stop R from using scientific notation
options("scipen"=100, "digits"=4)

##################################
#  User specifications for maps  #
##################################

bacteria <- c('eColi','ent','lachno','bacHum')
breakPts <- c(-Inf,1000,10000,100000,Inf)
subBasin <- 'WWKK1'
binColors <- c("palegreen3","skyblue2","sienna1","red3" )
MapPointColors <- c("WWTP"="firebrick3",
                    "Stream"="skyblue3",
                    "Storm"="gold1",
                    "Sanitary"="firebrick3")
MapPointShapes<-c("WWTP"=23,
                  "Stream"=25,
                  "Manhole"=21,
                  "Outfall"=22,
                  "Manhole outfall"=22)
MapLineSizes<-c("Stream"=3,
                "Sanitary"=1,
                "Storm"=1)

cached.path <- "cached_data"
cached.figures <- "cached_figures"

##############
#  Lab Data  #
##############
df <- readRDS(file.path(cached.path, "mergedData.rds"))
df <- filter(df, State == "WI")

######################
#  Site information  #
######################
ssloc <- readRDS(file.path(cached.path,"siteWI.rds"))
basinspecs <- readRDS(file.path(cached.path,"basinSpecs.rds"))

##############
#  Map Stuff #
##############
hydro_line <- readRDS(file.path(cached.path, "hydro_line.rds"))

# #Assigning colors and shapes based on site characteristics
# ssloc$MapPointShapes<-ifelse(ssloc$SiteLocType=="WWTP",23,
#                              ifelse(ssloc$SiteLocType=="Stream", 25,
#                                     ifelse(ssloc$SiteLocType=="Manhole", 21,
#                                            ifelse(ssloc$SiteLocType=="Outfall",22,
#                                                   ifelse(ssloc$SiteLocType=="Manhole outfall",22,18)))))
# ssloc$MapPointColors<-ifelse(ssloc$SystemType=="WWTP","firebrick3",
#                              ifelse(ssloc$SystemType=="Stream", "skyblue3",
#                                     ifelse(ssloc$SystemType=="Storm", "gold1",
#                                            ifelse(ssloc$SystemType=="Sanitary","firebrick3","white"))))


#########################
#  Making Point Graphs  #
#########################

##Get bacteria data ready
#Simplify data
dfs <- df[,c('SiteID','fieldID','pdate','eventNum',bacteria)]

#Create stacked dataset
dfs.s<-melt(dfs,id=c("SiteID","fieldID","pdate","eventNum"))
#Create simpler date column
dfs.s$Date<-as.Date(dfs.s$pdate)
#Create factor (with levels ordered by date) for event number, so we don't have to use date for plotting
# ...this was done because we ran into two issues:
# ... 1. 8/10/2015 had both baseflow and storm samples (because a storm came through midday)
# ... 2. event number 03 corresponded to two different dates (3/11/15 and 3/12/15, in GMT)
eventNum.levels<-unique(arrange(unique(select(dfs.s,pdate,eventNum)),pdate)$eventNum)
dfs.s$eventNum<-factor(dfs.s$eventNum,levels=eventNum.levels)
#Create integer for each bacteria, to use on y-axis for plotting
dfs.s$variable <- factor(dfs.s$variable,levels=bacteria)
dfs.s$PlotPos <- as.numeric(dfs.s$variable)
#Convert values to numeric
dfs.s$value<-as.numeric(dfs.s$value)
#Assign bin to each sample result, based on value
dfs.s$valuebin<- as.numeric(cut(dfs.s$value,breakPts))
dfs.s$valuebin <- as.factor(dfs.s$valuebin)
#Filter sampling site file to just intersect with most DS sewer/watershed of interest (!!!Careful, haven't figured out how to deal with Jones Island site yet, it gets excluded here!!!)
if (filter(basinspecs, DSSiteID==subBasin)$BasinType=="sewershed"){
  #Filter to just the sites that are relevent for the sewershed maps
  ssloc.sub <- filter(ssloc,SystemType!="WWTP"&SystemType!="Stream"&SystemType!="Sanitary")
  #Filter to just the sewershed of interest
  ssloc.sub <- filter(ssloc.sub,SewershedOutfall==subBasin)
} else if(filter(basinspecs, DSSiteID==subBasin)$BasinType=="watershed"){
  #Filter to just the sites that are relevant for the watershed maps
  ssloc.sub <- filter(ssloc,SiteLocType!="WWTP"&SiteLocType!="Manhole")
  #Filter to just the watershed of interest (right now, only KK is an option, but eventually there will be more...)
  # ssloc.sub <- filter(ssloc.sub,WatershedDSSite==subBasin) ###FIX ONCE HAVE ACCESS TO DRIVE!!!!!!!!!!!
} else{
  ssloc.sub <- ssloc
}
#Filter to area of interest
dfs.s.sub <- dfs.s %>%
  separate(fieldID,c("Site","sampleNum"),"-") %>%
  filter(Site %in% ssloc.sub$Name_Updt) %>%
  mutate(plotColors = binColors[valuebin])

#Create point plot, to look at, basic facet wrap 
#This also creates the legend that we'll save and use on the map later
p.f <- ggplot(dfs.s.sub,aes(x=eventNum,y=PlotPos,colour=plotColors,label=value)) + 
  geom_point(size=4,shape=15) +
  scale_colour_identity("Bacteria\nconcentration,\ncn/100ml",labels=c(paste("<",breakPts[2],sep=""),
                                        paste(breakPts[2],"-",breakPts[3],sep=""),
                                        paste(breakPts[3],"-",breakPts[4],sep=""),
                                        paste(">",breakPts[4],sep="")),
                        breaks=binColors, guide="legend") +
  geom_text(colour='black')+
  theme(
    axis.line=element_blank(),
    # axis.text.x=element_blank(),
    # axis.text.y=element_blank(),
    # axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    # legend.position="none",
    legend.key=element_blank(),
    legend.background=element_rect(fill="transparent"),
    panel.background=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank()
  ) +
  facet_wrap(~Site)

#Saving the legend as an object
legend_graphs <- g_legend(p.f)
#Looking at the facet-wrap graphic without the legend
p.f + theme(legend.position="none")


#Creating point plots to be used on the map
siteNames <- unique(dfs.s.sub$Site)
#Limiting events to only those used in this subwatershed
dfs.s.sub$eventNum<-droplevels(dfs.s.sub$eventNum)
plotList <- list()
for (i in 1:length(siteNames)) {
  #Create site subset for data
  temp<-filter(dfs.s.sub,Site==unique(dfs.s.sub$Site)[i])  
  #Add blank rows for any missing dates
  temp.j<-data.frame(eventNum=levels(dfs.s.sub$eventNum))
  temp.j$eventNum<-factor(temp.j$eventNum,levels=levels(temp$eventNum))
  temp.j<-left_join(temp.j,temp,by=c("eventNum"="eventNum"))
  #Make plot
  p <-ggplot(temp.j,aes(x=eventNum,y=PlotPos,colour=plotColors)) + 
          geom_point(size=4,shape=15) +
          scale_colour_identity()+
          scale_y_discrete(breaks=c(1:length(bacteria)),
                           labels=bacteria) +
          scale_x_discrete(breaks=levels(dfs.s.sub$eventNum),
                            labels=levels(dfs.s.sub$eventNum)) +
          theme(
            axis.line=element_blank(),
            axis.text.x=element_text(colour="white", face="bold"),
            axis.text.y=element_text(colour="white", face="bold"),
            axis.ticks=element_line(colour="white"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            # legend.position="none",
            panel.background=element_blank(),
            panel.border = element_rect(fill=NA, size=0.5, colour="white"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            plot.title=element_text(size=12, face="bold", colour="white")
          ) +
          ggtitle(siteNames[i])
#  assign(paste("p.",levels(factor(dfs.s.sub$Site))[i], sep=""),p)
 plotList[[i]] <- p
#  rm(p)
}

#Creating fake graphic, just so that we can snag the legend
fake <- ggplot(ssloc.sub) +
          geom_point(data=ssloc.sub, aes(x=LONG_DD, y=LAT_DD, fill=SystemType, shape=SiteLocType), alpha = 0.8, size=5, show.legend = TRUE) +
          scale_shape_manual(values=MapPointShapes, name="Map point \nshape denotes \nsite type") +
          scale_fill_manual(values=MapPointColors, guide=FALSE) +
          theme(legend.key = element_blank(), legend.position="right")
#Saving the legend as an object
legend_sites <- g_legend(fake)


###########################
#  Putting graphs on map  #
###########################

##Obtaining imagery for appropriate watershed extent
# centerCoords <- apply(BBox,1,mean)
#imagery.base <- get_map(maptype="hybrid", source = "google", location=centerCoords,zoom=14)
#Subset basin specs
basinspecs.sub <- filter(basinspecs,DSSiteID==subBasin)
imagery.base <- get_map(maptype="hybrid", source = "google", location=c(basinspecs.sub$LONG_Center,basinspecs.sub$LAT_Center),zoom=basinspecs.sub$ZoomLevel)


#Save to pdf
filenm <- file.path(cached.figures, paste0(paste("MappedData",basinspecs.sub$BasinType,subBasin,sep="_"),".pdf"))
pdf(filenm, width=16, height=14, onefile=FALSE)

##Plotting the base map, in two steps
#First step, the base map, without points or data
if(basinspecs.sub$BasinType=="sewershed"){
  pmap <- ggmap(imagery.base, extent="panel", legend="right") +
            annotate("rect",xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill="white",alpha=0.35) +
            geom_path(data=hydro_line,
                      aes(x=long,y=lat,group=group,colour="Stream", size="Stream"),  show.legend=TRUE) + #A little wonky; breaks in lines where there aren't any!!!
            #   geom_polygon(aes(x=long, y=lat, group=group), fill='skyblue3', 
            #                colour='blue', data=hydro_poly) + #Not working well; polys not filling appropriately!!!
            geom_path(data=san_pipelines,
                      aes(x=long,y=lat,group=group, colour="Sanitary", size="Sanitary"), show.legend=TRUE) +
            geom_path(data=stm_pipelines,
                      aes(x=long,y=lat,group=group, colour="Storm", size="Storm"), show.legend=TRUE) +
            scale_colour_manual(name="Map color \ndenotes system \ntype",
                                values=MapPointColors)+
            scale_size_manual(name="Map color \ndenotes system \ntype",
                              values=MapLineSizes) +
            coord_cartesian() +
            theme(legend.key = element_blank())
} else { #same as above, just without the sanitary and storm linework (those two geom_paths)
  pmap <- ggmap(imagery.base, extent="panel", legend="right") +
            annotate("rect",xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill="white",alpha=0.35) +
            geom_path(data=hydro_line,
                      aes(x=long,y=lat,group=group,colour="Stream", size="Stream"),  show.legend=TRUE) + #A little wonky; breaks in lines where there aren't any!!!
            #   geom_polygon(aes(x=long, y=lat, group=group), fill='skyblue3', 
            #                colour='blue', data=hydro_poly) + #Not working well; polys not filling appropriately!!!
            scale_colour_manual(name="Map color \ndenotes system \ntype",
                              values=MapPointColors)+
            scale_size_manual(name="Map color \ndenotes system \ntype",
                              values=MapLineSizes) +
            coord_cartesian() +
            theme(legend.key = element_blank())
}
#Saving the legend as an object
legend_systems<-g_legend(pmap)

#Saving graphic without legend
pmap<-pmap + 
  theme(legend.position="none")

#Second step, adding the points
pmap<-pmap+
  geom_point(data=ssloc.sub, aes(x=LONG_DD, y=LAT_DD, fill=SystemType, shape=SiteLocType), alpha = 0.8, size=5, show.legend = TRUE) +
  scale_shape_manual(values=MapPointShapes, guide=FALSE) +
  scale_fill_manual(values=MapPointColors, guide=FALSE) 


#Now, plot the graphs on top of the basemap
for ( i in 1:length(siteNames)){
  dftemp <- filter(ssloc.sub,Name_Updt==siteNames[i])
  #Making objects out of the plot positions GraphLAT and GraphLONG objects, so that don't have to have multiple references in PDF call
  GraphLAT<-dftemp[,paste(capitalize(filter(basinspecs, DSSiteID==subBasin)$BasinType),"GraphLAT",sep="")]
  GraphLONG<-dftemp[,paste(capitalize(filter(basinspecs, DSSiteID==subBasin)$BasinType),"GraphLONG",sep="")]
  #Mapping
  pmap <- pmap + annotation_custom(grob = ggplotGrob(plotList[[i]]), 
                                   xmin = GraphLONG-2.5038*exp(-0.602*basinspecs.sub$ZoomLevel)*length(levels(dfs.s.sub$eventNum)), 
                                   xmax = GraphLONG+2.5038*exp(-0.602*basinspecs.sub$ZoomLevel)*length(levels(dfs.s.sub$eventNum)), 
                                   ymin = GraphLAT-4.5295*exp(-0.648*basinspecs.sub$ZoomLevel)*length(bacteria), 
                                   ymax = GraphLAT+4.5295*exp(-0.648*basinspecs.sub$ZoomLevel)*length(bacteria)) +
                  annotate("segment", 
                           x = dftemp$LONG_DD, 
                           xend = ifelse(dftemp$LONG_DD-GraphLONG > 0,
                                        GraphLONG+2.5038*exp(-0.602*basinspecs.sub$ZoomLevel)*length(levels(dfs.s.sub$eventNum)), 
                                        GraphLONG-2.5038*exp(-0.602*basinspecs.sub$ZoomLevel)*length(levels(dfs.s.sub$eventNum))),
                           y = dftemp$LAT_DD, 
                           yend = ifelse(dftemp$LAT_DD-GraphLAT > 0,
                                        GraphLAT+4.5295*exp(-0.648*basinspecs.sub$ZoomLevel)*length(bacteria), 
                                        GraphLAT-4.5295*exp(-0.648*basinspecs.sub$ZoomLevel)*length(bacteria)),
                           colour = "white") 
}


#Arrange the graph and various legends
grid.arrange(pmap, arrangeGrob(legend_sites,legend_systems,legend_graphs,ncol=1),nrow=1,ncol=2,widths=c(14,2))

#Turning off pdf
dev.off()

# shell.exec(filenm)
  



