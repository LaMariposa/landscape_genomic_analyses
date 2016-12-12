library(alphahull)
library(rgeos)
library(sp)
library(maptools)
library(raster)
source("/home/megan/programs/others_scripts/ahull_to_SPLDF.r")

#set output director
outdir="/home/megan/Desktop/emel_lb1234/4.analysis/gdm/"
outfile="emel_alasuboccur_20k"
setwd(outdir)
OzCoast=shapefile("/home/megan/megan/research/gis_data/oz_coast/aus5m_coast_no_norfolk.shp")

#input occurrence records
occurcsv="/home/megan/megan/research/eucalyptus/Emelliodora/ala_occurrence/records-2016-11-23_emel_sub/records-2016-11-23.csv"
#occurcsv="/home/megan/megan/research/eucalyptus/Emelliodora/lb_trees/1.samples_qc/emel_lb_samples.csv"

#read in occurrence data
occur=read.delim(occurcsv, sep=",", header=T)
dim(subset(occur, specificEpithet!="Eucalyptus melliodora")) #check all melliodora
dim(subset(occur, specificEpithet=="Eucalyptus melliodora")) #count records
unique(occur$geodeticDatum) #check gps projection
occur_uniq=unique(cbind(occur$decimalLongitude, occur$decimalLatitude))  #get unique locations

#parameters
al=15 #alpha parameter (lower is tighter, higher is looser)

#generate shape/hull
alphashape=ashape(occur_uniq,alpha=al) #alpha shape
alphahull=ahull(occur_uniq, alpha=al) #alpha hull
alphahull_sp=ahull_to_SPLDF(alphahull) #convert to SpatialLinesDataFrame

#visualize
plot(alphashape,col=c("blue","black"))
plot(alphahull, do.shape=T,col=c(6,4,rep(1,4)))

plot(alphahull_sp)
points(occur$decimalLongitude, occur$decimalLatitude, pch=20)
plot(OzCoast, add=T)

#add buffer
alphahull_sp_buf=gBuffer(alphahull_sp, width=.18)

#final polygon
poly=SpatialPolygons(list(Polygons(list(alphahull_sp_buf@polygons[[1]]@Polygons[[1]]),ID=1)))
plot(poly)
points(occur$decimalLongitude, occur$decimalLatitude, pch=20)
plot(OzCoast, add=T)

df=data.frame(id=getSpPPolygonsIDSlots(poly))
row.names(df)=getSpPPolygonsIDSlots(poly)

poly_df=SpatialPolygonsDataFrame(poly, data=df)

writeSpatialShape(poly_df, outfile)




