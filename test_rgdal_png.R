#R CMD BATCH --no-save --no-restore '--args -wd "/media_sf_disco_dati/R/SusceptibilityAnalysis/MEMPHIS_test/debug_png_esa/" -shp "training.shp"' "test_rgdal_png.R" "susceptibilty.log"

rm(list=(ls()))
graphics.off()
#setwd("X:/R/grid_to_xyz/")
pars <-commandArgs(trailingOnly=TRUE)

if (length(table(pars == "-wd"))==2)
{
  wd_selected<-pars[which(pars=="-wd")+1]
} else
{
  #wd_selected<-"/home/geomorfologia/hakan/R/ELS_model/Gorkha/20171117T153639/"
  wd_selected<-"X:/R/SusceptibilityAnalysis/MEMPHIS_test/debug_png_esa/"
}
setwd(wd_selected)

if (length(table(pars == "-shp"))==2)
{
  shape_training<-pars[which(pars=="-shp")+1]
} else
{
  shape_training<-"training.shp"
  #wd_selected<-"X:/R/SusceptibilityAnalysis/Messina_Tool_Paper/soglia2_Random/"
}

color_ramp_fun<-colorRamp(c(rgb(38,115,0,max=255),rgb(255,255,0,max=255),rgb(255,0,0,max=255)))
color_ramp_palette_fun<-colorRampPalette(c(rgb(38,115,0,max=255),rgb(255,255,0,max=255),rgb(255,0,0,max=255)))
#color_ramp_palette_fun(length(breaks.histogram.values)-1)

breaks.histogram.values<-c(0,0.2,0.45,0.55,0.8,1)

# Plot and export of maps
breaks.map.susceptibility<-c(0,0.2,0.45,0.55,0.8,1.0001)
breaks.map.uncertainty<-c(0,0.001,0.005,0.01,0.05,0.1,0.5,1)
breaks.map.matching.code<-c(1,2,3,4,5)
color.vector.susceptibility<-color_ramp_palette_fun(length(breaks.histogram.values)-1)
require(RColorBrewer)
color.vector.uncertainty<-rev(brewer.pal(length(breaks.map.uncertainty)-1,"YlOrRd"))
color.vector.matching<-c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(215,215,158,max=255),rgb(115,115,0,max=255))

library(rgdal)
library(raster)
shape_training<-readOGR(dsn=shape_training,layer="training")


### -------------- Exporting shapefile and relative png -------------- ###

writeOGR(shape_training,dsn="result_training.shp",layer="training",driver="ESRI Shapefile",overwrite_layer=TRUE)
export_EPSG_code<-"4326"
layer_gridded<-shape_training
layer_gridded@data<-as.data.frame(layer_gridded@data[,"MOD_CLASS"])
gridded(layer_gridded)<-TRUE
layer_gridded_raster<-raster(layer_gridded)
#res(layer_gridded_raster)<-gridparameters(layer_gridded)[1,2]
res(layer_gridded_raster)<-30
layer_gridded_raster_reproject<-projectRaster(from=layer_gridded_raster,crs=CRS(paste("+init=epsg:",export_EPSG_code,sep="")))
layer_gridded_raster_reproject<-as(layer_gridded_raster_reproject, "SpatialPixelsDataFrame")
fullgrid(layer_gridded_raster_reproject)
### Reclassifying 
layer_gridded_raster_reproject@data[,1][layer_gridded_raster_reproject@data[,1]>0.5 & layer_gridded_raster_reproject@data[,1]<=1]<-1
layer_gridded_raster_reproject@data[,1][layer_gridded_raster_reproject@data[,1]>=0 & layer_gridded_raster_reproject@data[,1]<=0.5]<-0
pngcolors<-gray(c(1,0))[as.numeric(names(table(layer_gridded_raster_reproject@data[,1])))+1]
pnglabels<-c("0: Stable","1: Unstable")[as.numeric(names(table(layer_gridded_raster_reproject@data[,1])))+1]
### Writing .png .pngw files 
writeGDAL(layer_gridded_raster_reproject,"result_training.png",drivername = "PNG", type = "Byte",colorTable=list(pngcolors), catNames=list(pnglabels),mvFlag=2,options=c("WORLDFILE=YES"))
file.rename(from="result_training.wld", to="result_training.pngw")
### Writing .properties files 
pngid<-1
pngtitle<-"Shapefile training"
pngdate<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
pnggeom<-paste("POLYGON((",paste(paste(expand.grid(layer_gridded@bbox[1,],layer_gridded@bbox[2,])[,1],expand.grid(layer_gridded@bbox[1,],layer_gridded@bbox[2,])[,2],sep=" "),collapse=","),"))",sep="")
pngimgurl<-"https://raw.githubusercontent.com/maurorossi/dcs-LAND-SE/master/src/main/app-resources/job_template_landse/legend_binary.PNG"
propertiesstring<-paste("identifier=",pngid,"\ntitle=",pngtitle,"\ndate=",pngdate,"\ngeometry=",pnggeom,"\nimage_url=",pngimgurl,sep="")
write.table(propertiesstring,"result_training.shp.properties",row.names=FALSE,col.names=FALSE,quote=FALSE)



### -------------- Exporting tiff and relative png -------------- ###

layer_gridded<-shape_training
layer_gridded@data<-as.data.frame(layer_gridded@data[,"MOD_PROB"])
gridded(layer_gridded)<-TRUE
layer_gridded_raster<-raster(layer_gridded)
#res(layer_gridded_raster)<-gridparameters(layer_gridded)[1,2]
res(layer_gridded_raster)<-30
print(plot(layer_gridded_raster,col=color.vector.susceptibility,breaks=round(breaks.map.susceptibility,2)))
#zoom(layer_gridded_raster)		

writeRaster(layer_gridded_raster, filename="result_Model_Susceptibility_Map.tif", format="GTiff", overwrite=TRUE)


require(rgdal)
### Reprojecting
export_EPSG_code<-"4326"
layer_gridded_raster_reproject<-projectRaster(from=layer_gridded_raster,crs=CRS(paste("+init=epsg:",export_EPSG_code,sep="")))
layer_gridded_raster_reproject<-as(layer_gridded_raster_reproject, "SpatialPixelsDataFrame")
fullgrid(layer_gridded_raster_reproject)
### Reclassifying 
layer_gridded_raster_reproject@data[,1][layer_gridded_raster_reproject@data[,1]>0.8 & layer_gridded_raster_reproject@data[,1]<=1]<-5
layer_gridded_raster_reproject@data[,1][layer_gridded_raster_reproject@data[,1]>0.55 & layer_gridded_raster_reproject@data[,1]<=0.8]<-4
layer_gridded_raster_reproject@data[,1][layer_gridded_raster_reproject@data[,1]>0.45 & layer_gridded_raster_reproject@data[,1]<=0.55]<-3
layer_gridded_raster_reproject@data[,1][layer_gridded_raster_reproject@data[,1]>0.2 & layer_gridded_raster_reproject@data[,1]<=0.45]<-2
layer_gridded_raster_reproject@data[,1][layer_gridded_raster_reproject@data[,1]>=0 & layer_gridded_raster_reproject@data[,1]<=0.2]<-1
pngcolors<-round(as.matrix(data.frame(color_ramp_fun(breaks.histogram.values[as.numeric(names(table(layer_gridded_raster_reproject@data[,1])))+1]),alpha=rep(255,length(table(layer_gridded_raster_reproject@data[,1]))))))
colnames(pngcolors)<-c("red","green","blue","alpha")
pngcolors<-rgb(pngcolors,maxColorValue = 255)
pnglabels<-c("1: 0-0.20","2: 0.20-0.45","3: 0.45-0.55","4: 0.55-0.80","5: 0.80-1")[as.numeric(names(table(layer_gridded_raster_reproject@data[,1])))]
### Writing .png .pngw files 
writeGDAL(layer_gridded_raster_reproject,"result_Model_Susceptibility_Map.png",drivername = "PNG", type = "Byte",colorTable=list(pngcolors), catNames=list(pnglabels),mvFlag=6,options=c("WORLDFILE=YES"))
file.rename(from="result_Model_Susceptibility_Map.wld", to="result_Model_Susceptibility_Map.pngw")
### Writing .properties files 
pngid<-1
pngtitle<-"Susceptibility map"
pngdate<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
pnggeom<-paste("POLYGON((",paste(paste(expand.grid(layer_gridded@bbox[1,],layer_gridded@bbox[2,])[,1],expand.grid(layer_gridded@bbox[1,],layer_gridded@bbox[2,])[,2],sep=" "),collapse=","),"))",sep="")
pngimgurl<-"https://raw.githubusercontent.com/maurorossi/dcs-LAND-SE/master/src/main/app-resources/job_template_landse/legend_susceptibility.PNG"
propertiesstring<-paste("identifier=",pngid,"\ntitle=",pngtitle,"\ndate=",pngdate,"\ngeometry=",pnggeom,"\nimage_url=",pngimgurl,sep="")
write.table(propertiesstring,"result_Model_Susceptibility_Map.tif.properties",row.names=FALSE,col.names=FALSE,quote=FALSE)

