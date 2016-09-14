# revision datos carlos modelacion de cultivos


#cargar librerias----
library(reshape)
library(rgdal)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(ncdf4)
library(raster)
library(rasterVis)
library(dismo)
library(zoom)
library(RColorBrewer)

# definicion de directorios----------
grd<- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/06-Monfreda data (Rendimiento, Area)/Bean/"
pic<- "C:/Users/CEGONZALEZ/Documents/BIDCarlos/"

# datos de siembra, rendimientos y hectareas

#monfreda-----

cname_mon <- "bean_5min"  
ncfname_mon <- paste(grd,cname_mon,".nc", sep="")
b <- raster::brick(paste(grd,cname_mon,".nc", sep=""), lvar=4)
plot(b)

harvarea_m <- b[[1]]
yield_m    <- b[[2]]
q_area_m   <- b[[3]]
q_yield_m  <- b[[4]]


#spam----- 

grd2<- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/07-SPAM_data/SPAM2005/physical.area/"

cname_spam_rf <- "spam2005v2r0_physical-area_bean_rainfed"  
cname_spam_ir <- "spam2005v2r0_physical-area_bean_irrigated" 

#secano
ncfname_spam_rf <- paste(grd2,cname_spam_rf,".nc", sep="")
rfspam_are <- paste(grd,cname_spam_rf,".nc", sep="")
rfspam_are <- raster::brick(paste(grd2,cname_spam_rf,".nc", sep=""), lvar=2)
open <- nc_open(ncfname_spam_rf)
print(open)
projection(rfspam_are) <- "+proj=longlat +datum=WGS84"
plot(rfspam_are)


#riego
ncfname_spam_ir <- paste(grd2,cname_spam_ir,".nc", sep="")
irspam_are <- paste(grd,cname_spam_ir,".nc", sep="")
irspam_are <- raster::brick(paste(grd2,cname_spam_ir,".nc", sep=""), lvar=2)
open <- nc_open(ncfname_spam_ir)
print(open)
projection(irspam_are) <- "+proj=longlat +datum=WGS84"
plot(irspam_are)


#mirca------

# cultivo numero 17 pulses/ numero de meses 

#forma1
grd3<- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/09-MIRCA2000/Growing_area_grids/"
month<- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012")
treat = c('crop_17_rainfed','crop_17_irrigated')  #riego o secano
abr_t <- c("r", "i")

for (m in 1:length(month)){
  
  for (t in 1:length(treat)) {
    
    eval(parse(text=paste('p_', abr_t[t], '_', month[m],' <- raster(paste(grd3, treat[t] , "_", month[m], ".asc", sep=""))', sep='')))
    
  }
}

#forma2
mirca_rainfed <- list()
for (m in 1:length(month)){
  mirca_rainfed[[m]] <- raster(paste(grd3, "crop_17_rainfed_", month[m], ".asc", sep=""))
}
mirca_rainfed <- raster::stack(mirca_rainfed)
names(mirca_rainfed)

mirca_irrigated <- list()
for (m in 1:length(month)){
  mirca_irrigated[[m]] <- raster(paste(grd3, "crop_17_irrigated_", month[m],".asc", sep=""))
}

mirca_irrigated <- raster::stack(mirca_irrigated)
names(mirca_irrigated)


# procesamiento y visualizacion espacial-------------
#shapefile
World_shp <- "C:/Users/CEGONZALEZ/Documents/cassava/SPATIAL/gisdata/"
World_1 <- shapefile(paste0(World_shp,"G2014_2013_0.shp"))


plot(World_1) # shapefile

head(World_1@data) # con el arroba puedo conocer los datos dentro de shape file
View(World_1)

# para eliminar a la Antarctiva, greeland

sur<- World_1[World_1@data$ADM0_NAME=="Uruguay"| World_1@data$ADM0_NAME=="Argentina"|  World_1@data$ADM0_NAME=="Chile"|
              World_1@data$ADM0_NAME=="Paraguay"| World_1@data$ADM0_NAME=="Brazil"| World_1@data$ADM0_NAME=="Peru"
              |World_1@data$ADM0_NAME=="Bolivia",]

#   (xmin, xmax, ymin, ymax)
sur<- crop(sur, extent(-80, -20, -60, -10))
plot(sur)
m_area_bean_sur<- crop(harvarea_m, sur)

# para no mostrar los ceros en el raster
m_area_bean_sur[which(m_area_bean_sur[]==0)] <- NA


#grafica monfreda
p <- levelplot(m_area_bean_sur, layers=1,par.settings = RdBuTheme , margin = FALSE, main = "Bean Area\n Monfreda ")
p<- p  + layer(sp.lines(sur, lwd=0.7, col="darkgray"))  
p

tiff(filename=paste(pic,"AreaBeanMonfreda.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p

dev.off()


# grafica Spam
#secano
rfspam_are2 <- rfspam_are[[1]]
spam_r_sur_bean <- crop(rfspam_are2, sur)
spam_r_sur_bean[which(spam_r_sur_bean[]==0)]<-NA

p1 <- levelplot(spam_r_sur_bean, layers=1,par.settings = RdBuTheme , margin = FALSE, main = "Bean Rainfed Area\n SPAM 2005V2 ")
p1<- p1  + layer(sp.lines(sur, lwd=0.7, col="darkgray"))  
p1

tiff(filename=paste(pic,"AreaBeanRainfedSPAM.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p1

dev.off()


# riego
irspam_are <- irspam_are[[1]]

spam_i_sur_bean <- crop(irspam_are, sur)
spam_i_sur_bean[which(spam_i_sur_bean[]==0)]<-NA

p2 <- levelplot(spam_i_sur_bean, layers=1,par.settings = RdBuTheme , margin = FALSE, main = "Bean Irrigated Area\n SPAM 2005V2 ")
p2<- p2  + layer(sp.lines(sur, lwd=0.7, col="darkgray"))  
p2

tiff(filename=paste(pic,"AreaBeanIrrigatedSPAM.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p2

dev.off()

# mirca secano
mirca_i_sur_bean <- crop(mirca_irrigated, sur)
mirca_i_sur_bean2<- mirca_i_sur_bean[[1:6]]
mirca_i_sur_bean3<- mirca_i_sur_bean[[7:12]]
mirca_i_sur_bean2[which(mirca_i_sur_bean2[]==0)]<- NA
mirca_i_sur_bean3[which(mirca_i_sur_bean3[]==0)]<- NA


# enero a junio
p3<- levelplot(mirca_i_sur_bean2,par.settings = RdBuTheme,margin = FALSE, main = "MIRCA Irrigated area-planting date\n from January to June ") 
p3<- p3+ layer(sp.lines(sur, lwd=0.7, col="darkgray"))  
p3

tiff(filename=paste(pic,"IrrigadoAreaMircaEnero-Junio.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p3

dev.off()

# Julio -diciembre
p4<- levelplot(mirca_i_sur_bean3,par.settings = RdBuTheme,margin = FALSE, main = "MIRCA Irrigated area-planting date\n from July to December ") 
p4<- p4+ layer(sp.lines(sur, lwd=0.7, col="darkgray"))  
p4

tiff(filename=paste(pic,"IrrigadoAreaMircaJulio-Dic.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p4

dev.off()

# mirca secano
mirca_r_sur_bean <- crop(mirca_rainfed, sur)
mirca_r_sur_bean2<- mirca_r_sur_bean[[1:6]]
mirca_r_sur_bean3<- mirca_r_sur_bean[[7:12]]
mirca_r_sur_bean2[which(mirca_r_sur_bean2[]==0)]<- NA
mirca_r_sur_bean3[which(mirca_r_sur_bean3[]==0)]<- NA



# enero a junio
p5<- levelplot(mirca_r_sur_bean2,par.settings = RdBuTheme,margin = FALSE, main = "MIRCA Rainfed area-planting date\n from January to June ") 
p5<- p5+ layer(sp.lines(sur, lwd=0.7, col="darkgray"))  
p5

tiff(filename=paste(pic,"RainfedAreaMircaEnero-Junio.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p5

dev.off()

# Julio -diciembre
p6<- levelplot(mirca_r_sur_bean3,par.settings = RdBuTheme,margin = FALSE, main = "MIRCA Rainfed area-planting date\n from July to December ") 
p6<- p6+ layer(sp.lines(sur, lwd=0.7, col="darkgray"))  
p6

tiff(filename=paste(pic,"RainfedAreaMircaJulio-Dic.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p6

dev.off()
