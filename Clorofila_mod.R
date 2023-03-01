#First you need to install the next Librarys::::
install.packages("raster")

library("raster")
library("sp")
library("sf")
library("rgdal")
library("maptools")
library("rgeos")
library("dplyr")
library("ggplot2")
library("ncdf4")
library("rasterVis")


###PRUEBA PARA GITHUB
#############



################Me ubico en el directorio donde los datos de LANDSAT 8
setwd("~/viaje_Belgica/ProyectoINA/Taller_PROSAT_Algoritmo_semiemp_Parte_1")

#######################UBICO DIRECTIRIO DE IMAGEN DONDE ESTÁ LA IMAGEN LANDAST
setwd("~/viaje_Belgica/ProyectoINA/Taller_PROSAT_Algoritmo_semiemp_Parte_1/L8")

#######################LISTO LOS ARCHIVOS QUE HAY DENTRO DEL DIRECTORIO DE LANDSAT 8 L2 de reflectancia
list.files()

######################GENERO VARIABLE CON EL NOMBRE DE LOS ARCHIVOS QUE CONTIENEN BANDAS DE REFLECTNCIA
files= list.files(pattern='*SR_B',full.names=TRUE)

#############################################GENERO LISTA DE NOMBRES QUE LES QUIERO DAR EN R
nombre=c("B1","B2","B3","B4","B5","B6","B7")

###############################################USAR ESTO PARA convertir a raster muchas imagenes y nombrarlas de manera mas corta (variable nombre)

for(i in seq_along(files)) {
  out = raster(files[i])
  assign(paste(nombre[i]),out)
}

#############################################ARMO CUBO DE DATOS CON LAS BANDAS DE REFLECTANCIA DE IMAGEN LANDAST
L8_22_02_2017_stack=stack(B1,B2,B3,B4,B5,B6,B7)

##############################################sI SE QUIERE CONOCER LA INFORMACION DEL RASTER 
L8_22_02_2017_stack

#class      : RasterStack 
#dimensions : 7741, 7681, 59458621, 7  (nrow, ncol, ncell, nlayers)
#resolution : 30, 30  (x, y)
#extent     : 278985, 509415, -3628815, -3396585  (xmin, xmax, ymin, ymax)
#crs        : +proj=utm +zone=20 +datum=WGS84 +units=m +no_defs 
#names      : LC08_L2SP//2_T1_SR_B1, LC08_L2SP//2_T1_SR_B2, LC08_L2SP//2_T1_SR_B3, LC08_L2SP//2_T1_SR_B4, LC08_L2SP//2_T1_SR_B5, LC08_L2SP//2_T1_SR_B6, LC08_L2SP//2_T1_SR_B7 
#min values :                     0,                     0,                     0,                     0,                     0,                     0,                     0 
#max values :                 65535,                 65535,                 65535,                 65535,                 65535,                 65535,                 65535 



#############################################SI SE QUIERE EXPORTAR AL DISCO
writeRaster(x =L8_22_02_2017_stack, filename ="L8_22_02_2017_stack", format ='GTiff', overwrite = TRUE)

#############################################VISUALIZO RGB

plotRGB(L8_22_02_2017_stack,
        r =4, g = 3, b = 2,
        stretch = "hist",
        axes = TRUE,
        main = "RGB Landsat Bands 4, 3, 2")
box(col = "white")

#############################################VISUALIZO SIN EJES


plotRGB(L8_22_02_2017_stack, 4, 3, 2, stretch='hist')

###############################RECORTO LA IMAGEN CON LA EXTENSION DE LA ZONA EN DONDE ESTA EL EMBALSE (LA CONOZCO, LO SACO DE GOOGLE EARTH)
extent_sr= as(raster::extent(356325, 363765, -3477405, -3465255 ), "SpatialPolygons")  #(xmin, xmax, ymin, ymax) X=LON, Y=LAT
proj4string(extent_sr) <- "+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs "  ####esta en UTM 20N como la imagen LANDSAT
plot(extent_sr, add=TRUE)

##############################RECORTO ZONA DE INTERES PARA EL STACK DE LANDSAT8
L8_22_02_2017_stack_crop=crop(L8_22_02_2017_stack, extent_sr)

plot(L8_22_02_2017_stack_crop)

##############################APLICO FACTOR DE ESCALA A LAS BANDAS 0.0001
L8_22_02_2017_stack_crop=L8_22_02_2017_stack_crop*0.0001

#################################VISUALIZO PARA VER SI SE HIZO BIEN ## [[1]] ES BANDA 1
plot(L8_22_02_2017_stack_crop[[1]])
plotRGB(L8_22_02_2017_stack_crop,
        r = 4, g = 3, b = 2,
        stretch = "hist",
        axes = TRUE,
        main = "RGB Landsat Bands 4, 3, 2")
box(col = "white")


##############################HAGO FUNCION DE MNDWI DE 22-02-2017 PARA HACER LA MASCARA DE AGUA, Xu ET AL, 2005

MNDWI <- function(img, k, i) {
  GREEN <- img[[k]]
  SWIR <- img[[i]]
  vi <- (GREEN - SWIR) / (GREEN + SWIR)
  return(vi)
}

##############################CALCULO MNDWI PARA LUEGO HACER MASCARA DE AGUA SI QUISIERA.
MNDWI_22_02_2017<- MNDWI(L8_22_02_2017_stack_crop, 3, 6)
plot(MNDWI_22_02_2017, col = rev(terrain.colors(10)), main = 'Landsat-MNDWI')

##############################HAGO MASCARA DEL SAN ROQUE PARA ESA FECHA CON EL INDICE MNDWI

SR_22_02_2017vec <- rasterToPolygons(MNDWI_22_02_2017, fun=function(x){x>0}, dissolve=TRUE)
plot(SR_22_02_2017vec, col=3, fill=1)

#############################QUEDAN ALGUNOS PUNTITOS FUERA DEL LAGO, ENTONCES LO EDITO EN QGIS O USO UN VECTOR QUE TENGO
#############################ABRO VECTOR DEL LAGO YA EDITADO y PUNTOS DE MUESTREO

setwd("~/viaje_Belgica/ProyectoINA/Taller_PROSAT_Algoritmo_semiemp_Parte_1")
san_roque=rgdal::readOGR("vector_lago_22_02_2017.shp", layer="vector_lago_22_02_2017") #will load the shapefile to your dataset.
plot(san_roque) 
####si no funciona lo anterior probar## san_roque=sf::st_read("vector_lago_22_02_2017.shp")

puntosSR=readOGR("puntos_muestreo32620.shp", layer="puntos_muestreo32620") #will load the shapefile to your dataset.
plot(puntosSR, add=TRUE) 


###############################ENMASCARO LA TIERRA EN LA BANDA LANDAST 8 RECORTADA PARA QUEDARME SOLO CON AGUA
L8_22_02_2017_stack_mask <- mask(x =L8_22_02_2017_stack_crop, mask =san_roque)

###############################Si no funciona hacer
L8_22_02_2017_stack_mask <- raster::mask(x =L8_22_02_2017_stack_crop, mask =san_roque)
plot(L8_22_02_2017_stack_mask)

###############################VEO EL CONTENIDO DEL VECTOR DE PUNTOS DE MUESTREO
puntosSR

#class       : SpatialPointsDataFrame 
#features    : 8 
#extent      : 358869.2, 362888.5, -3474250, -3470539  (xmin, xmax, ymin, ymax)
#crs         : +proj=utm +zone=20 +datum=WGS84 +units=m +no_defs 
#variables   : 5
#names       : id,          lat,           lon, sitio, cloroa 
#min values  :  1, 358869.18199, -3474249.5048,  CENT,   27.6 
#max values  :  8, 362888.52908, -3470539.2383,    ZB,  288.5 

puntosSR[[1]]
[1] "1" "2" "3" "6" "4" "5" "7" "8"
> puntosSR[[2]]
[1] 360039.0 360880.3 361659.1 361659.0 362888.5 360279.3 359679.2 358869.2
> puntosSR[[3]]
[1] -3470539 -3471020 -3471790 -3472368 -3471969 -3472149 -3472899 -3474250
> puntosSR[[4]]
[1] "SAT4" "SAT3" "ZA"   "ZB"   "GAR"  "CENT" "SAT2" "SAT1"
 puntosSR[[5]]
[1]  56.7  94.7  53.8 288.5 197.1 127.1 132.2  27.6

##############No aparecía esta columna en el archivo de puntos 
puntosSR$cloroa=c(56.7,94.7, 53.8, 288.5, 197.1, 127.1, 132.2, 27.6)

##############################LOS NOMBRE DE LAS BANDAS DEL STACK QUE USARE PARA EXTRAER DATOS DE REFLECTANCIA SON MUY LARGOS, RENOMBRAR
names(L8_22_02_2017_stack_mask)
#[1] "LC08_L2SP_229082_20170222_20200905_02_T1_SR_B1" "LC08_L2SP_229082_20170222_20200905_02_T1_SR_B2"
#[3] "LC08_L2SP_229082_20170222_20200905_02_T1_SR_B3" "LC08_L2SP_229082_20170222_20200905_02_T1_SR_B4"
#[5] "LC08_L2SP_229082_20170222_20200905_02_T1_SR_B5" "LC08_L2SP_229082_20170222_20200905_02_T1_SR_B6"
#[7] "LC08_L2SP_229082_20170222_20200905_02_T1_SR_B7"

names(L8_22_02_2017_stack_mask)=c("B1","B2","B3","B4","B5","B6","B7")



##########################################EXTRAIGO VALORES DE REFLECTANCIA

sp <- cbind(puntosSR[[2]], puntosSR[[3]])  ###ARMO VECTOR CON (LAT,LON) PARA EXTRAER LOS PUNTOS DE REFLECTANCIA
sp <- SpatialPoints(sp)
crs(sp)=crs(puntosSR)
p=extract(L8_22_02_2017_stack_mask, sp, method="bilinear")

###########################################SI QUISIERA EXTRAER EL PROMEDIO DE UN DATO DE REFLECTANCIA EN UN BUFFRE DE 90 M
pmean<-extract(r,puntosSR_2017, buffer=90, fun=mean)

##########################################ARMO UNA TABLA UNIENDO REFLECTANCIA CON CLOROFILA

Tcloroa=data.frame(id=puntosSR$id, ref=p,sitio=puntosSR$sitio, cloroa=puntosSR$cloroa)
########LA VISUALIZO
Tcloroa
############################################GENERO UNA BANDA NUEVA B5/B4 YA QUE CORRELACIONA BIEN CON CLOROFILA, TAMBIEN DE LOGARITMO DE CONCENTRACION DE CLOROFILA
Tcloroa$b4sb5=Tcloroa$ref.B4/Tcloroa$ref.B5
Tcloroa$logcloroa=log(Tcloroa$cloroa)

Tcloroa
##########################################EXTRAIGO PUNTO DE LA GARGANTA DE LA TABLA
Tcloroa_singar=Tcloroa[-5,]
#########################################HAGO MODELOS DE CLOROFILA-A con banda b4 (RED) y b5(NIR)

fit1=lm(Tcloroa_singar$cloroa~Tcloroa_singar$b4sb5)
summary(fit1)

fit2=lm(Tcloroa_singar$logcloroa~Tcloroa_singar$b4sb5)
summary(fit2)
##############################VISUALIZO
ggplotRegression <- function (fit) {
  require(ggplot2)
  r2=round(signif(summary(fit)$adj.r.squared, 5),3)
  slope=round(signif(fit$coef[[2]], 5),3)
  Intercept=round(signif(fit$coef[[1]],5 ),3)
  if (round(signif(summary(fit)$coef[2,4], 5),3)>0.001)
    (p=paste("=",round(signif(summary(fit)$coef[2,4], 5),3)))
  else
    p="< 0.001"
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",r2,
                       "Intercept =",Intercept,
                       " Slope =",slope,
                       " P",p))
}
###################################### aplico la funcion para graficar
ggplotRegression(fit1)################funciona mejor este
ggplotRegression(fit2)
########################################HAGO MAPA DE CLOROFILA
###Ecuacion que obtenemos de fit1
[cloroa]=-1306.5*B4/B5 + 1357.2 

Mapa_cloroa=-1306.5*(L8_22_02_2017_stack_mask$B4/L8_22_02_2017_stack_mask$B5)+1357.2
writeRaster(x =Mapa_cloroa, filename ="Mapa_cloroa", format ='GTiff', overwrite = TRUE)
Mapa_cloroa[Mapa_cloroa<0] <- NA

plot(Mapa_cloroa, main= "[clorofila-a] ug/L")

###################CALCULAR EL INDICE DE DEFINIDO POR CARLSON
Mapa_TSI<- (9.81*log(Mapa_cloroa))+30.6
plot(Mapa_TSI, main= "TSI 22-02-2017")

