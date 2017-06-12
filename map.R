
library(maps) #mapas simples, eixos, escala, cidades 
library(mapdata) #base de dados WorldHires e rios
library(rworldmap) #outra base de dados de mapas do mundo
library(maptools) #Ler ESRI shapefiles 
library(mapproj) #Projeções e grids
library(ggmap) #Gmaps, OSM + mapas baseados em ggplot2
library(rgdal)
library(maps)

par(mar = c(1, 1, 1, 1))
m <- map("world", "Brazil", fill = T, col = "grey90")
map.axes()
map.scale(ratio = F, cex = 0.7) #tentem ratio = T
abline(h = 0, lty = 2)
map(, , add = T)
map.cities(country = "Brazil", minpop = 3000000, pch = 19, cex = 1.2)# pacote maps
#map.grid(m, col = "grey50", font = 1, lwd = 0.7 , pretty = F)#library(mapproj)
#map(, "brazil", project = "albers", par = c(-2, -22), lwd = 2)
#map.grid(m, lwd = 2, font = 1, cex = 0.7, col = "grey70") 
legend(-0.3, 4.37, legend =  c("Albers", "WGS84"), col = c("black", "grey70"), lty = c(1, 2), lwd = 2, horiz = T, bty = "o", title = "Projeção", box.col = "white")
library(rgdal)
# maptools::readShapePoly
br <- readOGR(dsn = "./map/shapefiles/brasil/BRA_adm_shp",layer = "BRA_adm0") # 0: país, 1: estados, 2: municípiosestados1 <- readOGR( dsn = "./map/shapefiles/brasil/BRA_adm_shp/BRA_adm1.shp") # 0: país, 1: estados, 2: municípios
cidades <- readOGR( dsn = "./map/shapefiles/brasil/BRA_adm_shp",layer = "BRA_adm2")
biomas <- readOGR( dsn = "./map/shapefiles/brasil", "BR_BIOMAS_IBGE")
estados <- readOGR("./map/shapefiles/brasil/BRA_adm_shp/", "BRA_adm1")
proj4string(estados)
names(cidades)
head(cidades$NAME_1)
plot(RJ, add = T)
RJ1 <- estados[estados@data$NAME_1 == "Rio de Janeiro", ] ##seleciona e cria um novo shapefile
RJ <- cidades[cidades@data$NAME_1 == "Rio de Janeiro", ] ##seleciona e cria um novo shapefile
plot(br)
png(filename = "location.png")

par(mar = c(3, 3, 2, 2))
plot(RJ1)
map.axes()
map.scale(ratio = F)
plot(RJ[RJ$NAME_2 == "Silva Jardim", ], add = T, col = "grey70") #seleciona mas não cria um novo shapefile, apenas na hora de plotar
plot(br, add = T)

dev.off()

PDA <- readOGR("./map/shapefiles/rbpda/", "LimitedaRB")
br101 <- readOGR("./map/shapefiles/rbpda/", "BR_101")
saojoao <- readOGR("./map/shapefiles/rbpda/", "riosaojoao")
SOSma <- readOGR(dsn = "./map/shapefiles/brasil/SOSMA/", "rema_fd4d801731725513a4d77aa9bb35534b5305") 

plot(PDA)
plot(br101, add = T, col = "grey")
plot(saojoao, col = "dodgerblue", add = T)
map.axes()
plot(SOSma, add = T) #### NAO FUNCIONA PORQUE ESTA EM OUTRA PROJECAO!
#map.scale() também não fica bom. 

proj4string(SOSma) 
plot(SOSma)
map.axes()#SOS está tudo em outra projeção 

proj4string(PDA) <- CRS("+init=epsg:29193")## isto é UTM em SAD69 brasileiro
proj4string(br101) <- CRS("+init=epsg:29193")## isto é UTM em SAD69 brasileiro
proj4string(saojoao) <- CRS("+init=epsg:29193")## isto é UTM em SAD69 brasileiro

PDA.wsg84 <- spTransform(PDA, CRS("+proj=longlat +datum=WGS84"))
SOS.wgs84 <- spTransform(SOSma, CRS("+proj=longlat +datum=WGS84"))
br101.wgs84 <- spTransform(br101, CRS("+proj=longlat +datum=WGS84"))
saojoao.wgs84 <- spTransform(saojoao, CRS("+proj=longlat +datum=WGS84"))
#plot(RJ, add = T, names = T) ##nao fica legal

fuego1993 <- readOGR(dsn = "./map/queimadas",layer = "incendio1993")
proj4string(fuego1993) <- CRS("+init=epsg:29193")## isto é UTM em SAD69 brasileiro
fuego1993.wsg84 <- spTransform(fuego1993, CRS("+proj=longlat +datum=WGS84"))

fuego2002 <- readOGR(dsn = "./map/queimadas",layer = "incendio2002")
proj4string(fuego2002)
fuego2002.wsg84 <- spTransform(fuego2002, CRS("+proj=longlat +datum=WGS84"))

fuego2007 <- readOGR(dsn = "./map/queimadas",layer = "Incendio_2007")
proj4string(fuego2007)
fuego2007.wsg84 <- spTransform(fuego2007, CRS("+proj=longlat +datum=WGS84"))

fuego2010 <- readOGR(dsn = "./map/queimadas",layer = "incendio2010p")
proj4string(fuego2010)
fuego2010.wsg84 <- spTransform(fuego2010, CRS("+proj=longlat +datum=WGS84"))

parcelas <- readOGR(dsn = "./map/shapefiles/", layer = "parcelasandre")
proj4string(parcelas)
parcelas.wsg84 <- spTransform(parcelas, CRS("+proj=longlat +datum=WGS84"))
parcelas_j <- readOGR(dsn = "./map/shapefiles/", layer = "parcelasjeronimo")
proj4string(parcelas_j)
parcelas_j_wsg84 <- spTransform(parcelas_j, CRS("+proj=longlat +datum=WGS84"))

plot(PDA.wsg84, add = F, border = "black", lwd = 3)
plot(saojoao.wgs84, add= T, border = "dodgerblue", lwd = 2)
plot(SOS.wgs84, add = T, col = alpha("green", 0.3), border = alpha("darkgreen", 0.5))
plot(br101.wgs84, add= T, col = "red", lwd= 2)
map.scale(ratio = F)
map.axes()
plot(fuego1993.wsg84, add = T, border = "grey", col = alpha("grey", 0.3))
plot(fuego2002.wsg84, add = T, border = "grey", col = alpha("grey", 0.3))
plot(fuego2007.wsg84, add = T, border = "grey", col = alpha("grey", 0.3))
plot(fuego2010.wsg84, add = T, border = "grey", col = alpha("grey", 0.3))
points(parcelas.wsg84, bg = rep(c("red","orange"),each = 3), pch = 21, cex = 1.5)

points(parcelas_j_wsg84[1:9,], bg =rep(c("yellow","green","darkgreen"),each = 3), pch = 21, cex = 1.5)
legend(x = -42.26,y = -22.5, fill = c(alpha("green",0.3),alpha("grey",0.3)), 
       legend = c("Forest remnants \n SOS Mata Atlântica 2008","Fires 1993-2010"), bty = "n")     
legend(x = -42.26,y = -22.51, pch = 21, 
       legend = c("High-disturbance (2010, three times)", "Medium disturbance (2002, twice)", "Low disturbance (1990, once)", "Secondary forests", "Mature forests"), bty = "n", pt.bg = c("red", "orange", "yellow","green","darkgreen"), pt.cex = 1.3)     


proj4string(PDA.wsg84)
plot(fuego1993,add = F, col = "orange")

plot(SOS.wgs84, add = F, col = alpha("green", 0.5), border = alpha("white", 0))
plot(PDA.wsg84, add = T, border = "red", lwd = 2)
plot(RJ, add = T) 
class(RJ)
pts.RJ <- getSpatialPolygonsLabelPoints(RJ)
text(pts.RJ@coords, labels = RJ$NAME_2)
map.axes()
map.scale(y = -22.78, ratio = F, cex = 0.7, font = 2)


MA <- biomas[biomas$CD_LEGENDA =  = "MATA ATLANTICA", ]

plot(br, border = "grey50")
plot(MA, col = alpha("darkgreen", 0.5), add = T)
map.axes()
map.scale(x = -45, ratio = F)

source("./fct/create_scale_bar.R")
brasil.mapa <- borders("worldHires", regions = "Brazil", fill = "grey90", colour = "black")
bra.map <- map_data("worldHires", "Brazil")

mapa.1 <- ggplot() + brasil.mapa + coord_equal()  ##borders inclui o mapa como uma camada no fundo 
mapa.1
mapa.2 <- ggplot() + 
  geom_polygon(data = bra.map, aes(x = long, y = lat, group = group)) +
  coord_equal() # map_data cria um dataframe que deve ser acrescentado como geom_polygon()
mapa.2

MA <- biomas[biomas$CD_LEGENDA =  = "MATA ATLANTICA", ]

MAf <- fortify(MA)
PDAf <- fortify (PDA.wsg84)

mapa.2 <- mapa.1 +
  scaleBar(lon = -40, lat = -30, distanceLon = 500, distanceLat = 70, distanceLegend = 150, dist.unit = "km", orientation = FALSE) +
  geom_polygon(data = MAf, aes(x = long, y = lat, group = group), fill = "darkgreen")
mapa.2


library(ggmap)
pda <- get_map("Reserva Biológica de Poço das Antas", zoom = 12)
pda.map <-   ggmap(pda, extent =  "panel")
pda.map

#pda <- get_map("Reserva Biológica de Poço das Antas", zoom = 12, source =  "stamen", maptype = "toner")
# pda.toner <-   ggmap(pda, extent =  "panel")
#pda.toner

#pda <- get_map("Reserva Biológica de Poço das Antas", zoom = 12, source = "osm")
#pda.osm <-   ggmap(pda, extent =  "panel")
#pda.osm

mapa.test <- pda.map +
  geom_polygon(data = PDAf, aes(x = long, y = lat, group = group), 
               fill = alpha("white", 0.2), color = "black", size = 0.5) 

mapa.test          
#ggsave(filename = "map1.png", device = "png")

library(raster)
alt <- raster("./rasters/h_dem/hdr.adf")
plot(alt)

arquivos <- list.files("./rasters", full.names = T)
head(arquivos)
env <- stack(arquivos)

par(mar = c(5, 4, 1, 1))
plot(env[[1:9]])

MA <- biomas[biomas$CD_LEGENDA =  = "MATA ATLANTICA", ]
plot(MA)

#### CROP THEN MASK
alt.crop <- crop(x = alt, y = MA)
plot(alt.crop)
alt.crop <- mask(alt.crop, MA)
plot(alt.crop)
plot(MA, add = T)

plot(alt.crop)
plot(alt.crop>0, legend =  F)

dir.create("./MataAtlantica")
writeRaster(alt.crop, filename = "./MataAtlantica/alt", format =  "GTiff", overwrite = T)
### depois dá para ler diretamente estes arquivos ----
alt.mat <- raster("./MataAtlantica/alt.tif")
plot(alt.mat)

caryocar <- read.delim("./occurrence data/caryocar.txt")
head(caryocar)
plot(alt>0, legend = F, col = "grey")
points(caryocar$lon, caryocar$lat, pch = 19, cex = 0.5)

library(spatstat)
ch <- convexhull.xy(cbind(caryocar$lon, caryocar$lat))
plot(ch, add = T, col = alpha("green", 0.5))



library(rgbif)
library(sp)
euterpe <- occ_data(scientificName = "Euterpe edulis", hasCoordinate = T)
euterpe <- SpatialPoints(cbind(euterpe$data$decimalLongitude, euterpe$data$decimalLatitude))
if(!dir.exists("occs")){
  dir.create("./occs")
  write.table(euterpe, "./occs/euterpe.txt",  row.names = F)
}


euterpe2 <- read.delim("./occs/euterpe.txt", sep = "")
plot(MA)
points(euterpe2[, c(1, 2)], pch = 19, col = "red", cex = 0.8)
map.axes()
map.scale(ratio = F, relwidth = 0.3, x = -45)
```


## Informação adicional

# R Special Interest Group on using Geographical data and Mapping https://stat.ethz.ch/mailman/listinfo/r-sig-geo  
# TaskView: Analysis of Spatial Data: https://cran.r-project.org/web/views/Spatial.html  
# Lovelace, R., & Cheshire, J. (2014). Introduction to visualising spatial data in R. National Centre for Research Methods Working Papers, 14(03). Retrieved from https://github.com/Robinlovelace/Creating-maps-in-R
29  
# Kahle & Wickham 2012 - ggmaps: https://dl.dropboxusercontent.com/u/24648660/ggmap%20useR%202012.pdf
# Kim Gilbert http://www.zoology.ubc.ca/~kgilbert/mysite/Miscellaneous_files/R_MakingMaps.pdf  
# Kevin Johnson http://www.kevjohnson.org/making-maps-in-r/  
# Santiago Begueria. Mapping with ggplot2: hexbin maps. http://santiago.begueria.es/2016/04/mapping-with-ggplot2-hexbin-maps/
# http://geog.uoregon.edu/GeogR/topics/maps01.html
# A função para acrescentar escala em ggplot2: http://editerna.free.fr/wp/?p = 76