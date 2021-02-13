######### Matriz de Distancias ########
x = c(-4,2)
y = c(-3,5)
puntos = cbind(x,y)
plot(puntos)
d1 = dist(puntos,method = "euclidean")
d2 = dist(puntos,method = "manhattan")
d3 = dist(puntos,method = "canberra")

######### Matriz de Distancias Geogr�ficas ########
#install.packages("geosphere")
library(geosphere)
ciudad = data.frame(nombre = c("Quito","Guayaquil","Cuenca","Ambato","Azogues"),
                    lat  = c(-0.22985,
                             -2.1961601,
                             -2.9005499,
                             -1.2490799,
                             -2.7396901),
                    lon  = c(-78.5249481,
                             -79.8862076,
                             -79.0045319,
                             -78.6167526,
                             -78.8486023))

geo = distm(cbind(ciudad$lon,ciudad$lat))/1000

###### Mapa #######
library(rworldmap)
newmap = getMap(resolution = "low")
plot(newmap, xlim = c(-100, 100), ylim = c(20, 40), asp = 1)
plot(newmap, xlim = c(-100, -50), ylim = c(-10, 10), asp = 1)
plot(newmap, xlim = c(-80, -75), ylim = c(-5, 2), asp = 1)
points(ciudad$lon,ciudad$lat, col = "red", cex = .6)

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=ciudad$lon, lat=ciudad$lat, popup=ciudad$nombre)
m  # Print the map

