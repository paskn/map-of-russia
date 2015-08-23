library(ggplot2)
library(maptools)
# http://gis-lab.info/qa/osm-adm.html
shape <- readShapeLines("./data/adm4_region_f")
projection <- CRS("+init=epsg:3413 +lon_0=105")
proj4string(shape) <- CRS("+proj=longlat +datum=WGS84")
map_prof <- spTransform(shape, projection)

map <- fortify(map_prof)

df2 <- sp::merge(map, map_prof, by.x = 'id', by.y = "row.names")

# this weird sequence is windows-specific, please consider before running
write.csv(df2, "df2.csv", fileEncoding = "CP1251")

df2 <- read.csv("df2.csv", encoding="UTF-8")
# end of weird sequence

russia_adm_map <- df2
save(russia_adm_map, file = "./data/processed/russia_adm_map.RData")


