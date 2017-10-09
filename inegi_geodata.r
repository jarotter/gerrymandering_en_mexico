library(maptools)
library(rgeos)
cdmx <- readShapePoly("/Users/jarotter/Documents/DataLab/Gerrymandering/data/ECEG2010/poblacion/secciones_E09.shp")

names(cdmx) <- cdmx %>%
  names() %>%
  tolower()

secciones_ine <- cdmx_2015 %>%
  pull(seccion) %>%
  unique()

secciones_inegi <- cdmx@data %>%
  pull(seccion) %>%
  unique()

diferencia <- setdiff(secciones_inegi, secciones_ine)
