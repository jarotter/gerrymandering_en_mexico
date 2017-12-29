library(dplyr)
library(readr)
library(tidyr)
library(maptools)
library(rgeos)
library(stringr)
library(tidygraph)
library(Matrix)
library(igraph)
rm(list = ls())

#Leer la data electoral
cdmx <- readShapePoly("/Users/jarotter/Documents/DataLab/gerrymandering_en_mexico/data/integrated.shp")
cdmx <- cdmx@data %>%
  select(-NOM_MUN) %>%
  arrange(seccion)

#Y arreglar sus nombres
names(cdmx) <- cdmx %>%
  names() %>%
  tolower()

#Ahora vamos a leer los pesos sumados de vecinos.
neighbors <- read_csv("//Users/jarotter/Documents/DataLab/gerrymandering_en_mexico/data/cdmx_neighboors.csv", col_names = TRUE) %>%
  select(-basura)

#Y a guardar la suma de cada centro con sus vecinos.
sums <- neighbors %>%
  group_by(centro) %>%
  summarise(weight = sum(longitud))
names(sums) <- c('seccion', 'weight')

#Vamos a marcar cuáles secciones son interiores.
cdmx<- cdmx %>%
  left_join(sums, by = 'seccion') %>%
  mutate(is_inner = ifelse(abs(perimeter-weight)<0.1, TRUE, FALSE)) %>%
  select(-vecinos)
rm(sums)

#Necesitamos el vértice cero
zero <- data.frame(pobtot = NA, distrito = NA, municipio = NA, seccion = 0L, area = 0.0, perimeter = 0.0, weight = 0, is_inner = FALSE)

cdmx <- cdmx %>%
  rbind(zero) %>%
  arrange(seccion) %>%
  select(seccion, everything(), -weight)
rm(zero)

names(cdmx) <- c('section', 'population', 'ife15', 'county', 'area', 'perimeter', 'is_inner')

cdmx_graph <- graph_from_data_frame(
  d = neighbors,
  directed = FALSE,
  vertices = cdmx
)

