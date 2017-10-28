library(dplyr)
library(readr)
library(tidyr)
library(maptools)
library(rgeos)
library(stringr)
library(tidygraph)
library(Matrix)
library(igraph)

cdmx <- readShapePoly("/Users/jarotter/Documents/DataLab/gerrymandering_en_mexico/data/poblacion/secciones.shp")
cdmx <- cdmx@data

names(cdmx) <- cdmx %>%
  names() %>%
  tolower()

cdmx$nom_mun <- cdmx$nom_mun %>%
  tolower() %>%
  str_replace_all(" ", "_")

cdmx <- cdmx %>%
  arrange(seccion)

adj_matrix <- read_csv("/Users/jarotter/Documents/DataLab/shared_borders.csv", col_names = FALSE)
#Pesa demasiado para GitHub, hablar con Bruno para subirla al ftp de datalab y leerla desde ahí.
adj_matrix <- adj_matrix[2:nrow(adj_matrix),2:ncol(adj_matrix)]
names(adj_matrix) <- c(1:5546)

#¿Es simétrica?
a3 <- adj_matrix - t(adj_matrix)
a3 %>% 
  as.matrix() %>%
  norm("o")
#Parece que sí, pero quiero suponer que el error de 1e-11 es numérico.
rm(a3)

#¿Está bien armada?
which(rowSums(adj_matrix) == 0)
which(colSums(adj_matrix) == 0)
#Pero vemos en qgis que de la 4912 en adelante no existen. La patológica es 4608. Llenamos a mano los valores, pero igual
#quiero guardar los demás porque más adelante van a ser necesarios
patologicos <- which(rowSums(adj_matrix) == 0)[-1]

adj_matrix[4608, 4607] <- adj_matrix[4607, 4608] <- 441.83
adj_matrix[4608, 4627] <- adj_matrix[4627, 4608] <- 268.261
adj_matrix[4608, 4995] <- adj_matrix[4995, 4608] <- 267.609
adj_matrix[4608, 4609] <- adj_matrix[4609, 4608] <- 200

adj_matrix <- adj_matrix %>%
  as.matrix() %>%
  Matrix(sparse = TRUE)

cdmx_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "max", weighted = TRUE) %>%
  delete_vertices(patologicos) %>%
  set_vertex_attr("poblacion", value = cdmx$pobtot) %>%
  set_vertex_attr("area", value = cdmx$area) %>%
  set_vertex_attr("2015", value = cdmx$distrito)


  