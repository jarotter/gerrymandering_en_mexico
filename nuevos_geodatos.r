library(tidyverse)
library(maptools)
library(igraph)
library(tidygraph)
rm(list = ls())

#Leer la data electoral
full_2015 <- read_delim("data/diputados_2015.csv", delim = "|", col_names = TRUE, skip = 5)
names(full_2015) <- full_2015 %>%
  names() %>%
  tolower()
cdmx_2015 <- full_2015 %>%
  filter(estado == 9) %>%
  select(seccion, distrito, 12:28)
rm(full_2015)


#Leer la data geográfica
cdmx <- readShapePoly("data/cdmx.shp")
cdmx <- cdmx@data %>%
  select(-NOM_MUN) %>%
  arrange(seccion)

#Ahora vamos a leer los pesos sumados de vecinos.
neighbors <- read_csv("data/cdmx_neighboors.csv", col_names = TRUE) %>%
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

names(cdmx) <- c("poblacion", "ine10", "delegacion", "seccion", "area", "perimetro", "peso", "is_inner")

#Necesitamos el vértice exterior
outside <- data.frame(poblacion = 0, ine10 = 0, delegacion = 0, seccion = 6000L, area = 0.0, perimetro = 0.0, peso = 0, is_inner = FALSE)
cdmx <- cdmx %>%
  rbind(outside) %>%
  arrange(seccion) %>%
  select(seccion, everything(), -peso)
rm(outside)


#Leemos la redistritacion de 2018
ine18 <- read_csv("data/ine18.csv")
names(ine18) <- c("ine18", "seccion")
cdmx <- cdmx %>%
  full_join(ine18) %>%
  mutate(distrito = ine18)

#Revisar si coinciden los que no son NA
cdmx_2015 %>%
  select(seccion, distrito) %>%
  unique() %>%
  left_join(cdmx, by = 'seccion') %>%
  transmute(distintos = distrito.x != ine10) %>%
  summarise(sum(distintos, na.rm = TRUE))

#Buscamos secciones que existieron sólo después del mapa de 2010
nuevas <- cdmx %>%
  filter(is.na(ine10)) %>%
  pull(seccion)
#Sólo hay que ver cuántos votos perdimos for the sake of science
votos_perdidos_seciones_nuevas <- cdmx_2015 %>%
  filter(seccion %in% nuevas) %>%
  pull(total_votos) %>%
  sum()
#Como no existen en el mapa, podemos ignorarlas de la geodata sin perder conexidad.
cdmx <- cdmx %>%
  filter(!(seccion %in% nuevas))
#Y eliminarlas de la data electoral también
cdmx_2015 <- cdmx_2015 %>%
  filter(!(seccion %in% nuevas))

#Ahora buscamos secciones que dejaron de existir para 2018
perdidas <- cdmx %>%
  filter(is.na(ine18)) %>%
  pull(seccion)
#Pero el exterior no debe usarse
perdidas <- perdidas[-length(perdidas)]
#Si en 2015 aún existitian, no podemos usar esa data electoral
cdmx_2015 %>%
  filter(seccion %in% perdidas)
#No hay, nos despreocupamos.
#Pero no podemos borrarlas del mapa porque perderíamos conexidad. Unimos cada sección perdida a su vecino con mayor
  #frontera común. Después de esto ya podemos reducir la tabla de vecinos.
reemplazos <- neighbors %>%
  filter(centro %in% perdidas) %>%
  group_by(centro) %>%
  filter(longitud == max(longitud))
#Ahora eliminamos las entradas duplicadas de vecinos para el grafo
neighbors<- neighbors %>%
  filter(centro < vecino)

#Ya que tenemos el instructivo, vamos a programarlo
for(i in 1:6){
  remp <- reemplazos[i, 'centro'] %>%
    pull(centro)
  ind_remp <- which(cdmx$seccion == remp)
  
  repl <- reemplazos[i, 'vecino'] %>%
    pull(vecino)
  ind_repl <- which(cdmx$seccion == repl)
  
  cdmx[ind_remp, 'poblacion'] <- cdmx[ind_remp, 'poblacion'] + cdmx[ind_repl, 'poblacion']
  cdmx[ind_remp, 'area'] <- cdmx[ind_remp, 'area'] + cdmx[ind_repl, 'area']
  cdmx[ind_remp, 'delegacion'] <- cdmx[ind_repl, 'delegacion']
  cdmx[ind_remp, 'perimetro'] <- cdmx[ind_remp, 'perimetro'] + cdmx[ind_repl, 'perimetro'] - reemplazos[i, 'longitud']
  cdmx[ind_remp, 'is_inner'] <- cdmx[ind_remp, 'is_inner'] && cdmx[ind_repl, 'is_inner']
  
  ind_remp <- which(neighbors$centro == remp)
  for(ind in ind_remp){
    neighbors[ind, 'centro'] <- repl
  }
  
  ind_remp <- which(neighbors$vecino == remp)
  for(ind in ind_remp){
    neighbors[ind, 'vecino'] <- repl
  }
}

#Borrar toda la basura generada
rm(i, ind, ind_remp, ind_repl, remp, repl, reemplazos)

#Y ahora sí podemos eliminar de la geodata las secciones viejas sin perder conexidad
cdmx <- cdmx %>%
  filter(!(seccion %in% perdidas))

#Antes de generar el grafo, añadimos a todos las secciones exteriores un arista hacia el exterior
n <- nrow(cdmx)
for(i in 1:(n-1)){
  if(!(cdmx[i, 'is_inner'])){
    ind <- which(sums$seccion == cdmx[i, 'seccion'])
    temp <- c(cdmx[i, 'seccion'], 6000, cdmx[i, 'perimetro'] - sums[ind, 'weight'])
    names(temp) <- c('centro', 'vecino', 'longitud')
    neighbors <- neighbors %>%
      rbind(temp)
  }
}

cdmx[nrow(cdmx), 'ine18'] <- cdmx[nrow(cdmx), 'distrito'] <- 0
rm(i, ind, n, sums, temp, ine18)

names(neighbors) <- c('from', 'to', 'weight')
neighbors$from <- neighbors$from %>% as.integer()
neighbors <- neighbors %>%
  arrange(from)
neighbors_short <- neighbors

#Duplicamos la tabla para mejorar detect_conflicting
N <- nrow(neighbors)
neighbors <- neighbors %>%
  rbind(neighbors)
for(i in N:(2*N)){
  aux <- neighbors[i,'from']
  neighbors[i,'from'] <- neighbors[i,'to']
  neighbors[i,'to'] <- aux
}
neighbors <- neighbors %>%
  arrange(from)
neighbors_short <- neighbors_short %>%
  arrange(from)

#Crear el grafo
cdmx_graph <- graph_from_data_frame(
  d = neighbors_short,
  directed = FALSE,
  vertices = cdmx
)






