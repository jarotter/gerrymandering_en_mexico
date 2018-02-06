source("nuevos_geodatos.r")
source("simulacion_con_diccionario.R")

#Hacer diccionario para hashear en O(1)
dict <- rep(-1, 6000)
for(i in 1:6000){
  aux <- which(cdmx$seccion == i)
  if(length(aux) > 0){
    dict[i] <- aux
  }
}

#Fijar conflictivos para detectarlos en O(pequeño)
conflictivos_iniciales <- detect_conflicting(neighbors, cdmx, dict)
neighbors <- neighbors %>%
  cbind(conflictivos_iniciales) %>%
  mutate(conflictivos = conflictivos_iniciales)

distritaciones <- cdmx$seccion

num_distritaciones <- 1
for(i in 1:num_distritaciones){
  xi <- take_one_sample(cdmx_graph, neighbors, cdmx, wd, wp, wi, dict)
  distritaciones <- cbind(distritaciones, xi)
}