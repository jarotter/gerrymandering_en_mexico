source("nuevos_geodatos.r")
source("simulacion_naca.r")

#Hacer diccionario para hashear en O(1)
dict <- rep(-1, 6000)
for(i in 1:6000){
  aux <- which(cdmx$seccion == i)
  if(length(aux) > 0){
    dict[i] <- aux
  }
}

#Fijar conflictivos para detectarlos en O(pequeño)
system.time(l <- detect_conflicting(neighbors, cdmx, dict))
conflictivos_iniciales <- l[[1]]
duales <- l[[2]]
neighbors <- neighbors %>%
  cbind(conflictivos_iniciales) %>%
  mutate(conflictivos = conflictivos_iniciales) %>%
  cbind(duales)

#Distribución inicial de perímetros
perimetros_iniciales <- boundaries(neighbors, cdmx, dict)


distritaciones <- cdmx$seccion

num_distritaciones <- 1
for(i in 1:num_distritaciones){
  xi <- take_one_sample(cdmx_graph, neighbors, cdmx, wd, wp, wi, dict, perimetros_iniciales)
  distritaciones <- cbind(distritaciones, xi)
}