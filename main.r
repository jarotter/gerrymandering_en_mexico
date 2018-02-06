source("nuevos_geodatos.r")
source("simulacion.R")

N <- max(cdmx$seccion)+1
dict <- data_frame(seccion = seq(0, N), index = -1)
for(i in 1:N){
  print(dict[i, 'seccion'])
  aux <- which(cdmx$seccion == pull(dict[i, 'seccion']))
  print(aux)
  if(length(aux) > 0){
    dict[i, 'index'] <- aux
  }
}

conflictivos_iniciales <- detect_conflicting(neighbors, cdmx)

neighbors <- neighbors %>%
  cbind(conflictivos_iniciales) %>%
  mutate(conflictivos = conflictivos_iniciales)

distritaciones <- cdmx$seccion

num_distritaciones <- 1
for(i in 1:num_distritaciones){
  xi <- take_one_sample(cdmx_graph, neighbors, cdmx, wd, wp, wi)
  distritaciones <- cbind(distritaciones, xi)
}