source("nuevos_geodatos.r")
source("simulacion_eficiente.r")

#Hacer diccionario para hashear en O(1)
dict <- rep(-1, 6000)
for(i in 1:6000){
  aux <- which(cdmx$seccion == i)
  if(length(aux) > 0){
    dict[i] <- aux
  }
}

#Fijar conflictivos para detectarlos en O(pequeño)
l <- detect_conflicting(neighbors, cdmx, dict)
conflictivos_iniciales <- l[[1]]
duales <- l[[2]]
neighbors <- neighbors %>%
  cbind(conflictivos_iniciales) %>%
  mutate(conflictivos = conflictivos_iniciales) %>%
  cbind(duales)

#Distribución inicial de perímetros
perimetros_iniciales <- boundaries(neighbors, cdmx, dict) %>%
  '/'(2)

#Conteo inicial de delegaciones
conteo_delegaciones <- inicializar_county_score(cdmx, dict)


distritaciones <- cdmx$ine18 %>% 
  as.tibble()
names(distritaciones) <- 'ine18'

num_distritaciones <- 10
for(i in 1:num_distritaciones){
  print(i)
  xi <- take_one_sample(cdmx_graph, neighbors, cdmx, wd, wp, wi, dict, perimetros_iniciales, conteo_delegaciones)
  colname <- paste('p',ncol(distritaciones), sep="")
  distritaciones <- cbind(distritaciones, xi)
  names(distritaciones)[ncol(distritaciones)] <- colname
  cdmx$distrito <- cdmx$ine18
  neighbors$conflictivos <- neighbors$conflictivos_iniciales
  perimetros_iniciales <- boundaries(neighbors, cdmx, dict)
}