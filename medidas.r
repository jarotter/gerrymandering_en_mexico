proporcion_desperdicidados <- function(V,distritacion){
  desperdiciados <- 0
  votos_totales <- 0
  
  for(i in 1:24){
    secciones_dist <- which(distritacion==i)
    votos_partido <- colSums(V[secciones_dist,c(3:12,17:18)],na.rm=TRUE)
    orden <- order(votos_partido,decreasing = TRUE)
    votos_totales <- votos_totales + sum(votos_partido)
    desperdiciados <- desperdiciados + sum(votos_partido) - 2*as.integer(votos_partido[orden[2]])
  }
  
  return(desperdiciados/votos_totales)
}
