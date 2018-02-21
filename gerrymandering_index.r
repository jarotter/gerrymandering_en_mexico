library(tidyverse)

gerrymandering_score(V,distritacion,permutacion){
  votos_originales <- V %>%
                      group_by(ine18) %>%
                      select(pan:ph) %>%
                      colSums() %>%
                      mutate(total=sum(pan:ph)) %>%
                      '/'(total) %>%
                      select(pan:ph)
  votos_nuevos <- V %>%
                  mutate(nueva_dist=distritacion) %>%
                  group_by(nueva_dist) %>%
                  select(pan:ph) %>%
                  colSums() %>%
                  mutate(total=sum(pan:ph)) %>%
                  '/'(total) %>%
                  select(pan:ph) %>%
                  '['(permutacion)
  
  return(norm(votos_originales-votos_nuevos,type="F")/sqrt(nrow(votos)))
}

metropolis_gerrymandering(V,distritacion,permutacion,beta){
  indices <- 1:length(permutacion)
  c(swap_1,swap_2) <- sample(indices,2)
  indices[c(swap_1,swap_2)] <- indices[c(swap_2,swap_1)]
  nueva_permutacion <- permutacion[indices]
  rho <- exp(gerrymandering_score(V,distritacion,nueva_permutacion)-gerrymandering_score(permutacion))
}

gerrymandering_index(V,distritacion){
  permutacion <- 1:length(distritacion)
  for(i in 1:4000){
    permutacion <- metropolis_gerrymandering(V=V,distritacion=distritacion,
                                             permutacion=permutacion,beta=0)
  }
  
  for(i in 1:6000){
    permutacion <- metropolis_gerrymandering(V=V,distritacion=distritacion,
                                             permutacion=permutacion,beta=i/6000)
  }
  
  for(i in 1:2000){
    permutacion <- metropolis_gerrymandering(V=V,distritacion=distritacion,
                                             permutacion=permutacion,beta=1)
  }
  
  return(gerrymandering_score(V,distritacion,permutacion))
}
