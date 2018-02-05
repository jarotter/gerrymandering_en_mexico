library(tidyverse)
library(dplyr)
library(tidygraph)

#CONSTANTES
pop_tot <- sum(cdmx$poblacion, na.rm = TRUE)
n_dis <- 27
wc <- wd <- wp <- lower <- 0
upper <- 100000


#FUNCIONES
pop_score_duke <- function(G){
  pop_ideal <- round(pop_tot / n_dis)
  
  score <- G %>%
    activate(nodes) %>%
    as_tibble() %>%
    group_by(distrito) %>%
    summarise(dist_pop = sum((poblacion/n_dis - 1)^2)) %>%
    summarise(score = sum(dist_pop)) %>%
    as.numeric()
  
  return(score)
}

is_conflicting <- function(G, u, v){
  flag <- G %>%
    activte(vertices) %>%
    as_tibble() %>%
    filter(seccion %in% c(u,v)) %>%
    pull(distrito) %>%
    unique() %>%
    length() 
    
  return(flag < 2)
    
}

detect_conflicting <- function(G){
  edge_tibble <- G %>%
    activate(edges) %>%
    as_tibble()
  n <- nrow(edge_tibble)
  indices <- rep(FALSE, n)
  
  for(i in 1:n){
    u <- edge_tibble[i, 'from']
    v <- edge_attr[i, 'to']
    indices[i] <- is_conflicting(u, v)
  }
}

boundaries <- function(G, n_dis){
  edge_tibble <- G %>%
    activate(edges) %>%
    as_tibble()

  n_sec <- nrow(edge_tibble)
  bdy <- rep(0, n_dis)

  for(i in 1:n_sec){
    u <- edge_tibble[i, 'from']
    v <- edge_tibble[i, 'to']
    if(is_conflicting(u,v)){
      bdy[u] <- bdy[u] + edge_tibble[i, 'weight']
      bdy[v] <- bdy[v] + edge_tibble[i, 'weight']
    }
  }
  
  return(bdy)
  
}

area <- function(G, n_dis){
  return(
    G %>%
    activate(nodes) %>%
    group_by(distrito) %>%
    summarise(dist_area = sum(area)) %>%
    summarise(score = sum(dist_area)) %>%
    as.numeric()
  )
}

iso_score_duke <- function(G, n_dis){
  areas <- area(G, n_dis)
  bdys <- boundaries(G, n_dis)
  
  areas/bdys %>%
    sum()
}

county_score_duke <- function(G, n_dis){
  distritos_por_delegacion <- G %>%
    activate(nodes) %>%
    as_tibble() %>%
    group_by(delegacion) %>%
    summarise(num_distritos = distrito %>% unique() %>% length()) %>%
    filter(num_distritos >= 2) %>%
    filter(!is.na(delegacion))
  
  w2 <- G %>% 
    activate(nodes) %>%
    as_tibble() %>%
    filter(!is.na(distrito)) %>%
    group_by(distrito, delegacion) %>%
    count() %>%
    arrange(delegacion, n) %>%
    group_by(delegacion) %>%
    summarise(sec_largest = n %>% sort(na.last = TRUE, decreasing = TRUE) %>% '['(2),
              n_tot = sum(n)) %>%
    filter(!is.na(sec_largest)) %>%
    filter(!is.na(delegacion)) %>%
    mutate(proporcion = sec_largest / n_tot) %>%
    summarise(score = proporcion %>% sqrt() %>% sum())
    
  w3 <- G %>%
    activate(nodes) %>%
    as_tibble() %>%
    filter(!is.na(distrito)) %>%
    group_by(distrito, delegacion) %>%
    count() %>%
    group_by(delegacion) %>%
    summarise(all_but_two = n %>% sort(na.last = NA, decreasing = TRUE) %>% '['(-c(1,2)) %>% sum(),
              n_tot = sum(n)) %>%
    arrange(delegacion) %>%
    mutate(proporcion = all_but_two / n_tot) %>%
    summarise(score = proporcion %>% sqrt() %>% sum())
    
  return(w2*sum(distritos_por_delegacion[num_distritos == 2]) + w3*M*sum(distritos_por_delegacion[num_distritos > 2]))

}

estacionaria <- function(G){
  
}

generar_nuevo_estado <- function(G){
  G_new <- G
  conflicting <- which(detect_conflicting(G))
  w <- sample(conflicting, 1)
  p <- rbinom(1, 1, 1/2)
  if(rbinom(1,1,1/2)){
    G_new <- G %>%
      set_vertex_attr('distrito', u, get.vertex.attribute(G, 'distrito', v))
  }else{
    G_new <- G %>%
      set_vertex_attr('distrito', v, get.vertex.attribute(G, 'distrito', u))
  }
  return(G_new)
}

revisar_conexidad <- function(G){
  node_tibble <- G %>%
    activate(nodes) %>%
    as_tibble()

  for(i in 1:27){
    indices <- which(node_tibble$distrito == i)
    n_comps <- G %>%
      induced_subgraph(indices) %>%
      count_components()
    if(n_comps > 1 ){
      return(FALSE)
    }
  }
  return(TRUE)
}

una_iteracion <- function(G, beta){
  G_new <- generar_nuevo_estado(G, beta)
  
  if(revisar_conexidad(G_new)){
    rho <- sum(detect_conflicting(G)) %>%
      '/'(sum(detect_conflicting(G_new))) %>%
      '*'(exp(-beta*(score(G_new)-score(G))))
    if(runif(1) < rho){
      G <- G_new
    }
  }
}


take_one_sample <- function(G){
    for(i in 1:40000){
      una_iteracion(G, 0)
    }
  
  lin_beta <- seq(from = 0, to = 1, length.out = 60000)
  for(i in 1:60000){
    una_iteracion(G, lin_beta[i])
  }
  
  for(i in 1:20000){
    una_itearcion(G, 1)
  }
}





