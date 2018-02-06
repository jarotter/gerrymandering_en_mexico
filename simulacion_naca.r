library(tidyverse)
library(dplyr)
library(tidygraph)

#CONSTANTES
pop_tot <- sum(cdmx$poblacion, na.rm = TRUE)
n_dis <- 24
wd <- 0.4
wi <- 2.5
wp <- 30000

#FUNCIONES
pop_score_duke <- function(V){
  pop_ideal <- round(pop_tot / n_dis)
  
  score <- V %>%
    group_by(distrito) %>%
    summarise(pop_dist = sum(poblacion)) %>%
    transmute(prop_dist = (pop_dist / pop_ideal - 1)^2) %>%
    summarise(score = sum(prop_dist)) %>%
    as.numeric()
  
  return(score)
}

is_conflicting <- function(V, u, v, dict){
  return(V[dict[u], 'distrito'] != V[dict[v], 'distrito'])
}

detect_conflicting <- function(E, V, dict){
  n <- nrow(E)
  indices <- rep(FALSE, n)
  
  for(i in 1:n){
    u <- E[i, 'from'] %>% pull()
    v <- E[i, 'to'] %>% pull()
    indices[i] <- is_conflicting(V, u, v, dict)
  }
  
  return(indices)
}

binary_search <- function(a, x){
  lo <- 0
  hi <- length(a)
  
  while(lo+1<hi){
    mi <- floor((lo+hi)/2)
    if(a[mi]>=x){
      hi <- mi
    }
    else{
      lo <- mi
    }
  }
  
  return(hi)
}


detect_conflicting_lim_scope <- function(E, V, u, dict){
  indices <- E$conflictivos
  ind_u <- binary_search(E$from, u)
  x <- E[ind_u, 'from']
  
  while(x == u){
    y <- E[ind_u, 'to']
    ind_primera_y <- binary_search(E$from, y)
    ind_ultima_y <- binary_search(E$from, y+1) -1
    ind_dual <- binary_search(E$to[ind_primera_y:ind_ultima_y], x) + ind_primera_y -1
    indices[ind_dual] <- indices[ind_u] <- is_conflicting(V, x, y, dict)
    ind_u <- ind_u + 1
    x <- E[ind_u, 'from']
  }
  return(indices)
}

boundaries <- function(E, V, dict){
  
  n <- nrow(E)
  bdy <- rep(0, n_dis)
  
  for(i in 1:n){
    u <- E[i, 'from']
    v <- E[i, 'to']
    
    if(E[i, 'conflictivos']){
      d1 <- cdmx[dict[u], 'distrito']
      d2 <- cdmx[dict[v], 'distrito']
      if(d1 != 0){
        bdy[d1] <- bdy[d1] + E[i, 'weight']
      }
      if(d2 != 0){
        bdy[d2] <- bdy[d2] + E[i, 'weight']
      }
    }
  }
  
  return(bdy)
  
}

area <- function(V){
  return(
    V %>%
      group_by(distrito) %>%
      filter(distrito != 0) %>%
      summarise(dist_area = sum(area))
  )
}

iso_score_duke <- function(E, V, dict){
  areas <- area(V) %>% 
    pull(dist_area)
  bdys <- boundaries(E, V, dict)
  
  score <- bdys^2/areas
  return(sum(score))
}

county_score_duke <- function(V){
  distritos_por_delegacion <- V %>%
    group_by(delegacion) %>%
    summarise(num_distritos = distrito %>% unique() %>% length()) %>%
    filter(num_distritos >= 2) %>%
    filter(!is.na(delegacion))
  
  w2 <- V %>%
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
  
  w3 <- V %>%
    filter(!is.na(distrito)) %>%
    group_by(distrito, delegacion) %>%
    count() %>%
    group_by(delegacion) %>%
    summarise(all_but_two = n %>% sort(na.last = NA, decreasing = TRUE) %>% '['(-c(1,2)) %>% sum(),
              n_tot = sum(n)) %>%
    arrange(delegacion) %>%
    mutate(proporcion = all_but_two / n_tot) %>%
    summarise(score = proporcion %>% sqrt() %>% sum())
  
  
  cuantos_2 <- sum(distritos_por_delegacion$num_distritos == 2)
  cuantos_mas <- sum(distritos_por_delegacion$num_distritos > 2)
  
  return(w2*cuantos_2 + w3*100*cuantos_mas)
  
}



revisar_conexidad <- function(E, V_temp){
  temp_graph <- graph_from_data_frame(
    d = E,
    directed = FALSE,
    vertices = V_temp
  )
  for(i in 1:n_dis){
    indices <- which(V_temp$distrito == i)
    n_comps <- temp_graph %>%
      induced_subgraph(indices) %>%
      count_components()
    if(n_comps != 1 ){
      return(FALSE)
    }
  }
  return(TRUE)
}


score <- function(E, V, wd, wp, wi, dict){
  return(wd*county_score_duke(V) + wp*pop_score_duke(V) + wi*iso_score_duke(E,V, dict))
}

una_iteracion <- function(G, E, V, beta, wd, wp, wi, dict){
  l <- generar_nuevo_estado(E, V, dict)
  V_temp <- l[[1]]
  u <- l[[2]]
  temp_graph <- graph_from_data_frame(
    d = E,
    directed = FALSE,
    vertices = V_temp
  )
  
  if(revisar_conexidad(E, V_temp)){
    new_conflicting <- detect_conflicting_lim_scope(E, V_temp, u, dict)
    rho <- sum(E$conflictivo) %>%
      '/'(sum(new_conflicting)) %>%
      '*'(exp(-beta*(score(E, V_temp, wd, wp, wi, dict)-score(E, V, wd, wp, wi, dict))))
    if(runif(1) < rho){
      E_temp <- E
      E_temp$conflictivo <- new_conflicting
      return(list(temp_graph, E_temp, V_temp))
    }
  }
  return(list(G, E, V))
}

generar_nuevo_estado <- function(E, V, dict){
  temp <- V
  conflicting <- which(E$conflictivos & E$to!=6000)
  
  ind_samp <- sample(conflicting, 1)
  u <- E[ind_samp,'from']
  v <- E[ind_samp, 'to']
  cual <- rbinom(1, 1, 1/2)
  if(cual){
    temp[dict[u], 'distrito'] <- V[dict[v], 'distrito']
    return(list(temp, u))
  }else{
    temp[dict[v], 'distrito'] <- V[dict[u], 'distrito']
    return(list(temp, v))
  }
}


take_one_sample <- function(G, E, V, wd, wp, wi, dict){
  l <- list(G, E, V)
  
  print('primer for externo')
  for(i in 1:40000){
    print(i)
    l <- una_iteracion(l[[1]], l[[2]], l[[3]], 0, wd, wp, wi, dict)
  }
  
  print('segundo for externo')
  lin_beta <- seq(from = 0, to = 1, length.out = 60000)
  for(i in 1:60000){
    print(i)
    l <- una_iteracion(l[[1]], l[[2]], l[[3]], lin_beta[i], wd, wp, wi, dict)
  }
  
  print('tercer for externo')
  for(i in 1:20000){
    print(i)
    l <-  una_iteracion(l[[1]], l[[2]], l[[3]], 1, wd, wp, wi, dict)
  }
  
  return(l[[3]]$distrito)
  print('muestra tomada')
}






