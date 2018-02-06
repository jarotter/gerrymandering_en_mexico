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
  #tic("pop score")
  pop_ideal <- round(pop_tot / n_dis)
  
  score <- V %>%
    group_by(distrito) %>%
    summarise(pop_dist = sum(poblacion)) %>%
    transmute(prop_dist = (pop_dist / pop_ideal - 1)^2) %>%
    summarise(score = sum(prop_dist)) %>%
    as.numeric()
  
  #toc()
  return(score)
}

is_conflicting <- function(V, u, v, dict){
  return(V[dict[u], 'distrito'] != V[dict[v], 'distrito'])
}

detect_conflicting <- function(E, V, dict){
  n <- nrow(E)
  indices <- rep(FALSE, n)
  duales <- rep(0, n)
  
  for(i in 1:n){
    if(i %% 1001 == 1){
      print(i)
    }
    u <- E[i, 'from'] %>% pull()
    v <- E[i, 'to'] %>% pull()
    indices[i] <- is_conflicting(V, u, v, dict)
    
    ind_v <- binary_search(E$from, v)
    while(ind_v <= n & E[ind_v, 'to']!=u){
      ind_v <- ind_v + 1
    }
    duales[i] <- ind_v
  }
  
  return(list(indices, duales))
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
  indices  <- E$conflictivos
  ind_u <- binary_search(E$from, u)
  x <- E[ind_u, 'from']
  
  n <- nrow(E)
  
  while(ind_u <= n & E[ind_u, 'from'] == u){
    ind_dual <- E$duales[ind_u]
    y <- E[ind_u, 'to']
    indices[ind_dual] <- indices[ind_u] <- is_conflicting(V,x,y,dict )
    ind_u <- ind_u + 1
  }
  return(indices)
}

boundaries <- function(E, V, dict){
  tic("boundaries")
  n <- nrow(E)
  bdy <- rep(0, n_dis)
  
  for(i in 1:n){
    u <- E[i, 'from']
    v <- E[i, 'to']
    
    if(E[i, 'conflictivos']){
      d1 <- cdmx[dict[u], 'ine18']
      d2 <- cdmx[dict[v], 'ine18']
      if(d1 != 0){
        bdy[d1] <- bdy[d1] + E[i, 'weight']
      }
      if(d2 != 0){
        bdy[d2] <- bdy[d2] + E[i, 'weight']
      }
    }
  }
  
  toc()
  return(bdy)
}

update_boundaries <- function(E, V, V_temp, v, dict, perimetros){
  n <- nrow(E)
  distrito_anterior <- V[dict[v], 'distrito']
  distrito_nuevo <- V_temp[dict[v], 'distrito']
  
  ind_primera <- binary_search(E$from, v)
  ind <- ind_primera
  while(ind <= n & E$from[ind]==v){
    if(V[dict[E$to[ind]], 'distrito'] == distrito_anterior){
      perimetros[distrito_anterior] <- perimetros[distrito_anterior] + E$weight[ind]
    }else{
      perimetros[distrito_anterior] <- perimetros[distrito_anterior] - E$weight[ind]
    }
    
    if(V[dict[E$to[ind]], 'distrito'] == distrito_nuevo){
      perimetros[distrito_nuevo] <- perimetros[distrito_nuevo] - E$weight[ind]
    }
    else{
      perimetros[distrito_nuevo] <- perimetros[distrito_nuevo] + E$weight[ind]
    }
    ind <- ind + 1
  }
  perimetros[distrito_anterior] <- perimetros[distrito_anterior]
  
  return(perimetros)
}

area <- function(V){
  #tic("area")
  return(
    V %>%
      group_by(distrito) %>%
      filter(distrito != 0) %>%
      summarise(dist_area = sum(area))
  )
  #toc()
}

iso_score_duke <- function(E, V, V_temp, v, dict, perimetros){
  #tic("iso score")
  areas <- area(V) %>% 
    pull(dist_area)

  bdys <- update_boundaries(E, V, V_temp, v, dict, perimetros)

  score <- bdys^2/areas
  #toc()
  return(sum(score))
}

county_score_duke <- function(V){
  #tic("county score")
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
  
  #toc()
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


score <- function(E, V, V_temp, v, wd, wp, wi, dict, perimetros){
  return(wd*county_score_duke(V) + wp*pop_score_duke(V) + wi*iso_score_duke(E, V, V_temp, v, dict, perimetros))
}

una_iteracion <- function(G, E, V, beta, wd, wp, wi, dict, perimetros){
  #tic("generar estado")
  l <- generar_nuevo_estado(E, V, dict)
  #toc()
  V_temp <- l[[1]]
  u <- l[[2]]
  temp_graph <- graph_from_data_frame(
    d = E,
    directed = FALSE,
    vertices = V_temp
  )
  #tic("revisar conexidad")
  if(revisar_conexidad(E, V_temp)){
    #toc()
    new_conflicting <- detect_conflicting_lim_scope(E, V_temp, u, dict)

    #tic("calcular j")
    rho <- sum(E$conflictivos) %>%
      '/'(sum(new_conflicting)) %>%
      '*'(exp(-beta*(score(E=E, V=V, V_temp=V, v=u, wd=wd, wp=wp, wi=wi, dict=dict, perimetros=perimetros)
                     -score(E=E, V=V, V_temp=V_temp, v=u, wd=wd, wp=wp, wi=wi, dict=dict, perimetros=perimetros))))
    if(runif(1) < rho){
      #toc()
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


take_one_sample <- function(G, E, V, wd, wp, wi, dict, perimetros){
  l <- list(G, E, V)
  
  tic('primer for externo')
  for(i in 1:400){
    l <- una_iteracion(l[[1]], l[[2]], l[[3]], 0, wd, wp, wi, dict, perimetros)
  }
  toc()
  
  tic('segundo for externo')
  lin_beta <- seq(from = 0, to = 1, length.out = 600)
  for(i in 1:600){
    l <- una_iteracion(l[[1]], l[[2]], l[[3]], lin_beta[i], wd, wp, wi, dict, perimetros)
  }
  toc()
  
  tic('tercer for externo')
  for(i in 1:200){
    l <-  una_iteracion(l[[1]], l[[2]], l[[3]], 1, wd, wp, wi, dict, perimetros)
  }
  toc()
  
  return(l[[3]]$distrito)
  print('muestra tomada')
}






