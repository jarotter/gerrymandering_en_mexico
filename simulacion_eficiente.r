library(tidyverse)
library(tictoc)
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
  return(
    V %>%
      group_by(distrito) %>%
      filter(distrito != 0) %>%
      summarise(dist_area = sum(area))
  )
}

iso_score_duke <- function(E, V, V_temp, v, dict, perimetros){

  areas <- area(V) %>% 
    pull(dist_area)

  bdys <- update_boundaries(E, V, V_temp, v, dict, perimetros)

  score <- bdys^2/areas

  return(sum(score))
}


# county_score_duke <- function(V){
#   tic("county score")
#   distritos_por_delegacion <- V %>%
#     group_by(delegacion) %>%
#     summarise(num_distritos = distrito %>% unique() %>% length()) %>%
#     filter(num_distritos >= 2) %>%
#     filter(!is.na(delegacion))
#   
#   w2 <- V %>%
#     filter(!is.na(distrito)) %>%
#     group_by(distrito, delegacion) %>%
#     count() %>%
#     arrange(delegacion, n) %>%
#     group_by(delegacion) %>%
#     summarise(sec_largest = n %>% sort(na.last = TRUE, decreasing = TRUE) %>% '['(2),
#               n_tot = sum(n)) %>%
#     filter(!is.na(sec_largest)) %>%
#     filter(!is.na(delegacion)) %>%
#     mutate(proporcion = sec_largest / n_tot) %>%
#     summarise(score = proporcion %>% sqrt() %>% sum())
#   
#   w3 <- V %>%
#     filter(!is.na(distrito)) %>%
#     group_by(distrito, delegacion) %>%
#     count() %>%
#     group_by(delegacion) %>%
#     summarise(all_but_two = n %>% sort(na.last = NA, decreasing = TRUE) %>% '['(-c(1,2)) %>% sum(),
#               n_tot = sum(n)) %>%
#     arrange(delegacion) %>%
#     mutate(proporcion = all_but_two / n_tot) %>%
#     summarise(score = proporcion %>% sqrt() %>% sum())
#   
#   
#   cuantos_2 <- sum(distritos_por_delegacion$num_distritos == 2)
#   cuantos_mas <- sum(distritos_por_delegacion$num_distritos > 2)
#   
#   toc()
#   return(w2*cuantos_2 + w3*100*cuantos_mas)
#   
# }


##Intentar usar el estado anterior para bajar complejidad 
revisar_conexidad <- function(G, E, V, V_temp, u){
  
  dis <- V[dict[u], 'distrito']
  
  G_temp <- set.vertex.attribute(G, 'distrito', dict[u], dis)

  indices <- which(get.vertex.attribute(G_temp, 'distrito') == dis)
  
  n_comps <- G_temp %>%
    induced_subgraph(indices) %>%
    count_components()
  
  return(list(n_comps == 1, G_temp))
}


score <- function(E, V, V_temp, v, wd, wp, wi, dict, perimetros, conteo_delegaciones_temp){
  return(wd*county_score(conteo_delegaciones_temp) + wp*pop_score_duke(V) + wi*iso_score_duke(E, V, V_temp, v, dict, perimetros))
}

una_iteracion <- function(G, E, V, beta, wd, wp, wi, dict, perimetros, conteo_delegaciones){
  l <- generar_nuevo_estado(E, V, dict)
  
  V_temp <- l[[1]]
  u <- l[[2]]
  
  revcon <- revisar_conexidad(G, E, V, V_temp, u)
  
  if(revcon[[1]]){
    new_conflicting <- detect_conflicting_lim_scope(E, V_temp, u, dict)
    conteo_delegaciones_temp <- update_county_score(V, V_temp, u, conteo_delegaciones)

    rho <- sum(E$conflictivos) %>%
      '/'(sum(new_conflicting))
    if(beta != 0){
      rho <- rho * (exp(-beta*(score(E, V, V_temp, u, wd, wp, wi, dict, perimetros, conteo_delegaciones)
                               -score(E, V, V, u, wd, wp, wi, dict, perimetros, conteo_delegaciones_temp))))
    }
    if(runif(1) < rho){
      G <- revcon[[2]]
      #print('Cambio aceptado con rho igual a ')
      #print(rho)
      E_temp <- E
      E_temp$conflictivo <- new_conflicting
      return(list(G, E_temp, V_temp))
    }
  }
  return(list(G, E, V))
}

generar_nuevo_estado <- function(E, V, dict){
  temp <- V
  conflicting <- which(E$conflictivos & E$to!=6000 & E$from!=6000)
  
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


take_one_sample <- function(G, E, V, wd, wp, wi, dict, perimetros, conteo_delegaciones){
  l <- list(G, E, V)
  
  tic('primer for externo')
  for(i in 1:4000){
    if(i %% 1001 == 1){
      print(i)
    }
    #tic('Una iteración toma')
    l <- una_iteracion(l[[1]], l[[2]], l[[3]], 0, wd, wp, wi, dict, perimetros, conteo_delegaciones)
    #toc()
  }
  toc()
  
  lin_beta <- seq(from = 0, to = 1, length.out = 6000)
  tic('segundo for externo')
  for(i in 1:6000){
    if(i %% 1001 == 1){
      print(i)
    }
    tic('Una iteración real tarda')
    l <- una_iteracion(l[[1]], l[[2]], l[[3]], lin_beta[i], wd, wp, wi, dict, perimetros, conteo_delegaciones)
    toc()
  }
  toc()
  
  tic('tercer for interno')
  for(i in 1:2000){
    if(i %% 1001 == 1){
      print(i)
    }
    l <-  una_iteracion(l[[1]], l[[2]], l[[3]], 1, wd, wp, wi, dict, perimetros, conteo_delegaciones)
  }
  toc()
  
  return(l[[3]]$distrito)
  print('muestra tomada')
}

inicializar_county_score <- function(V, dict){
  conteo <- rep(0, 24*17) %>%
    matrix(nrow = 17)
    n <- nrow(V)
  for(i in 1:n){
    dis <- V[i, 'distrito']
    del <- V[i, 'delegacion']
    conteo[del, dis] <- conteo[del, dis] + 1
  }
  return(conteo)
}

update_county_score <- function(V, V_temp, u, conteo_delegaciones){
  u <- dict[u]
  old_dist <- V[u, 'distrito'] %>% as.integer()
  new_dist <- V_temp[u, 'distrito'] %>% as.integer()
  del <- V[u, 'delegacion'] %>% as.integer()
  
  if(old_dist != new_dist){
    conteo_delegaciones[del, new_dist] <- conteo_delegaciones[del, new_dist] + 1
    conteo_delegaciones[del, old_dist] <- conteo_delegaciones[del, old_dist] - 1
  }
  return(conteo_delegaciones)
  
}

county_score <- function(conteo_delegaciones_temp){

  w2 <- wm2 <-  0
  n2 <- nm2 <- 0
  for(i in 1:17){
    if(sum(conteo_delegaciones_temp[i,] > 0) == 2){
      w2 <- conteo_delegaciones_temp[i,] %>%
        sort(decreasing = TRUE) %>%
        '['(2) %>%
        '/'(sum(conteo_delegaciones_temp[i,])) %>%
        sqrt() %>%
        '+'(w2)
      n2 <- n2 + 1
    }
    else if(sum(conteo_delegaciones_temp[i,] > 0) > 2){
      wm2 <- conteo_delegaciones[i,] %>%
        sort(decreasing = TRUE) %>%
        '['(-c(1,2)) %>%
        sum() %>%
        '/'(sum(conteo_delegaciones_temp[i,])) %>%
        sqrt() %>%
        '+'(wm2)
      nm2 <- nm2 + 1
    }
  }

  return(w2*n2 + 100*nm2*wm2)
  
}
