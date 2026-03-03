construir_arbol <- function(n, recombinante = T, chooser = F){
  
  if(recombinante){
    if(chooser){
      return(matrix(list(), nrow = n + 1, ncol = n+1))
    }
    else{
      return(matrix(NA, nrow = n + 1, ncol = n+1))
    }
    
  }
  
  
}

llenar_pagos_finales <- function(arbol_subyacente, expiracion, arbol_opcion, tipo, k, ko = 0){
  for (j in 1:ncol(arbol_subyacente)){
    
    if (tipo == "PUTAM" || tipo =="PUTEU"){
      if (ko == 0){
      arbol_opcion[nrow(arbol_opcion),j] = max(0, k - arbol_subyacente[expiracion + 1, j])
      }
      else{
        if(arbol_subyacente[expiracion + 1, j] >= ko){
          arbol_opcion[nrow(arbol_opcion),j] = max(0, k - arbol_subyacente[expiracion + 1, j])
        }
        else{
          arbol_opcion[nrow(arbol_opcion),j] = 0
        }
      }
    }
    if (tipo == "CHOOSER"){
      arbol_opcion[nrow(arbol_opcion),j] = max(0, k - arbol_subyacente[expiracion + 1, j])
    }
  }
  return(arbol_opcion)
}

valorar_opcion <- function(arbol_subyacente, tipo, n, p, r, delta, k, ko = 0){
  
  if (tipo == "PUTAM"){
      arbol <- construir_arbol(n, T)
    
      arbol <- llenar_pagos_finales(arbol_subyacente, n, arbol, tipo, k, ko)
      arbol <- valorar_opcion_put_americana(arbol, k, p, r, delta, arbol_subyacente, ko)
  }
  else if (tipo == "CHOOSER"){
    arbol <- construir_arbol(n, T, T)
    
    arbol <- llenar_pagos_finales(arbol_subyacente, n, arbol, tipo, k, ko)
    arbol <- valorar_opcion_put_americana(arbol, k, p, r, delta, arbol_subyacente, ko)
  }else if (tipo == "PUTEU"){
    arbol <- construir_arbol(n, T)
    
    arbol <- llenar_pagos_finales(arbol_subyacente, n, arbol, tipo, k, ko)
    arbol <- valorar_opcion_put_europea(arbol, k, p, r, delta, arbol_subyacente, ko)
  }
  return(arbol)
  
}

valorar_opcion_put_americana <- function(arbol, k, p, r, delta, arbol_accion, ko = 0){
  
  for(i in nrow(arbol):2){
    for (j in 1:(i-1)){
      down <- arbol[i, j]
      up <-  arbol[i, j+1]
      
      if (ko == 0){
        arbol[i-1, j] = max(exp(-r*delta)*(p*up + (1-p) * down), max(0, k - arbol_accion[i-1, j]))
      }
      else{
        if(arbol_accion[i-1, j]>=ko){
          arbol[i-1, j] = max(exp(-r*delta)*(p*up + (1-p) * down), max(0, k - arbol_accion[i-1, j]))
        }
        else{
          arbol[i-1, j] = max(exp(-r*delta)*(p*up + (1-p) * down), 0)
          #arbol[i-1, j] = 0
        }
      }
    }
  }
  
  return(arbol)
  
}

valorar_opcion_put_europea <- function(arbol, k, p, r, delta, arbol_accion, ko = 0){
  
  for(i in nrow(arbol):2){
    for (j in 1:(i-1)){
      down <- arbol[i, j]
      up <-  arbol[i, j+1]
      
      arbol[i-1, j] = exp(-r*delta)*(p*up + (1-p) * down)
      
    }
  }
  
  return(arbol)
  
}