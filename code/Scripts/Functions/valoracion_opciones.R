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

construir_arbol_chooser <- function(n, shout){
  n_arboles_ini <- (n-shout) + 1
  n_arbol_final <- shout + 1
  
  print(n_arbol_final)
  
  arbol_call <- matrix(NA, nrow =n + 1, ncol = n+1)
  arbol_put <- matrix(NA, nrow = n + 1, ncol = n+1)
  arbol_final <- matrix(NA, nrow = n + 1, ncol = shout+1)
  
  return (list(call = arbol_call, put = arbol_put, final = arbol_final))
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
      
      ## primer elemento es put segundo elemento es call
      val <- c(max(0, k - arbol_subyacente[expiracion + 1, j]), max(0, arbol_subyacente[expiracion + 1, j] - k))
      arbol_opcion[[nrow(arbol_opcion),j]] = val
    }
  }
  return(arbol_opcion)
}

valorar_opcion <- function(arbol_subyacente, tipo, n, p, r, delta, k, ko = 0, shout = 0){
  
  if (tipo == "PUTAM"){
      arbol <- construir_arbol(n, T)
    
      arbol <- llenar_pagos_finales(arbol_subyacente, n, arbol, tipo, k, ko)
      arbol <- valorar_opcion_put_americana(arbol, k, p, r, delta, arbol_subyacente, ko)
  }
  else if (tipo == "CHOOSER"){
    arboles <- construir_arbol_chooser(n, shout)
    
    arbol_call <- arboles$call
    arbol_put <- arboles$put
    arbol_final <- arboles$final
    
    print(arbol_call)
    print(arbol_put)
    print(arbol_final)

    arbol <- valorar_chooser(arbol_call, arbol_put, arbol_final, k, p, r, delta, arbol_subyacente, shout, n)
    
  }else if (tipo == "PUTEU"){
    arbol <- construir_arbol(n, T)
    
    arbol <- llenar_pagos_finales(arbol_subyacente, n, arbol, tipo, k, ko)
    arbol <- valorar_opcion_put_europea(arbol, k, p, r, delta, arbol_subyacente, ko)
  }
  return(arbol)
  
}

valorar_chooser <- function(arbol_call, arbol_put, arbol_final, k, p, r, delta, arbol_subyacente, shout, n){
  
  ## LLENAMOS PAGOS FINALES
  
  for (j in 1:ncol(arbol_call)){
 
        arbol_call[nrow(arbol_call),j] = max(0,arbol_subyacente[n + 1, j] -k)
        arbol_put[nrow(arbol_put),j] = max(0, k - arbol_subyacente[n + 1, j])
      
    }
    
  for(i in nrow(arbol_call):(shout+2)){
    for (j in 1:(i-1)){
      if (i>(shout+1)){
      call_down <- arbol_call[i, j]
      call_up <-  arbol_call[i, j+1]
      
      put_down <- arbol_put[i, j]
      put_up <-  arbol_put[i, j+1]
      
      arbol_call[i-1, j] <-  exp(-r*delta)*(p*call_up + (1-p) * call_down)
      arbol_put[i-1, j] <-  exp(-r*delta)*(p*put_up + (1-p) * put_down)
      }
    }
  }
  
  for (j in 1:(shout+1)){
    
    val_call <- arbol_call[shout+1, j]
    val_put <- arbol_put[shout+1, j]
    
    
    arbol_final[shout+1, j] = max(val_call, val_put)
    
  }
  
  for(i in (shout+1):2){
    for (j in 1:(i-1)){
      up <- arbol_final[i, j]
      down <-  arbol_final[i, j+1]
      
      
      arbol_final[i-1, j] = exp(-r*delta)*(p*up + (1-p) * down)
    }
  }
  
  
  return (list(call = arbol_call, put=arbol_put, final=arbol_final))
  
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