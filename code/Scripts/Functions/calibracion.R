calcular_u <- function(r, p, n, sigma){
  u <- r + (1-p)*sqrt(n/(p*(1-p))) * sigma
  return(u)
}

calcular_d <- function(r, p, n, sigma){
  d <- r - p*sqrt(n/(p*(1-p))) * sigma
  return(d)
}

construir_arbol_accion <- function(s0, u, d, n, dt){
  arbol <- matrix(NA, nrow = n+1, ncol= n+1)
  
  for (i in 0:n){
    for(j in 0:i){
      arbol[i+1,j+1] = s0 * exp((d*(i-j)+u*j)*dt)
    }
  }
  return(arbol)
}

construir_serie_bono <- function(b0, r, n, dt){
  bond <- matrix(b_0 * exp(r * (0:(n)) * dt), ncol =1)
  
  return(bond)
}