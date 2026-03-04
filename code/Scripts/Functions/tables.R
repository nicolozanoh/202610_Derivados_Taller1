library(knitr)
library(kableExtra)

calibration_table <- function(s_0, n, delta, q_star, r, dias_por_ano, sd_yearly, u, d, b_0, ko){

  params_table <- data.frame(
    Parameter = c(
      "Precio inicial de la acción", "Número de pasos", "Tamaño del paso",
      "Probabilidad riesgo-neutral", "Tasa libre de riesgo",
      "Días de trading/año", "Volatilidad anualizada",
      "Factor alcista", "Factor bajista",
      "Precio inicial del bono", "Tasa de crecimiento del bono",
      "Strike (Put Am. y KO)", "Barrera knock-out", "Strike (Chooser)"
    ),
    Symbol = c(
      "$S_0$", "$n$", "$\\delta$", "$q^*$", "$r$",
      "--", "$\\sigma$", "$u$", "$d$",
      "$B_0$", "$r$",
      "$K$", "$KO$", "$K$"
    ),
    Value = c(
      round(s_0,2), round(n), round(delta, 2), round(q_star,2), round(r,2),
      round(dias_por_ano), round(sd_yearly,2), round(u, 2), round(d, 2),
      round(b_0), round(r, 2),
      300, ko, 310
    )
  )
  
  params_table$Value <- formatC(params_table$Value, format = "f", digits = 2)
  
  latex_table <- kable(
    params_table,
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("Parámetro", "Símbolo", "Valor"),
    caption   = "Parámetros del Modelo -- Árbol Binomial de Valoración de Opciones",
    label     = "parametros",       
    align     = c("l", "c", "r")
  ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    pack_rows("Mercado y Árbol", 1, 5,  bold = TRUE, latex_gap_space = "0.5em") %>%
    pack_rows("Volatilidad",     6, 9,  bold = TRUE, latex_gap_space = "0.5em") %>%
    pack_rows("Bono",           10, 11, bold = TRUE, latex_gap_space = "0.5em") %>%
    pack_rows("Opciones",       12, 14, bold = TRUE, latex_gap_space = "0.5em")

  
  writeLines(latex_table, "output/parameters_table.tex")
}

tree_table <- function(arbol, title, file, label){
  arbol_mat <- t(arbol)
  
  # Reemplazar NA con cadena vacía para presentación
  arbol_display <- apply(arbol_mat, 2, function(col) {
    ifelse(is.na(col), "", formatC(col, format = "f", digits = 2))
  })
  
  arbol_display <- as.data.frame(arbol_display)
  colnames(arbol_display) <- paste0("$t_{", 0:12, "}$")
  rownames(arbol_display) <- NULL
  
  latex_arbol <- kable(
    arbol_display,
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = paste("Árbol Binomial --", title),
    label    = label,
    align    = rep("r", 13)
  ) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down"),  # scale_down porque son 13 columnas
      font_size     = 8
    ) %>%
    add_header_above(c("Paso" = 13), escape = FALSE)
  
  writeLines(latex_arbol, paste0("output/", file,".tex"))
}