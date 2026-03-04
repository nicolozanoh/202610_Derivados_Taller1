rm(list = ls())

# Cargamos funciones
source(file = "code\\Scripts\\functions\\utils.R")
source(file = "code\\Scripts\\functions\\calibracion.R")
source(file = "code\\Scripts\\functions\\valoracion_opciones.R")
source(file = "code\\Scripts\\functions\\tables.R")

# Cargar librerías
pkg  <- c(
    "reticulate",
    "readxl",
    "dplyr",
    "tidyverse",
    "ggplot2",
    "knitr",
    "kableExtra"
)

load_packages(pkg)

use_python("taller1deriv/Scripts/python.exe", required = TRUE)

# Revisamos si los archivos de datos existen.
if (!file.exists("data/raw/AAPL_price.xlsx")) {
    source_python("code/Scripts/descargar data.py")
}

if (!file.exists("data/raw/data bloomberg.xlsx")) {
    stop("El archivo 'data bloomberg.xlsx' no se encuentra en 'data/raw'.")
}

# Cargamos los datos
df_apple <- read_excel("data/raw/AAPL_price.xlsx")
df_bloomberg <- read_excel("data/raw/data bloomberg.xlsx")

## Punto 2

# Variables
s_0  <- 300
n  <- 12
delta  <- 1 / n
q_star  <-  0.5
r  <- 0.04
b_0  <- 100
dias_por_ano  <- 252

# Calculamos la serie de precios del bono

bono <- construir_serie_bono(b_0, r, n, delta)

# Calculamos el retorno logaritmico
df_apple  <- df_apple  %>%
    arrange(Date)  %>%
    mutate(log_return = log(Close / lag(Close)))  %>%
    drop_na()

df_apple <- df_apple[df_apple$Date <= "2026-02-13",]

df_apple <- df_apple[df_apple$Date >= "2025-01-01",]

# Calculamos la volatilidad anualizada
#sd_yearly  <- sd(df_apple$log_return) * sqrt(dias_por_ano)

sd_yearly <- 0.32
cat("La volatilidad anualizada del retorno logarítmico de las acciones de Apple es:", sd_yearly, "\n")

u <- calcular_u(r, q_star, n, sd_yearly)
d <- calcular_d(r, q_star, n, sd_yearly)

u
d

# Calculamos el precio de la accion de booble en cada nodo
arbol_booble <- construir_arbol_accion(s_0, u, d, n, delta)

# Put Americana con Expiracion a 1 año y strike 300
k <- 300

arbol_a <- valorar_opcion(arbol_booble, "PUTAM", n, q_star, r, delta, k)

# Put Americana con Expiracion a 1 año, strike 300 y ko 240
k <- 300
ko <- 240

arbol_b <- valorar_opcion(arbol_booble, "PUTAM", n, q_star, r, delta, k, ko)

arbol_bb <- valorar_opcion(arbol_booble, "PUTEU", n, q_star, r, delta, k, ko)

# Chooser ar the money
#k <- 310
#arbol_b <- valorar_opcion(arbol_booble, "CHOOSER", n, q_star, r, delta, k, ko)

# sacamos tablas para el documento

calibration_table(s_0, n, delta, q_star, r, dias_por_ano, sd_yearly, u, d, b_0, ko)
tree_table(arbol_booble, "Precio del Subyacente", "arbol_subyacente")
tree_table(arbol_a, "Opci\'on Americana", "arbol_puntoa")
#tree_table(arbol_b_eur)