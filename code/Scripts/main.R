rm(list = ls())

# Cargamos funciones
source(file = "code\\Scripts\\functions\\utils.R")

# Cargar librerías
pkg  <- c(
    "reticulate",
    "readxl",
    "dplyr",
    "tidyverse"
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
s_bono  <- 100
dias_por_ano  <- 360

# Calculamos la serie de precios del bono
s_bono_k  <- s_bono * exp(r * (0:(n - 1)) * delta)

df_bono  <- data.frame(
    k = 0:(n - 1),
    precio = s_bono_k
)

# Calculamos el retorno logaritmico
df_apple  <- df_apple  %>%
    arrange(Date)  %>%
    mutate(log_return = log(Close / lag(Close)))  %>% 
    drop_na()

# Calculamos el retorno logaritmico anualizado
df_apple  <- df_apple  %>%
    mutate(log_return_annualized_SG = log_return * dias_por_ano,
            log_return_annualized_NL = log(Close/lag(Close, n = dias_por_ano)))  %>% 
            drop_na()

# Calculamos la volatilidad anualizada

cat("\nVolatilidad anualizada: sd(log(Close/Close_{-1}))*sqrt(", dias_por_ano, "):\n")
sd(df_apple$log_return) * sqrt(dias_por_ano)

cat("\nVolatilidad anualizada: sd(log_return * (", dias_por_ano, ")):\n")
sd(df_apple$log_return_annualized_SG)

cat("\nVolatilidad anualizada: sd(log(Close/Close_{", dias_por_ano, "}))\n")
sd(df_apple$log_return_annualized_NL)
