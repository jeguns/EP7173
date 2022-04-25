
# Paquetes ----------------------------------------------------------------

library(pacman)
p_load(readxl,tidyr, skimr, dplyr, naniar, mice, VIM, ggplot2, gam, RBtest, 
       tidymodels, norm, cat)
#install.packages("ForImp")
library(ForImp)

# Identificación ----------------------------------------------------------

# ================ #
# 03 - datos1.xlsx #
# ================ #

# Conjunto de datos simulado con 4 atributos
# F1: Factor con niveles A, B, C, D
# F2: Factor con niveles P, Q, R
# Y: Medición cuantitativa
# X: Covariable
# Se desea ejecutar el análisis de un diseño bajo un experimento factorial, 
# ¿tenemos valores perdidos? ¿cuántos son? ¿en dónde se ubican?

(datos1 <- read_excel("03 - datos1.xlsx"))

(datos1 |> complete(F1,F2) -> datos1)

# ¿Tenemos valores perdidos?
datos1 |> any_na()

# ¿Cuántos son los datos perdidos?
datos1 |> n_missing() 
datos1 |> n_miss()
datos1 |> prop_miss()
datos1 |> pct_miss()
datos1 |> n_complete() 

# ¿En qué columnas o filas están los datos perdidos?
datos1 |> skim() 
datos1 |> miss_var_summary()
datos1 |> miss_var_table()
datos1 |> miss_case_summary()
datos1 |> miss_case_table()
datos1 |> vis_miss() 
datos1 |> gg_miss_upset()
apply(is.na(datos1), 2, which) 
datos1 |> missingness()
datos1 |> is.na() |> colSums()

# ¿Cómo podemos visualizar la ubicación de los datos perdidos?
datos1 |> is.na() 
datos1 |> as_shadow()
datos1 |> bind_shadow()
datos1[!complete.cases(datos1),]

# ================ #
# 03 - datos2.xlsx #
# ================ #

# Conjunto de datos simulado con 4 atributos
# F1: Factor con niveles A, B, C, D
# F2: Factor con niveles P, Q, R
# Y: Medición cuantitativa
# X: Covariable
# Se desea ejecutar el análisis de un diseño bajo un experimento factorial, 
# ¿tenemos valores perdidos? De ser así, ¿cómo lidiaremos con ellos?

(datos2 <- read.csv2("03 - datos2.csv"))
datos2 |> skim()

# ¿Cómo podemos asignar datos perdidos?

datos2 |> 
  mutate(F1 = replace(F1, F1==" ",NA),
         F2 = na_if(F2, "")) |> 
  replace_with_na(list(Y = c(98,99,999))) -> datos2_a
datos2_a |> skim()

common_na_strings
common_na_numbers
cadenas_comunes = c(common_na_strings, " ")
numeros_comunes = c(common_na_numbers,98,99)
(datos2 |> 
    replace_with_na_all(condition = ~.x %in% cadenas_comunes) |> 
    replace_with_na_all(condition = ~.x %in% numeros_comunes) -> datos2)

# Explorando patrones
datos2 |> md.pattern()
datos2 |> md.pairs()
datos2 |> select(Y,X) |> marginplot()
datos2 |> gg_miss_upset()

