
# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(writexl)
library(lubridate)
library(readr)
library(car)
library(fastDummies)
library(skimr)
library(GGally)

# Integración -------------------------------------------------------------

d1 = data.frame(Estudiante = c("Huapaya","Gavidia","Navarro","Villafuerte","Ayllon","Gutierrez","Jaimes"),
                Carrera = c("Industrias","Biología","Forestal","Ambiental","Agronomía","Agronomía","Economía"))
d2 = data.frame(Carrera = c("Agronomía","Agrícola","Ambiental","Biología","Industrias"), 
                Facultad = c("Agronomía","Agrícola","Ciencias","Ciencias","Industrias"))
d1 |> inner_join(d2)
d1 |> semi_join(d2)
d1 |> left_join(d2)
d2 |> right_join(d1)
d1 |> right_join(d2)
d1 |> anti_join(d2)
d1 |> full_join(d2)

read.csv2('02 - estudiantes.csv') -> datos_estudiantes
read_excel('02 - carreras.xlsx') -> datos_carreras
read_xls('02 - zonas.xls') -> datos_zonas

datos_estudiantes |> 
  inner_join(datos_zonas) |> 
  inner_join(datos_carreras) -> datos_integrados

write_xlsx(x = datos_integrados, path = "02 - integrado.xlsx")

datos_integrados |> 
  filter(ZONA == "ESTE" & FACULTAD == "Economía") |> 
  count()

# Transformación ----------------------------------------------------------

read_excel("02 - notas.xlsx") -> datos_notas
datos_integrados |> inner_join(datos_notas, 
                               by = c("NOMBRE"="NOMBRE",
                                      "APELLIDO1"="APELLIDOP",
                                      "APELLIDO2"="APELLIDOM")) -> datos_transformacion

datos_transformacion |> 
  mutate('NOMBRE COMPLETO' = paste(NOMBRE,APELLIDO1,APELLIDO2)) |> 
  mutate(FNAC = FNAC |> as.Date(format="%d/%m/%Y")) |> 
  mutate(EDAD = ((today()-FNAC)/365) |> floor() |> as.numeric()) -> datos_transformado

a <- "20/01/2022"
a |> str()
a1 <- a |> dmy()
a1
a1 |> str()

b <- "2022.03.28"
b |> str()
b1 <- b |> ymd()
b1
b1 |> str()

d <- "28-Mar-2022"
d |> str()
d1 <- d |> dmy()
d1
d1 |> str()
d1 |> year()
d1 |> month()
d1 |> week()
d1 |> day()
d1 |> yday()  
d1 |> wday()  

datos_transformacion |> 
  mutate('NOMBRE COMPLETO' = paste(NOMBRE,APELLIDO1,APELLIDO2)) |> 
  mutate(FNAC = FNAC |> dmy()) |> 
  mutate(EDAD = ((today()-FNAC)/365) |> floor() |> as.numeric()) -> datos_transformado

datos_transformado$NOTA |> hist()
datos_transformado$NOTA |> shapiro.test()
datos_transformado$NOTA |> powerTransform() -> trans_bc
trans_bc$lambda
bcPower(datos_transformado$NOTA,trans_bc$lambda) |> hist()
bcPower(datos_transformado$NOTA,trans_bc$lambda) |> shapiro.test()
datos_transformado |> 
  mutate(NOTA1 = bcPower(NOTA,trans_bc$lambda)) -> datos_transformado

datos_transformado |> 
  mutate(RANGO_EDAD = cut(EDAD, 
                          breaks = c(0,11,17,29,59,120),
                          labels = c("Niño","Adolescente","Joven","Adulto","Adulto mayor"))) -> datos_transformado

datos_transformado |> 
  dummy_cols(select_columns = "ZONA") |> 
  select(-ZONA_CALLAO) -> datos_transformado

datos_transformacion |> 
  mutate('NOMBRE COMPLETO'= paste(NOMBRE,APELLIDO1,APELLIDO2),
         FNAC       = FNAC |> dmy(),
         EDAD       = ((today()-FNAC)/365) |> ceiling()  |> as.numeric(),
         NOTA1      = bcPower(NOTA,trans_bc$lambda),
         RANGO_EDAD = cut(EDAD, 
                          breaks = c(-Inf,11,17,29,59,Inf),
                          labels = c("Niño","Adolescente","Joven","Adulto","Adulto mayor"))) |> 
  dummy_cols(select_columns = "ZONA") |> 
  select(-ZONA_CALLAO) |> 
  mutate_at(vars(starts_with("ZONA")), as.factor) -> datos_transformado

write.csv(datos_transformado, '02 - transformado.csv', row.names = FALSE)

datos_transformado |> mutate_if(is.numeric,log)

datos_transformado |> mutate_at(c("NOMBRE","APELLIDO1"),toupper)

datos_transformado |> transmute('NOMBRE COMPLETO'= paste(NOMBRE,APELLIDO1,APELLIDO2))
         
datos_transformado |> transmute_if(is.character,toupper)

datos_transformado |> transmute_if(is.numeric,log)

datos_transformado |> transmute_at(c("NOMBRE","APELLIDO1"),toupper)

# Normalización -----------------------------------------------------------

datos_transformado -> datos_normalizacion

xmax <- function(x, na.rm = TRUE) {
  return(x/max(x))}
datos_normalizacion |> mutate(NOTA2 = NOTA |> xmax()) -> datos_normalizados1
datos_normalizados1 |> pull(NOTA2) |> hist()

minmax <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))}
datos_normalizacion |> mutate(NOTA2 = NOTA |> minmax()) -> datos_normalizados2
datos_normalizados2 |> pull(NOTA2) |> hist()

minmaxab <- function(x, a, b, na.rm = TRUE) {
  return(a+(x- min(x))/(max(x)-min(x))*(b-a))}
datos_normalizacion |> mutate(NOTA2 = NOTA |> minmaxab(0,10)) -> datos_normalizados3
datos_normalizados3 |> pull(NOTA2) |> hist()

datos_normalizacion |> mutate(NOTA2 = NOTA |> scale()) -> datos_normalizados4
datos_normalizados4 |> pull(NOTA2) |> hist()

edecimal <- function(x, j, na.rm = TRUE) {
  return(x/10^j)}
datos_normalizacion |> mutate(NOTA2 = NOTA |> edecimal(2)) -> datos_normalizados5
datos_normalizados5 |> pull(NOTA2) |> hist()

write.csv(datos_normalizados4, '02 - normalizado.csv', row.names = FALSE)

# Limpieza ----------------------------------------------------------------

datos_normalizados4 -> datos_limpieza

datos_limpieza |> skim()

datos_limpieza |> pull(FACULTAD) |> table()
datos_limpieza |> 
  mutate(FACULTAD = ifelse(FACULTAD=="Meteorología","Ciencias",FACULTAD)) -> datos_limpio

datos_limpio |> 
  filter(FACULTAD=="Ciencias" & EDAD %in% c(20,21)) -> datos_limpio1

datos_limpio |> 
  filter(FACULTAD=="Zootecnia" & EDAD >=18 & EDAD < 24) -> datos_limpio2

datos_limpio |> 
  filter(FNAC >= ymd("2000-01-01")) -> datos_limpio3

datos_limpio |> 
  filter(!ZONA %in% c("CENTRO","SUR") & NOTA<10) -> datos_limpio4

# Reducción ---------------------------------------------------------------

datos_limpio -> datos_reduccion

datos_reduccion |> 
  select(-NOMBRE,-APELLIDO1,-APELLIDO2,-FNAC,-NOTA,-NOTA1,-EDAD) |> 
  select(`NOMBRE COMPLETO`, DISTRITO, ZONA_CENTRO, ZONA_ESTE, ZONA_NORTE, ZONA_SUR, 
         RANGO_EDAD,CARRERA,FACULTAD,NOTA2)-> datos_final

datos_limpio |> select_if(is.numeric)

datos_limpio |> select_at(vars(starts_with('APE')),tolower)

datos_limpio |> select_at(c("NOMBRE","APELLIDO1"),tolower)

datos_final |> skim()

