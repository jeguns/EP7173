
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

# Conjunto de 10 registros simuladoS con 4 atributos
# F1: Factor con niveles A, B, C, D
# F2: Factor con niveles P, Q, R
# Y: Medición cuantitativa
# X: Covariable
# Se desea ejecutar el análisis de un diseño bajo un experimento
# factorial,¿tenemos valores perdidos? ¿cuántos son? 
# ¿en dónde se ubican?

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
datos1 |> gg_miss_case()
datos1 |> gg_miss_var()
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
#X11();datos2 |> select(Y,X) |> marginplot()
datos2 |> gg_miss_upset()

# ================ #
# 03 - datos3.xlsx #
# ================ #

# Conjunto de datos de 20 observaciones y 6 columnas: 
# IQ: Coeficiente intelectual del trabajador
# JP_COM: Desempeño laboral (columna completa)
# JP_MCAR: Desempeño laboral (valores perdidos acorde a un mecanismo MCAR)
# JP_MAR: Desempeño laboral (valores perdidos acorde a un mecanismo MAR)
# JP_MNAR: Desempeño laboral (valores perdidos acorde a un mecanismo MNAR)
# AREA: Área a la que pertenece el trabajador

datos3 = read_excel('03 - datos3.xlsx', na = c("-"))
datos3 |> miss_var_summary()
datos3 |> group_by(AREA) |> miss_var_summary()
datos3 |> gg_miss_var()
datos3 |> gg_miss_fct(fct=AREA)
x11();datos3 |> matrixplot(sortby=1)
datos3 |> matrixplot(sortby=4)

# MCAR vs MAR

datos3 |> 
  bind_shadow() |> 
  group_by(JP_MCAR_NA) |>
  summarise_at(.vars = c("IQ"), 
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

datos3 |>
  bind_shadow() |>
  group_by(JP_MAR_NA) |>
  summarise_at(.vars = c("IQ"), 
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

datos3 |>
  bind_shadow() |>
  group_by(JP_MNAR_NA) |>
  summarise_at(.vars = c("IQ"), 
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

datos3 |> select(IQ,JP_COM,AREA)  |> RBtest()
datos3 |> select(IQ,JP_MCAR,AREA) |> RBtest()
datos3 |> select(IQ,JP_MAR,AREA)  |> RBtest()
datos3 |> select(IQ,JP_MNAR,AREA) |> RBtest()

# Enfoques simples --------------------------------------------------------

# Eliminación de casos

datos3[complete.cases(datos3),]
datos3 |> na.omit()
datos3 |> drop_na()
datos3 |> drop_na(JP_MCAR)
datos3 |> drop_na(any_of(c("JP_MCAR","JP_MNAR")))

datos3$JP_COM  |> mean()
datos3$JP_MCAR |> mean()
datos3$JP_MCAR |> mean(na.rm=T)
datos3$JP_MAR  |> mean(na.rm=T)
datos3$JP_MNAR |> mean(na.rm=T)

datos3 |> select_if(is.numeric) |> cor()
datos3 |> select_if(is.numeric) |> cor(use = "complete.obs")
datos3 |> select_if(is.numeric) |> cor(use = "pairwise.complete.obs")


# Imputación individual Hot Deck

# ================ #
# 03 - datos4.xlsx #
# ================ #

# Conjunto de datos de 7 observaciones y 3 columnas: 
# ACT_FISICA: variable cualitativa que indica si la persona realiza o no actividad física regularmente
# PESO: variable cuantitativa que indica el peso de una persona, en kg
# ALTURA: variable cuantitativa que indica la altura de una persona, en m

datos4 = read_excel("03 - datos4.xlsx")

datos4 |> hotdeck()
datos4 |> hotdeck(variable = "PESO", domain_var = "ACT_FISICA")
datos4 |> hotdeck(variable = "PESO", domain_var = "ACT_FISICA") |> arrange(ACT_FISICA)
datos4 |> hotdeck(variable = "PESO", ord_var = "ALTURA")
datos4 |> hotdeck(variable = "PESO", ord_var = "ALTURA") |> arrange(ALTURA)

# Imputación individual por la media

# ================ #
# 03 - datos5.xlsx #
# ================ #

# Conjunto de datos de 8 observaciones y 3 columnas: 
# x1: variable cuantitativa discreta
# x2: variable cuantitativa continua
# x3: variable cualitativa con 3 categorías

(datos5 = read_excel('03 - datos5.xlsx'))

datos5 |> 
  mutate(x1 = replace(x1, is.na(x1), median(x1, na.rm = TRUE)))

datos5 |> 
  mutate(x1 = replace(x1, is.na(x1), mean(x1, na.rm = TRUE)))

datos5 |> 
  mutate(x1 = replace(x1, is.na(x1), round(mean(x1, na.rm = TRUE))))

datos5 |> 
  mutate_all( ~ ifelse(is.na(.x) & is.numeric(.x), round(mean(.x, na.rm = T)), .))

library(modeest) # moda → función mfv

datos5 |> mutate(x3 = as.factor(x3)) |> na.gam.replace() -> datos5_mod
datos5 |> str()
datos5_mod |> str()
datos5 |> md.pattern()
datos5_mod |> md.pattern()
datos5 |> gg_miss_upset()
datos5_mod |> gg_miss_upset()

# Imputación individual por regresión

# ================ #
# 03 - datos6.xlsx #
# ================ #

# Conjunto de datos de 62 observaciones y 10 columnas, almacenado en el paquete VIM
# Para más detalles consulte en ?VIM::sleep

data(sleep)

sleep |> select(Dream,Sleep,BodyWgt,BrainWgt) -> datos6
datos6 |> head()

datos6 %>% regressionImp(Dream ~ Sleep, data = .) -> datos6a # sí %>%, no |> 
datos6a |> filter(Dream_imp==TRUE) |> arrange(Sleep) |> head()
datos6 |> filter(is.na(Dream)) |> arrange(Sleep) |> head()

datos6 %>% regressionImp(Dream ~ BodyWgt + BrainWgt, data = .) -> datos6b
datos6b |> filter(Dream_imp==TRUE) |>  arrange(BodyWgt,BrainWgt) |> head()
datos6 |> filter(is.na(Dream)) |> arrange(BodyWgt,BrainWgt) |> head()

datos6 |> mice(method = "norm.predict") |> complete() -> datos6c
datos6c |> head()
datos6c |> n_missing() 

datos6 |> mice(method = "norm.nob") |> complete() -> datos6d
datos6d |> head()
datos6d |> n_missing() 

# Imputación por máxima verosimilitud -------------------------------------

dnorm(5.4,5,1)
dnorm(5.4,6,1)

dnorm(5.4,5,1,log=T)
dnorm(5.4,6,1,log=T)

dnorm(5.4,5,1,log=T)+dnorm(0.2,5,1,log=T)+dnorm(4,5,1,log=T)+dnorm(3.8,5,1,log=T)+dnorm(6.3,5,1,log=T)
dnorm(5.4,6,1,log=T)+dnorm(0.2,6,1,log=T)+dnorm(4,6,1,log=T)+dnorm(3.8,6,1,log=T)+dnorm(6.3,6,1,log=T)

# Usando el paquete norm2
p_load(norm2)
set.seed(122)
datos6 |> emNorm() -> estimacion
estimacion |> summary()
estimacion |> mcmcNorm() -> iteraciones
iteraciones |> summary()
iteraciones |> impNorm() |> data.frame() -> datos6e # imputación con norm2, sin transformar
datos6e |> head()
datos6 |> head()

library(psych)
datos6 |> multi.hist(global=FALSE)
pv = NULL
for(i in 1:4){pv[i] = shapiro.test(datos6[,i])$p.value}
pv |> round(4) 

library(car) 
(datos6$Dream + 0.01) |> powerTransform() -> trans_bc_dream
trans_bc_dream$lambda
datos6$Sleep |> powerTransform() -> trans_bc_sleep
trans_bc_sleep$lambda
datos6$BodyWgt |> powerTransform() -> trans_bc_bodywgt
trans_bc_bodywgt$lambda
datos6$BrainWgt |> powerTransform() -> trans_bc_brainwgt
trans_bc_brainwgt$lambda

datos6 |> 
  transmute(Dream = bcPower(Dream+0.01,trans_bc_dream$lambda),
            Sleep = bcPower(Sleep,trans_bc_sleep$lambda),
            BodyWgt = bcPower(Sleep,trans_bc_bodywgt$lambda),
            BrainWgt = bcPower(Sleep,trans_bc_brainwgt$lambda)) -> datos6_boxcox

datos6_boxcox |> multi.hist(global=FALSE)
pv_boxcox = NULL
for(i in 1:4){pv_boxcox[i] = shapiro.test(datos6_boxcox[,i])$p.value}
pv_boxcox |> round(4) 

datos6_boxcox |> emNorm() -> estimacion_boxcox
estimacion_boxcox |> summary()
estimacion_boxcox |> mcmcNorm() -> iteraciones_boxcox
iteraciones_boxcox |> impNorm() -> imputacion_boxcox 
imputacion_boxcox 

p_load(forecast)
imputacion_boxcox |> 
  data.frame() |> 
  mutate(Dream = InvBoxCox(Dream,lambda=trans_bc_dream$lambda)-0.01,
         Sleep = InvBoxCox(Sleep,lambda=trans_bc_sleep$lambda),
         BodyWgt  = InvBoxCox(BodyWgt,lambda=trans_bc_bodywgt$lambda),
         BrainWgt = InvBoxCox(BrainWgt,lambda=trans_bc_brainwgt$lambda)) -> imputacion_boxcox

datos6 |> head() # datos originales
datos6e |> head() # datos imputados sin boxcox
imputacion_boxcox |>  head() # datos imputados con boxcox

# Usando el paquete mix
p_load(mix)
data(stlouis) 
rngseed(44882) 
stlouis |> prelim.mix(3) -> prelim 
prelim |> em.mix() -> estim
prelim |> da.mix(estim,steps=100) -> new_estim
prelim |> imp.mix(new_estim, stlouis) -> imputa
imputa |> head()
stlouis |> head()

# Imputación por KNN ------------------------------------------------------

datos6 
datos6 |> dim() |> prod()
datos6 |> n_missing() 
datos6 |> pct_miss()
datos6 |> pct_miss_var()
datos6 |> pct_miss_case()
datos6 |> skim()
datos6 |> gg_miss_upset()

p_load(matrixStats)
datos6 |> VIM::kNN(methodStand = "range") -> datos6_imp1
datos6 |> VIM::kNN(numFun = weightedMean, weightDist = TRUE, methodStand = "range") -> datos6_imp2
datos6_imp1 |> n_miss()
datos6_imp2 |> n_miss()
datos6_imp1 |> head()
datos6_imp2 |> head()
datos6_imp1 |> gg_miss_upset()
datos6_imp2 |> gg_miss_upset()

# Imputación por K-means --------------------------------------------------

p_load(ClustImpute)

datos6 |> str()

datos6 |> ClustImpute(nr_cluster = 3) -> datos6_imp3
datos6_imp3
datos6_imp3$complete_data
datos6_imp3$clusters
datos6_imp3$centroids

datos6 |> scale() |> data.frame() |> ClustImpute(nr_cluster = 3) -> datos6_imp4
datos6_imp4
datos6_imp4$complete_data
datos6_imp4$clusters
datos6_imp4$centroids
attr(datos6 |> scale(),"scaled:center") -> center
attr(datos6 |> scale(),"scaled:scale") -> scale
t(t(datos6_imp4$complete_data) * scale + center) -> datos6_imputados
datos6_imputados |> head()
datos6 |> head()